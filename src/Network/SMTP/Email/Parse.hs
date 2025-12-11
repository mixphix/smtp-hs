{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Network.SMTP.Email.Parse
  ( Email
  , Mailbox (..)
  , mailboxText
  , email
  , mailbox
  , mailboxes
  , unsafeEmail
  , domainPart
  , emailText
  , emailByteString
  , localPart
  , validEmail
  , validMailbox
  , validateEmail
  , validateMailbox
  , validateMailboxes
  )
where

import Control.Applicative (Alternative (empty))
import Control.Applicative.Combinators.NonEmpty qualified as Parse (some)
import Control.Monad (join, void, when)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Foldable (fold)
import Data.Foldable1 (fold1)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, toList)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text (encodeUtf8)
import Data.Word (Word8)
import Language.Haskell.TH (ExpQ, listE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Text.Parsec
  ( ParsecT
  , Stream
  , char
  , crlf
  , digit
  , eof
  , oneOf
  , parse
  , sepEndBy1
  , tab
  , (<?>)
  )
import Text.Parsec qualified as Parse
import Text.Parsec.Text (Parser)

data Mailbox = Mailbox
  { mailboxName :: Maybe Text
  , mailboxEmail :: Email
  }

-- | Turn a 'Mailbox' value into a reparseable string.
mailboxText :: Mailbox -> Text
mailboxText Mailbox{..} = case mailboxName of
  Nothing -> emailText mailboxEmail
  Just mn ->
    fold
      [ mn
      , " <"
      , emailText mailboxEmail
      , ">"
      ]

-- |
-- Abstract data type representing an email address.
--
-- Use 'localPart' and 'domainPart' to extract those substrings,
-- and 'emailText' or 'emailByteString' for the complete address.
data Email = Email !Text !Text

unsafeEmail :: Text -> Text -> Email
unsafeEmail = Email

email :: QuasiQuoter
email =
  QuasiQuoter
    { quoteExp = qemail emailexp
    , quotePat = error "email is not supported as a pattern"
    , quoteDec = error "email is not supported at top-level"
    , quoteType = error "email is not supported as a type"
    }
 where
  qemail p s =
    case validateEmail $ Text.pack s of
      Left err -> error $ "Invalid quasi-quoted email address: " <> err
      Right e -> p e

emailexp :: Email -> ExpQ
emailexp e =
  let lp = localPart e
      dp = domainPart e
   in [|Email lp dp|]

mailbox :: QuasiQuoter
mailbox =
  QuasiQuoter
    { quoteExp = qmailbox mailboxexp
    , quotePat = error "mailbox is not supported as a pattern"
    , quoteDec = error "mailbox is not supported at top-level"
    , quoteType = error "mailbox is not supported as a type"
    }
 where
  qmailbox p s =
    case validateMailbox $ Text.pack s of
      Left err -> error $ "Invalid quasi-quoted mailbox: " <> err
      Right e -> p e

mailboxexp :: Mailbox -> ExpQ
mailboxexp Mailbox{..} = [|Mailbox mailboxName $(emailexp mailboxEmail)|]

mailboxes :: QuasiQuoter
mailboxes =
  QuasiQuoter
    { quoteExp = qmailbox (listE . map mailboxexp . toList)
    , quotePat = error "mailboxes is not supported as a pattern"
    , quoteDec = error "mailboxes is not supported at top-level"
    , quoteType = error "mailboxes is not supported as a type"
    }
 where
  qmailbox p s =
    case validateMailboxes $ Text.pack s of
      Left err -> error $ "Invalid quasi-quoted mailbox list: " <> err
      Right e -> p e

emailText :: Email -> Text
emailText (Email l d) = l <> "@" <> d

emailByteString :: Email -> ByteString
emailByteString (Email l d) = Text.encodeUtf8 l <> "@" <> Text.encodeUtf8 d

localPart, domainPart :: Email -> Text
localPart (Email l _) = l
domainPart (Email _ d) = d

validEmail :: Text -> Bool
validEmail = not . null . validateEmail

validMailbox :: Text -> Bool
validMailbox = not . null . validateMailbox

validateEmail :: Text -> Either String Email
validateEmail = first show . parse (addrSpec' <* eof) ""

validateMailbox :: Text -> Either String Mailbox
validateMailbox = first show . parse (mailbox_ <* eof) ""

validateMailboxes :: Text -> Either String (NonEmpty Mailbox)
validateMailboxes = first show . parse (mailboxList <* eof) ""

-- Parsing

blank :: (Applicative f, Monoid m) => f m
blank = pure mempty

ignore :: (Applicative f, Monoid m) => f x -> f m
ignore = (*> blank)

fsome :: (Stream s m t, Semigroup a) => ParsecT s u m a -> ParsecT s u m a
fsome = fmap fold1 . Parse.some

fmany :: (Stream s m t, Monoid a) => ParsecT s u m a -> ParsecT s u m a
fmany = fmap fold . Parse.many

option :: (Stream s m t) => a -> ParsecT s u m a -> ParsecT s u m a
option x = Parse.option x . Parse.try

infix 2 ?

(?) :: (Alternative f) => Bool -> a -> f a
b ? a = if b then pure a else empty

-- backtracking
infixl 4 <|>

(<|>) :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
a <|> b = Parse.choice [Parse.try a, Parse.try b]

-- backtracking
choice :: (Stream s m t) => [ParsecT s u m a] -> ParsecT s u m a
choice ps = Parse.choice (Parse.try <$> ps)

atLeast :: (Stream s m t) => Word -> ParsecT s u m a -> ParsecT s u m [a]
atLeast n p = case n of
  0 -> Parse.many p
  _ -> liftA2 (:) p $ atLeast (n - 1) p

upto :: (Stream s m t) => Word -> ParsecT s u m a -> ParsecT s u m [a]
upto n p = case n of
  0 -> blank
  _ -> liftA2 (:) p (upto (n - 1) p) <|> blank

btwn :: (Stream s m t) => Word -> Word -> ParsecT s u m a -> ParsecT s u m [a]
btwn n m p = case n of
  0 -> upto m p
  _ -> liftA2 (:) p $ btwn (n - 1) (m - 1) p

string :: (Stream s m Char) => Text -> ParsecT s u m Text
string = fmap Text.pack . Parse.string . Text.unpack

tmany :: ParsecT s u m Char -> ParsecT s u m Text
tmany p = Text.pack <$> Parse.many p

tsome :: (Stream s m t) => ParsecT s u m Char -> ParsecT s u m Text
tsome p = Text.pack . toList <$> Parse.some p

tatLeast :: (Stream s m t) => Word -> ParsecT s u m Char -> ParsecT s u m Text
tatLeast n p = case n of
  0 -> tmany p
  _ -> liftA2 Text.cons p (tatLeast (n - 1) p)

tupto :: (Stream s m t) => Word -> ParsecT s u m Char -> ParsecT s u m Text
tupto n p = case n of
  0 -> blank
  _ -> liftA2 Text.cons p (tupto (n - 1) p) <|> blank

tbtwn ::
  (Stream s m Char) =>
  Word ->
  Word ->
  ParsecT s u m Char ->
  ParsecT s u m Text
tbtwn n m p = case n of
  0 -> tupto m p
  _ -> liftA2 Text.cons p $ tbtwn (n - 1) (m - 1) p

tcount :: (Stream s m Char) => Word -> ParsecT s u m Char -> ParsecT s u m Text
tcount n p = case n of
  0 -> blank
  _ -> liftA2 Text.cons p (tcount (n - 1) p)

ranges :: [[Word8]] -> Parser Char
ranges rs = oneOf $ foldMap (map $ chr . fromIntegral) rs

vchar :: Parser Char
vchar = oneOf ['!' .. '~']

nul, sp, cr, lf :: Parser Char
nul = char '\0'
sp = char ' '
cr = char '\r'
lf = char '\n'

-- | Equivalent to @[0-9A-Za-z]@.
alphaNum :: Parser Char
alphaNum = ranges [[48 .. 57], [65 .. 90], [97 .. 122]]

wsp :: Parser Char
wsp = sp <|> tab

-- 3.2.1

quotedPair :: Parser Text
quotedPair =
  (string "\\" <> (tcount 1 vchar <|> tcount 1 wsp))
    <|> obsQP

-- 3.2.2

fws :: Parser Text
fws =
  (option "" (tmany wsp <> tcount 1 crlf) <> tsome wsp)
    <|> obsFws

ctext :: Parser Char
ctext =
  ranges [[33 .. 39], [42 .. 91], [93 .. 126]]
    <|> obsCtext

ccontent :: Parser Text
ccontent = tcount 1 ctext <|> quotedPair <|> Parse.try comment

comment :: Parser Text
comment =
  fold
    [ string "("
    , fmany (option "" fws <> ccontent <> option "" fws)
    , string ")"
    ]

cfws :: Parser Text
cfws =
  (option "" fws <> fsome comment <> option "" fws)
    <|> fws

-- 3.2.3

atext :: Parser Char
atext = alphaNum <|> oneOf "!#$%&'*+-/=?^_`{|}~"

atom :: Parser Text
atom = option "" cfws <> tsome atext <> option "" cfws

dotAtomText :: Parser Text
dotAtomText = tsome atext <> fmany (string "." <> tsome atext)

dotAtom :: Parser Text
dotAtom = option "" cfws <> dotAtomText <> option "" cfws

specials :: Parser Char
specials = oneOf "()<>[]:;@\\,.\""

-- 3.2.4

qtext :: Parser Char
qtext =
  ranges [[33], [35 .. 91], [93 .. 126]]
    <|> obsQtext

qcontent :: Parser Text
qcontent = tcount 1 qtext <|> quotedPair

quotedString :: Parser Text
quotedString =
  fold
    [ option "" cfws
    , string "\""
    , option "" fws
    , fold <$> fmany qcontent `Parse.sepBy` fws
    , option "" fws
    , string "\""
    , option "" cfws
    ]

-- 3.2.5

word :: Parser Text
word = atom <|> quotedString

phrase :: Parser (NonEmpty Text)
phrase = Parse.some word <|> obsPhrase

unstructured :: Parser Text
unstructured =
  (fmany (option "" fws <> tcount 1 vchar) <> tmany wsp)
    <|> obsUnstruct

-- 3.3

dateTime :: Parser Text
dateTime =
  fold
    [ option "" (dayOfWeek <> string ",")
    , date
    , time
    , option "" cfws
    ]

dayOfWeek :: Parser Text
dayOfWeek = (option "" fws <> dayName) <|> obsDayOfWeek

dayName :: Parser Text
dayName = choice $ map string ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

date :: Parser Text
date = fold [day, month, year]

day :: Parser Text
day = fold [option "" fws, tbtwn 1 2 digit, fws] <|> obsDay

month :: Parser Text
month =
  choice $
    map
      string
      [ "Jan"
      , "Feb"
      , "Mar"
      , "Apr"
      , "May"
      , "Jun"
      , "Jul"
      , "Aug"
      , "Sep"
      , "Oct"
      , "Nov"
      , "Dec"
      ]

year :: Parser Text
year = fold [fws, tatLeast 4 digit, fws] <|> obsYear

time :: Parser Text
time = timeOfDay <> zone

timeOfDay :: Parser Text
timeOfDay =
  fold
    [ hour
    , string ":"
    , minute
    , option "" (string ":" <> second)
    ]

hour :: Parser Text
hour = tcount 2 digit <|> obsHour

minute :: Parser Text
minute = tcount 2 digit <|> obsMinute

second :: Parser Text
second = tcount 2 digit <|> obsSecond

zone :: Parser Text
zone = fold [fws, tcount 1 (oneOf "+-"), tcount 4 digit] <|> obsZone

-- 3.4

address :: Parser [Mailbox]
address = fmap pure mailbox_ <|> group

mailbox_ :: Parser Mailbox
mailbox_ = nameAddr <|> (Mailbox Nothing <$> addrSpec')

nameAddr :: Parser Mailbox
nameAddr =
  liftA2
    Mailbox
    (fmap (Text.unwords . toList) <$> Parse.optionMaybe displayName)
    angleAddr

angleAddr :: Parser Email
angleAddr =
  ( option "" cfws
      *> char '<'
      *> addrSpec'
      <* char '>'
      <* option "" cfws
  )
    <|> obsAngleAddr

group :: Parser [Mailbox]
group =
  displayName
    *> char ':'
    *> option [] groupList
    <* char ';'
    <* option "" cfws

displayName :: Parser (NonEmpty Text)
displayName = phrase

mailboxList :: Parser (NonEmpty Mailbox)
mailboxList =
  liftA2 (:|) mailbox_ (Parse.many (char ',' *> mailbox_))
    <|> obsMboxList

addressList :: Parser [Mailbox]
addressList =
  (fold <$> liftA2 (:) address (Parse.many (char ',' *> address)))
    <|> obsAddrList

groupList :: Parser [Mailbox]
groupList = fmap toList mailboxList <|> fmap (const []) (cfws <|> ignore obsGroupList)

-- 3.4.1

addrSpec' :: Parser Email
addrSpec' = do
  l <- localpart

  -- Maximum length of local-part is 64, per RFC3696
  when (Text.length l > 64) $
    fail "local-part of email is too long (more than 64 octets)"

  void (char '@') <?> "at sign"
  d <- domain

  -- Maximum length is 254, per Erratum 1690 on RFC3696
  when (Text.length l + Text.length d > 253) $
    fail "email address is too long (more than 254 octets)"

  pure $ Email l d

addrSpec :: Parser Text
addrSpec = fmap emailText addrSpec'

localpart :: Parser Text
localpart = dotAtom <|> quotedString <|> obsLocalPart

domain :: Parser Text
domain = domainname <|> domainliteral <|> obsDomain

domainname :: Parser Text
domainname = do
  dom <- Text.intercalate "." <$> domainlabel `sepEndBy1` string "."
  Text.length dom <= 253 ? dom

domainlabel :: Parser Text
domainlabel = do
  content <-
    option "" cfws
      *> liftA2 (:|) alphaNum (Parse.many (alphaNum <|> char '-'))
      <* option "" cfws
  length content <= 63
    && NonEmpty.last content /= '-' ? Text.pack (toList content)

domainliteral :: Parser Text
domainliteral =
  fmap fold $
    option "" cfws
      *> char '['
      *> Parse.many (option "" fws <> dtext)
      <* option "" fws
      <* char ']'
      <* option "" cfws

dtext :: Parser Text
dtext =
  tcount 1 (ranges [[33 .. 90], [94 .. 126]])
    <|> obsDtext

-- 3.5

message :: Parser Text
message =
  (fields <|> fmap Text.unlines obsFields)
    <> option "" (tcount 1 crlf <> body)

body :: Parser Text
body =
  ( fmap fold (Parse.many (tupto 998 text <> tcount 1 crlf))
      <> tupto 998 text
  )
    <|> obsBody

text :: Parser Char
text = ranges [[1 .. 9], [11, 12], [14 .. 127]]

-- 3.6

fields :: Parser Text
fields =
  fmap fold $
    ( fmap fold
        . Parse.many
        $ choice
          [ liftA2 (:) trace (Parse.many optionalField)
          , Parse.many $
              choice
                [ resentDate
                , resentFrom <&> \(m :| ms) ->
                    "Resent-From:" <> mailboxText m <> foldMap ((", " <>) . mailboxText) ms
                , resentSender <&> \m -> "Resent-Sender:" <> mailboxText m
                , resentTo <&> \x ->
                    "Resent-To:" <> flip (maybe "") (nonEmpty x) \(m :| ms) ->
                      mailboxText m <> foldMap ((", " <>) . mailboxText) ms
                , resentCc <&> \x ->
                    "Resent-Cc:" <> flip (maybe "") (nonEmpty x) \(m :| ms) ->
                      mailboxText m <> foldMap ((", " <>) . mailboxText) ms
                , resentBcc <&> \x ->
                    "Resent-Bcc:" <> flip (maybe "") (nonEmpty x) \(m :| ms) ->
                      mailboxText m <> foldMap ((", " <>) . mailboxText) ms
                , resentMsgId
                ]
          ]
    )
      <> Parse.many
        ( choice
            [ origDate
            , from <&> \(m :| ms) ->
                "From:" <> mailboxText m <> foldMap ((", " <>) . mailboxText) ms
            , sender <&> \m -> "Sender:" <> mailboxText m
            , replyTo <&> \x ->
                "Reply-To:" <> flip (maybe "") (nonEmpty x) \(m :| ms) ->
                  mailboxText m <> foldMap ((", " <>) . mailboxText) ms
            , to <&> \x ->
                "To:" <> flip (maybe "") (nonEmpty x) \(m :| ms) ->
                  mailboxText m <> foldMap ((", " <>) . mailboxText) ms
            , cc <&> \x ->
                "Cc:" <> flip (maybe "") (nonEmpty x) \(m :| ms) ->
                  mailboxText m <> foldMap ((", " <>) . mailboxText) ms
            , bcc <&> \x ->
                "Bcc:" <> flip (maybe "") (nonEmpty x) \(m :| ms) ->
                  mailboxText m <> foldMap ((", " <>) . mailboxText) ms
            , messageId
            , inReplyTo
            , references
            , subject
            , comments
            , keywords
            , optionalField
            ]
        )

-- 3.6.1

origDate :: Parser Text
origDate = string "Date:" *> dateTime <* crlf

-- 3.6.2

from :: Parser (NonEmpty Mailbox)
from = string "From:" *> mailboxList <* crlf

sender :: Parser Mailbox
sender = string "Sender:" *> mailbox_ <* crlf

replyTo :: Parser [Mailbox]
replyTo = string "Reply-To:" *> addressList <* crlf

-- 3.6.3

to :: Parser [Mailbox]
to = string "To:" *> addressList <* crlf

cc :: Parser [Mailbox]
cc = string "Cc:" *> addressList <* crlf

bcc :: Parser [Mailbox]
bcc = string "Bcc:" *> (addressList <|> ignore cfws) <* crlf

-- 3.6.4

messageId :: Parser Text
messageId = string "Message-ID:" <> msgId <> tcount 1 crlf

inReplyTo :: Parser Text
inReplyTo = string "In-Reply-To:" <> fmap fold (Parse.many1 msgId) <> tcount 1 crlf

references :: Parser Text
references = string "References:" <> fmap fold (Parse.many1 msgId) <> tcount 1 crlf

msgId :: Parser Text
msgId =
  option "" cfws
    <> ( fold
           <$> sequenceA
             [ string "<"
             , idLeft
             , string "@"
             , idRight
             , string ">"
             ]
       )
    <> option "" cfws

idLeft :: Parser Text
idLeft = dotAtomText <|> obsIdLeft

idRight :: Parser Text
idRight = dotAtomText <|> noFoldLiteral <|> obsIdRight

noFoldLiteral :: Parser Text
noFoldLiteral = string "[" <> fmap fold (Parse.many dtext) <> string "]"

-- 3.6.5

subject :: Parser Text
subject = string "Subject:" *> unstructured <* crlf

comments :: Parser Text
comments = string "Comments:" *> unstructured <* crlf

keywords :: Parser Text
keywords =
  string "Keywords:"
    <> fmap fold phrase
    <> fmap fold (Parse.many $ string "," <> fmap fold phrase)
    <* crlf

-- 3.6.6

resentDate :: Parser Text
resentDate = string "Resent-Date:" *> dateTime <* crlf

resentFrom :: Parser (NonEmpty Mailbox)
resentFrom = string "Resent-From:" *> mailboxList <* crlf

resentSender :: Parser Mailbox
resentSender = string "Resent-Sender:" *> mailbox_ <* crlf

resentTo :: Parser [Mailbox]
resentTo = string "Resent-To:" *> addressList <* crlf

resentCc :: Parser [Mailbox]
resentCc = string "Resent-Cc:" *> addressList <* crlf

resentBcc :: Parser [Mailbox]
resentBcc = string "Resent-Bcc:" *> (addressList <|> ignore cfws) <* crlf

resentMsgId :: Parser Text
resentMsgId = string "Resent-Message-ID:" <> msgId <* crlf

-- 3.6.7

trace :: Parser Text
trace = Parse.optional return_ *> fmap Text.unlines (Parse.many1 received)

return_ :: Parser Text
return_ = string "Return-Path:" *> path <* crlf

path :: Parser Text
path =
  fmap emailText angleAddr <|> do
    Parse.optional cfws
    void (string "<")
    Parse.optional cfws
    void (string ">")
    Parse.optional cfws
    pure mempty

received :: Parser Text
received =
  string "Received:"
    *> (fmap fold (Parse.many receivedToken) <> string ";" <> dateTime)
    <* crlf

receivedToken :: Parser Text
receivedToken = choice [word, fmap emailText angleAddr, addrSpec, domain]

-- 3.6.8

optionalField :: Parser Text
optionalField = (fieldName <> string ":" <> unstructured) <* crlf

fieldName :: Parser Text
fieldName = tsome ftext

ftext :: Parser Char
ftext = ranges [[33 .. 57], [59 .. 126]]

-- OBSOLETE

-- 4.1

obsNoWsCtl :: Parser Char
obsNoWsCtl = ranges [[1 .. 8], [11, 12], [14 .. 31], [127]]

obsCtext, obsQtext, obsUtext :: Parser Char
obsCtext = obsNoWsCtl
obsQtext = obsNoWsCtl
obsUtext = nul <|> obsNoWsCtl <|> vchar

obsQP :: Parser Text
obsQP = string "\\" <> tcount 1 (choice [nul, obsNoWsCtl, lf, cr])

obsBody :: Parser Text
obsBody =
  Text.pack . fold
    <$> Parse.many
      ( ( Parse.many lf
            *> Parse.many cr
            *> Parse.many ((nul <|> text) <* Parse.many lf <* Parse.many cr)
        )
          <|> fmap pure crlf
      )

obsUnstruct :: Parser Text
obsUnstruct =
  fmap fold <$> Parse.many $
    (tmany lf *> tmany cr *> tmany (obsUtext <* Parse.many lf <* Parse.many cr))
      <|> ("" <$ fws)

obsPhrase :: Parser (NonEmpty Text)
obsPhrase = (:|) <$> word <*> Parse.many (word <|> string "." <|> cfws)

obsPhraseList :: Parser [Text]
obsPhraseList =
  option [] (fmap toList phrase <|> fmap pure cfws)
    <> Parse.many
      ( fmap fold . (:)
          <$> string ","
          <*> option [] (fmap toList phrase <|> fmap pure cfws)
      )

-- 4.2

obsFws :: Parser Text
obsFws = fsome (tcount 1 wsp) <> fmany (tcount 1 crlf <> fsome (tcount 1 wsp))

-- 4.3

obsDayOfWeek :: Parser Text
obsDayOfWeek = option "" cfws <> dayName <> option "" cfws

obsDay :: Parser Text
obsDay = option "" cfws <> tbtwn 1 2 digit <> option "" cfws

obsYear :: Parser Text
obsYear = option "" cfws <> tatLeast 2 digit <> option "" cfws

obsHour :: Parser Text
obsHour = option "" cfws <> tcount 2 digit <> option "" cfws

obsMinute :: Parser Text
obsMinute = option "" cfws <> tcount 2 digit <> option "" cfws

obsSecond :: Parser Text
obsSecond = option "" cfws <> tcount 2 digit <> option "" cfws

obsZone :: Parser Text
obsZone =
  choice
    [ string "UT"
    , string "GMT"
    , string "EST"
    , string "EDT"
    , string "CST"
    , string "CDT"
    , string "MST"
    , string "MDT"
    , string "PST"
    , string "PDT"
    , tcount 1 $ ranges [[65 .. 73], [75 .. 90], [97 .. 105], [107 .. 122]]
    ]

-- 4.4

obsAngleAddr :: Parser Email
obsAngleAddr =
  option "" cfws
    *> char '<'
    *> (obsRoute {- should be ignored -} *> addrSpec')
    <* char '>'
    <* option "" cfws

obsRoute :: Parser [Text]
obsRoute = obsDomainList <* char ':'

obsDomainList :: Parser [Text]
obsDomainList = do
  void $ Parse.many (cfws <|> string ",")
  void $ char '@'
  dom <- domain
  doms <-
    Parse.many $
      char ','
        *> Parse.optionMaybe cfws
        *> Parse.optionMaybe (char '@' *> domain)

  pure (dom : catMaybes doms)

obsMboxList :: Parser (NonEmpty Mailbox)
obsMboxList = do
  void . Parse.many $ option "" cfws *> char ','
  mb <- mailbox_
  mbs <-
    Parse.many $
      char ',' *> Parse.optionMaybe (fmap Just mailbox_ <|> (Nothing <$ cfws))
  pure $ mb :| mapMaybe join mbs

obsAddrList :: Parser [Mailbox]
obsAddrList = do
  void . Parse.many $ option "" cfws *> char ','
  mb <- address
  mbs <-
    Parse.many $
      char ',' *> Parse.optionMaybe (fmap Just address <|> (Nothing <$ cfws))
  pure $ mb <> concatMap (fromMaybe [] . join) mbs

obsGroupList :: Parser ()
obsGroupList = void do
  Parse.many1 (option "" cfws *> char ',') *> option "" cfws

obsLocalPart :: Parser Text
obsLocalPart =
  fmap fold
    . liftA2 (:) word
    $ Parse.many (liftA2 Text.cons (char '.') word)

obsDomain :: Parser Text
obsDomain =
  fmap fold
    . liftA2 (:) atom
    $ Parse.many (liftA2 Text.cons (char '.') atom)

obsDtext :: Parser Text
obsDtext = tcount 1 obsNoWsCtl <|> quotedPair

-- 4.5

obsFields :: Parser [Text]
obsFields =
  Parse.many $
    choice
      [ obsReturn
      , obsReceived
      , obsOrigDate
      , obsFrom
      , obsSender
      , obsReplyTo
      , obsTo
      , obsCc
      , obsBcc
      , obsMessageId
      , obsInReplyTo
      , obsReferences
      , obsSubject
      , obsComments
      , obsKeywords
      , obsResentDate
      , obsResentFrom
      , obsResentSend
      , obsResentRply
      , obsResentTo
      , obsResentCc
      , obsResentBcc
      , obsResentMid
      , obsOptional
      ]

-- 4.5.1

obsOrigDate :: Parser Text
obsOrigDate =
  string "Date"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> dateTime
    <> tcount 1 crlf

-- 4.5.2

obsFrom :: Parser Text
obsFrom =
  string "From"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> ( mailboxList <&> \(m :| ms) ->
           mailboxText m <> foldMap ((", " <>) . mailboxText) ms
       )
    <> tcount 1 crlf

obsSender :: Parser Text
obsSender =
  string "Sender"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> (mailbox_ <&> \m -> mailboxText m)
    <> tcount 1 crlf

obsReplyTo :: Parser Text
obsReplyTo =
  string "Reply-To"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> ( addressList <&> \x -> flip (maybe "") (nonEmpty x) \(m :| ms) ->
           mailboxText m <> foldMap ((", " <>) . mailboxText) ms
       )
    <> tcount 1 crlf

obsTo :: Parser Text
obsTo =
  string "To"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> ( addressList <&> \x -> flip (maybe "") (nonEmpty x) \(m :| ms) ->
           mailboxText m <> foldMap ((", " <>) . mailboxText) ms
       )
    <> tcount 1 crlf

obsCc :: Parser Text
obsCc =
  string "Cc"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> ( addressList <&> \x -> flip (maybe "") (nonEmpty x) \(m :| ms) ->
           mailboxText m <> foldMap ((", " <>) . mailboxText) ms
       )
    <> tcount 1 crlf

obsBcc :: Parser Text
obsBcc =
  string "Bcc"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> ( ( addressList <&> \x -> flip (maybe "") (nonEmpty x) \(m :| ms) ->
             mailboxText m <> foldMap ((", " <>) . mailboxText) ms
         )
           <|> ( fmap fold (Parse.many (option "" cfws <> string ","))
                   <> option "" cfws
               )
       )
    <> tcount 1 crlf

-- 4.5.4

obsMessageId :: Parser Text
obsMessageId =
  string "Message-ID"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> msgId
    <> tcount 1 crlf

obsInReplyTo :: Parser Text
obsInReplyTo =
  string "In-Reply-To"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> fmap fold (Parse.many (fmap fold phrase <|> msgId))
    <> tcount 1 crlf

obsReferences :: Parser Text
obsReferences =
  string "References"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> fmap fold (Parse.many (fmap fold phrase <|> msgId))
    <> tcount 1 crlf

obsIdLeft :: Parser Text
obsIdLeft = localpart

obsIdRight :: Parser Text
obsIdRight = domain

-- 4.5.5

obsSubject :: Parser Text
obsSubject =
  string "Subject"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> unstructured
    <> tcount 1 crlf

obsComments :: Parser Text
obsComments =
  string "Comments"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> unstructured
    <> tcount 1 crlf

obsKeywords :: Parser Text
obsKeywords =
  string "Keywords"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> fmap fold obsPhraseList
    <> tcount 1 crlf

-- 4.5.6

obsResentFrom :: Parser Text
obsResentFrom =
  string "Resent-From"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> ( mailboxList <&> \(m :| ms) ->
           mailboxText m <> foldMap ((", " <>) . mailboxText) ms
       )
    <> tcount 1 crlf

obsResentSend :: Parser Text
obsResentSend =
  string "Resent-Sender"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> (mailbox_ <&> \m -> mailboxText m)
    <> tcount 1 crlf

obsResentDate :: Parser Text
obsResentDate =
  string "Resent-Date"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> dateTime
    <> tcount 1 crlf

obsResentTo :: Parser Text
obsResentTo =
  string "Resent-To"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> ( addressList <&> \x -> flip (maybe "") (nonEmpty x) \(m :| ms) ->
           mailboxText m <> foldMap ((", " <>) . mailboxText) ms
       )
    <> tcount 1 crlf

obsResentCc :: Parser Text
obsResentCc =
  string "Resent-Cc"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> ( addressList <&> \x -> flip (maybe "") (nonEmpty x) \(m :| ms) ->
           mailboxText m <> foldMap ((", " <>) . mailboxText) ms
       )
    <> tcount 1 crlf

obsResentBcc :: Parser Text
obsResentBcc =
  string "Resent-Bcc"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> ( ( addressList <&> \x -> flip (maybe "") (nonEmpty x) \(m :| ms) ->
             mailboxText m <> foldMap ((", " <>) . mailboxText) ms
         )
           <|> ( fmap fold (Parse.many (option "" cfws <> string ","))
                   <> option "" cfws
               )
       )
    <> tcount 1 crlf

obsResentMid :: Parser Text
obsResentMid =
  string "Resent-Message-ID"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> msgId
    <> tcount 1 crlf

obsResentRply :: Parser Text
obsResentRply =
  string "Resent-Reply-To"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> ( addressList <&> \x -> flip (maybe "") (nonEmpty x) \(m :| ms) ->
           mailboxText m <> foldMap ((", " <>) . mailboxText) ms
       )
    <> tcount 1 crlf

-- 4.5.7

obsReturn :: Parser Text
obsReturn =
  string "Return-Path"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> path
    <> tcount 1 crlf

obsReceived :: Parser Text
obsReceived =
  string "Received"
    <> ignore (Parse.many wsp)
    <> string ":"
    <> fmap fold (Parse.many receivedToken)
    <> tcount 1 crlf

-- 4.5.8

obsOptional :: Parser Text
obsOptional =
  fieldName
    <> ignore (Parse.many wsp)
    <> string ":"
    <> unstructured
    <> tcount 1 crlf

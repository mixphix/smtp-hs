{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Network.SMTP.Email.Parse
  ( Email
  , Mailbox (..)
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

import Control.Applicative (Alternative (empty), liftA2)
import Control.Block
import Control.Monad (join, void, when)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Semigroup (stimes)
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
  , endBy1
  , eof
  , many1
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
validateEmail = first show . parse (addrSpec <* eof) ""

validateMailbox :: Text -> Either String Mailbox
validateMailbox = first show . parse (mailbox' <* eof) ""

validateMailboxes :: Text -> Either String (NonEmpty Mailbox)
validateMailboxes = first show . parse (mailboxList <* eof) ""

-- Parsing

infix 2 ?>

(?>) :: (Alternative f) => Bool -> a -> f a
b ?> a = if b then pure a else empty

-- backtracking
infixl 4 <|>

(<|>) :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
a <|> b = Parse.choice [Parse.try a, Parse.try b]

-- backtracking
choice :: (Stream s m t) => [ParsecT s u m a] -> ParsecT s u m a
choice ps = Parse.choice (Parse.try <$> ps)

atLeast :: (Stream s m t) => Word -> ParsecT s u m a -> ParsecT s u m [a]
atLeast 0 p = Parse.many p
atLeast n p = liftA2 (:) p $ atLeast (n - 1) p

upto :: (Stream s m t) => Word -> ParsecT s u m a -> ParsecT s u m [a]
upto n p = choice . reverse $ [0 .. n] <&> (`stimes` fmap pure p)

btwn :: (Stream s m t) => Word -> Word -> ParsecT s u m a -> ParsecT s u m [a]
btwn 0 n p = upto n p
btwn m n p = liftA2 (:) p $ btwn (m - 1) (n - 1) p

string :: (Stream s m Char) => Text -> ParsecT s u m Text
string = fmap Text.pack . Parse.string . Text.unpack

tmany :: ParsecT s u m Char -> ParsecT s u m Text
tmany p = Text.pack <$> Parse.many p

tmany1 :: (Stream s m t) => ParsecT s u m Char -> ParsecT s u m Text
tmany1 p = Text.pack <$> Parse.many1 p

tatLeast :: (Stream s m t) => Word -> ParsecT s u m Char -> ParsecT s u m Text
tatLeast 0 p = tmany p
tatLeast n p = liftA2 Text.cons p (tatLeast (n - 1) p)

tupto :: (Stream s m t) => Word -> ParsecT s u m Char -> ParsecT s u m Text
tupto n p = choice . reverse $ [0 .. n] <&> (`stimes` fmap Text.singleton p)

tbtwn ::
  (Stream s m Char) =>
  Word ->
  Word ->
  ParsecT s u m Char ->
  ParsecT s u m Text
tbtwn 0 n p = tupto n p
tbtwn m n p = liftA2 Text.cons p $ tbtwn (m - 1) (n - 1) p

tcount :: (Stream s m Char) => Word -> ParsecT s u m Char -> ParsecT s u m Text
tcount 0 _ = pure ""
tcount n p = liftA2 Text.cons p (tcount (n - 1) p)

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

wsp1 :: Parser ()
wsp1 = void (many1 wsp)

-- 3.2.1

quotedpair :: Parser Text
quotedpair =
  liftA2 Text.cons (char '\\') (Text.singleton <$> (vchar <|> wsp))
    <|> obsQP

-- 3.2.2

fws :: Parser ()
fws =
  (Parse.optionMaybe (Parse.many wsp *> crlf) *> void (many1 wsp))
    <|> obsFws

ctext :: Parser Char
ctext =
  ranges [[33 .. 39], [42 .. 91], [93 .. 126]]
    <|> obsCtext

ccontent :: Parser Text
ccontent =
  fmap Text.singleton ctext
    <|> quotedpair
    <|> comment

comment :: Parser Text
comment =
  fmap Text.unwords $
    char '('
      *> Parse.many (Parse.optionMaybe fws *> ccontent)
      <* fws
      <* char ')'

cfws :: Parser ()
cfws =
  void (many1 (Parse.optionMaybe fws *> comment) <* fws)
    <|> fws

-- 3.2.3

atext :: Parser Char
atext = alphaNum <|> oneOf "!#$%&'*+/=?^_`{|}~-"

atom :: Parser Text
atom = Parse.optionMaybe cfws *> tmany1 atext <* Parse.optionMaybe cfws

dotAtomText :: Parser Text
dotAtomText = Text.intercalate "." <$> tmany1 atext `endBy1` string "."

dotAtom :: Parser Text
dotAtom =
  fmap fold $
    Parse.optionMaybe cfws
      *> many1 dotAtomText
      <* Parse.optionMaybe cfws

-- 3.2.4

qtext :: Parser Char
qtext =
  ranges [[33], [35 .. 91], [93 .. 126]]
    <|> obsQtext

qcontent :: Parser Text
qcontent = fmap Text.singleton qtext <|> quotedpair

quotedstring :: Parser Text
quotedstring =
  Parse.optionMaybe cfws
    *> ( fold
          <$> sequence
            [ Text.singleton <$> char '"'
            , fold <$> Parse.many (Parse.optionMaybe fws *> qcontent)
            , fmap Text.singleton $ Parse.optionMaybe fws *> char '"'
            ]
       )
    <* Parse.optionMaybe cfws

-- 3.2.5

word :: Parser Text
word = atom <|> quotedstring

phrase :: Parser [Text]
phrase = many1 word <|> obsPhrase

unstructured :: Parser Text
unstructured =
  (tmany (Parse.optionMaybe fws *> vchar) <* Parse.many wsp)
    <|> obsUnstruct

-- 3.3

dateTime :: Parser Text
dateTime =
  fold [Parse.option "" (dayOfWeek <> string ","), date, time]
    <* Parse.optional cfws

dayOfWeek :: Parser Text
dayOfWeek = (Parse.optional fws *> dayName) <|> obsDayOfWeek

dayName :: Parser Text
dayName = choice $ map string ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

date :: Parser Text
date = fold [day, month, year]

day :: Parser Text
day = (Parse.optional fws *> tbtwn 1 2 digit <* fws) <|> obsDay

month :: Parser Text
month =
  choice . map string $
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
year = (fws *> tatLeast 4 digit <* fws) <|> obsYear

time :: Parser Text
time = timeOfDay <> zone

timeOfDay :: Parser Text
timeOfDay =
  fold
    [ hour
    , string ":"
    , minute
    , Parse.option "" $ string ":" <> second
    ]

hour :: Parser Text
hour = tcount 2 digit <|> obsHour

minute :: Parser Text
minute = tcount 2 digit <|> obsMinute

second :: Parser Text
second = tcount 2 digit <|> obsSecond

zone :: Parser Text
zone = (fws *> liftA2 Text.cons (oneOf "+-") (tcount 4 digit)) <|> obsZone

-- 3.4

addresses :: Parser [Mailbox]
addresses = fmap pure mailbox' <|> group

mailbox' :: Parser Mailbox
mailbox' = nameAddr <|> (Mailbox Nothing <$> addrSpec)

nameAddr :: Parser Mailbox
nameAddr =
  liftA2
    Mailbox
    (fmap Text.unwords <$> Parse.optionMaybe phrase)
    angleAddr

angleAddr :: Parser Email
angleAddr =
  ( Parse.optionMaybe cfws
      *> char '<'
      *> addrSpec
      <* char '>'
      <* Parse.optionMaybe cfws
  )
    <|> obsAngleAddr

group :: Parser [Mailbox]
group =
  displayName
    *> char ':'
    *> (fromMaybe [] <$> Parse.optionMaybe groupList)
    <* char ';'
    <* Parse.optionMaybe cfws

displayName :: Parser [Text]
displayName = phrase

mailboxList :: Parser (NonEmpty Mailbox)
mailboxList =
  liftA2 (:|) mailbox' (Parse.many (char ',' *> mailbox'))
    <|> obsMboxList

addressList :: Parser [Mailbox]
addressList =
  (fold <$> liftA2 (:) addresses (Parse.many (char ',' *> addresses)))
    <|> obsAddrList

groupList :: Parser [Mailbox]
groupList = fmap toList mailboxList <|> fmap (const []) (cfws <|> obsGroupList)

-- 3.4.1

addrSpec :: Parser Email
addrSpec = do
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

localpart :: Parser Text
localpart = dotAtom <|> quotedstring <|> obsLocalPart

domain :: Parser Text
domain = domainname <|> domainliteral <|> obsDomain

domainname :: Parser Text
domainname = do
  dom <- Text.intercalate "." <$> domainlabel `sepEndBy1` string "."

  Text.length dom <= 253 ?> dom

domainlabel :: Parser Text
domainlabel = do
  content <-
    Parse.optionMaybe cfws
      *> liftA2 (:|) alphaNum (Parse.many (alphaNum <|> char '-'))
      <* Parse.optionMaybe cfws

  (length content <= 63 && NonEmpty.last content /= '-')
    ?> Text.pack (toList content)

domainliteral :: Parser Text
domainliteral =
  fmap fold $
    Parse.optionMaybe cfws
      *> char '['
      *> Parse.many (Parse.optionMaybe fws *> dtext)
      <* Parse.optionMaybe fws
      <* char ']'
      <* Parse.optionMaybe cfws

dtext :: Parser Text
dtext = fmap Text.singleton (ranges [[33 .. 90], [94 .. 126]]) <|> obsDtext

obsPhrase :: Parser [Text]
obsPhrase =
  liftA2 (:) word . Parse.many $
    word
      <|> (Text.singleton <$> char '.')
      <|> ("" <$ cfws)

-- 3.5

message :: Parser Text
message =
  (fields <|> obsFields)
    <> Parse.option "" (fmap Text.singleton crlf <> body)

body :: Parser Text
body =
  ( fmap fold (Parse.many (tupto 998 text <> fmap Text.singleton crlf))
      <> tupto 998 text
  )
    <|> obsBody

text :: Parser Char
text = ranges [[1 .. 9], [11, 12], [14 .. 127]]

-- 3.6

fields :: Parser Text
fields =
  fmap fold $
    ( fmap fold . Parse.many $
        choice
          [ liftA2 (:) trace (Parse.many optionalField)
          , Parse.many $
              choice
                [ resentDate
                , resentFrom
                , resentSender
                , resentTo
                , resentCc
                , recentBcc
                , resentMsgId
                ]
          ]
    )
      <> Parse.many
        ( choice
            [ origDate
            , from
            , sender
            , replyTo
            , to
            , cc
            , bcc
            , messageId
            , inReplyTo
            , references
            , subject
            , comments
            , keywords
            , optionalField
            ]
        )

optionalField :: Parser Text
optionalField = undefined

trace :: Parser Text
trace = undefined

resentDate :: Parser Text
resentDate = undefined

resentFrom :: Parser Text
resentFrom = undefined

resentSender :: Parser Text
resentSender = undefined

resentTo :: Parser Text
resentTo = undefined

resentCc :: Parser Text
resentCc = undefined

recentBcc :: Parser Text
recentBcc = undefined

resentMsgId :: Parser Text
resentMsgId = undefined

origDate :: Parser Text
origDate = undefined

from :: Parser Text
from = undefined

sender :: Parser Text
sender = undefined

replyTo :: Parser Text
replyTo = undefined

to :: Parser Text
to = undefined

cc :: Parser Text
cc = undefined

bcc :: Parser Text
bcc = undefined

messageId :: Parser Text
messageId = undefined

inReplyTo :: Parser Text
inReplyTo = undefined

references :: Parser Text
references = undefined

subject :: Parser Text
subject = undefined

comments :: Parser Text
comments = undefined

keywords :: Parser Text
keywords = undefined

-- OBSOLETE

-- 4.1

obsNoWsCtl :: Parser Char
obsNoWsCtl = ranges [[1 .. 8], [11, 12], [14 .. 31], [127]]

obsCtext, obsQtext, obsUtext :: Parser Char
obsCtext = obsNoWsCtl
obsQtext = obsNoWsCtl
obsUtext = char '\0' <|> obsNoWsCtl <|> vchar

obsQP :: Parser Text
obsQP =
  liftA2 Text.cons (char '\\') $
    Text.singleton <$> choice [nul, obsNoWsCtl, lf, cr]

obsUnstruct :: Parser Text
obsUnstruct =
  fmap fold <$> Parse.many $
    ( Parse.many lf
        *> Parse.many cr
        *> tmany (obsUtext <* Parse.many lf <* Parse.many cr)
    )
      <|> ("" <$ fws)

-- 4.2

obsFws :: Parser ()
obsFws = void (many1 wsp *> Parse.many (crlf *> many1 wsp))

-- 4.4

obsAngleAddr :: Parser Email
obsAngleAddr =
  Parse.optionMaybe cfws
    *> char '<'
    *> (obsRoute {- should be ignored -} *> addrSpec)
    <* char '>'
    <* Parse.optionMaybe cfws

obsRoute :: Parser [Text]
obsRoute = obsDomainList <* char ':'

obsDomainList :: Parser [Text]
obsDomainList = do
  void $ Parse.many (cfws <|> void (char ','))
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
  void . Parse.many $ Parse.optionMaybe cfws *> char ','
  mb <- mailbox'
  mbs <-
    Parse.many $
      char ',' *> Parse.optionMaybe (fmap Just mailbox' <|> (Nothing <$ cfws))
  pure $ mb :| mapMaybe join mbs

obsAddrList :: Parser [Mailbox]
obsAddrList = do
  void . Parse.many $ Parse.optionMaybe cfws *> char ','
  mb <- addresses
  mbs <-
    Parse.many $
      char ',' *> Parse.optionMaybe (fmap Just addresses <|> (Nothing <$ cfws))
  pure $ mb <> concatMap (fromMaybe [] . join) mbs

obsGroupList :: Parser ()
obsGroupList =
  void (many1 (Parse.optionMaybe cfws *> char ',') *> Parse.optionMaybe cfws)

obsLocalPart :: Parser Text
obsLocalPart =
  fmap fold . liftA2 (:) word $
    Parse.many (liftA2 Text.cons (char '.') word)

obsDomain :: Parser Text
obsDomain =
  fmap fold . liftA2 (:) atom $
    Parse.many (liftA2 Text.cons (char '.') atom)

obsDtext :: Parser Text
obsDtext = fmap Text.singleton obsNoWsCtl <|> quotedpair

obsFields :: Parser Text
obsFields = undefined

obsBody :: Parser Text
obsBody = undefined

obsDayOfWeek :: Parser Text
obsDayOfWeek = undefined

obsDay :: Parser Text
obsDay = undefined

obsYear :: Parser Text
obsYear = undefined

obsHour :: Parser Text
obsHour = undefined

obsMinute :: Parser Text
obsMinute = undefined

obsSecond :: Parser Text
obsSecond = undefined

obsZone :: Parser Text
obsZone = undefined
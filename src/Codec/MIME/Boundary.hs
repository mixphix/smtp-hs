module Codec.MIME.Boundary
  ( Boundary (..)
  )
where

import Control.Applicative (liftA2)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Random.Stateful
  ( Random
  , StatefulGen
  , Uniform (..)
  , UniformRange (..)
  )

-- | Get a list of results by iterating a monadic action.
applyNM :: (Monad m) => Int -> a -> (a -> m b) -> m [b] -> m [b]
applyNM n a f = case n of
  0 -> id
  _ -> applyNM (pred n) a f . liftA2 (:) (f a)

-- |
-- A wrapper around 'Text' such that the 'Uniform' instance generates
-- a sequence of ten ASCII alphanumeric characters preceded by "=_",
-- so that the boundary never appears in quoted-printable encodings.
newtype Boundary = Boundary Text

instance Uniform Boundary where
  uniformM :: StatefulGen g m => g -> m Boundary
  uniformM g =
    Boundary . ("=_" <>) . Text.pack . map (ls !!)
      <$> applyNM 10 g (uniformRM (0, l)) (pure [])
   where
    ls = fold [['A' .. 'Z'], ['a' .. 'z'], ['0' .. '9']]
    l = pred $ length ls

instance UniformRange Boundary where
  uniformRM :: StatefulGen g m => (Boundary, Boundary) -> g -> m Boundary
  uniformRM _ = uniformM

instance Random Boundary

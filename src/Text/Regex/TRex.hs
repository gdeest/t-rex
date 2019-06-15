{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Regex.TRex
  ( Compiled
  , compile
  , match
  , RE
  , module Data.Functor.Alt
  , int
  , (<&>), (</>)
  , sepBy
  , many1
  , opt
  , raw
  , str
  ) where

import Data.Array ((!))
import Data.Functor.Alt
import Data.String(IsString(..))
import Data.String.ToString
import Text.Regex.Base hiding (match)

import Data.ByteString (ByteString)
import qualified Text.Regex.PCRE.ByteString as BS

-- | 'RE r' represents a regular expression that parses an 'r'. It is abstract.
data RE r where
  Eps :: RE ()
  Map :: (a -> b) -> RE a -> RE b
  App :: RE (a -> b) -> RE a -> RE b
  Raw :: Int -> ByteString -> RE ByteString
  Alt :: RE a -> RE b -> RE (Either a b)
  Opt :: RE a -> RE (Maybe a)
  Rep :: RE a -> RE [a]

instance IsString (RE ByteString) where
  fromString = str 

instance Functor RE where
  fmap = Map

instance Applicative RE where
  pure x = Map (const x) Eps
  (<*>) = App

instance Alt RE where
  (<!>) r1 r2 = (either id id) <$> (Alt r1 r2)
  many = Rep
  some r = (:) <$> r <*> many r

regexStr :: RE r -> ByteString
regexStr re = case re of
  Eps -> mempty
  Raw _ s -> "(" <> s <> ")"
  Alt ra rb -> "(?:(" <> regexStr ra <> ")|(" <> regexStr rb <> "))"
  Opt ra -> "(" <> regexStr ra <> ")?"
  App ra rb -> regexStr ra <> regexStr rb
  Rep ra -> "((?:" <> regexStr ra <> ")*)"
  Map _ ra -> regexStr ra

mkFullRegex :: RE r -> ByteString
mkFullRegex re = "^" <> regexStr re <> "$"

newtype Compiled x = Compiled (BS.Regex, MatchTree x)

data MatchTree x where
  MatchEps :: MatchTree ()
  MatchRaw :: !Int -> MatchTree ByteString
  MatchApp
    :: MatchTree (a -> b)
    -> MatchTree a
    -> MatchTree b
  MatchMap
    :: (a -> b) -> MatchTree a -> MatchTree b
  MatchOpt :: Int -> MatchTree a -> MatchTree (Maybe a)
  MatchAlt ::
    (Int, MatchTree a) ->
    (Int, MatchTree b) ->
    MatchTree (Either a b)
  MatchRep ::
    BS.Regex ->
    Int ->
    Int ->
    MatchTree a ->
    MatchTree [a]

match
  :: Compiled x
  -> ByteString
  -> Maybe x
match (Compiled (re, tree)) s = extractResult s tree <$>
  matchOnce @BS.Regex re s

extractResult
  :: ByteString
  -> MatchTree x
  -> MatchArray
  -> x
extractResult s tree matchArray =
  let peek :: forall y. MatchTree y -> y
      peek tree' = extractResult s tree' matchArray in
  case tree of
    MatchEps -> ()
    MatchRaw i -> extract (matchArray ! i) s
    MatchApp tf ta -> peek tf $ peek ta
    MatchMap f ta -> f $ peek ta
    MatchOpt i ta ->
      -- XXX: May trigger double lookup of (matchArray ! i).
      -- Can we do better ?
      case extract (matchArray ! i) s of
        "" -> Nothing
        _ ->
          Just $ peek ta
    MatchAlt (i, ta) (_, tb) ->
      case extract (matchArray ! i) s of
        "" -> Right $ peek tb
        _ -> Left $ peek ta
    MatchRep re i j ta -> extractRep re j ta (extract (matchArray ! i) s)

  where extractRep _ _ _ "" = []
        extractRep re j ta s' =
          case matchOnce @BS.Regex re s' of
            Nothing -> error "Invariant violation."
            Just ma ->
              extractResult s' ta ma :
              extractRep re j ta (extract (ma ! j) s')

compile :: RE x -> Compiled x
compile r = Compiled
    ( makeRegex @BS.Regex @BS.CompOption @BS.ExecOption $ mkFullRegex r -- :: re
    , snd $ buildTree 1 r
    )

  where
    buildTree :: forall y. Int ->  RE y -> (Int, MatchTree y)
    buildTree i r' = case r' of
      Eps -> (i, MatchEps)
      App ra rb ->
        let (i1, ta) = buildTree i ra
            (i2, tb) = buildTree i1 rb
        in (i2, MatchApp ta tb)
      Map f ra ->
        let (i', t) = buildTree i ra
        in (i', MatchMap f t)
      Raw n _ -> (i+n+1, MatchRaw i)
      Opt rOpt ->
        let (i', t) = buildTree (i+1) rOpt
        in (i', MatchOpt i t)
      Alt ra rb ->
        let (i1, ta) = buildTree (i+1) ra
            (i2, tb) = buildTree (i1+1) rb
        in (i2, MatchAlt (i+1, ta) (i2+1, tb))
      Rep rRep ->
        let r1 = rRep <&> Rep rRep
            compiled = makeRegex @BS.Regex @BS.CompOption @BS.ExecOption $
              mkFullRegex r1
            (j, t) = buildTree 1 rRep
        in (i+j, MatchRep compiled i j t)

int :: RE Int
int = Map (read @Int . toString ) (raw 0 "-?[0-9]+")

infixl <&>
(<&>) :: RE a -> RE b -> RE (a, b)
ra <&> rb = (,) <$> ra <*> rb

infixl </>
(</>) :: RE a -> RE b -> RE (Either a b)
(</>) = Alt

-- | One or more. This is a somewhat more explicit alias to 'some'.
many1 :: RE a -> RE [a]
many1 = some

sepBy :: RE a -> RE b -> RE [a]
sepBy ra rs = (:) <$> ra <*> many (rs *> ra)

opt :: RE a -> RE (Maybe a)
opt = Opt

str :: String -> RE ByteString
str = raw 0 . fromString . escapeREString

-- | Use a raw regular expression to return a string. The 'Int' parameter
-- indicates how many groups are contained within the underlying regexp.
raw :: Int -> ByteString -> RE ByteString
raw = Raw

-- -- The following functions are taken straight from regex-1.0.1.3 internals. A
-- -- dependence on the whole package does not seem justified, here.

-- -- Convert a string into a regular expression that will match that
-- -- string
escapeREString :: String -> String
escapeREString = foldr esc []
  where
    esc c t | isMetaChar c = '\\' : c : t
            | otherwise    = c : t

-- returns True iff the character is an RE meta character ('[', '*', '{', etc.)
isMetaChar :: Char -> Bool
isMetaChar c = case c of
  '^'  -> True
  '\\' -> True
  '.'  -> True
  '|'  -> True
  '*'  -> True
  '?'  -> True
  '+'  -> True
  '('  -> True
  ')'  -> True
  '['  -> True
  ']'  -> True
  '{'  -> True
  '}'  -> True
  '$'  -> True
  _    -> False

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
  ( RegexBackend(..)
  , CompiledRE
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
import Data.Coerce (coerce)
import Data.Functor.Alt
import Data.Proxy (Proxy(..))
import Data.String(IsString(..))
import Data.String.ToString
import Text.Regex.Base hiding (match)

import qualified Text.Regex.TDFA.String as Str
import qualified Data.ByteString as BS
import qualified Text.Regex.TDFA.ByteString as BS

-- | 'RE s r' represents a regular expression that parses strings of type 's'
-- and returns a result of type 'r'. It is abstract.
data RE s r where
  Eps :: RE s ()
  Map :: (a -> b) -> RE s a -> RE s b
  App :: RE s (a -> b) -> RE s a -> RE s b
  Raw :: Int -> s -> RE s s
  Alt :: RE s a -> RE s b -> RE s (Either a b)
  Opt :: RE s a -> RE s (Maybe a)
  Rep :: RE s a -> RE s [a]

instance IsString s => IsString (RE s s) where
  fromString = str

instance Functor (RE s) where
  fmap = Map

instance Applicative (RE s) where
  pure x = Map (const x) Eps
  (<*>) = App

instance Alt (RE s) where
  (<!>) r1 r2 = (either id id) <$> (Alt r1 r2)
  many = Rep
  some r = (:) <$> r <*> many r

regexStr :: (IsString s, Monoid s) => RE s r -> s
regexStr re = case re of
  Eps -> mempty
  Raw _ s -> "(" <> s <> ")"
  -- XXX: The enclosing group could be made non-capturing. Unfortunately, this
  -- isn't supported by regex-tdfa.
  -- Does it warrant a switch to regex-pcre ?
  Alt ra rb -> "((" <> regexStr ra <> ")|(" <> regexStr rb <> "))"
  Opt ra -> "(" <> regexStr ra <> ")?"
  App ra rb -> regexStr ra <> regexStr rb
  Rep ra -> "((" <> regexStr ra <> ")*)"
  Map _ ra -> regexStr ra

mkFullRegex :: (IsString s, Monoid s) => RE s r -> s
mkFullRegex re = "^" <> regexStr re <> "$"

class RegexBackend s where
  type Compiled s :: * -> *

  compile :: RE s x -> CompiledRE s x
  match :: CompiledRE s x -> s -> Maybe x

newtype CompiledRE s x = CompiledRE (s -> Maybe x)


instance RegexBackend [Char] where
  type Compiled [Char] = CompiledRE [Char]
  compile re =
    let ce =
          compileRE'(Proxy @Str.CompOption) (Proxy @Str.ExecOption) re
    in
      CompiledRE $ matchRE ce

  match = coerce

instance RegexBackend BS.ByteString where
  type Compiled BS.ByteString = CompiledRE BS.ByteString
  compile re =
    let ce =
          compileRE'(Proxy @BS.CompOption) (Proxy @BS.ExecOption) re
    in
      CompiledRE $ matchRE ce

  match = coerce

data MatchTree re s x where
  MatchEps :: MatchTree re s ()
  MatchRaw :: !Int -> MatchTree re s s
  MatchApp
    :: MatchTree re s (a -> b)
    -> MatchTree re s a
    -> MatchTree re s b
  MatchMap
    :: (a -> b) -> MatchTree re s a -> MatchTree re s b
  MatchOpt :: Int -> MatchTree re s a -> MatchTree re s (Maybe a)
  MatchAlt ::
    (Int, MatchTree re s a) ->
    (Int, MatchTree re s b) ->
    MatchTree re s (Either a b)
  MatchRep ::
    re ->
    Int ->
    Int ->
    MatchTree re s a ->
    MatchTree re s [a]

matchRE :: forall compOpts execOpts s re x.
  ( IsString s
  , Monoid s
  , Show s
  , Eq s
  , RegexMaker re compOpts execOpts s
  , RegexLike re s
  ) =>
  (re, MatchTree re s x) ->
  s ->
  Maybe x
matchRE (re, tree) s = extractResult s tree <$> matchOnce re s

extractResult :: forall re s x.
  ( RegexLike re s
  , IsString s
  , Show s
  , Eq s
  ) =>
  s ->
  MatchTree re s x ->
  MatchArray
  -> x
extractResult s tree matchArray =
  let peek :: forall y. MatchTree re s y -> y
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
          case matchOnce re s' of
            Nothing -> error "Invariant violation."
            Just ma ->
              extractResult s' ta ma :
              extractRep re j ta (extract (ma ! j) s')

compileRE' :: forall compOpts execOpts s re x.
  ( IsString s
  , Monoid s
  , Show s
  , Eq s
  , RegexMaker re compOpts execOpts s
  , RegexLike re s
  ) =>
  Proxy compOpts ->
  Proxy execOpts ->
  RE s x -> (re, MatchTree re s x)
compileRE' _ _ r =
    ( makeRegex $ mkFullRegex r :: re
    , snd $ buildTree 1 r
    )

  where
    buildTree :: forall y. Int ->  RE s y -> (Int, MatchTree re s y)
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
        let (i1, ta) = buildTree (i+2) ra
            (i2, tb) = buildTree (i1+1) rb
        in (i2, MatchAlt (i+2, ta) (i2+1, tb))
      Rep rRep ->
        let r1 = rRep <&> Rep rRep
            compiled = makeRegex $ mkFullRegex r1 :: re
            (j, t) = buildTree 1 rRep
        in (i+j+1, MatchRep compiled i j t)

int :: forall s. (IsString s, ToString s) => RE s Int
int = Map (read @Int . toString @s) (raw 0 "-?[0-9]+")

infixl <&>
(<&>) :: RE s a -> RE s b -> RE s (a, b)
ra <&> rb = (,) <$> ra <*> rb

infixl </>
(</>) :: RE s a -> RE s b -> RE s (Either a b)
(</>) = Alt

-- | One or more. This is a somewhat more explicit alias to 'some'.
many1 :: RE s a -> RE s [a]
many1 = some

sepBy :: RE s a -> RE s b -> RE s [a]
sepBy ra rs = (:) <$> ra <*> many (rs *> ra)

opt :: RE s a -> RE s (Maybe a)
opt = Opt

str :: IsString s => String -> RE s s
str = raw 0 . fromString . escapeREString

-- | Use a raw regular expression to return a string. The 'Int' parameter
-- indicates how many groups are contained within the underlying regexp.
raw :: Int -> s -> RE s s
raw = Raw

-- The following functions are taken straight from regex-1.0.1.3 internals. A
-- dependence on the whole package does not seem justified, here.

-- Convert a string into a regular expression that will match that
-- string
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

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Regex.Safe
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

import Data.Array (Array, (!))
import Data.Coerce (coerce)
import Data.Functor.Alt
import Data.Maybe(fromJust)
import Data.Proxy (Proxy(..))
import Data.String(IsString(..))
import Text.Regex.Base

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
  -- The enclosing group could be made non-capturing. Unfortunately, this isn't
  -- supported by regex-tdfa.
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

newtype CompiledRE s x = CompiledRE { apply :: s -> Maybe x }

instance RegexBackend [Char] where
  type Compiled [Char] = CompiledRE [Char]
  compile = coerce $ compileRE (Proxy @Str.CompOption) (Proxy @Str.ExecOption)
  match = coerce

instance RegexBackend BS.ByteString where
  type Compiled BS.ByteString = CompiledRE BS.ByteString
  compile = coerce $ compileRE (Proxy @BS.CompOption) (Proxy @BS.ExecOption)
  match = coerce

data MatchTree re s x where
  MatchEps :: MatchTree re s ()
  MatchRaw :: Int -> MatchTree re s s
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
    MatchTree re s (a, [a]) ->
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
matchRE (re, tree) str = extractResult str tree <$> matchOnce re str

extractResult :: forall re s x.
  ( RegexLike re s
  , IsString s
  , Eq s
  ) =>
  s ->
  MatchTree re s x ->
  MatchArray
  -> x
extractResult str tree matchArray =
  let peek :: forall y. MatchTree re s y -> y
      peek tree' = extractResult str tree' matchArray in
  case tree of
    MatchEps -> ()
    MatchRaw i -> extract (matchArray ! i) str
    MatchApp tf ta -> peek tf $ peek ta
    MatchMap f ta -> f $ peek ta
    MatchOpt i ta ->
      -- XXX: May trigger double lookup of (matchArray ! i).
      -- Can we do better ?
      case extract (matchArray ! i) str of
        "" -> Nothing
        _ -> Just $ peek ta
    MatchAlt (i, ta) (_, tb) ->
      case extract (matchArray ! i) str of
        "" -> Left $ peek ta
        _ -> Right $ peek tb
    MatchRep re i ta ->
      case extract (matchArray ! i) str of
        "" -> []
        s -> uncurry (:) $
          case matchOnce re s of
            Nothing -> error "Invariant violation."
            Just ma -> extractResult s ta ma

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
compileRE' = undefined


compileRE :: forall compOpts execOpts s re x.
  ( IsString s
  , Monoid s
  , Show s
  , Eq s
  , RegexMaker re compOpts execOpts s
  , RegexLike re s
  ) =>
  Proxy compOpts ->
  Proxy execOpts ->
  RE s x -> (s -> Maybe x)
compileRE pc pe r str =
    let re = makeRegex $ mkFullRegex r :: re
        result = matchOnce re str
    in
      snd . getContent 1 r <$> result
  where
    getContent
      :: forall r. Int -- Num of capture groups before / next group
      -> RE s r
      -> MatchArray
      -> (Int, r)
    getContent i r ms = case r of
      Eps -> (i, ())
      App ra rb ->
        let (i', retA) = getContent i ra ms
            (i'', retB) = getContent i' rb ms in
          (i'', retA retB)
      Raw n _ -> (i+n+1, extract (ms ! i) str)
      Map f r ->
        let (i', ret) = getContent i r ms in (i', f ret)
      Opt r ->
        let matchedStr = extract (ms ! i) str in
          case matchedStr of
            "" -> (i+1+nGroups r, Nothing)
            _ -> Just <$> getContent (i+1) r ms
      Alt ra rb ->
        let matched1 = extract (ms ! (i+2)) str
            matched2 = extract (ms ! (i+ng1+3)) str
            (_, content1) = getContent (i+2) ra ms
            (_, content2) = getContent (i+3+ng1) rb ms
            (ng1, ng2) = (nGroups ra, nGroups rb) in
        case (matched1, matched2) of
          ("", _) -> (i+3+ng1+ng2, Right content2)
          -- Invariant: at least one matches if we get here !
          _ -> (i+4+ng1+ng2, Left content1)

      Rep r ->
        let r' = r <&> (Rep r)
            ng = nGroups r + 2
            matchedStr = extract (ms ! i) str in
        ( i+ng
        , case matchedStr of
            "" -> []
            s ->
              uncurry (:) $
                -- Invariant: this is necessarily a match.
                case compileRE pc pe r' s of
                  Just ret -> ret
                  Nothing ->
                    error $ "Invariant violation: can't parse " <>
                    show matchedStr <> " with: " <> show (regexStr r')
        )

nGroups :: RE s x -> Int
nGroups re = case re of
  Eps -> 0
  Raw n _ -> n+1
  Alt r1 r2 -> 3 + nGroups r1 + nGroups r2
  Opt r -> nGroups r + 1
  App r1 r2 -> nGroups r1 + nGroups r2
  Rep r -> nGroups r + 2
  Map _ r -> nGroups r

int :: RE String Int
int = Map (read @Int) (raw 0 "-?[0-9]+")

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

-- returns True iff the charactr is an RE meta character ('[', '*', '{', etc.)
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

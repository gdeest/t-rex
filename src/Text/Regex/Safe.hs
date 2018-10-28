{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
  ) where

import Data.Array (Array, (!))
import Data.Functor.Alt
import Data.Maybe(fromJust)
import Data.Proxy (Proxy(..))
import Data.String(IsString(..))
import Text.Regex.Base

import qualified Text.Regex.TDFA.String as Str
import qualified Data.ByteString as BS
import qualified Text.Regex.TDFA.ByteString as BS

data RE s r where
  Eps :: RE s ()
  Map :: (a -> b) -> RE s a -> RE s b
  App :: RE s (a -> b) -> RE s a -> RE s b
  Raw :: s -> RE s s
  Alt :: RE s a -> RE s b -> RE s (Either a b)
  Opt :: RE s a -> RE s (Maybe a)
  Rep :: RE s a -> RE s [a]

instance IsString s => IsString (RE s s) where
  fromString = Raw . fromString

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
  Raw s -> "(" <> s <> ")"
  -- The enclosing group could be made non-capturing. Unfortunately, this isn't
  -- supported by regex-tdfa.
  Alt ra rb -> "((" <> regexStr ra <> ")|(" <> regexStr rb <> "))"
  Opt ra -> "(" <> regexStr ra <> ")?"
  App ra rb -> regexStr ra <> regexStr rb
  Rep ra -> "((" <> regexStr ra <> ")*)"
  Map _ ra -> regexStr ra

mkFullRegex :: (IsString s, Monoid s) => RE s r -> s
mkFullRegex re = "^" <> regexStr re <> "$"

type CompiledRE s x = s -> Maybe x

class RegexBackend s where
  compile :: RE s x -> CompiledRE s x

instance RegexBackend [Char] where
  compile = compileRE (Proxy @Str.CompOption) (Proxy @Str.ExecOption)

instance RegexBackend BS.ByteString where
  compile = compileRE (Proxy @BS.CompOption) (Proxy @BS.ExecOption)


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
  RE s x -> CompiledRE s x
compileRE pc pe r str = -- trace str $
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
      Raw _ -> (i+1, extract (ms ! i) str)
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
        case matchedStr of
          "" -> (i+ng, [])
          s ->
            -- Invariant: this is necessarily a match.
            let (x, xs) =
                  case compileRE pc pe r' s of
                    Just ret -> ret
                    Nothing ->
                      error $ "Invariant violation: can't parse " <>
                        show matchedStr <> " with: " <> show (regexStr r')
            in
              (i+ng, x:xs)

nGroups :: RE s x -> Int
nGroups re = case re of
  Eps -> 0
  Raw _ -> 1
  Alt r1 r2 -> 3 + nGroups r1 + nGroups r2
  Opt r -> nGroups r + 1
  App r1 r2 -> nGroups r1 + nGroups r2
  Rep r -> nGroups r + 2
  Map _ r -> nGroups r

int :: RE String Int
int = Map (read @Int) (Raw "-?[0-9]+")

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

raw :: s -> RE s s
raw = Raw


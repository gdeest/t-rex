{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Text.Regex.Safe where

import Data.Array (Array, (!))
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
  Str :: s -> RE s s
  Alt :: RE s a -> RE s b -> RE s (Either a b)
  Opt :: RE s a -> RE s (Maybe a)
  Rep :: RE s a -> RE s [a]

instance IsString s => IsString (RE s s) where
  fromString = Str . fromString

instance Functor (RE s) where
  fmap = Map

instance Applicative (RE s) where
  pure x = Map (const x) Eps
  (<*>) = App

regexStr :: (IsString s, Monoid s) => RE s r -> s
regexStr re = case re of
  Eps -> mempty
  Str s -> "(" <> s <> ")"
  Alt ra rb -> "((" <> regexStr ra <> ")|(" <> regexStr rb <> "))"
  Opt ra -> "(" <> regexStr ra <> ")?"
  App ra rb -> "(" <> regexStr ra <> ")" <> "(" <> regexStr rb <> ")"
  Rep ra -> "((" <> regexStr ra <> ")*)"
  Map _ ra -> "(" <> regexStr ra <> ")"

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
        result = (\(_, ms, _) -> ms) <$> matchOnceText re str
    in
      snd . getContent 1 r <$> result
  where
    getContent
      :: forall r. Int -- Num of capture groups before / next group
      -> RE s r
      -> (MatchText s)
      -> (Int, r)
    getContent i r ms = case r of
      Eps -> (i, ())
      App ra rb ->
        let (i', retA) = getContent (i+1) ra ms
            (i'', retB) = getContent (i'+1) rb ms in
          (i'', retA retB)
      Str _ -> (i+1, fst (ms ! i))
      Map f r ->
        let (i', ret) = getContent (i+1) r ms in (i', f ret)
      Opt r ->
        let (matchedStr, _) = ms ! i in
          case matchedStr of
            "" -> (i+1+nGroups r, Nothing)
            _ -> Just <$> getContent (i+1) r ms
      Alt ra rb ->
        let (matched1, _) = ms ! (i+2)
            (matched2, _) = ms ! (i+ng1+3)
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
            (matchedStr, _) = ms ! i in
        case matchedStr of
          "" -> (i+ng, [])
          s ->
            -- Invariant: this is necessarily a match.
            let (x, xs) =
                  case compileRE pc pe r' s of
                    Just ret -> ret
                    Nothing ->
                      error $ "Can't parse " <> show matchedStr <> " with: " <> (show (regexStr r'))
            in
              (i+ng, x:xs)

nGroups :: RE s x -> Int
nGroups re = case re of
  Eps -> 0
  Str _ -> 1
  Alt r1 r2 -> 3 + nGroups r1 + nGroups r2
  Opt r -> nGroups r + 1
  App r1 r2 -> 2 + nGroups r1 + nGroups r2
  Rep r -> nGroups r + 2
  Map _ r -> nGroups r + 1

int :: RE String Int
int = Map (read @Int) (Str "-?[0-9]+")

infixl <&>
(<&>) :: RE s a -> RE s b -> RE s (a, b)
ra <&> rb = (,) <$> ra <*> rb

many :: RE s a -> RE s [a]
many = Rep

many1 :: RE s a -> RE s [a]
many1 re = (:) <$> re <*> many re

sepBy :: RE s a -> RE s b -> RE s [a]
sepBy ra rs = (:) <$> ra <*> many (rs *> ra)

opt :: RE s a -> RE s (Maybe a)
opt = Opt

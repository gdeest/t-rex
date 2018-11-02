{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Text.Regex.Safe
import Data.ByteString (ByteString)

main :: IO ()
main = hspec $ do
  it "returns Nothing on failure" $ do
    let re = "b" :: RE ByteString ByteString
        ce = compile re
    match ce "a" `shouldBe` Nothing

  it "captures optional patterns as Maybes" $ do
    let re = opt "a" :: RE ByteString (Maybe ByteString)
        ce = compile re
    match ce "a" `shouldBe` Just (Just "a")
    match ce "" `shouldBe` Just Nothing

  it "captures alternative patterns as Either" $ do
    let re = str "a" </> str "b"
               :: RE ByteString (Either ByteString ByteString)
        ce = compile re
    match ce "a" `shouldBe` Just (Left "a")
    match ce "b" `shouldBe` Just (Right "b")

  it "captures sequences as pairs" $ do
    let re = "a" <&> ("b" <&> "c") :: RE ByteString (ByteString, (ByteString,ByteString))
        ce = compile re
    match ce "abc" `shouldBe` Just ("a", ("b", "c"))

  it "captures repetitions as lists" $ do
    let re = many "a" :: RE ByteString [ByteString]
        ce = compile re
    match ce "aaaa" `shouldBe` Just ["a", "a", "a", "a"]
    let re' = many ("a" <&> "b") :: RE ByteString [(ByteString, ByteString)]
        ce' = compile re'
    match ce' "abababab" `shouldBe` Just [("a","b"), ("a", "b"), ("a", "b"), ("a", "b")]

  it "gracefully handles nested patterns" $ do
    let re = many ("a" <&> opt "b") </> str "c"
               :: RE ByteString (Either [(ByteString, Maybe ByteString)] ByteString)
        ce = compile re
        ab = ("a", Just "b")
        a = ("a", Nothing)
    match ce "abaaab" `shouldBe` Just (Left [ab, a, a, ab])
    match ce "c" `shouldBe` Just (Right "c")

  it "discards prefixes with *>" $ do
    let re = many (str "a") *> many (str "v") :: RE ByteString [ByteString]
        ce = compile re
        re' = str "aaaa" *> fmap id (raw 0 "v+") :: RE ByteString ByteString
        ce' = compile re'
    match ce "aaaavvvv" `shouldBe` Just ["v", "v", "v", "v"]
    match ce' "aaaavvvv" `shouldBe` Just "vvvv"

  it "parses separated lists" $ do
    let re = str "a" `sepBy` str "," :: RE ByteString [ByteString]
        re' = str "c" <&> re
        ce = compile re
        ce' = compile re'
    match ce "a,a,a,a" `shouldBe` Just ["a", "a", "a", "a"]
    match ce' "ca,a,a,a" `shouldBe` Just ("c", ["a", "a", "a", "a"])

  it "parses integers" $ do
    let ce = compile int :: CompiledRE ByteString Int
    match ce "-123" `shouldBe` Just (-123)
    match ce "567" `shouldBe` Just 567
    match ce "5aa" `shouldBe` Nothing

  it "parses separated lists" $ do
    let ce = compile (int `sepBy` str ",") :: CompiledRE ByteString [Int]
    match ce "-123" `shouldBe` Just [-123]
    match ce "567" `shouldBe` Just [567]
    match ce "567,123" `shouldBe` Just [567,123]
    match ce "-342,78" `shouldBe` Just [-342,78]
    match ce "5aa" `shouldBe` Nothing

  it "computes sums" $ do
    let re :: RE ByteString Int
        re =
          fmap sum $
          str "Compute the sum of: " *> int `sepBy` str ","
        ce = compile re
    match ce "Compute the sum of: 1,-123,58" `shouldBe` Just (-64)

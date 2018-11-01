{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Text.Regex.Safe

main :: IO ()
main = hspec $ do
  it "returns Nothing on failure" $ do
    let re = "b" :: RE String String
        ce = compile re
    match ce "a" `shouldBe` Nothing

  it "captures optional patterns as Maybes" $ do
    let re = opt "a" :: RE String (Maybe String)
        ce = compile re
    match ce "a" `shouldBe` Just (Just "a")
    match ce "" `shouldBe` Just Nothing

  it "captures alternative patterns as Either" $ do
    let re = str "a" </> str "b"
               :: RE String (Either String String)
        ce = compile re
    match ce "a" `shouldBe` Just (Left "a")
    match ce "b" `shouldBe` Just (Right "b")

  it "captures sequences as pairs" $ do
    let re = "a" <&> "b" :: RE String (String, String)
        ce = compile re
    match ce "ab" `shouldBe` Just ("a", "b")

  it "captures repetitions as lists" $ do
    let re = many "a" :: RE String [String]
        ce = compile re
    match ce "aaaa" `shouldBe` Just ["a", "a", "a", "a"]

  it "gracefully handles nested patterns" $ do
    let re = many ("a" <&> opt "b") </> "c"
               :: RE String (Either [(String, Maybe String)] String)
        ce = compile re
        ab = ("a", Just "b")
        a = ("a", Nothing)
    match ce "abaaab" `shouldBe` Just (Left [ab, a, a, ab])
    match ce "c" `shouldBe` Just (Right "c")

  it "discards prefixes with *>" $ do
    let re = many (str "a") *> many (str "v") :: RE String [String]
        ce = compile re
        re' = str "aaaa" *> fmap id (raw 0 "v+") :: RE String String
        ce' = compile re'
    match ce "aaaavvvv" `shouldBe` Just ["v", "v", "v", "v"]
    match ce' "aaaavvvv" `shouldBe` Just "vvvv"

  it "parses separated lists" $ do
    let re = str "a" `sepBy` str "," :: RE String [String]
        re' = str "c" <&> re
        ce = compile re
        ce' = compile re'
    match ce "a,a,a,a" `shouldBe` Just ["a", "a", "a", "a"]
    match ce' "ca,a,a,a" `shouldBe` Just ("c", ["a", "a", "a", "a"])

  it "parses integers" $ do
    let ce = compile int
    match ce "-123" `shouldBe` Just (-123)
    match ce "567" `shouldBe` Just 567
    match ce "5aa" `shouldBe` Nothing

  it "parses separated lists" $ do
    let ce = compile (int `sepBy` str ",")
    match ce "-123" `shouldBe` Just [-123]
    match ce "567" `shouldBe` Just [567]
    match ce "567,123" `shouldBe` Just [567,123]
    match ce "-342,78" `shouldBe` Just [-342,78]
    match ce "5aa" `shouldBe` Nothing

  it "computes sums" $ do
    let re :: RE String Int
        re =
          fmap sum $
          str "Compute the sum of: " *> int `sepBy` str ","
        ce = compile re
    match ce "Compute the sum of: 1,-123,58" `shouldBe` Just (-64)

{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Text.Regex.Safe

main :: IO ()
main = hspec $ do
  it "returns Nothing on failure" $ do
    let re = "b" :: RE String String
    compile re "a" `shouldBe` Nothing

  it "captures optional patterns as Maybes" $ do
    let re = opt "a" :: RE String (Maybe String)
        match = compile re
    match "a" `shouldBe` Just (Just "a")
    match "" `shouldBe` Just Nothing

  it "captures alternative patterns as Either" $ do
    let re = Alt "a" "b"
               :: RE String (Either String String)
        match = compile re
    match "a" `shouldBe` Just (Left "a")
    match "b" `shouldBe` Just (Right "b")

  it "captures sequences as pairs" $ do
    let re = "a" <&> "b" :: RE String (String, String)
        match = compile re
    match "ab" `shouldBe` Just ("a", "b")

  it "captures repetitions as lists" $ do
    let re = many "a" :: RE String [String]
        match = compile re
    match "aaaa" `shouldBe` Just ["a", "a", "a", "a"]

  it "gracefully handles nested patterns" $ do
    let re = Alt (many ("a" <&> opt "b")) "c"
               :: RE String (Either [(String, Maybe String)] String)
        match = compile re
        ab = ("a", Just "b")
        a = ("a", Nothing)
    match "abaaab" `shouldBe` Just (Left [ab, a, a, ab])
    match "c" `shouldBe` Just (Right "c")

  it "discards prefixes with *>" $ do
    let re = many (Str "a") *> many (Str "v") :: RE String [String]
        match = compile re
        re' = Str "aaaa" *> (Map id $ Str "v+") :: RE String String
        match' = compile re'
    match "aaaavvvv" `shouldBe` Just ["v", "v", "v", "v"]
    match' "aaaavvvv" `shouldBe` Just "vvvv"

  it "parses separated lists" $ do
    let re = sepBy (Str "a") (Str ",") :: RE String [String]
        re' = Str "c" <&> re
        match = compile re
        match' = compile re'
    match "a,a,a,a" `shouldBe` Just ["a", "a", "a", "a"]
    match' "ca,a,a,a" `shouldBe` Just ("c", ["a", "a", "a", "a"])

  it "parses integers" $ do
    let match = compile int
    match "-123" `shouldBe` Just (-123)
    match "567" `shouldBe` Just 567
    match "5aa" `shouldBe` Nothing

  it "parses separated lists" $ do
    let match = compile (int `sepBy` Str ",")
    match "-123" `shouldBe` Just [-123]
    match "567" `shouldBe` Just [567]
    match "567,123" `shouldBe` Just [567,123]
    match "-342,78" `shouldBe` Just [-342,78]
    match "5aa" `shouldBe` Nothing

  it "computes sums" $ do
    let re :: RE String Int
        re =
          fmap sum $
          Str "Compute the sum of: " *> int `sepBy` Str ","
        match = compile re
    match "Compute the sum of: 1,-123,58" `shouldBe` Just (-64)

import Test.Hspec

import Text.Regex.Safe

main :: IO ()
main = hspec $ do
  it "returns Nothing on failure" $ do
    compile (Str "b") "a" `shouldBe` Nothing

  it "captures optional patterns as Maybes" $ do
    let re = compile $ Opt (Str "a")
    re "a" `shouldBe` Just (Just "a")
    re "" `shouldBe` Just Nothing

  it "captures alternative patterns as Either" $ do
    let re = compile $ Alt (Str "a") (Str "b")
    re "a" `shouldBe` Just (Left "a")
    re "b" `shouldBe` Just (Right "b")

  it "captures sequences as pairs" $ do
    let re = compile $ (Str "a") <&> (Str "b")
    re "ab" `shouldBe` Just ("a", "b")

  it "captures repetitions as lists" $ do
    let re = compile $ Rep (Str "a")
    re "aaaa" `shouldBe` Just ["a", "a", "a", "a"]

  it "gracefully handles nested patterns" $ do
    let re = compile $ Alt (Rep ((Str "a") <&> (Opt (Str "b")))) (Str "c")
        ab = ("a", Just "b")
        a = ("a", Nothing)
    re "abaaab" `shouldBe` Just (Left [ab, a, a, ab])
    re "c" `shouldBe` Just (Right "c")

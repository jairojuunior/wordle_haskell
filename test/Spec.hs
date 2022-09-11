import           Test.Tasty
import           Test.Tasty.HUnit
import Jogo

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Letreco" [testCreateFeedbackString, testValidateChar]

testCreateFeedbackString :: TestTree
testCreateFeedbackString = 
    testGroup 
        "createFeedbackString (cria símbolos com feedback por letra da palavra fornecida)" 
        [ testCase 
            "JAIRO vs EMILI"
            ((createFeedbackString "JAIRO" "EMILI") @?= "✖✖✔✖✖"),
          testCase 
            "UFABC vs UFSCA"
            ((createFeedbackString "UFABC" "UFSCA") @?= "✔✔?✖?"),
          testCase 
            "GORDO vs MAGRO"
            ((createFeedbackString "GORDO" "MAGRO") @?= "???✖✔"),
          testCase 
            "ITAUL vs BRADE"
            ((createFeedbackString "ITAUL" "BRADE") @?= "✖✖✔✖✖"),
          testCase 
            "TEMOR vs RUMOR"
            ((createFeedbackString "TEMOR" "RUMOR") @?= "✖✖✔✔✔"),
          testCase 
            "REMOU vs REFAZ"
            ((createFeedbackString "REMOU" "REFAZ") @?= "✔✔✖✖✖"),
          testCase 
            "AMORA vs AMORA"
            ((createFeedbackString "AMORA" "AMORA") @?= "✔✔✔✔✔"),
          testCase 
            "MINAS vs GORRO"
            ((createFeedbackString "MINAS" "GORRO") @?= "✖✖✖✖✖"),
          testCase 
            "SALTO vs LOTAS"
            ((createFeedbackString "SALTO" "LOTAS") @?= "?????")
        ]

testValidateChar :: TestTree
testValidateChar = 
    testGroup
        "validateChar (aceita apenas caracteres entre A e Z, sem acento ou caracteres especiais)"
        [testCase "A aceito" ((validateChar 'A') @?= True),
         testCase "a aceito" ((validateChar 'a') @?= True),
         testCase "Z aceito" ((validateChar 'Z') @?= True),
         testCase "z aceito" ((validateChar 'z') @?= True),
         testCase "Í não aceito" ((validateChar 'Í') @?= False),
         testCase "í não aceito" ((validateChar 'í') @?= False),
         testCase "! não aceito" ((validateChar '!') @?= False),
         testCase "0 não aceito" ((validateChar '0') @?= False),
         testCase "@ não aceito" ((validateChar '@') @?= False)]
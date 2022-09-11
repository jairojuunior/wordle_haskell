--A interface foi inspirada no tutorial https://github.com/NorfairKing/tui-base
--Alguns códigos do tutorial foram reutilizados 

module Jogo where

import System.IO
import System.Random
import System.Directory
import Data.Char
import Data.Time.Clock
import Data.Time.LocalTime

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Input.Events

import Brick (Widget, simpleMain, (<+>), str, withBorderStyle, joinBorders)
import Brick.Widgets.Center (center, hCenterWith)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)


data GamerState = Working | InvalidWord | WrongWord | IncompleteWord | Victory | GameOver deriving (Show, Eq)

terminalUserInterface :: IO ()
terminalUserInterface = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print (answer endState)

data TuiState =
  TuiState { tuiGamerState :: GamerState,
             word :: String, --Currrent input
             wordls :: [[Char]], --List of user inputs
             feedbackls :: [[Char]], --List of user inputs
             answer :: String, --Secret, the answer
             allAnswers :: [String],
             attemptsLeft :: Int,
             generator :: StdGen}
  deriving (Show, Eq)

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty []
    }

buildInitialState :: IO TuiState
buildInitialState = do 
  state <- return Working
  allAnswers <- (getWordsFromFile "./resources/palavras_comprimento_5.csv")
  word <- return ""
  wordls <- return ["", "", "", "", "", ""]
  feedbackls <- return ["", "", "", "", "", ""]
  attemptsLeft <- return 5
  currTime <- getCurrentTime
  let seed = floor $ utctDayTime currTime :: Int
  generator <- return (mkStdGen seed)
  pure TuiState {tuiGamerState = state, 
                 allAnswers = (map . map) toUpper allAnswers,
                 word = word, 
                 wordls = wordls, 
                 feedbackls = feedbackls,
                 answer = giveRandomElement ((map . map) toUpper allAnswers) generator, 
                 attemptsLeft= attemptsLeft,
                 generator = generator}
                 

--FUNÇÕES QUE DESENHAM ELEMENTOS NA TELA

--Concatena elementos da interface
drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [(joinBorders $
              withBorderStyle unicode $ 
              borderWithLabel (str "WORDLE HASKELL") 
              $ vBox 
              $ concat [[drawUserFeedback $ tuiGamerState ts], 
                        [drawWord $ (wordls ts) !! 0],
                        [drawFeedbackSigns $ (feedbackls ts) !! 0],
                        [drawWord $ (wordls ts) !! 1],
                        [drawFeedbackSigns $ (feedbackls ts) !! 1],
                        [drawWord $ (wordls ts) !! 2],
                        [drawFeedbackSigns $ (feedbackls ts) !! 2],
                        [drawWord $ (wordls ts) !! 3],
                        [drawFeedbackSigns $ (feedbackls ts) !! 3],
                        [drawWord $ (wordls ts) !! 4],
                        [drawFeedbackSigns $ (feedbackls ts) !! 4],
                        [drawWord $ (wordls ts) !! 5],
                        [drawFeedbackSigns $ (feedbackls ts) !! 5],
                        [drawFooterLabel]] ) 
              <=> drawFooterMenu ]


drawPath :: FilePath -> Widget n
drawPath = str

--Escreve palavra na janela do programa, aplicando estilização para indicar caracteres restantes
drawWord :: [Char] -> Widget n
drawWord s = hCenterWith (Just ' ') (str pal)
  where
    pal = case length s of
      0 -> "_ _ _ _ _"
      1 -> [s!!0] ++ " _ _ _ _"
      2 -> [s!!0] ++ " " ++ [s!!1] ++ " _ _ _"
      3 -> [s!!0] ++ " " ++ [s!!1] ++ " " ++ [s!!2] ++ " _ _"
      4 -> [s!!0] ++ " " ++ [s!!1] ++ " " ++ [s!!2] ++ " " ++ [s!!3] ++ " _"
      5 -> [s!!0] ++ " " ++ [s!!1] ++ " " ++ [s!!2] ++ " " ++ [s!!3] ++ " " ++ [s!!4]

--Escreve palavra na janela do programa, aplicando estilização para indicar caracteres restantes
drawFeedbackSigns :: [Char] -> Widget n
drawFeedbackSigns s = hCenterWith (Just ' ') (str pal)
  where
    pal = if (length s) > 0 then [s!!0] ++ " " ++ [s!!1] ++ " " ++ [s!!2] ++ " " ++ [s!!3] ++ " " ++ [s!!4]
          else " "

--Exibe feedback pro usuário na primeira linha da janela do programa
drawUserFeedback :: GamerState -> Widget n
drawUserFeedback s 
  | s == IncompleteWord = strWrap "Palavra incompleta: Digite os 5 caracteres."
  | s == InvalidWord    = strWrap "Palavra inválida: Termo não existe no vocabulário do Letreco."
  | s == WrongWord      = strWrap "Palavra incorreta, continue tentando."
  | s == GameOver       = strWrap "Você perdeu."
  | s == Victory        = strWrap "Parabéns, você descobriu a palavra!"
  | otherwise           = strWrap " "

--Exibe legenda (especializa drawText)
drawFooterLabel :: Widget n
drawFooterLabel = drawText " ✔: Existe nesta posição.  ?: Existe em outra posição.  ✖: Não existe na palavra."

--Exibe menu na parte inferior do programa (especializa drawText)
drawFooterMenu :: Widget n
drawFooterMenu = drawText "ENTER: Enviar resposta.  BACKSPACE: Apagar.  HOME: Nova partida.  ESC: Sair do programa."


drawText :: String -> Widget n
drawText t = strWrap t

--FUNÇÕES QUE TRATAM EVENTOS

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KEsc) [] -> halt s -- sair do programa (esc)
        EvKey (KHome) [] -> continue (newGame s) -- iniciar nova partida
        EvKey (KEnter) [] -> continue (checkAnswer s) -- verificar resposta
        EvKey (KBS) [] -> continue (remCharFromTuiStateWord s) -- apaga (backspace)
        EvKey (KChar c) [] -> continue (addCharToTuiStateWord c s) -- adiciona letra
        EvKey _ [] -> continue s
    _ -> continue s

--Verifica resposta e altera estados (state)
checkAnswer :: TuiState -> TuiState
checkAnswer state
  | length(word state) < 5                                        = stateIncomplete
  | (word state) == (answer state)                                = stateVictory
  | not ((word state) `elem` (allAnswers state))                  = stateInvalid
  | ((word state) /= (answer state)) && (attemptsLeft state) > 0  = stateIncorrect
  | ((word state) /= (answer state)) && (attemptsLeft state) == 0 = stateGameOver
  where
    stateIncomplete = TuiState {tuiGamerState = IncompleteWord, 
                                word = word state, 
                                wordls = wordls state, 
                                answer = answer state, 
                                allAnswers = allAnswers state,
                                feedbackls = feedbackls state,
                                attemptsLeft = attemptsLeft state}
    stateInvalid = TuiState {tuiGamerState = InvalidWord, 
                              word = word state, 
                              wordls = wordls state, 
                              answer = answer state, 
                              allAnswers = allAnswers state,
                              feedbackls = feedbackls state,
                              attemptsLeft = attemptsLeft state}
    stateVictory = TuiState {tuiGamerState = Victory, 
                             word = word state, 
                             wordls = wordls state, 
                             answer = answer state, 
                             allAnswers = allAnswers state,
                             feedbackls = [if wrd /= "" then createFeedbackString wrd (answer state)
                                           else "" | wrd <- (wordls state)],
                             attemptsLeft = attemptsLeft state,
                             generator = generator state}
    stateIncorrect = TuiState {tuiGamerState = WrongWord, 
                              word = "", 
                              wordls = wordls state, 
                              answer = answer state, 
                              allAnswers = allAnswers state,
                              feedbackls = [if wrd /= "" then createFeedbackString wrd (answer state)
                                            else "" | wrd <- (wordls state)],
                              attemptsLeft = (attemptsLeft state)-1,
                              generator = generator state}
    stateGameOver = TuiState {tuiGamerState = GameOver, 
                              word = word state, 
                              wordls = wordls state, 
                              answer = answer state, 
                              allAnswers = allAnswers state,
                              feedbackls = [if wrd /= "" then createFeedbackString wrd (answer state)
                                            else "" | wrd <- (wordls state)],
                              attemptsLeft = attemptsLeft state,
                              generator = generator state}
  
addCharToTuiStateWord :: Char -> TuiState -> TuiState
addCharToTuiStateWord c state = if (length (word state) < 5) 
  && (validateChar c) && ((tuiGamerState state) /= GameOver) && ((tuiGamerState state) /= Victory) then newTuiState else state
  where
    newWord = (word state) ++ [toUpper c]
    newTuiState = TuiState {tuiGamerState = Working, 
                            word = newWord, 
                            wordls = if (attemptsLeft state) == 5 then [newWord, ((wordls state) !! 1), ((wordls state) !! 2), ((wordls state) !! 3), ((wordls state) !! 4), ((wordls state) !! 5)]   
                              else if (attemptsLeft state) == 4 then [((wordls state) !! 0), newWord, ((wordls state) !! 2), ((wordls state) !! 3), ((wordls state) !! 4), ((wordls state) !! 5)] 
                              else if (attemptsLeft state) == 3 then [((wordls state) !! 0), ((wordls state) !! 1), newWord, ((wordls state) !! 3), ((wordls state) !! 4), ((wordls state) !! 5)] 
                              else if (attemptsLeft state) == 2 then [((wordls state) !! 0), ((wordls state) !! 1), ((wordls state) !! 2), newWord, ((wordls state) !! 4), ((wordls state) !! 5)] 
                              else if (attemptsLeft state) == 1 then [((wordls state) !! 0), ((wordls state) !! 1), ((wordls state) !! 2), ((wordls state) !! 3), newWord, ((wordls state) !! 5)] 
                              else [((wordls state) !! 0), ((wordls state) !! 1), ((wordls state) !! 2), ((wordls state) !! 3), ((wordls state) !! 4), newWord],                         
                            answer = answer state, 
                            allAnswers = allAnswers state,
                            feedbackls = feedbackls state,
                            attemptsLeft = attemptsLeft state,
                            generator = generator state}

remCharFromTuiStateWord :: TuiState -> TuiState
remCharFromTuiStateWord state = if (length (word state) > 0) 
  && ((tuiGamerState state) /= GameOver) && ((tuiGamerState state) /= Victory) then newTuiState else state
  where
    newWord = init (word state)
    newTuiState = TuiState {tuiGamerState = Working, 
                            word = newWord, 
                            wordls = if (attemptsLeft state) == 5 then [newWord, ((wordls state) !! 1), ((wordls state) !! 2), ((wordls state) !! 3), ((wordls state) !! 4), ((wordls state) !! 5)]   
                              else if (attemptsLeft state) == 4 then [((wordls state) !! 0), newWord, ((wordls state) !! 2), ((wordls state) !! 3), ((wordls state) !! 4), ((wordls state) !! 5)] 
                              else if (attemptsLeft state) == 3 then [((wordls state) !! 0), ((wordls state) !! 1), newWord, ((wordls state) !! 3), ((wordls state) !! 4), ((wordls state) !! 5)] 
                              else if (attemptsLeft state) == 2 then [((wordls state) !! 0), ((wordls state) !! 1), ((wordls state) !! 2), newWord, ((wordls state) !! 4), ((wordls state) !! 5)] 
                              else if (attemptsLeft state) == 1 then [((wordls state) !! 0), ((wordls state) !! 1), ((wordls state) !! 2), ((wordls state) !! 3), newWord, ((wordls state) !! 5)] 
                              else [((wordls state) !! 0), ((wordls state) !! 1), ((wordls state) !! 2), ((wordls state) !! 3), ((wordls state) !! 4), newWord],     
                            answer = answer state, 
                            feedbackls = feedbackls state,
                            attemptsLeft = attemptsLeft state,
                            allAnswers = allAnswers state,
                            generator = generator state}

--Reinicia estado para nova partida
newGame :: TuiState -> TuiState
newGame state = newTuiState
  where
    newAnswer = giveRandomElement (allAnswers state) (generator state)
    newTuiState = TuiState {tuiGamerState = Working, 
                            word = "", 
                            wordls = ["", "", "", "", "", ""],
                            feedbackls = ["", "", "", "", "", ""],
                            answer = newAnswer, 
                            allAnswers = allAnswers state,
                            attemptsLeft = 5,
                            generator = generator state}


--Cria string de feedback. Esta string é exibida usando drawFeedbackSigns
createFeedbackString :: [Char] -> [Char] -> [Char]
createFeedbackString wrd ans = feedbackStr
  where
    ansHasCharsOfwrd = hasCharsOn wrd ans
    ansIsPositionedwrd = isCharInPosition wrd ans
    feedbackStr = feedbackSigns ansHasCharsOfwrd ansIsPositionedwrd

feedbackSigns :: [Bool] -> [Bool] -> [Char]
feedbackSigns (hc:hcs) (ip:ips) = if hcs /= [] then [feedbackSign hc ip] ++ (feedbackSigns hcs ips)
                                  else [feedbackSign hc ip]

feedbackSign :: Bool -> Bool -> Char
feedbackSign hasChar isPositioned
  | isPositioned == True  = '✔'
  | hasChar == True       = '?'
  | otherwise             = '✖'

hasCharOn :: Char -> [Char] -> Bool
hasCharOn c (a:as) = if c == a then True 
                     else if as == [] && c /= a then False
                     else hasCharOn c as

hasCharsOn :: [Char] -> [Char] -> [Bool]
hasCharsOn [] _ = [False] --Never used (haskell...)
hasCharsOn _ [] = [False] --Never used
hasCharsOn (c:cs) as = if cs /= [] then [hasCharOn c as] ++ (hasCharsOn cs as)
                       else [hasCharOn c as]

isCharInPosition :: [Char] -> [Char] -> [Bool]
isCharInPosition [] _ = [False]
isCharInPosition _ [] = [False]
isCharInPosition (c:cs) (a:as) = if cs /= [] && c == a then [True] ++ (isCharInPosition cs as)  
                                 else if cs /= [] then [False] ++ (isCharInPosition cs as)
                                 else if cs == [] && c == a then [True]
                                 else [False]


--AUXILIARY FUNCTIONS

--Source: https://stackoverflow.com/questions/23264788/convert-all-the-elements-in-a-file-into-a-array-in-haskell
getWordsFromFile :: String -> IO [String]
getWordsFromFile file = readFile file >>= return . words


--Source: https://stackoverflow.com/questions/52643395/string-contains-only-the-alphabet-in-haskell
validateChar :: Char -> Bool
validateChar c = (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))

giveRandomElement :: [String] -> StdGen -> String
giveRandomElement ls generator = ls !! rand where
  n = length ls
  (rand, _) = randomR (0,(n-1)) generator
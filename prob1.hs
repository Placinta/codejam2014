{-# OPTIONS_GHC -w -fno-warn-unused-imports -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import System.IO
import Data.Matrix
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Attoparsec.Char8
import Lens.Family
import Lens.Family.TH
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Attoparsec as CA
import Control.Monad.Trans.Resource


data CardArrangement = CardArrangement { _mat :: Matrix Int } deriving (Show)
type RowAnswer = Int
data Observing = Observing { _rowAnswer :: RowAnswer, _cardArrangement :: CardArrangement } deriving (Show)
data TestCase = TestCase { _obs1 :: Observing, _obs2 :: Observing } deriving (Show)
type TestCaseCount = Int
data Program = Program { _testCaseCount :: TestCaseCount, _testcases :: [TestCase] } deriving (Show)
data CardsFoundState = CardsFoundState {
	_cardsFound :: Int,
	_listOfCards :: [Int]
} deriving (Show)

$(mkLenses ''CardArrangement)
$(mkLenses ''Observing)
$(mkLenses ''TestCase)
$(mkLenses ''Program)
$(mkLenses ''CardsFoundState)

matrixSize = 4

generator (x, y) = matrixSize * (x - 1) + y
testMatrix = matrix matrixSize matrixSize generator

parseCard :: Parser Int
parseCard = do
	card <- decimal
	skipSpace
	return card

parseCardArrangement :: Parser CardArrangement
parseCardArrangement = do
	cards <- count 16 parseCard
	return $ CardArrangement $ makeArrangement cards
	where 
		makeArrangement cards = fromList matrixSize matrixSize cards

parseObserving :: Parser Observing
parseObserving = do
	rowAnswer <- decimal
	skipSpace
	cardArrangement <- parseCardArrangement
	return $ Observing rowAnswer cardArrangement

parseTestCase :: Parser TestCase
parseTestCase = do
	obs1 <- parseObserving
	skipSpace
	obs2 <- parseObserving
	return $ TestCase obs1 obs2

parseProgram :: Parser Program
parseProgram = do
	testCaseCount <- decimal
	skipSpace
	testCases <- count testCaseCount parseTestCase
	return $ Program testCaseCount testCases

solveEverything :: Program -> IO ()
solveEverything (Program count testcases) = do
	results <- mapM sovlveTestCase testcases
	let resultsToPrint = (zip [1..] results)
	let outputLines = map (\(i, result) -> "Case #" ++ show i ++ ": " ++ result) resultsToPrint
	let output = L.intercalate "\n" outputLines
	print output
	writeFile "prob1_res.txt" output
	

sovlveTestCase :: TestCase -> IO (String) 
sovlveTestCase (TestCase obs1 obs2) = do
	let result = outputFunction $ computeMagic obs1 obs2
	return result

computeMagic :: Observing -> Observing -> CardsFoundState
computeMagic obs1 obs2 = 
    result
	where 
	      firstRow = getRow (obs1^.rowAnswer)  $ obs1^.cardArrangement.mat
	      secondRow = getRow (obs2^.rowAnswer) $ obs2^.cardArrangement.mat
	      result = F.foldl cardFinder (CardsFoundState 0 []) firstRow
	      cardFinder cardsFoundState cardNumber = 
	      	if V.elem cardNumber secondRow == True
	      	then cardsFoundState & cardsFound +~ 1 & listOfCards <>~ [cardNumber]
	      	else cardsFoundState

outputFunction :: CardsFoundState -> String
outputFunction record = 
	let value = 
		case record^.cardsFound of
		0 -> "Volunteer cheated!"
		1 -> show . head $ record^.listOfCards
		_ -> "Bad magician!"
	in value
	


attoParsecProgramParser :: String -> IO (Maybe Program)
attoParsecProgramParser filename = do
	file <- B.readFile filename
	let parseResult = do
		parseOnly parseProgram file
	case parseResult of
		Left err -> do 
			print err
			return Nothing
		Right program -> return $ Just program

--programSink :: (MonadResource m) => Sink B.ByteString m ()
--programSink = do
--	value <- await
--	liftIO $ print value

conduitProgramParser :: String -> IO (Maybe Program)
conduitProgramParser filename = runResourceT $ do
	program <- CB.sourceFile filename $$ CA.sinkParser parseProgram
	return $ Just program

parserChooser :: Int -> (String -> IO (Maybe Program))
parserChooser choice = case choice of
	1 -> attoParsecProgramParser
	2 -> conduitProgramParser
	_ -> attoParsecProgramParser

main :: IO ()
main = do 
	let filename = "prob1.txt"
	programState <- parserChooser 2 filename
	case programState of
		Just program -> solveEverything program
		Nothing -> do 
			print "Error parsing file"
			return ()
	
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
import qualified Debug.Trace as DT


type FarmCost = Double
type ProductionRate = Double
type CookieCount = Double
type TimeSpent = Double
type TestCaseCount = Int

data TestCase = TestCase { _farmCost :: CookieCount, _farmRate :: ProductionRate, _goal :: CookieCount } deriving (Show)

data CookieState = CookieState { 
	_minTime :: TimeSpent, 
	_timeSpent :: TimeSpent,
	_currentCookies :: CookieCount, 
	_currentRate :: ProductionRate, 
	_info :: TestCase 
	} deriving (Show)

data ProblemResult = ProblemResult { _timeTillFarm :: TimeSpent, _timeTillGoal :: TimeSpent } deriving (Show)

data Program = Program { _testCaseCount :: TestCaseCount, _testcases :: [TestCase] } deriving (Show)

initialRate = 2.0 :: ProductionRate
initialCookies = 0 :: CookieCount

$(mkLenses ''TestCase)
$(mkLenses ''Program)
$(mkLenses ''ProblemResult)
$(mkLenses ''CookieState)

parseTestCase :: Parser TestCase
parseTestCase = do
	cost <- double
	skipSpace
	rate <- double
	skipSpace
	goal <- double
	skipSpace
	return $ TestCase cost rate goal

parseProgram :: Parser Program
parseProgram = do
	testCaseCount <- decimal
	skipSpace
	testCases <- count testCaseCount parseTestCase
	return $ Program testCaseCount testCases

solveAllTestcases :: String -> Program -> IO ()
solveAllTestcases output_filename (Program count testcases) = do
	results <- mapM sovlveTestCase testcases
	let resultsToPrint = (zip [1..] results)
	let outputLines = map (\(i, result) -> "Case #" ++ show i ++ ": " ++ result) resultsToPrint
	let output = L.intercalate "\n" outputLines
	print output
	writeFile output_filename output
	

sovlveTestCase :: TestCase -> IO (String) 
sovlveTestCase testcase = do
	let result = outputFunction $ computeTimeTillCookies testcase
	return result


timeTillNextFarm :: ProductionRate -> CookieCount -> CookieCount -> TimeSpent
timeTillNextFarm currentRate currentCookies farmCost = (farmCost - currentCookies) / currentRate	

computetimeTillGoal :: ProductionRate -> CookieCount -> CookieCount -> TimeSpent
computetimeTillGoal currenRate currentCookies goal = (goal - currentCookies) / currenRate

initialState :: TestCase -> CookieState
initialState testcase = (CookieState 10000000.0 0 initialCookies initialRate testcase)

computeTimeTillCookies :: TestCase -> CookieState
computeTimeTillCookies testcase = 
	computeTimeTillCookiesHelper $ initialState testcase
	
computeTimeTillCookiesHelper :: CookieState -> CookieState
computeTimeTillCookiesHelper cookieState =
	if spentForGoal > previousGoalTime 
		then cookieState & timeSpent .~ previousGoalTime
	else let newState = cookieState & currentRate +~ (infoRates^.farmRate) & timeSpent +~ nextFarmIn & minTime .~ spentForGoal
	     in computeTimeTillCookiesHelper newState
	where
		rate = cookieState^.currentRate
		cookies = cookieState^.currentCookies
		infoRates = cookieState^.info
		spentUntilNow = cookieState^.timeSpent
		nextFarmIn = timeTillNextFarm rate cookies (infoRates^.farmCost)
		goalIn = computetimeTillGoal rate cookies (infoRates^.goal)
		spentForFarm = spentUntilNow + nextFarmIn
		spentForGoal = spentUntilNow + goalIn
		newCookieCount = cookies + (nextFarmIn * rate - (infoRates^.farmCost))
		previousGoalTime = cookieState^.minTime


outputFunction :: CookieState -> String
outputFunction record = 
	let value = show (record^.minTime) in value


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
	let input_filename = "prob2.txt"
	let output_filename = "prob2_res.txt"
	programState <- parserChooser 2 input_filename
	case programState of
		Just program -> solveAllTestcases output_filename program
		Nothing -> do 
			print "Error parsing file"
			return ()
	
{-|
Module      : Main
Description : ECS713. CW2 - Yahoo Finance. Main module used for the program
Copyright   : (c) Zhanelya Subebayeva, January 2015
License     : GNU
Maintainer  : subebo91@inbox.ru
Stability   : experimental
Portability : MacOS, Linux

Within this module the main interactive functionality is provided.
This module is linked with: "DataType", "DB", "NW" and "Parse"
-}

module Main where

import System.Environment (getArgs)
import qualified System.Exit as Exit
import qualified Data.Either as Either

import DataType
import NW
import DB
import Parse

-- * Interaction
{-| The 'main' function is the principle point of interaction with the user.
    @ghc Main.hs@ will build the source together, if all the modules are in the same folder.
    Use @./Main@ to run the program
-}
main :: IO()
main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          interactWith function
        myFunction = f

-- |The 'interactWith' function provides interaction with user        
interactWith :: a -- ^The 'a' argument is used to pass a parsing function
                -> IO()
interactWith function = do
  -- initialise database with 2 tables
  initialise
  name <- showIntro
  showMenu name

-- |The 'showIntro' function shows introduction and ask for user name
showIntro :: IO(String) -- ^The 'IO'('String') argument is used to return a username 
showIntro = do
  putStrLn "Hello! What is your name?"
  name <- getLine
  putStrLn $ "Welcome to Stock Prices, " ++ name ++ "!"  
  return name

-- |The 'showMenu' function shows menu options and run menu until user exits
showMenu :: String -- ^The 'String' argument is used to pass a username
            -> IO()  
showMenu name= do
  putStrLn ""
  
  putStrLn "What do you want to do? (type in a number to continue)"
  putStrLn " 1 List companies present in DB"
  putStrLn " 2 Download new stock prices"
  putStrLn " 3 Show stock prices for a certain company from DB"
  putStrLn " 4 Run a query function on data"
  putStrLn " 5 Exit"
  
  action <- getLine
  case action of
  	"1" -> listCompanies
  	"2" -> downloadPrices name
  	"3" -> getPrices
  	"4" -> runQuery
  	"5" -> exit name
  	_ -> putStrLn "... Sorry, unknown command"
  showMenu name

-- * Downloading data  
-- |The 'downloadPrices' function downloads stock prices for a specified company
downloadPrices :: String -- ^The 'String' argument is used to pass a username
                  -> IO()
downloadPrices name = do
  putStrLn ""		
  putStrLn $ "Prices will be stored under your name, " ++ name ++"."
  putStrLn "Please enter company abbreviation"
  putStrLn "(if you don't know any - enter MSFT, YHOO or AAPL):"
  
  cName <- getLine 
  exists <- queryCompany cName
  if (length exists) > 0 
    then
       putStrLn exists
    else do
       putStrLn $ "... Downloading " ++ cName ++ " prices ..."
       let link = "http://real-chart.finance.yahoo.com/table.csv?s=" ++ cName
       urlData <- downloadURL link
       -- check if link is valid
       if Either.isRight urlData 
         then do 
             insertMany cName (fn $ lines $ getFromRight urlData)
             insertCompany cName name
             putStrLn ("... " ++ cName ++ " data was downloaded to DB")
         else putStrLn "... Invalid company name" 

-- * Query functions   
-- |The 'listCompanies' function shows companies list from DB
listCompanies :: IO()
listCompanies = do
  putStrLn "... Loading DB ..."
  companies <- queryCompanies
  putStr ""
      
-- |The 'getPrices' function gets stock prices from DB for a specified company
getPrices :: IO()  
getPrices = do
  putStrLn "Please enter company abbreviation (e.g. MSFT, YHOO):"  
  cName <- getLine
  stock <- queryStock cName
  putStr ""

-- |The 'runQuery' function shows available to user queries  
runQuery :: IO()
runQuery = do
  putStrLn "Please chose a query to process"
  putStrLn "(please note, data is provided for the last 100 days only):"
  putStrLn " 1 Get the cheapest close price of the stock among the companies from DB"
  putStrLn " 2 Get the most expensive close price of the stock among the companies from DB"
  putStrLn " 3 Get the highest stock volume among the companies stored in DB"
  putStrLn " 4 Get volume, open and close prices for a company at a certain date"
  q <- getLine
  putStrLn $ "... Executing " ++ q ++ " ..."
  case q of 
    "1" -> do price <- queryMinPrice
              putStr ""
    "2" -> do price <- queryMaxPrice
              putStr ""
    "3" -> do price <- queryMaxVolume
              putStr ""
    "4" -> do 
              putStrLn ("Please enter company abbreviation (e.g. MSFT, YHOO)")
              company <- getLine
              putStrLn ("Please enter date in a format yyyy-mm-dd, (e.g. 2015-01-09)")
              date <- getLine
              stock <- queryDate company date
              putStr ("")
    _ -> putStrLn "... Invalid query"

-- * Exit function
-- |The 'exit' function exits the program upon user choice of "Exit" option
exit :: String -- ^The 'String' argument is used to pass a username
        -> IO()
exit name = do
  putStrLn $ "Are you sure you want to exit, " ++ name ++ "?"
  putStrLn " 1 Yes"
  putStrLn " 2 No"
  answer <- getLine
  if answer == "1" then Exit.exitSuccess else showMenu name

-- * Helper functions  
-- |The 'getFromRight' function parses Either format to get a value
getFromRight :: Either a b -- ^The 'Either' argument is used to enable error handling
                -> b -- ^The 'b' argument is used to return a value
getFromRight (Right str) = str

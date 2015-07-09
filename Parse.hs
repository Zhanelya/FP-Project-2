{-|
Module      : Parse
Description : Parsing functionality for the program
Copyright   : (c) Zhanelya Subebayeva, January 2015
License     : GNU
Maintainer  : subebo91@inbox.ru
Stability   : experimental
Portability : MacOS, Linux

Within this module the functions to parse downloaded data into Haskell accessible data are provided.
-}

module Parse where

import DataType

-- * Functions to prepare input
-- |The 'f' function splits data from web page into lines and pass it to fn
f :: String -> String
f input =  show (fn (lines input))

-- |The 'fn' function returns [] if no lines received, otherwise processes the lines
fn :: [String] -> [Price]
fn [] = []
fn (l:ls) = if null ls then [] else process ls

-- |The 'process' function has a purpose to process data from web page	
process :: [String] -> [Price]		
process ls = parse $ reverse(take100 ls [] 0)

-- |The 'take100' function takes first 100 lines of the document
take100 :: (Num a, Eq a) => [a1] -> [a1] -> a -> [a1]
take100 [] acc n = acc
take100 ls acc 100 = acc
take100 (l:ls) acc n = take100 ls (l:acc) (n+1)

-- * Functions to parse input
-- |The 'parse' function parses the whole document
parse :: [String] -> [Price]
parse [] = []
parse (l:ls) = (parseLine l):(parse ls)

-- |The 'parseLine' function parses each line of .csv file
parseLine :: String -> Price
parseLine l = Price {date = parseDate l,
                     open = parseOpen l,
                     high = parseHigh l,
                     low = parseLow l,
                     close = parseClose l,
                     volume = parseVolume l,
                     adj_close = parseAdjCl l}

-- |The 'parseDate' function parses date
parseDate :: String -- ^Text
             -> String -- ^Date
parseDate [] = error "err"
parseDate cs = takeWhile (/=',') cs

-- |The 'parseOpen' function parses open price
parseOpen :: String -- ^Text
             -> String -- ^Open price
parseOpen [] = error "err"
parseOpen cs = takeWhile (/=',') $ getRest cs

-- |The 'parseHigh' function parses high price
parseHigh :: String -- ^Text
             -> String -- ^ High price
parseHigh [] = error "err"
parseHigh cs = takeWhile (/=',') $ getRest $ getRest cs

-- |The 'parseLow' function parses low price
parseLow :: String -- ^Text
            -> String -- ^Low price
parseLow [] = error "err"
parseLow cs = takeWhile (/=',') $getRest $ getRest $ getRest cs

-- |The 'parseClose' function parse close price
parseClose :: String -- ^Text
              -> String -- ^Close price
parseClose [] = error "err"
parseClose cs = takeWhile (/=',') $ getRest $ getRest $ getRest $ getRest cs

-- |The 'parseVolume' function parse stock volume
parseVolume :: String -- ^Text
              -> Int -- ^ Stock volume
parseVolume [] = error "err"
parseVolume cs = read $ takeWhile (/=',') $ getRest $ getRest $ getRest $ getRest $ getRest cs

-- |The 'parseAdjCl' function parse adj close price
parseAdjCl :: String -- ^Text
              -> String -- ^Adj Close
parseAdjCl [] = error "err"
parseAdjCl cs = takeWhile (/=',') $ getRest $ getRest $ getRest $ getRest $ getRest $ getRest cs

-- * Helper functions
-- |The 'getRest' function is a helper function to parse a line
getRest :: String -- ^Text
           -> String -- ^Drop anything that is already parsed
getRest cs = tail $ dropWhile (/=',') cs
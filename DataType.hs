{-|
Module      : DataType
Description : Principle DataType used for the program
Copyright   : (c) Zhanelya Subebayeva, January 2015
License     : GNU
Maintainer  : subebo91@inbox.ru
Stability   : experimental
Portability : MacOS, Linux

Within this module the main DataType used for the program is provided together with its sample data.
-}

module DataType where
-- * DataType
-- | 'Price' constructor stores stock prices in a form of a Haskell datatype
data Price = Price{
				date 		:: String,
				open 		:: String,
				high 		:: String,
				low			:: String,
				close		:: String,
				volume 		:: Int,
				adj_close 	:: String} deriving (Show)

-- * Sample data	
price1::Price
-- |@Price{date = "1991-08-20",open = "23",high = "25",low = "22", close = "24",volume = 1,adj_close = "12"}@		  
price1 = Price{date = "1991-08-20",open = "23",high = "25",low = "22", close = "24",volume = 1,adj_close = "12"}		

price2::Price
-- |@Price{date = "1991-09-20",open = "23",high = "25",low = "22", close = "24",volume = 1,adj_close = "12"}@
price2 = Price{date = "1991-09-20",open = "23",high = "25",low = "22", close = "24",volume = 1,adj_close = "12"}	
-- |Sample price list	
prices::[Price]
prices = [price1,price2]	
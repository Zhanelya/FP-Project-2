{-|
Module      : DB
Description : DB functionality for the program
Copyright   : (c) Zhanelya Subebayeva, January 2015
License     : GNU
Maintainer  : subebo91@inbox.ru
Stability   : experimental
Portability : MacOS, Linux

Within this module @DB@ access functions allowing to query and to store the data are provided.
-}

module DB where

import qualified Database.HDBC as HDBC
import Database.HDBC.Sqlite3 as SQL
import Data.Time.Clock
import Data.Time.Calendar

import DataType

-- * Initialising DB
{-|The 'initialise' function has a purpose to initialise database and 2 tables - companies list and stock prices
   companies table consists of company name, date downloaded and stored in DB and username who downloaded it
   stock prices table consists of company name, dates and corresponding volume and a range of stock prices for each date
-}
initialise :: IO ()
initialise = do
   conn <- connectSqlite3 "prices.db"
   HDBC.run conn "CREATE TABLE IF NOT EXISTS 'prices' ('Stock' varchar(40) NOT NULL, 'Date' DATE NOT NULL, 'Open' double(40) DEFAULT NULL, 'High' double(40) DEFAULT NULL, 'Low' double(40) DEFAULT NULL, 'Close' double(40) DEFAULT NULL,'Volume' bigint(11) DEFAULT NULL, 'Adj Close' double(40) DEFAULT NULL)"[]
   HDBC.run conn "CREATE TABLE IF NOT EXISTS 'companies' ('Company' varchar(10) NOT NULL, 'DownloadDate' DATE NOT NULL, 'Name' varchar(20) NOT NULL)"[]
   HDBC.commit conn
   HDBC.disconnect conn  

-- * Query functions
-- |The 'queryStock' has a purpose to query stock prices	   
queryStock :: String -- ^ The 'String' argument to provide the company name to query
		      -> IO ()
queryStock stock =
	do conn <- connectSqlite3 "prices.db"
	   let query = "SELECT * FROM prices WHERE Stock = ?"
	   r <- HDBC.quickQuery' conn query [HDBC.toSql (stock)]
	   mapM_ putStrLn (map convRow r)
	   if length (map convRowC r) == 0 then putStrLn "... Invalid company name" else putStrLn ""
	   HDBC.disconnect conn

-- | The 'queryDate' function has a purpose to get company information at a certain date (volume, open, close prices)
queryDate :: String -- ^ The 'String' argument to provide the company name to query
             -> String -- ^ The 'String' argument to provide the date to query
             -> IO ()
queryDate company date =
	do conn <- connectSqlite3 "prices.db"
	   let query = "SELECT * FROM prices WHERE Date = ? AND Stock = ?"
	   r <- HDBC.quickQuery' conn query [HDBC.toSql (date), HDBC.toSql (company)]
	   if length (map convRowC r) == 0 then putStrLn "... No information found" else putStrLn ""
	   HDBC.disconnect conn
	   mapM_ putStrLn (map convRow r)

{-|The 'queryMinPrice' function has a purpose to get the lowest stock close price among the companies stored in DB 
   (for each company only 100 rows are stored)
-}   
queryMinPrice :: IO()
queryMinPrice = do
	conn <- connectSqlite3 "prices.db"
	let query = "SELECT Close, Stock, Date FROM prices WHERE Close = (SELECT MIN(Close) FROM prices)"
	r <- HDBC.quickQuery' conn query []
	HDBC.disconnect conn
	mapM_ putStrLn (map convRowP r)
	
{-|The 'queryMaxPrice' function has a purpose to get the highest stock close price among the companies stored in DB 
   (for each company only 100 rows are stored)
-}
queryMaxPrice :: IO()
queryMaxPrice = do
	conn <- connectSqlite3 "prices.db"
	let query = "SELECT Close, Stock, Date FROM prices WHERE Close = (SELECT MAX(Close) FROM prices)"
	r <- HDBC.quickQuery' conn query []
	HDBC.disconnect conn
	mapM_ putStrLn (map convRowP r)

-- |The 'convRowP' function is a helper function to process prices rows to get close price	
convRowP :: [HDBC.SqlValue] -> String
convRowP [sqlClose, sqlStock, sqlDate] = close ++ ", " ++ stock ++ " " ++ date
    where close = show ((HDBC.fromSql sqlClose)::Double)
          stock = (HDBC.fromSql sqlStock)
          date = case HDBC.fromSql sqlDate of
            Just x -> x
            Nothing -> "NULL"

{-|The 'queryMaxVolume' function has a purpose to get the highest stock volume among the companies stored in DB 
  (for each company only 100 rows are stored)
-}
queryMaxVolume :: IO()
queryMaxVolume = do
	conn <- connectSqlite3 "prices.db"
	let query = "SELECT Volume, Stock, Date FROM prices WHERE Volume = (SELECT MAX(Volume) FROM prices)"
	r <- HDBC.quickQuery' conn query []
	HDBC.disconnect conn
	mapM_ putStrLn (map convRowV r)
	
-- |The 'convRowV' is a helper function to process prices rows to get volume
convRowV :: [HDBC.SqlValue] -> String
convRowV [sqlVolume, sqlStock, sqlDate] = volume ++ ", " ++ stock ++ " " ++ date
    where volume = show ((HDBC.fromSql sqlVolume)::Integer)
          stock = (HDBC.fromSql sqlStock)
          date = case HDBC.fromSql sqlDate of
            Just x -> x
            Nothing -> "NULL"
            	
-- |The 'queryCompanies' function has a purpose to get companies list
queryCompanies :: IO ()
queryCompanies =
	do conn <- connectSqlite3 "prices.db"
	   let query = "SELECT * FROM companies"
	   r <- HDBC.quickQuery' conn query []
	   mapM_ putStrLn (map convRowC r)
	   if length (map convRowC r) == 0 then putStrLn "... No companies found" else putStrLn ""
	   HDBC.disconnect conn

-- |The 'convRow' function is a helper function to process prices rows
convRow :: [HDBC.SqlValue] -> String
convRow [sqlStock, sqlDate,sqlOpen, sqlHigh, sqlLow, sqlClose, sqlVolume, sqlAdjClose] = stock ++ " " ++ date ++ " : volume " ++ volume ++ ", open price " ++ open ++ ", close price " ++ close
    where volume = show ((HDBC.fromSql sqlVolume)::Integer)
    	  open = show ((HDBC.fromSql sqlOpen)::Double)
          close = show ((HDBC.fromSql sqlClose)::Double)
          stock = (HDBC.fromSql sqlStock)
          date = case HDBC.fromSql sqlDate of
            Just x -> x
            Nothing -> "NULL"
            	   
-- |The 'queryCompany' function has a purpose to get a company (to check if it is present in DB)
queryCompany :: String -- ^ The 'String' argument to provide the company name to query
                -> IO (String)
queryCompany company =
	do conn <- connectSqlite3 "prices.db"
	   let query = "SELECT * FROM companies WHERE Company = ?"
	   r <- HDBC.quickQuery' conn query [HDBC.toSql (company)]
	   HDBC.disconnect conn	   
	   if length (map convRowC r) > 0 then return (company ++ " already exists") else return ""
	   
-- |The 'convRowC' function is a helper function to process companies rows	   
convRowC :: [HDBC.SqlValue] -> String
convRowC [sqlStock, sqlDate, sqlName] = stock ++ ": downloaded " ++ date ++ " by " ++ name
    where stock = (HDBC.fromSql sqlStock)
    	  name = (HDBC.fromSql sqlName)
          date = case HDBC.fromSql sqlDate of
            Just x -> x
            Nothing -> "NULL"
 
-- * Insert functions           
-- |The 'insert' function has a purpose to insert stock prices
insert :: Price -- ^ The 'Price' argument to provide the prices to be inserted
          -> IO()
insert price = 
    do conn <- connectSqlite3 "prices.db"
       stmt <- HDBC.prepare conn "INSERT INTO prices VALUES(?,?,?,?,?,?,?,?)"
       HDBC.execute stmt [HDBC.toSql "Stock",HDBC.toSql $ date price, HDBC.toSql $ open price, HDBC.toSql $ high price, HDBC.toSql $ low price, HDBC.toSql $ close price, HDBC.toSql $ volume price, HDBC.toSql $ adj_close price]
       HDBC.commit conn
       HDBC.disconnect conn
       
-- |The 'insertMany' function has a purpose to insert multiple rows of stock prices       
insertMany :: String -- ^ The 'String' argument to provide the company name to be inserted
              -> [Price] -- ^ The ['Price'] argument to provide the prices to be inserted
              -> IO()
insertMany stock prices =   
    do conn <- connectSqlite3 "prices.db"
       stmt <- HDBC.prepare conn "INSERT INTO prices VALUES(?,?,?,?,?,?,?,?)"
       HDBC.executeMany stmt $ formRows stock prices
       HDBC.commit conn
       HDBC.disconnect conn

-- |The 'insertCompany' function has a purpose to insert new company into companies list
insertCompany :: String -- ^ The 'String' argument to provide the company name to be inserted
                 -> String -- ^ The 'String' argument to provide the user name to be inserted
                 -> IO()
insertCompany company name = 
    do conn <- connectSqlite3 "prices.db"
       stmt <- HDBC.prepare conn "INSERT INTO companies VALUES(?,?,?)"
       date <- getCurrentTime >>= return . toGregorian . utctDay
       let parseDate = (getYear date) ++ "-" ++ (getMonth date) ++ "-" ++ (getDay date) 
       HDBC.execute stmt [HDBC.toSql company,HDBC.toSql parseDate, HDBC.toSql name]
       HDBC.commit conn
       HDBC.disconnect conn

-- |The type synonym 'Date' represents date format in a more understandable way
type Date = (Integer, Int, Int)

-- * Helper functions
-- |The 'getYear' function retrieves a year from date datatype
getYear :: Date -- ^ The 'Date' argument to provide the date to be parsed
           -> String -- ^ The 'String' argument to return a year
getYear (y,m,d) = show y

-- |The 'getMonth' function retrieves a month from date datatype
getMonth :: Date -- ^ The 'Date' argument to provide the date to be parsed
            -> String -- ^ The 'String' argument to return a month
getMonth (y,m,d) = show m

-- |The 'getDay' function retrieves a day from date datatype
getDay :: Date -- ^ The 'Date' argument to provide the date to be parsed
          -> String -- ^ The 'String' argument to return a day
getDay (y,m,d) = show d

-- |The 'formRows' function is a helper function to generate rows from stock prices data   
formRows :: String -- ^ The 'String' argument to represent company name
            -> [Price] -- ^ The ['Price'] argument to represent company prices
            -> [[HDBC.SqlValue]]
formRows stock [] = []
formRows stock (price:prices) = (formRow stock price):(formRows stock prices)

-- |The 'formRow' function is a helper function to generate a row from stock prices data
formRow :: String -- ^ The 'String' argument to represent company name
           -> Price -- ^ The 'Price' argument to represent company prices
           -> [HDBC.SqlValue]
formRow stock price = [HDBC.toSql stock,HDBC.toSql $ date price, HDBC.toSql $ open price, HDBC.toSql $ high price, HDBC.toSql $ low price, HDBC.toSql $ close price, HDBC.toSql $ volume price, HDBC.toSql $ adj_close price]
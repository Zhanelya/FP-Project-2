{-|
Module      : NW
Description : NW functionality for the program
Copyright   : (c) Zhanelya Subebayeva, January 2015
License     : GNU
Maintainer  : subebo91@inbox.ru
Stability   : experimental
Portability : MacOS, Linux

Within this module @NW@ access functions allowing to download data from web are provided.
-}

module NW where

import Network.URI as URI
import Network.HTTP as HTTP

-- * Sample links for .csv files
-- |@yhoo = "http://real-chart.finance.yahoo.com/table.csv?s=YHOO"@
yhoo = "http://real-chart.finance.yahoo.com/table.csv?s=YHOO"
-- |@appl = "http://real-chart.finance.yahoo.com/table.csv?s=AAPL"@
appl = "http://real-chart.finance.yahoo.com/table.csv?s=AAPL"
-- |@msft = "http://real-chart.finance.yahoo.com/table.csv?s=MSFT"@
msft = "http://real-chart.finance.yahoo.com/table.csv?s=MSFT"

-- * Downloading data
-- |The function 'downloadURL' downloads company data from a given url
downloadURL :: String -- ^The 'String' argument contains url link
               -> IO (Either String String) -- ^Contains downloaded content or error
downloadURL url =
    do resp <- simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
                (2,_,_) -> return $ Right (rspBody r)
                (3,_,_) -> -- A HTTP redirect
                  case findHeader HdrLocation r of
                     Nothing -> return $ Left (show r)
                     Just url -> downloadURL url
                _ -> return $ Left (show r)
    where request = Request {rqURI = uri,
                             rqMethod = GET,
                             rqHeaders = [],
                             rqBody = ""}
          uri = fromJust $ parseURI url

-- * Helper function          
-- |The function 'fromJust' has a purpose to parse Just (Maybe)
fromJust :: Maybe a -> a
fromJust Nothing = error "Invalid data"
fromJust (Just x) = x
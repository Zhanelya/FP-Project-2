#Functional Programming Project using Haskell Web connectivity libraries

###Web Application with DB Connectivity

This project implements a Haskell program for harvesting information from the Web. 

#####Components:

DB - Dqatabase functionality for the program
DataType - Principle DataType used for the program
Main - Main module used in the program to process data from Yahoo Finance and provide interactivity.
NW - Network functionality for the program
Parse - Parsing functionality for the program

#####Extra modules needed to run the code: 
• Database.HDBC
• Database.HDBC.Sqlite3
• Network.HTTP

#####In order to install these in your laptops or PC simply run the command:
$ cabal install HDBC
$ cabal install HDBC-sqlite3
$ cabal install HTTP
$ cabal install HaXml

For complete documentation, please check the haddock doc.

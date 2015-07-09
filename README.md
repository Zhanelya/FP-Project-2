#Functional Programming Project using Haskell Web connectivity libraries

###Web Application with DB Connectivity

This project implements a Haskell program for harvesting information from the Web. 

#####Components:

• **_DB_** - Database functionality for the program

• **_DataType_** - Principle DataType used for the program

• **_Main_** - Main module used in the program to process data from Yahoo Finance and provide interactivity

• **_NW_** - Network functionality for the program

• **_Parse_** - Parsing functionality for the program


#####Extra modules needed to run the code: 

• Database.HDBC

• Database.HDBC.Sqlite3

• Network.HTTP


#####In order to install these in your laptops or PC simply run the command:
```
$ cabal install HDBC

$ cabal install HDBC-sqlite3

$ cabal install HTTP

$ cabal install HaXml
```

For complete documentation, please check the haddock doc.

{-
 - name    : parseSqliteDDL
 -
 - author  : wada shunsuke
 - created : 2012/05/31
 - updated : 2012/05/31
 -
 - detail  : get a column define from sqlite3
 - TODO    : use data constructer to deal with attributes of defines
 -
 -}


module ParseSqliteDDL 
(
    main,
    getTableDdl,
    getTableDefine,
    SQLiteTableDefine,
    tableName,
    columnDefineList,
    connectSqlite3,
    H.disconnect
)
where

----------------
-- import module
----------------
import qualified Database.HDBC as H
import Database.HDBC.Sqlite3

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*),(*>))

----------------
-- data
----------------
data SQLiteTableDefine = SQLiteTableDefine {
    tableName        :: String,
    columnDefineList :: [(String,String)]
    }
    deriving (Show)

----------------
-- const
----------------
noneDef = SQLiteTableDefine { tableName = "NONE" , columnDefineList = [] }

----------------
-- sql
----------------
getTableDdlSQL = "SELECT SQL FROM SQLITE_MASTER WHERE TBL_NAME = ?"

----------------
-- main
----------------
main = do
    conn <- connectSqlite3 "mydb"
    sampleDDL <- getTableDdl conn "ms_task_id"

    -- table ’è‹`î•ñ‚ð sqlite3 ‚©‚çŽæ“¾‚·‚éB
    putStrLn "------------------------------"
    putStrLn " show table info "
    putStrLn "------------------------------"
    tableDef <- getTableDefine (concat sampleDDL)
    print $ show tableDef
    putStrLn "------------------------------"

    H.disconnect conn

----------------
-- function
----------------
getTableDdl :: Connection -> String -> IO [String]
getTableDdl conn tableName = do

    stmt <- H.prepare conn $ getTableDdlSQL
    H.execute stmt [H.toSql tableName]
    ret <- H.fetchAllRowsAL stmt
    let list = foldr (\x ys -> ( H.fromSql (snd $ x !! 0) :: String ) : ys) [] ret

    return list

getTableDefine :: String -> IO SQLiteTableDefine
getTableDefine ddl = case (parse parserTableDefine "" ddl) of
                           Right res -> return res
                           Left  err -> return noneDef

parserTableDefine :: Parser SQLiteTableDefine
parserTableDefine = do string "CREATE TABLE "
                       tblName <- manyTill parserOraObjectLetter (string "\n")
                       string "(\n"
                       colDefList <- parserCollumnDefine

                       let sqliteTableDefine = SQLiteTableDefine { tableName        = tblName,
                                                                   columnDefineList = colDefList }
                                                                   
                       return sqliteTableDefine

parserOraObjectLetter :: Parser Char
parserOraObjectLetter = do res <- try letter <|> char '_'
                           return res

parserCollumnDefine :: Parser [(String,String)]
parserCollumnDefine = do 
    res <- manyTill parserCollumnDefine' (string ")" )
    return res

parserCollumnDefine' :: Parser (String,String)
parserCollumnDefine' = do
    many (char ' ')
    columnName   <- manyTill parserOraObjectLetter (char ' ')
    many (char ' ')
    columnDefine <- manyTill anyChar (string ",\n" <|> string "\n")
    return (columnName , columnDefine)

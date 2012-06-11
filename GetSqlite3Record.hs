module GetSqlite3Record
where

import qualified ParseSqliteDDL as P
import qualified Database.HDBC as H
import Database.HDBC.Sqlite3

main :: IO ()
main = do putStr ""

generateSelectQuery :: P.SQLiteTableDefine -> String
generateSelectQuery def = "SELECT " ++ (genCols def) ++ " FROM " ++ ( P.tableName def )
    where genCols xs = init $ concat ( foldr (\x ys -> ( fst x ++ "," ):ys ) [""] $ P.columnDefineList xs )

getRecord :: Connection -> String -> IO [[(String,String)]]
getRecord conn query = do
    stmt <- H.prepare conn query
    H.execute stmt []
    ret <- H.fetchAllRowsAL stmt
    return $ foldr (\list ys -> (map (\l -> genTupple l) list ): ys) [] ret
    where genTupple :: (String, (H.SqlValue)) -> (String,String)
          genTupple (x,y) = if y == H.SqlNull then
                               (x, "")
                            else
                               (x, H.fromSql y)

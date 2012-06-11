import System.IO
import Data.Char (toUpper)
import qualified ParseSqliteDDL as P
import qualified GetSqlite3Record as G

main = do 
    conn  <- P.connectSqlite3 "mydb"
    ddl   <- P.getTableDdl conn "kr_taskstatus"
    def   <- P.getTableDefine (concat ddl)
    let query = G.generateSelectQuery def
    putStrLn query
    records <- G.getRecord conn query

    output <- openFile "test.html" WriteMode
    hPutStrLn output "<html>"
    hPutStrLn output "<body>"

    hPutStr output "<title>"
    hPutStr output (P.tableName def)
    hPutStr output "</title>"

    hPutStr   output "<h1 align=center>"
    hPutStr   output $ map toUpper $ P.tableName def
    hPutStrLn output "</h1>"
    hPutStr output "\n"

    hPutStrLn output "<table border=4 width=250 align=center>"
    genHtmlTableHeader output def
    genHtmlTableBody   output records 0

    hPutStrLn output "</table>"

    hPutStrLn output "</body>"
    hPutStrLn output "</html>"
    hClose output

    P.disconnect conn
    putStrLn "done"

genHtmlTableHeader :: Handle -> P.SQLiteTableDefine -> IO ()
genHtmlTableHeader output def = do
    mapM_ putTableBody $ P.columnDefineList def
    where putTableBody d = do hPutStr   output "<th>"
                              hPutStr   output $ fst d
                              hPutStrLn output "</th>"

genHtmlTableBody :: Handle -> [[(String,String)]] -> Int -> IO ()
genHtmlTableBody output []     i = return ()
genHtmlTableBody output (x:xs) i = do
    hPutStr   output "<tr>"
    genHtmlTableBodyChildren x i
    hPutStrLn output "</tr>"
    genHtmlTableBody output xs (i + 1)
    where genHtmlTableBodyChildren [] i     = return ()
          genHtmlTableBodyChildren (y:ys) i = do if (odd i) then
                                                    hPutStr   output "<td bgcolor=\"#FFF5EE\">"
                                                 else
                                                    hPutStr   output "<td bgcolor=\"#FFE4E1\">"
                                                 hPutStr   output $ snd y
                                                 hPutStr   output "</td>"
                                                 genHtmlTableBodyChildren ys i

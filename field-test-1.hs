import Data.Maybe (fromJust)
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Convertible.Base

data Cog = Cog {
  vcogFname :: String,
  vcogCh :: String,
  vcogX :: Int,
  vcogY :: Int
}

convRow1 :: [SqlValue] -> Cog
convRow1 [sFile,sLetter,sX,sY] = Cog { 
  vcogFname = (fromJust . fromSql) sFile, 
  vcogCh = (fromJust . fromSql) sLetter, 
  vcogX = (fromJust . fromSql) sX, 
  vcogY = (fromJust . fromSql) sY }

convRow2 :: [SqlValue] -> Cog
convRow2 [sFile,sLetter,sX,sY] = Cog { 
  vcogFname = f sFile, 
  vcogCh = f sLetter, 
  vcogX = f sX, 
  vcogY = f sY }
  where
    f :: Data.Convertible.Base.Convertible SqlValue c => SqlValue -> c
    f = fromJust . fromSql

convRow3 :: [SqlValue] -> Cog
convRow3 [sFile,sLetter,sX,sY] = Cog { 
  vcogFname = fromSql sFile, 
  vcogCh = fromSql sLetter, 
  vcogX = fromSql sX, 
  vcogY = fromSql sY }

main = do
  putStrLn ""


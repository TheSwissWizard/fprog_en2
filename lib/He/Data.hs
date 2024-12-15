module He.Data (saveIdea, deleteIdea, listIdeas, readIdea) where

import System.Directory (listDirectory, removeFile, doesFileExist)
import Control.Monad (when)

filePath :: String -> String
filePath fileName = "ideas/" ++ fileName

saveIdea :: String -> String -> IO()
saveIdea fileName content = do 
    writeFile (filePath fileName) content

listIdeas :: IO [FilePath]
listIdeas = do
    listDirectory "ideas/"

deleteIdea :: String -> IO ()
deleteIdea fileName = do 
    exists <- doesFileExist (filePath fileName)
    when exists $ removeFile $ filePath fileName

readIdea :: String -> IO (Maybe String)
readIdea fileName = do
    fileExists <- doesFileExist (filePath fileName)
    if fileExists
        then fmap Just (readFile (filePath fileName))
        else return Nothing
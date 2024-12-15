{-# LANGUAGE OverloadedStrings #-}

module He.Web where

import qualified Data.Text.Lazy as LT
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty (ActionM, file, get, html, scotty, setHeader, post, liftIO, captureParam)
import Web.Scotty.Trans (middleware, formParam, redirect)
import He.Html ( createPage )
import He.Data (saveIdea, listIdeas, deleteIdea, readIdea)
import Data.List (intersect)
import Data.Char (isAlphaNum)

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  get  "/styles.css" styles
  get  "/icon.png" icon
  get  "/" $ mainAction Nothing Nothing
  post "/he/save" saveAction
  post "/he/:idea/delete" deleteAction
  get  "/he/:idea" editAction

styles :: ActionM ()
styles = do
  setHeader "Content-Type" "text/css"
  file "static/styles.css"

icon :: ActionM ()
icon = do
  setHeader "Content-Type" "image/png"
  file "static/icon.png"

htmlString :: String -> ActionM ()
htmlString = html . LT.pack

mainAction :: Maybe String -> Maybe String -> ActionM ()
mainAction currentIdea currentContent = do
  ideas <- liftIO listIdeas
  mappedIdeas <- liftIO $ mapReferencingIdeas ideas
  htmlString $ createPage currentIdea currentContent mappedIdeas

saveAction :: ActionM ()
saveAction = do
  title <- formParam "title"
  content <- formParam "content"
  liftIO $ saveIdea title content
  redirect "/"

deleteAction :: ActionM ()
deleteAction = do
  idea <- captureParam "idea"
  liftIO $ deleteIdea idea
  redirect "/"

editAction :: ActionM ()
editAction = do
  idea <- captureParam "idea"
  content <- liftIO $ readIdea idea
  case content of
    Nothing -> redirect "/"
    Just c -> mainAction (Just idea) (Just c)

mapReferencingIdeas :: [String] -> IO [(String, [String])]
mapReferencingIdeas ideas = mapM group ideas
  where
    group :: String -> IO (String, [String])
    group idea = do
      maybeContent <- readIdea idea
      case maybeContent of
        Nothing -> return (idea, [])
        Just content -> do
          let references = extractReferences ideas content
          return (idea, references)
          
    extractReferences :: [String] -> String -> [String]
    extractReferences allIdeas content =
      let wordsInContent = words $ removeSpecialChars content
      in intersect allIdeas wordsInContent
    
    removeSpecialChars :: String -> String
    removeSpecialChars = map (\c -> if isAlphaNum c || c == ' ' then c else ' ')
module He.Html where

import Data.List (intersperse)
import Prelude hiding (div)

{-# ANN module "HLint: ignore Eta reduce" #-}

type Html = String

createPage :: Maybe String  -> Maybe String -> [(String, [String])] -> Html
createPage currentIdea currentContent mappedIdeas =
  unlines
    [ "<!DOCTYPE html>",
      ea "html" [("lang", "en")] $
        unlines
          [ header,
            e "body" $
              unlines [
                e "h1" "Helium",
                ideaForm currentIdea currentContent,
                graph mappedIdeas,
                collection $ map fst mappedIdeas
              ]
          ]
    ]

ideaForm :: Maybe String -> Maybe String -> Html
ideaForm idea content =
  form [("id", "ideaForm"), ("method", "post"), ("action", "/he/save")] $
    unlines
      [ div [] $ ea_ "input" $ inputAttrs idea,
        div [] $ ea "textarea" [("name", "content"), ("rows", "12")] $ textareaContent content ,
        div [] $ ea "button" [("type", "submit"), ("name", "save")] "Save"
      ]

inputAttrs :: Maybe String -> [(String, String)]
inputAttrs Nothing = [("type", "text"), ("name", "title"), ("id", "title")]
inputAttrs (Just s) = [("type", "text"), ("name", "title"), ("id", "title"), ("value", s)]

textareaContent :: Maybe String -> String
textareaContent Nothing = []
textareaContent (Just s) = s

graph :: [(String, [String])] -> Html
graph mappedIdeas =
  e "h2" "Graph" ++
  if null mappedIdeas
    then "No ideas yet :("
    else div [("class", "mermaid")] $ graphContent mappedIdeas

graphContent :: [(String, [String])] -> String
graphContent ideasList = unlines $ ["graph TD"] ++ nodes ++ edges ++ links
  where
    nodes = map fst ideasList
    edges = concatMap (\(idea, refs) -> [idea ++ " --> " ++ ref | ref <- refs]) ideasList
    links = [ "click " ++ node ++ " \"/he/" ++ node ++ "\" \"Go to " ++ node ++ "\" _blank" | node <- nodes ]

collection :: [String] -> Html
collection ideas =
  unlines
    [ e "h2" "Collection",
      if null ideas
        then "No ideas yet :("
        else e "table" $
          unlines
            [ tr $
                unlines [ th "Idea", th "action"],
                unlines $ map collectionItem ideas
            ]
    ]

collectionItem :: String -> Html
collectionItem idea =
  tr $
    unlines
      [ td $ link idea ("/he/" ++ idea),
        td $ form [("method", "post"), ("action", "/he/" ++ idea ++ "/delete")] $ ea "button" [("type", "submit")] "Delete"
      ]

header :: Html
header =
  e "head" $
    unlines
      [ ea_ "link" [("rel", "stylesheet"), ("href", "/styles.css")],
        ea_ "link" [("rel", "icon"), ("href", "/icon.png")],
        ea_ "meta" [("charset", "utf-8")],
        ea_ "meta" [("name", "viewport"), ("content", "width=device-width, initial-scale=1")],
        e "title" "Helium",
        ea "script" [("type", "module")] mermaidScript
      ]

mermaidScript :: String
mermaidScript =
  "import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';\n\
  \mermaid.initialize({startOnLoad:true});"

nbsp :: Html
nbsp = "&nbsp;"

e :: String -> Html -> Html
e tag kids = ea tag [] kids

ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat [ea_ tag attrs, kids, "</" ++ tag ++ ">"]

-- Use this function for self-closing tags like <img> or <br>
ea_ :: String -> [(String, String)] -> Html
ea_ tag attrs = "<" ++ tag ++ attrsHtml attrs ++ ">"

attrsHtml :: [(String, String)] -> String
attrsHtml [] = []
attrsHtml as = concat $ " " : intersperse " " (map attrHtml as)
  where
    attrHtml (key, value) = key ++ "='" ++ value ++ "'"

link :: String -> String -> Html
link text url = ea "a" [("href", url)] text

div :: [(String, String)] -> Html -> Html
div attrs kids = ea "div" attrs kids

form :: [(String, String)] -> Html -> Html
form attrs kids =
  ea "form" attrs kids

tr :: Html -> Html
tr kids = e "tr" kids

td :: Html -> Html
td kids = e "td" kids

th :: Html -> Html
th kids = e "th" kids
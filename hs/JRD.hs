module JRD where

import System.FilePath {- filepath -}
import qualified System.IO.Strict as I {- strict -}
import qualified Text.HTML.Light as H {- html-minimalist -}
import qualified Text.HTML.Light.Composite as H
import qualified Text.Pandoc as M {- pandoc -}
import qualified Text.XML.Light as X {- xml -}

type MD = [(String,String)]
type Image_Set = [String]
type State = (MD,Image_Set)

load_md :: FilePath -> [String] -> IO MD
load_md dir ps = do
  let f p = I.readFile (dir </> "data/md" </> p <.> "md")
  ms <- mapM f ps
  return (zip ps ms)

load_image_set :: FilePath -> IO Image_Set
load_image_set dir = do
  let fn = dir </> "data/hs/images.hs"
  fmap read (I.readFile fn)

md_html :: String -> String
md_html s =
    let s' = M.readMarkdown M.defaultParserState (s ++ "\n")
    in M.writeHtmlString M.defaultWriterOptions s'

std_html_attr :: [X.Attr]
std_html_attr = [H.lang "en" ]

std_meta :: String -> [X.Content]
std_meta p =
    [H.title [] [H.cdata ("jeremydrape.com: " ++ p)]
    ,H.meta_author "jeremy drape"
    ,H.meta_description "jeremy drape is a canberra based photographer"
    ,H.meta_content_type "text/html; charset=UTF-8"
    ,H.link_css "all" "css/jrd.css"
    ,H.meta_viewport "width=device-width,initial-scale=1,user-scalable=yes"
    ]

img_fn :: FilePath -> FilePath
img_fn nm = "data/jpeg" </> nm <.> "jpeg"

img_r_fn :: FilePath -> FilePath
img_r_fn nm = "data/jpeg/h-500" </> nm <.> "jpeg"

mk_img :: State -> String -> String
mk_img (md,_) i =
    let Just m = lookup "menu" md
        menu = H.div [H.class' "menu"] [H.cdata_raw (md_html m)]
        image = let ln = [H.href (img_fn i),H.target i]
                in H.div [H.class' "image"] [H.a ln [H.img [H.src (img_r_fn i)]]]
        hd = H.head [] (std_meta i)
        bd = H.body [H.class' "image"] [H.div [H.class' "main"] [menu, image]]
    in H.renderHTML5 (H.html std_html_attr [hd, bd])

module JRD where

import System.FilePath {- filepath -}
import qualified System.IO.Strict as I {- strict -}
import System.Process {- process -}
import qualified Text.HTML.Light as H {- html-minimalist -}
import qualified Text.HTML.Light.Composite as H {- html-minimalist -}
import qualified Text.Pandoc.Minus as M {- pandoc-minus -}
import qualified Text.XML.Light as X {- xml -}

type MD = [(String,String)]
type Image_Set = [(String,String)]
type State = (MD,Image_Set)

img_title :: Image_Set -> String -> Maybe String
img_title st k = lookup k st

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
std_meta _ =
    [H.title [] [H.cdata "jeremydrape.com"]
    ,H.meta_author "jeremy drape"
    ,H.meta_description "jeremy drape is a canberra based photographer"
    ,H.meta_content_type "text/html; charset=UTF-8"
    ,H.link_css "all" "css/jrd.css"
    ,H.meta_viewport "width=device-width,initial-scale=1,user-scalable=yes"
    ,H.meta [H.name "google-site-verification"
            ,H.content "Ujn7EZ-8e4SlmGvR5e7YFAAyjt4VphkNCQTLZqkuqkg"]
    ]

img_fn :: FilePath -> FilePath
img_fn nm = "data/jpeg" </> nm <.> "jpeg"

img_r_fn :: Int -> FilePath -> FilePath
img_r_fn sz nm = "data/jpeg/h-" ++ show sz </> nm <.> "jpeg"

div_c :: String -> [X.Content] -> X.Content
div_c c = H.div [H.class' c]

mk_frame :: State -> String -> [X.Content] -> String
mk_frame (md,_) mt cn =
    let Just m = lookup "menu" md
        menu = div_c "menu" [H.cdata_raw (md_html m)]
        hd = H.head [] (std_meta mt)
        bd = H.body [H.class' "image"] [div_c "main" (menu:cn)]
    in H.renderHTML5 (H.html std_html_attr [hd,bd])

mk_img_div :: Int -> String -> (String,Maybe String) -> X.Content
mk_img_div sz cl (i,t) =
    let ln = [H.href "."]
        im = [H.img [H.src (img_r_fn sz i)]]
        bd = case t of
               Nothing -> [H.a ln im]
               Just t' -> [H.a ln im,div_c "title" [H.cdata t']]
    in div_c (unwords ["image",cl]) bd

mk_img :: State -> (String,String) -> String
mk_img st (mt,nm) = mk_frame st mt [mk_img_div 500 "std" (mt,Just nm)]

mk_ix :: State -> String
mk_ix st =
    let (_,is) = st
        cn = map (\(k,_) -> mk_img_div 150 "ix" (k,Nothing)) is
    in mk_frame st "ix" cn

proc_resize :: IO ()
proc_resize = do
  _ <- rawSystem "sh" ["sh/resize.sh","data/jpeg"]
  return ()

module JRD where

import Data.List {- base -}
import System.FilePath {- filepath -}
import qualified System.IO.Strict as I {- strict -}
import System.Process {- process -}
import qualified Text.HTML.Light as H {- html-minimalist -}
import qualified Text.HTML.Light.Composite as H {- html-minimalist -}
import qualified Text.Pandoc.Minus as M {- pandoc-minus -}
import qualified Text.XML.Light as X {- xml -}

type MD = [(String,String)]
type Image_Name = String
type Image_Title = String
type Image = (Image_Name,Image_Title)
type Image_Set = [Image]
type State = (MD,Image_Set)

img_title :: Image_Set -> Image_Name -> Maybe Image_Title
img_title st k = lookup k st

img_lookup :: Image_Set -> Image_Name -> Maybe Image
img_lookup st k = find ((== k) . fst) st

-- > md <- load_md "/home/rohan/ut/www-jrd/" ["menu","about"]
load_md :: FilePath -> [String] -> IO MD
load_md dir ps = do
  let f p = I.readFile (dir </> "data/md" </> p <.> "md")
  ms <- mapM f ps
  return (zip ps ms)

load_image_set :: FilePath -> IO Image_Set
load_image_set dir = do
  let fn = dir </> "data/hs/images.hs"
  fmap read (I.readFile fn)

slideshow_pre :: MD -> [String]
slideshow_pre md =
    ["<hmtl>"
    ,"<head>"
    ,concatMap H.showHTML5 (std_meta "")
    ,"<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1/jquery.min.js\"></script>"
    ,"<script src=\"http://malsup.github.com/jquery.cycle2.js\"></script>"
    ,"</head>"
    ,"<body>"
    ,H.showHTML5 (menu_html md)
    ,"<div class=\"cycle-slideshow\""
    ,"     data-cycle-fx=\"fadeout\""
    ,"     data-cycle-timeout=\"8000\""
    ,"     data-cycle-speed=\"50\""
    ,"     data-cycle-next=\".next\""
    ,"     data-cycle-manual-fx=\"fadeout\""
    ,"     data-cycle-manual-speed=\"50\""
    ,"     data-cycle-caption=\"#caption\""
    ,"     data-cycle-caption-template=\"{{cycleTitle}}\""
    ]

slideshow_post :: [String]
slideshow_post =
    ["</div>"
    ,"<div id=\"caption\"></div>"
    ,"</body>"]

-- > let d = "/home/rohan/ut/www-jrd/"
-- > img <- load_image_set d
-- > md <- load_md d ["menu"]
-- > writeFile (d </> "ss.html") (gen_slideshow md img)
gen_slideshow :: MD -> Image_Set -> String
gen_slideshow md =
    let f (k,nm) = H.img [H.class' "next"
                         ,H.src ("data/jpeg/h-500" </> k <.> "jpeg")
                         ,H.mk_attr "data-cycle-title" nm
                         ,H.mk_attr "data-cycle-hash" k]
        pkg s = unlines (concat [slideshow_pre md,s,slideshow_post])
    in pkg . map (H.showHTML5 . f)

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

menu_html :: MD -> X.Content
menu_html md =
    case lookup "menu" md of
      Just m -> div_c "menu" [H.cdata_raw (md_html m)]
      _ -> div_c "menu" [H.cdata_raw "no menu?"]

mk_frame :: State -> String -> [X.Content] -> String
mk_frame (md,_) mt cn =
    let hd = H.head [] (std_meta mt)
        bd = H.body [H.class' "image"] [div_c "main" (menu_html md :cn)]
    in H.renderHTML5 (H.html std_html_attr [hd,bd])

mk_md :: State -> String -> String
mk_md (md,_) mt =
    let c = case lookup mt md of
              Just m -> div_c mt [H.cdata_raw (md_html m)]
              _ -> div_c mt [H.cdata_raw ("mk-md: ?" ++ mt ++ show md)]
        hd = H.head [] (std_meta mt)
        bd = H.body [H.class' mt] [div_c "main" [c]]
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

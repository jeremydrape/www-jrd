module JRD where

import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.FilePath {- filepath -}
import System.Process {- process -}
import Text.Printf {- base -}

import qualified Text.CSV.Lazy.String as C {- lazy-csv -}

import qualified Text.HTML.Minus as H {- html-minus -}
import qualified Text.Pandoc.Minus as M {- pandoc-minus -}
import qualified WWW.Minus.IO as W {- www-minus -}

-- * UTIL

div_c :: String -> [H.Content] -> H.Content
div_c c = H.div [H.class' c]

html_en :: [H.Content] -> H.Element
html_en = H.html [H.lang "en"]

md_to_html :: String -> String
md_to_html s =
    let s' = M.readMarkdown M.defaultParserState (s ++ "\n")
    in M.writeHtmlString M.defaultWriterOptions s'

-- * PATHS

prj_dir :: FilePath
prj_dir = "/home/rohan/ut/www-jrd/"

prj_img_fn :: FilePath -> FilePath
prj_img_fn nm = "data/jpeg" </> nm <.> "jpeg"

img_r_fn :: Int -> FilePath -> FilePath
img_r_fn sz nm = "data/jpeg/h-" ++ show sz </> nm <.> "jpeg"

md_fn :: FilePath -> FilePath
md_fn nm = "data/md" </> nm <.> "md"

-- * TYPES

type MD = [(String,String)]
type Series_Ix = String
type Image_File = String
type Image_Title = String
type Image_Ix = String
type Image_Z = Int
data Image = Image {img_ix0 :: Series_Ix
                   ,img_ix1 :: Image_Ix
                   ,img_z :: Image_Z
                   ,img_file :: Image_File
                   ,img_title :: Image_Title}
type Opt = [(String,String)]
type State = (Opt,MD,[Image],(Series_Ix,Image_Ix))

opt_lookup :: Opt -> String -> String -> String
opt_lookup o k def = fromMaybe def (lookup k o)

-- | Give both set index and title.
img_lookup_by_name :: Image_Title -> [Image] -> Maybe Image
img_lookup_by_name nm = find ((==) nm . img_title)

img_sort_by_z :: [Image] -> [Image]
img_sort_by_z = sortBy (compare `on` img_z)

st_opt_lookup :: State -> String -> String -> String
st_opt_lookup (o,_,_,_) = opt_lookup o

st_img_set :: State -> [Image]
st_img_set (_,_,img_set,(s_ix,i_ix)) =
    let f img = img_ix0 img == s_ix || img_ix1 img == i_ix
    in filter f img_set

-- * IO

-- > md <- load_md prj_dir ["menu","about"]
load_md :: FilePath -> [String] -> IO MD
load_md dir ps = do
  let f p = W.read_file_utf8 (dir </> md_fn p)
  ms <- mapM f ps
  return (zip ps ms)

load_opt_set :: FilePath -> IO Opt
load_opt_set dir = do
  let fn = dir </> "data/hs/opt.hs"
  fmap read (W.read_file_utf8 fn)

load_st :: FilePath -> IO State
load_st dir = do
  opt <- load_opt_set dir
  images <- load_image_data dir
  md <- load_md dir ["menu","about"]
  return (opt,md,images,("",""))

-- * SLIDESHOW

slideshow_pre :: State -> [String]
slideshow_pre st =
    ["<!DOCTYPE html>"
    ,"<html lang=\"en\">"
    ,"<head>"
    ,concatMap H.showHTML5 (jrd_meta "")
    ,"<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1/jquery.min.js\"></script>"
    ,"<script src=\"http://malsup.github.com/jquery.cycle2.js\"></script>"
    ,"<script src=\"http://malsup.github.io/min/jquery.cycle2.swipe.min.js\"></script>"
    ,"</head>"
    ,"<body>"
    ,"<div class=\"main\">"
    ,H.showHTML5 (menu_html st)
    ,"<div class=\"content\">"
    ,"<div class=\"cycle-slideshow\""
    ,"     data-cycle-fx=\"fadeout\""
    ,"     data-cycle-timeout=\"" ++ st_opt_lookup st "timeout" "8000" ++ "\""
    ,"     data-cycle-paused=\"" ++ st_opt_lookup st "paused" "false" ++ "\""
    ,"     data-cycle-swipe=\"" ++ st_opt_lookup st "swipe" "false" ++ "\""
    ,"     data-cycle-speed=\"50\""
    ,"     data-cycle-prev=\".prev\""
    ,"     data-cycle-next=\".next\""
    ,"     data-cycle-manual-fx=\"fadeout\""
    ,"     data-cycle-manual-speed=\"50\""
    ,"     data-cycle-caption=\"#caption\""
    ,"     data-cycle-caption-template=\"{{cycleTitle}}\">"
    ]

slideshow_post :: [String]
slideshow_post =
    ["</div>"
    ,"</div> <!-- end content -->"
    --,"<div class=\"title\" id=\"caption\"></div>" -- SEE data/md/menu.md
    ,"</div> <!-- end main -->"
    ,"<script>"
    ,"addEventListener('keydown' , function(ev) {"
    ,"  switch(ev.keyCode) {"
    ,"    case 37: $('.cycle-slideshow').cycle('prev'); break;"
    ,"    case 39: $('.cycle-slideshow').cycle('next'); break;"
    ,"  }"
    ,"});"
    ,"</script>"
    ,"</body>"]

-- > img <- load_image_group prj_dir
-- > md <- load_md prj_dir ["menu"]
-- > writeFile (prj_dir </> "ss.html") (mk_slideshow md img)
mk_slideshow :: State -> String
mk_slideshow st =
    let is = st_img_set st
        addr k = case st_opt_lookup st "image-url" "true" of
                   "false" -> Nothing
                   _ -> Just (H.mk_attr "data-cycle-hash" k)
        f img = H.img ([H.class' "next"
                       ,H.alt (img_file img)
                       ,H.src (img_r_fn 500 (img_file img))
                       ,H.mk_attr "data-cycle-title" (img_title img)] ++
                       catMaybes [addr (img_file img)])
        pkg s = unlines (concat [slideshow_pre st,s,slideshow_post])
        gen = pkg . map (H.showHTML5 . f)
    in gen is

-- * HTML

jrd_meta :: String -> [H.Content]
jrd_meta _ =
    [H.title [] [H.cdata "jeremydrape.com"]
    ,H.meta_author "jeremy drape"
    ,H.meta_description "jeremy drape is a canberra based photographer"
    ,H.meta_content_type "text/html; charset=UTF-8"
    ,H.link_css "all" "css/jrd.css"
    ,H.meta_viewport "width=device-width,initial-scale=1,user-scalable=yes"
    ,H.meta [H.name "google-site-verification"
            ,H.content "Ujn7EZ-8e4SlmGvR5e7YFAAyjt4VphkNCQTLZqkuqkg"]
    ]

menu_html :: State -> H.Content
menu_html (_,md,_,_) =
    case lookup "menu" md of
      Just m -> div_c "menu" [H.cdata_raw (md_to_html m)]
      _ -> div_c "menu" [H.cdata_raw "no menu?"]

mk_frame :: State -> String -> [H.Content] -> String
mk_frame st mt cn =
    let hd = H.head [] (jrd_meta mt)
        bd = H.body [H.class' "image"] [div_c "main" (menu_html st : cn)]
    in H.renderHTML5 (html_en [hd,bd])

mk_md :: State -> String -> String
mk_md (_,md,_,_) mt =
    let c = case lookup mt md of
              Just m -> div_c mt [H.cdata_raw (md_to_html m)]
              _ -> div_c mt [H.cdata_raw ("mk-md: " ++ mt ++ "?: " ++ unwords (map fst md))]
        hd = H.head [] (jrd_meta mt)
        bd = H.body [H.class' mt] [div_c "main" [c]]
    in H.renderHTML5 (html_en [hd,bd])

mk_img_div :: Int -> [String] -> (String,Maybe String) -> H.Content
mk_img_div sz cl (i,t) =
    let ln = [H.href "."]
        im = [H.img [H.src (img_r_fn sz i)]]
        bd = case t of
               Nothing -> [H.a ln im]
               Just t' -> [H.a ln im,div_c "title" [H.cdata t']]
    in div_c (unwords ("image" : "content" : cl)) bd

mk_img :: State -> (String,String) -> String
mk_img st (mt,nm) = mk_frame st mt [mk_img_div 500 ["std"] (mt,Just nm)]

img_id :: Int -> String
img_id n = printf "img_%04d" n

mk_ix :: State -> String
mk_ix st =
    let is = zip [0..] (img_sort_by_z (st_img_set st))
        sz = read (st_opt_lookup st "ix:image-size" "150")
        cn = map (\(n,img) -> mk_img_div sz ["ix",img_id n] (img_file img,Nothing)) is
    in mk_frame st "ix" [div_c "meta_ix" cn]

proc_resize :: IO ()
proc_resize = do
  _ <- rawSystem "sh" ["sh/resize.sh","data/jpeg"]
  return ()

-- * CSV

-- > load_image_data prj_dir
load_image_data :: FilePath -> IO [Image]
load_image_data dir = do
  let csv_fn = dir </> "data/csv/images.csv"
  str <- W.read_file_utf8 csv_fn
  let p = C.parseDSV False ',' str
      r = C.fromCSVTable (C.csvTable p)
      f [s_ix,i_ix,z,fn,nm] = Image s_ix i_ix (read z) fn nm
      f _ = error "load_image_data"
  return (map f r)

{-
> import Data.List
> img <- load_image_group prj_dir
> let f (n,l) = map (\(fn,nm) -> intercalate ";" [show n,fn,nm,"nil"]) l
> putStrLn$unlines$concatMap f (zip [0..] img)
-}

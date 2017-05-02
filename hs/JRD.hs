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

img_fn :: FilePath -> FilePath
img_fn nm = "data/jpeg" </> nm <.> "jpeg"

img_r_fn :: Int -> FilePath -> FilePath
img_r_fn sz nm = "data/jpeg/h-" ++ show sz </> nm <.> "jpeg"

md_fn :: FilePath -> FilePath
md_fn nm = "data/md" </> nm <.> "md"

-- * TYPES

type MD = [(String,String)]
type Image_Name = String
type Image_Title = String
type Image_Flags = String
type Image = (Image_Name,Image_Title,Image_Flags)
type Image_Set = [Image]
type Set_Title = String
type Image_Group = [(Set_Title,Image_Set)]
type Set_Index = Int
type Opt = [(String,String)]
type State = (Opt,MD,Image_Group,Maybe Set_Title)

opt_lookup :: Opt -> String -> String -> String
opt_lookup o k def = fromMaybe def (lookup k o)

-- | Give both set index and title.
img_grp_lookup :: Image_Group -> Image_Name -> Maybe (Set_Title,Image_Title)
img_grp_lookup g nm =
    let recur g' =
            case g' of
              [] -> Nothing
              (x,s):g'' ->
                  case find (\(fn,_,_) -> fn == nm) s of
                    Nothing -> recur g''
                    Just (_,r,_) -> Just (x,r)
    in recur g

st_opt_lookup :: State -> String -> String -> String
st_opt_lookup (o,_,_,_) = opt_lookup o

st_img_set :: State -> Image_Set
st_img_set (_,_,ig,ix) =
    case lookup (fromMaybe "" ix) ig of
      Nothing -> concat (map snd ig) -- all? --
      Just r -> r

{-
import Data.List {- base -}

st_img_title :: State -> Image_Name -> Maybe Image_Title
st_img_title (_,_,is) k = lookup k is

st_img_lookup :: State -> Image_Name -> Maybe Image
st_img_lookup (_,_,is) k = find ((== k) . fst) is
-}

-- * IO

-- > md <- load_md prj_dir ["menu","about"]
load_md :: FilePath -> [String] -> IO MD
load_md dir ps = do
  let f p = W.read_file_utf8 (dir </> md_fn p)
  ms <- mapM f ps
  return (zip ps ms)

{-
load_image_group :: FilePath -> IO Image_Group
load_image_group dir = do
  let fn = dir </> "data/hs/images.hs"
  fmap read (W.read_file_utf8 fn)
-}

load_opt_set :: FilePath -> IO Opt
load_opt_set dir = do
  let fn = dir </> "data/hs/opt.hs"
  fmap read (W.read_file_utf8 fn)

load_st :: FilePath -> IO State
load_st dir = do
  opt <- load_opt_set dir
  images <- load_image_data dir
  md <- load_md dir ["menu","about"]
  return (opt,md,images,Nothing)

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
        f (k,nm,_) = H.img ([H.class' "next"
                          ,H.alt k
                          ,H.src (img_r_fn 500 k)
                          ,H.mk_attr "data-cycle-title" nm] ++
                          catMaybes [addr k])
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
    let is = zip [0..] (st_img_set st)
        sz = read (st_opt_lookup st "ix:image-size" "150")
        cn = map (\(n,(k,_,_)) -> mk_img_div sz ["ix",img_id n] (k,Nothing)) is
    in mk_frame st "ix" [div_c "meta_ix" cn]

proc_resize :: IO ()
proc_resize = do
  _ <- rawSystem "sh" ["sh/resize.sh","data/jpeg"]
  return ()

-- * CSV

group_on :: Eq x => (a -> x) -> [a] -> [[a]]
group_on f = map (map snd) . groupBy ((==) `on` fst) . map (\x -> (f x,x))

collate_on_adjacent :: (Eq k,Ord k) => (a -> k) -> (a -> v) -> [a] -> [(k,[v])]
collate_on_adjacent f g =
    let h l = case l of
                [] -> error "collate_on_adjacent"
                l0:_ -> (f l0,map g l)
    in map h . group_on f

-- > load_image_data prj_dir
load_image_data :: FilePath -> IO Image_Group
load_image_data dir = do
  let csv_fn = dir </> "data/csv/images.csv"
  str <- W.read_file_utf8 csv_fn
  let p = C.parseDSV False ',' str
      r = C.fromCSVTable (C.csvTable p)
      f [s,fn,nm,opt] = (s,(fn,nm,opt))
      f _ = error "load_image_data"
  return (collate_on_adjacent fst snd (map f r))

{-
> import Data.List
> img <- load_image_group prj_dir
> let f (n,l) = map (\(fn,nm) -> intercalate ";" [show n,fn,nm,"nil"]) l
> putStrLn$unlines$concatMap f (zip [0..] img)
-}

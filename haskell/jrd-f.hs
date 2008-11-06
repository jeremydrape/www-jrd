import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import Text.HTML.Download
import qualified Text.Html.Light as H
import qualified Text.XML.Light as X

data Image = Image { identifier :: String
                   , secret :: String
                   , server :: String
                   , farm :: String
                   , title :: String }
             deriving (Read, Show, Eq)

mk_image :: X.Element -> Image
mk_image e = 
    let f s = maybe "" id (X.findAttr (X.QName s Nothing Nothing) e)
    in Image (f "id") (f "secret") (f "server") (f "farm") (f "title")

flickr_rest :: String -> [(String,String)] -> String
flickr_rest m a = 
    let xs = map (\(k,v) -> "&" ++ k ++ "=" ++ v) a
    in "http://api.flickr.com/services/rest/?method=" ++ m ++ concat xs

flickr_rest_keyed :: String -> String -> [(String,String)] -> String
flickr_rest_keyed m k a = flickr_rest m (("api_key", k) : a)

get_info_uri :: String -> String -> String
get_info_uri k n = 
    flickr_rest_keyed "flickr.photos.getInfo"
                      k
                      [("photo_id", n)]
                      
get_public_photos_uri :: String -> String -> Integer -> Integer -> String
get_public_photos_uri k u p n = 
    flickr_rest_keyed "flickr.people.getPublicPhotos" 
                      k 
                      [("user_id", u)
                      ,("page", show p)
                      ,("per_page", show n)]

get_public_photo_uri :: String -> String -> Integer -> String
get_public_photo_uri k u n = get_public_photos_uri k u n 1

http_response_parts :: [String] -> ([String], [String])
http_response_parts xs =
  let starts_with c (x:_) = c == x
      starts_with _ _ = False
  in span (not . (starts_with '<')) xs

delete_http_headers :: [String] -> [String]
delete_http_headers xs = snd (http_response_parts xs)

run_query :: String -> IO [String]
run_query u = do
  s <- openURL u
  return (delete_http_headers (lines s))

mk_query :: String -> IO (Maybe X.Element)
mk_query u = do
  xs <- run_query u
  return (X.parseXMLDoc (concat xs))

get_public_photos :: String -> String -> Integer -> Integer -> IO [Image]
get_public_photos k u p n = do
  e <- mk_query (get_public_photos_uri k u p n)
  let ps = maybe [] (X.findElements (X.QName "photo" Nothing Nothing)) e
  return (map mk_image ps)
 
get_info :: String -> String -> IO (Maybe Image)
get_info k n = do
  e <- mk_query (get_info_uri k n)
  let p = maybe Nothing (X.findElement (X.QName "photo" Nothing Nothing)) e
  return (maybe Nothing (Just . mk_image) p)

mk_uri :: Maybe Char -> Image -> String
mk_uri s p = let t = maybe "" (\c -> ['_', c]) s
             in "http://farm" ++ farm p ++ ".static.flickr.com/" ++ server p ++ 
                    "/" ++ identifier p ++ "_" ++ secret p ++ t ++ ".jpg"

dv :: String -> [H.Element] -> H.Element
dv c = H.div [H.class' c]

mk_div :: Image -> Maybe String -> H.Element
mk_div p n = dv "photo" [maybe i f n]
    where i = H.img [H.src (mk_uri Nothing p)
                    ,H.height "500px"
                    ,H.alt (title p)]
          f m = H.a [H.href (up 1 </> m)] [i]

std_html_attr :: [H.Attribute]
std_html_attr = 
    [H.xmlns "http://www.w3.org/1999/xhtml"
    ,H.xml_lang "en"
    ,H.lang "en" ]

std_meta :: String -> String -> [H.Element]
std_meta d s = 
    [H.title [] [H.CData d]
    ,H.meta [H.name "description", H.content d]
    ,H.link [H.rel "stylesheet", H.type' "text/css", H.href s] ]

mk_page :: FilePath -> String -> [H.Element] -> String
mk_page top t e = 
    H.renderXHTML 
     H.xhtml_1_0_strict 
      (H.html 
       [H.xmlns "http://www.w3.org/1999/xhtml"
       ,H.xml_lang "en"
       ,H.lang "en"] 
       [H.head [] (std_meta t (top </> "jrd-f.css")), H.body [] e])

mk_index :: String -> FilePath -> [(Image, Integer)] -> Image -> H.Element
mk_index s top is _c = 
    let f (i,n) = g (i,n) -- if i == c then H.CData (show n) else (g (i,n))
        g (i,n) = H.a 
                  [H.href (top </> "f" </> identifier i)] 
                  [H.CData (show n)
                  ,H.nbsp]
        h = H.span [H.class' "area"] [H.CData (s ++ ": ")]
    in dv "index" (h : intersperse (H.CData " ") (map f is))

menu :: FilePath -> H.Element
menu top = dv 
       "menu"
       [dv "jrd" [H.a [H.href "http://jeremydrape.com"] [H.CData "jeremy drape"]
                 ,H.CData " / photography"]
       ,dv "lks" (intersperse 
                  (H.CData ", ")
                  [H.a 
                   [H.href (top </> "f" </> show (head jrd_portfolio))] 
                   [H.CData "portfolio"]
                  ,H.a 
                   [H.href (top </> "f" </> "projects")] 
                   [H.CData "projects"]
                  ,H.a 
                   [H.href "http://horsehunting.blogspot.com/"
                   ,H.target "_blank"] 
                   [H.CData "blog"]
                  ,H.a 
                   [H.href (top </> "f" </> "bio")] 
                   [H.CData "bio"]
                  ,H.a 
                   [H.href (top </> "f" </> "contact")]
                   [H.CData "contact"]])]

up :: Int -> FilePath
up 0 = "."
up 1 = ".."
up n = ".." </> up (n - 1)

find_next :: [Image] -> Image -> Maybe String
find_next (i:j:xs) k | i == k = Just (identifier j)
                     | otherwise = find_next (j:xs) k
find_next _ _ = Nothing

write_page :: String -> [(Image, Integer)] -> Image -> IO ()
write_page s is i = 
    do let idx = mk_index s (up 2) is i
           d = up 1 </> "f" </> identifier i
           t = "jrd/f/" ++ identifier i
       createDirectoryIfMissing True d
       writeFile (d </> "index.html") (mk_page (up 2) t [ menu (up 2)
                                                        , mk_div i (find_next (map fst is) i)
                                                        , idx ])

write_front :: Image -> IO ()
write_front i = 
    do let d = up 1 </> "f"
           t = "jeremy drape / photographer" ++ identifier i
       createDirectoryIfMissing True d
       writeFile (d </> "index.html") (mk_page (up 1) t [menu (up 1), mk_div i Nothing])

mk_section :: (String,[String]) -> H.Element
mk_section (t,ls) = H.div [] [H.h2 [] [H.CData t] 
                             ,H.div [] (intersperse (H.br []) (map H.CData ls))]

mk_textual :: String -> [(String,[String])] -> IO ()
mk_textual t ls = do
  let d = up 1 </> "f" </> t
  createDirectoryIfMissing True d
  let e = replicate 4 (H.br [])
      c = e ++ map mk_section ls
      p = mk_page (up 2) t [menu (up 2), H.div [H.class' "text"] c]
  writeFile (d </> "index.html") p

mk_projects :: [(String, Integer)] -> IO ()
mk_projects ls = do
  let top = up 2
      t = "projects"
      d = up 1 </> "f" </> t
  createDirectoryIfMissing True d
  let e = replicate 4 (H.br [])
      c = e ++ map (\(s, i) -> H.div [] [H.a [H.href (top </> "f" </> show i)] [H.CData s]]) ls
      p = mk_page top  t [menu (up 2), H.div [H.class' "text"] c]
  writeFile (d </> "index.html") p

write_database :: [Integer] -> IO ()
write_database ns = 
    do createDirectoryIfMissing True "db"
       let key = "fc835bdbc725d54415ff763ee93f7c2d"
       is <- mapM (fmap fromJust . get_info key) (map show ns)
       let f (n, i) = writeFile (up 1 </> "f" </> "db" </> show n) (show i)
       mapM_ f (zip ns is)

read_database :: Integer -> IO Image
read_database n = 
    do s <- readFile (up 1 </> "f" </> "db" </> show n)
       return (read s)

write_picture_set :: String -> [Integer] -> IO [Image]
write_picture_set s ns = 
    do is <- mapM read_database ns
       let js = zip is [1..]
       mapM_ (write_page s js) is
       return is

data Flag = Rebuild
            deriving (Eq, Show)
    
options :: [OptDescr Flag]
options =
    [ Option ['r'] ["rebuild"] (NoArg Rebuild) "rebuild image database" ]

parse_options :: [String] -> [OptDescr Flag] -> IO ([Flag], [String])
parse_options as os = 
    let h = "usage: jrd-f [options]"
    in case getOpt Permute os as of
         (o, n, []) -> return (o, n)
         (_, _, es) -> ioError (userError (concat es ++ usageInfo h os))

rebuild :: IO ()
rebuild = 
    let is = jrd_portfolio ++ jrd_projects_2005 ++ jrd_projects_2008
    in write_database is

main :: IO ()
main = do
  as <- getArgs
  (os, _) <- parse_options as options
  if Rebuild `elem` os
    then rebuild
    else return ()
  is <- write_picture_set "portfolio" jrd_portfolio
  write_front (is !! 2)
  write_picture_set "projects 2005" jrd_projects_2005
  write_picture_set "projects 2008" jrd_projects_2008
  mk_textual "contact" jrd_contact
  mk_textual "bio" jrd_bio
  mk_projects [("projects 2008", jrd_projects_2008 !! 0)
              ,("projects 2005", jrd_projects_2005 !! 0)]

jrd_portfolio :: [Integer]
jrd_portfolio = 
    [2773687772
    ,2752508645
    ,2772814665
    ,2752510459
    ,2888690149
    ,2888689563
    ,2888688277
    ,2888647043
    ,2752613797
    ,2752505961
    ,2773734560
    ,2753341584
    ,2773617644
    ,2772917061
    ,2773744908
    ,2772897849
    ,2773739848]

jrd_projects_2008 :: [Integer]
jrd_projects_2008 =
    [2975815597
    ,2975930133
    ,2975678167
    ,2975830543
    ,2976523800
    ,2976807306
    ,2975943979
    ,2976795876
    ,2976515682
    ,2975591811
    ,2975667623
    ,2975588827
    ,2976781918
    ,2975938457
    ,2975673763
    ,2975820397
    ,2975959797
    ,2975842115
    ,2976690996
    ,2976518168
    ,2976800824
    ,2975812395
    ,2975955159]

jrd_projects_2005 :: [Integer]
jrd_projects_2005 =
    [2816331143
    ,2977077704
    ,2976226133
    ,2816330797
    ,2816331047
    ,2977074038
    ,2976228917
    ,2975921867
    ,2976779054
    ,2977086406
    ,2816339243
    ,2977086004
    ,2975957499
    ,2817190002
    ]

jrd_contact :: [(String,[String])]
jrd_contact = 
    [(""
     ,[""
      ,""
      ,""
      ,""
      ,"jeremy drape"
      ,"email:jeremy@jeremydrape.com"
      ,"http://www.jeremydrape.com/"
      ,"telephone:0406 627 085"])]

jrd_bio :: [(String,[String])]
jrd_bio = 
    [("Education"
     ,["Bachelor of Fine Art (Honours)"
      ,"Major - Photography"
      ,"2000 - 2004"
      ,"Victorian College of The Arts"])
    ,("Awards"
     ,["2003 - Dr David Rosenthal Award, VCA"
      ,"2001 - Theodor Urbach Award, VCA"])
    ,("Selected Group Exhibitions"
     ,["2007 - Polar - Margaret Lawrence Gallery"
      ,"2007 - Always On My Mind - TCB Gallery"
      ,"2004 - The Graduate Show - Margaret Lawrence Gallery"
      ,"2004 - VCA Photography Graduates - Span Galleries"
      ,"2003 - The Graduate Show - Margaret Lawrence Gallery"
      ,"2003 - Art of Protest - Bmw Edge Federation Square"])]

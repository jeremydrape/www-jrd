import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import Text.HTML.Download
import qualified Text.Html.Light as H
import Text.XML.Light

data Image = Image { identifier :: String
                   , secret :: String
                   , server :: String
                   , farm :: String
                   , title :: String }
             deriving (Show, Eq)

mk_image :: Element -> Image
mk_image e = 
    let f s = maybe "" id (findAttr (QName s Nothing Nothing) e)
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

mk_query :: String -> IO (Maybe Element)
mk_query u = do
  xs <- run_query u
  return (parseXMLDoc (concat xs))

get_public_photos :: String -> String -> Integer -> Integer -> IO [Image]
get_public_photos k u p n = do
  e <- mk_query (get_public_photos_uri k u p n)
  let ps = maybe [] (findElements (QName "photo" Nothing Nothing)) e
  return (map mk_image ps)
 
get_info :: String -> String -> IO (Maybe Image)
get_info k n = do
  e <- mk_query (get_info_uri k n)
  let p = maybe Nothing (findElement (QName "photo" Nothing Nothing)) e
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
          f m = H.a [H.href (".." </> m)] [i]

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

mk_index :: FilePath -> [(Image, Integer)] -> Image -> H.Element
mk_index top is c = 
    let f (i,n) = if i == c then H.CData (show n) else (g (i,n))
        g (i,n) = H.a 
                  [H.href (top </> "f" </> identifier i)] 
                  [H.CData (show n)
                  ,H.nbsp]
    in dv "index" (intersperse (H.CData " ") (map f is))

menu :: FilePath -> H.Element
menu top = dv 
       "menu"
       [dv "jrd" [H.a [H.href "http://jeremydrape.com"] [H.CData "jeremy drape"]
                 ,H.CData " / photography"]
       ,dv "lks" (intersperse 
                  (H.CData ", ")
                  [H.a 
                   [H.href (top </> "f" </> show (head jrd))] 
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

write_page :: [(Image, Integer)] -> Image -> IO ()
write_page is i = 
    do let idx = mk_index (up 2) is i
           d = ".." </> "f" </> identifier i
           t = "jrd/f/" ++ identifier i
       createDirectoryIfMissing True d
       writeFile (d </> "index.html") (mk_page (up 2) t [ menu (up 2)
                                                        , mk_div i (find_next (map fst is) i)
                                                        , idx ])

write_front :: Image -> IO ()
write_front i = 
    do let d = ".." </> "f"
           t = "jeremy drape / photographer" ++ identifier i
       createDirectoryIfMissing True d
       writeFile (d </> "index.html") (mk_page ".." t [menu (up 1), mk_div i Nothing])

mk_section :: (String,[String]) -> H.Element
mk_section (t,ls) = H.div [] [H.h2 [] [H.CData t] 
                             ,H.div [] (intersperse (H.br []) (map H.CData ls))]

mk_textual :: String -> [(String,[String])] -> IO ()
mk_textual t ls = do
  let d = "../f/" ++ t
  createDirectoryIfMissing True d
  let e = replicate 4 (H.br [])
      c = e ++ map mk_section ls
      p = mk_page "../.." t [menu (up 2), H.div [H.class' "text"] c]
  writeFile (d </> "index.html") p

main :: IO ()
main = do
  xs <- mapM (get_info "fc835bdbc725d54415ff763ee93f7c2d") (map show jrd)
  let is = catMaybes xs
      js = zip is [1..]
  mapM_ (putStrLn . show) is
  mapM_ (write_page js) is
  write_front (is !! 2)
  mk_textual "contact" contact
  mk_textual "bio" bio
  mk_textual "projects" [("",["coming soon..."])]

jrd :: [Integer]
jrd = [2773687772
      ,2752508645
      ,2772814665
      ,2752510459
      ,2752613797
      ,2752505961
      ,2773734560
      ,2753341584
      ,2773617644
      ,2772917061
      ,2773744908
      ,2772897849
      ,2773739848]

{-
jrd :: [String]
jrd = ["2649268247"
      ,"2650059364"
      ,"2649056823"
      ,"2649888018"
      ,"2649887554"
      ,"2649887314"
      ,"2649055337"
      ,"2649886604"
      ,"2649054539"
      ,"2649885782"
      ,"2649053773"
      ,"2649884956"]
-}

contact :: [(String,[String])]
contact = [(""
           ,[""
            ,""
            ,""
            ,""
            ,"jeremy drape"
            ,"email:jeremy@jeremydrape.com"
            ,"http://www.jeremydrape.com/"
            ,"telephone:0406 627 085"])]

bio :: [(String,[String])]
bio = [("Education"
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

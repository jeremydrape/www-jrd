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
             deriving (Show)

mk_image :: Element -> Image
mk_image e = 
    let f s = fromJust (findAttr (QName s Nothing Nothing) e)
    in Image (f "id") (f "secret") (f "server") (f "farm") (f "title")

flickr_rest :: String -> [(String,String)] -> String
flickr_rest m a = 
  let xs = map (\(k,v) -> "&" ++ k ++ "=" ++ v) a
  in "http://api.flickr.com/services/rest/?method=" ++ m ++ concat xs

flickr_rest_keyed :: String -> String -> [(String,String)] -> String
flickr_rest_keyed m k a = flickr_rest m (("api_key", k) : a)

get_public_photos_uri :: String -> String -> Integer -> Integer -> String
get_public_photos_uri k u p n = 
  flickr_rest_keyed "flickr.people.getPublicPhotos" k [("user_id", u)
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
 
mk_uri :: Maybe Char -> Image -> String
mk_uri s p = let t = maybe "" (\c -> ['_', c]) s
             in "http://farm" ++ farm p ++ ".static.flickr.com/" ++ server p ++ 
                    "/" ++ identifier p ++ "_" ++ secret p ++ t ++ ".jpg"

dv :: String -> [H.Element] -> H.Element
dv c = H.div [H.class' c]

mk_div :: Image -> H.Element
mk_div p =
    let l e = [H.a [H.href (mk_uri Nothing p)] e]
        i = l [H.img [H.src (mk_uri (Just 's') p)]]
        t = l [H.CData (title p)]
    in dv "node-a" [dv "image-a" i, dv "text-a" t]

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

mk_page :: [H.Element] -> String
mk_page e = 
    H.renderXHTML 
     H.xhtml_1_0_strict 
      (H.html [] [H.head [] (std_meta "tst" "../../jrd-f.css"), H.body [] e])

compareBy :: Ord a => (x -> a) -> x -> x -> Ordering
compareBy f x y = compare (f x) (f y)

main :: IO ()
main = do
  is <- get_public_photos "fc835bdbc725d54415ff763ee93f7c2d" "28389435@N07" 1 100
  let d = "../f/a"
      is' = sortBy (compareBy title) is
  createDirectoryIfMissing True d
  writeFile (d </> "index.html") (mk_page (map mk_div is'))
  mapM_ putStrLn (map show is')
  mapM_ putStrLn (map identifier is')

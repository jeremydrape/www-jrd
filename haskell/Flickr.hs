module Flickr where

import Data.List
import Data.Maybe
import Text.HTML.Download
import qualified Text.XML.Light as X

data Image = Image { identifier :: String
                   , secret :: String
                   , server :: String
                   , farm :: String
                   , title :: String }
             deriving (Read, Show, Eq)

query :: String -> [(String,String)] -> String
query h a =
    let xs = intercalate "&" (map (\(k, v) -> concat [k, "=", v]) a)
    in concat [h, "?", xs]

http_response_parts :: [String] -> ([String], [String])
http_response_parts =
  let starts_with c (x:_) = c == x
      starts_with _ _ = False
  in span (not . starts_with '<')

delete_http_headers :: [String] -> [String]
delete_http_headers = snd . http_response_parts

run_query :: String -> IO (Maybe X.Element)
run_query u = do
  s <- openURL u
  let xs = delete_http_headers (lines s)
  return (X.parseXMLDoc (concat xs))

mk_image :: X.Element -> Image
mk_image e =
    let f s = fromMaybe "" (X.findAttr (X.QName s Nothing Nothing) e)
    in Image (f "id") (f "secret") (f "server") (f "farm") (f "title")

flickr_rest :: String -> String -> [(String,String)] -> String
flickr_rest k m a =
    let h = "http://api.flickr.com/services/rest/"
    in query h (("method", m) : ("api_key", k) : a)

get_info_uri :: String -> String -> String
get_info_uri k n =
    flickr_rest k
                "flickr.photos.getInfo"
                [("photo_id", n)]

get_public_photos_uri :: String -> String -> Integer -> Integer -> String
get_public_photos_uri k u p n =
    flickr_rest k
                "flickr.people.getPublicPhotos"
                [("user_id", u)
                ,("page", show p)
                ,("per_page", show n)]

get_public_photo_uri :: String -> String -> Integer -> String
get_public_photo_uri k u n = get_public_photos_uri k u n 1

get_public_photos :: String -> String -> Integer -> Integer -> IO [Image]
get_public_photos k u p n = do
  e <- run_query (get_public_photos_uri k u p n)
  let ps = maybe [] (X.findElements (X.QName "photo" Nothing Nothing)) e
  return (map mk_image ps)

get_info :: String -> String -> IO (Maybe Image)
get_info k n = do
  e <- run_query (get_info_uri k n)
  let p = maybe Nothing (X.findElement (X.QName "photo" Nothing Nothing)) e
  return (maybe Nothing (Just . mk_image) p)

mk_uri :: Maybe Char -> Image -> String
mk_uri s p =
    let t = maybe "" (\c -> ['_', c]) s
    in concat ["http://farm", farm p, ".static.flickr.com/", server p
              ,"/", identifier p, "_", secret p, t, ".jpg"]

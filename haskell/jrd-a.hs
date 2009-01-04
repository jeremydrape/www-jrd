import Data.List
import Data.Maybe
import Flickr
import System.Directory
import System.FilePath
import qualified Text.HTML.Light as H
import qualified Text.XML.Light as X

dv :: String -> [X.Content] -> X.Content
dv c = H.div [H.class' c]

mk_div :: Image -> X.Content
mk_div p =
    let l e = [H.a [H.href (mk_uri Nothing p)] e]
        i = l [H.img [H.src (mk_uri (Just 's') p)]]
        t = l [H.cdata (title p)]
    in dv "node-a" [dv "image-a" i, dv "text-a" t]

std_html_attr :: [X.Attr]
std_html_attr =
    [H.xmlns "http://www.w3.org/1999/xhtml"
    ,H.xml_lang "en"
    ,H.lang "en" ]

std_meta :: String -> String -> [X.Content]
std_meta d s =
    [H.title [] [H.cdata d]
    ,H.meta [H.name "description", H.content d]
    ,H.link [H.rel "stylesheet", H.type' "text/css", H.href s] ]

mk_page :: [X.Content] -> String
mk_page e =
    let m = std_meta "jrd/f/a" "../../css/jrd-f.css"
    in H.renderXHTML
       H.xhtml_1_0_strict
       (H.html [] [H.head [] m, H.body [] e])

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

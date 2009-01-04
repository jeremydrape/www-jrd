import Data.List
import Data.Maybe
import Flickr
import System.Directory
import System.FilePath
import qualified Text.HTML.Light as H
import qualified Text.XML.Light as X

dv :: String -> [X.Content] -> X.Content
dv c = H.div [H.class' c]

mk_div :: Image -> Maybe String -> X.Content
mk_div p n =
    let i = H.img [H.src (mk_uri Nothing p)
                  ,H.height "500px"
                  ,H.alt (title p)]
        f m = H.a [H.href (up 1 </> m)] [i]
    in dv "photo" [maybe i f n]

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

mk_page :: FilePath -> String -> [X.Content] -> String
mk_page top t e =
    H.renderXHTML
     H.xhtml_1_0_strict
      (H.html
       [H.xmlns "http://www.w3.org/1999/xhtml"
       ,H.xml_lang "en"
       ,H.lang "en"]
       [H.head [] (std_meta t (top </> "css" </> "jrd-f.css")), H.body [] e])

mk_index :: String -> FilePath -> [(Image, Integer)] -> Image -> X.Content
mk_index s top is _c =
    let f (i,n) = g (i,n) -- if i == c then H.cdata (show n) else (g (i,n))
        g (i,n) = H.a
                  [H.href (top </> "f" </> identifier i)]
                  [H.cdata (show n)
                  ,H.nbsp]
        h = H.span [H.class' "area"] [H.cdata (s ++ ": ")]
    in dv "index" (h : intersperse (H.cdata " ") (map f is))

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
           pg = mk_page (up 2) t [ jrd_menu (up 2)
                                 , mk_div i (find_next (map fst is) i)
                                 , idx ]
       createDirectoryIfMissing True d
       writeFile (d </> "index.html") pg

write_front :: Image -> IO ()
write_front i =
    do let d = up 1 </> "f"
           t = "jeremy drape / photographer" ++ identifier i
           pg = mk_page (up 1) t [jrd_menu (up 1), mk_div i Nothing]
       createDirectoryIfMissing True d
       writeFile (d </> "index.html") pg

mk_section :: (String,[String]) -> X.Content
mk_section (t,ls) = H.div [] [H.h2 [] [H.cdata t]
                             ,H.div [] (intersperse (H.br []) (map H.cdata ls))]

mk_textual :: String -> [(String,[String])] -> IO ()
mk_textual t ls = do
  let d = up 1 </> "f" </> t
  createDirectoryIfMissing True d
  let e = replicate 4 (H.br [])
      c = e ++ map mk_section ls
      p = mk_page (up 2) t [jrd_menu (up 2), H.div [H.class' "text"] c]
  writeFile (d </> "index.html") p

mk_projects :: [(String, Integer)] -> IO ()
mk_projects ls = do
  let top = up 2
      t = "projects"
      d = up 1 </> "f" </> t
  createDirectoryIfMissing True d
  let e = replicate 4 (H.br [])
      f (s, i) = H.div 
                  [] 
                  (H.a
                   [H.href (top </> "f" </> show i)] 
                   [H.cdata s] : replicate 4 (H.br []))
      c = e ++ map f ls
      p = mk_page top  t [jrd_menu (up 2), H.div [H.class' "text"] c]
  writeFile (d </> "index.html") p

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

gen_files :: IO ()
gen_files =
    do is <- write_picture_set "portfolio" jrd_portfolio
       write_front (is !! 2)
       write_picture_set "projects 2005" jrd_projects_2005
       write_picture_set "projects 2008" jrd_projects_2008
       mk_textual "contact" jrd_contact
       mk_textual "bio" jrd_bio
       let (p8:_) = jrd_projects_2008
           (p5:_) = jrd_projects_2005
       mk_projects [("projects 2008", p8)
                   ,("projects 2005", p5)]

-- * jrd content

jrd_menu :: FilePath -> X.Content
jrd_menu top = dv
       "menu"
       [dv "jrd" [H.a [H.href "http://jeremydrape.com"] [H.cdata "jeremy drape"]
                 ,H.cdata " / photography"]
       ,dv "lks" (intersperse
                  (H.cdata ", ")
                  [H.a
                   [H.href (top </> "f" </> show (head jrd_portfolio))]
                   [H.cdata "portfolio"]
                  ,H.a
                   [H.href (top </> "f" </> "projects")]
                   [H.cdata "projects"]
                  ,H.a
                   [H.href "http://horsehunting.blogspot.com/"
                   ,H.target "_blank"]
                   [H.cdata "blog"]
                  ,H.a
                   [H.href (top </> "f" </> "bio")]
                   [H.cdata "bio"]
                  ,H.a
                   [H.href (top </> "f" </> "contact")]
                   [H.cdata "contact"]])]

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

-- * jrd-f/rebuild : database generator

write_database :: [Integer] -> IO ()
write_database ns =
    do let d = up 1 </> "f" </> "db"
       createDirectoryIfMissing True d
       let key = "fc835bdbc725d54415ff763ee93f7c2d"
       is <- mapM (fmap fromJust . get_info key) (map show ns)
       let f (n, i) = writeFile (d </> show n) (show i)
       mapM_ f (zip ns is)

rebuild :: IO ()
rebuild =
    let is = jrd_portfolio ++ jrd_projects_2005 ++ jrd_projects_2008
    in write_database is

import Data.List
import Data.Maybe
import Flickr
import System.Directory
import System.FilePath
import qualified Text.HTML.Light as H
import qualified Text.XML.Light as X

cdiv :: String -> [X.Content] -> X.Content
cdiv c = H.div [H.class' c]

mk_div :: Image -> Maybe String -> X.Content
mk_div p n =
    let i = H.img [H.src (mk_uri Nothing p)
                  ,H.alt (title p)]
        f m = H.a [H.href (up 1 </> m)] [i]
    in H.div [] [cdiv "photo" [maybe i f n]
                ,cdiv "photo-title" [H.cdata (title p)]]

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

type PICTURE = (Image, Integer)

mk_index :: String -> FilePath -> [PICTURE] -> Image -> X.Content
mk_index s top is _c =
    let g (i,n) = H.a
                  [H.href (top </> "f" </> identifier i)]
                  [H.cdata (show n)
                  ,H.nbsp]
        h = H.span [H.class' "area"] [H.cdata (s ++ ": ")]
    in cdiv "index" (h : intersperse (H.cdata " ") (map g is))

up :: Int -> FilePath
up 0 = "."
up 1 = ".."
up n = ".." </> up (n - 1)

find_next :: [Image] -> Image -> Maybe String
find_next (i:j:xs) k | i == k = Just (identifier j)
                     | otherwise = find_next (j:xs) k
find_next _ _ = Nothing

write_page :: String -> [PICTURE] -> Image -> IO ()
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
           t = "jeremy drape / photographer"
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

read_database :: Integer -> IO Image
read_database n =
    do s <- readFile (up 1 </> "f" </> "db" </> show n)
       return (read s)

write_picture_set :: String -> [(Integer, String)] -> IO [Image]
write_picture_set s ns =
    do is <- mapM read_database (map fst ns)
       let retitle i t = i { title = t }
           is' = zipWith retitle is (map snd ns)
       let js = zip is' [1..]
       mapM_ (write_page s js) is'
       return is'

gen_files :: IO ()
gen_files =
    do fi <- read_database 3383211501
       write_front fi
       write_picture_set "portfolio" jrd_portfolio
       write_picture_set "works" jrd_works
       mk_textual "contact" jrd_contact
       mk_textual "bio" jrd_bio

-- * jrd content

jrd_menu :: FilePath -> X.Content
jrd_menu top = cdiv
       "menu"
       [cdiv
        "lks"
        (intersperse
         (H.cdata ", ")
         [H.a
          [H.href "http://jeremydrape.com"]
          [H.cdata "Jeremy Drape"]
        ,H.a
          [H.href (top </> "f" </> show (fst (head jrd_portfolio)))]
          [H.cdata "Portfolio"]
         ,H.a
          [H.href (top </> "f" </> show (fst (head jrd_works)))]
          [H.cdata "Works"]
         ,H.a
          [H.href "http://jeremydrape.blogspot.com/"
          ,H.target "_blank"]
          [H.cdata "The Index"]
         ,H.a
          [H.href (top </> "f" </> "bio")]
          [H.cdata "About"]
         ,H.a
          [H.href (top </> "f" </> "contact")]
          [H.cdata "Contact"]])]

jrd_portfolio :: [(Integer, String)]
jrd_portfolio =
    [(3730845371, "michelle")
    ,(3637844930, "lou")
    ,(2888690149, "sanja")
    ,(3731646402, "jean-yve (sitting)")
    ,(3731649382, "salote")
    ,(2752505961, "emily (car)")
    ,(2773687772, "utako")
    ,(2888688277, "sanja (flash)")
    ,(2752613797, "emily")
    ,(3383211501, "jean-yve")
    ,(2752511921, "utako (eyes closed)")
    ,(2889523886, "chair")
    ]

jrd_works :: [(Integer, String)]
jrd_works =
    [(2649888018, "The Peak, 2005")
    ,(2977077704, "Child Looking, 2005")
    ,(2649054539, "The Ferry, 2005")
    ,(2977074038, "The Island, 2005")
    ,(2975921867, "Metro, 2005")
    ,(2816339243, "The Ladder, 2005")
    ,(2817190002, "Doorway, 2005")
    ,(2977147184, "Walking Man, 2007")
    ,(2650059364, "Untitled, 2007")
    ,(2975930133, "Tyres, 2008")
    ,(2976690996, "Sophie (nude), 2008")
    ,(3049866394, "Sophie (diptych), 2008")
    ,(3049038105, "Red Stripe, 2008")
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
    [(""
     ,["Jeremy Drape is a Melbourne based photographer who graduated from the Victorian College of the Arts in 2004."])]

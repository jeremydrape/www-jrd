import qualified Control.Monad.Random as R {- MonadRandom -}
import qualified Network.CGI as C {- cgi -}
import qualified System.Random.Shuffle as R {- random-shuffle -}
import qualified WWW.Minus.CGI as W {- www-minus -}
import qualified WWW.Minus.CGI.Editor as W {- www-minus -}

import JRD

e_config :: W.Config
e_config =
    W.Config {W.cfg_vcs = Just (W.Git
                               ,("jeremy drape <jeremy.drape@gmail.com>"
                                ,Nothing {-Just "sp"-}))
             ,W.cfg_url = "http://jeremydrape.com"
             ,W.cfg_pwd = Nothing}

scramble :: [a] -> IO [a]
scramble k = R.evalRandIO (R.shuffleM k)

-- | If unset or empty scramble entire image set, else select first
-- image and store remainder.  This is rather fragile...
choose_image :: State -> C.CGI Image
choose_image st = do
  let (_,im) = st
      k = length im
      scr = C.liftIO (scramble [0 .. k - 1])
  c <- C.getCookie "n"
  n0:n <- case c of
            Nothing -> scr
            Just s -> case words s of
                        [] -> scr
                        r -> return (map read r)
  _ <- C.setCookie (C.newCookie "n" (unwords (map show n)))
  return (im !! n0)

mk_front :: State -> W.Result
mk_front st = do
  (nm,tt) <- choose_image st
  W.utf8_html_output (mk_img st (nm,tt))

mk_front_ss :: State -> W.Result
mk_front_ss (md,img) = W.utf8_html_output (gen_slideshow md img)

dispatch :: State -> W.Parameters -> W.Result
dispatch st (m,p,q) = do
  case (m,p,q) of
    ("GET",_,[("e",d)]) -> W.edit_get d
    ("POST",_,[("e",_)]) -> W.edit_post e_config ""
    ("GET",_,[("i",i)]) -> case img_title (snd st) i of
                             Nothing -> mk_front st
                             Just t -> W.utf8_html_output (mk_img st (i,t))
    ("GET",_,[("m","ix")]) -> W.utf8_html_output (mk_ix st)
    ("GET",_,[("m","resize")]) -> C.liftIO proc_resize >>
                                  W.utf8_text_output "jrd: m=resize"
    ("GET",_,[("m","upload")]) -> W.upload_get "data/jpeg" ".jpeg"
    ("POST",_,[("m","upload")]) ->
        do r <- W.upload_post e_config
           C.liftIO proc_resize
           return r
    ("GET",_,[]) -> mk_front_ss st
    _ -> W.utf8_text_output "jrd: dispatch error"

main :: IO ()
main = do
  images <- load_image_set "."
  md <- load_md "." ["menu"]
  W.run_cgi (md,images) dispatch

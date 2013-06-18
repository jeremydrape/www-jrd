import Control.Applicative {- base -}
import qualified Network.CGI as C {- cgi -}
import qualified System.Random as R {- random -}
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

mk_front :: State -> Maybe String -> W.Result
mk_front st k = do
  k' <- return . (<|> k) =<< C.getCookie "n"
  let (_,im) = st
      im' = maybe im (\z -> filter ((/= z) . fst) im) k'
  n <- W.lift_io (R.randomRIO (0,length im' - 1))
  let (nm,tt) = im' !! n
  _ <- C.setCookie (C.newCookie "n" nm)
  W.utf8_html_output (mk_img st (nm,tt))

dispatch :: State -> W.Parameters -> W.Result
dispatch st (m,p,q) = do
  case (m,p,q) of
    ("GET",_,[("e",d)]) -> W.edit_get d
    ("POST",_,[("e",_)]) -> W.edit_post e_config ""
    ("GET",_,[("i",i)]) -> case img_title (snd st) i of
                             Nothing -> mk_front st Nothing
                             Just t -> W.utf8_html_output (mk_img st (i,t))
    ("GET",_,[("n",i)]) -> mk_front st (Just i)
    ("GET",_,[("m","ix")]) -> W.utf8_html_output (mk_ix st)
    ("GET",_,[("m","upload")]) -> W.upload_get "data/jpeg" ".jpeg"
    ("POST",_,[("m","upload")]) -> W.upload_post e_config
    ("GET",_,[]) -> mk_front st Nothing
    _ -> W.utf8_text_output "jrd: dispatch error"

main :: IO ()
main = do
  images <- load_image_set "."
  md <- load_md "." ["menu"]
  W.run_cgi (md,images) dispatch

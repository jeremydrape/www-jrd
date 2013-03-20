import Data.List {- base -}
import qualified Network.CGI as C {- cgi -}
import qualified System.Random as R {- random -}
import qualified WWW.Minus.CGI as W {- www-minus -}
import qualified WWW.Minus.CGI.Editor as W {- www-minus -}

import JRD

e_config :: W.Config
e_config =
    W.Config {W.cfg_vcs = Just (W.Darcs
                               ,("jeremy drape <jeremy.drape@gmail.com>"
                                ,Nothing))
             ,W.cfg_url = "http://jeremydrape.com"
             ,W.cfg_pwd = Nothing}

mk_front :: (MD, [String]) -> Maybe String -> W.Result
mk_front st k = do
  let (_,im) = st
      im' = maybe im (\k' -> delete k' im) k
  n <- W.lift_io (R.randomRIO (0,length im' - 1))
  let nm = im' !! n
  _ <- C.setCookie (C.newCookie "n" nm)
  W.utf8_html_output (mk_img st nm)

dispatch :: State -> W.Parameters -> W.Result
dispatch st (m,p,q) = do
  q' <- W.cq_stateful ["n"] q
  case (m,p,q') of
    ("GET",_,[("e",d)]) -> W.edit_get d
    ("POST",_,[("e",_)]) -> W.edit_post e_config ""
    ("GET",_,[("i",i)]) -> W.utf8_html_output (mk_img st i)
    ("GET",_,[("n",i)]) -> mk_front st (Just i)
    _ -> mk_front st Nothing

main :: IO ()
main = do
  images <- load_image_set "."
  md <- load_md "." ["menu"]
  W.run_cgi (md,images) dispatch

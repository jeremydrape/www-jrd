import System.FilePath {- filepath -}
import System.Random {- random -}
import System.Random.Shuffle {- random-shuffle -}
import qualified WWW.Minus.CGI as W {- www-minus -}
import qualified WWW.Minus.CGI.Editor as W

import JRD

e_config :: W.Config
e_config =
    W.Config {W.cfg_vcs = Just (W.Darcs
                               ,("jeremy drape <jeremy.drape@gmail.com>"
                                ,Nothing))
             ,W.cfg_url = "http://jeremydrape.com"
             ,W.cfg_pwd = Nothing}

dispatch :: State -> W.Parameters -> W.Result
dispatch st (m,p,q) =
    case (m,p,q) of
      ("GET",_,[("e",d)]) -> W.edit_get d
      ("POST",_,[("e",_)]) -> W.edit_post e_config ""
      _ -> W.utf8_html_output (mk_front st)

main :: IO ()
main = do
  images <- load_image_set "."
  md <- load_md "." ["menu"]
  g <- getStdGen
  let images' = shuffle' images (length images) g
  W.run_cgi (md,images') dispatch

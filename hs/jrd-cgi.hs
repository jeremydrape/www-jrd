import qualified Control.Monad.Random as R {- MonadRandom -}
import qualified Network.CGI as C {- cgi -}
import qualified System.Random.Shuffle as R {- random-shuffle -}

import qualified WWW.Minus.CGI as W {- www-minus -}
import qualified WWW.Minus.CGI.Editor as W {- www-minus -}

import qualified JRD as J

e_config :: W.Config
e_config =
    W.Config {W.cfg_vcs = Just (W.Git
                               ,("jeremy drape <jeremy.drape@gmail.com>"
                                ,Just "www-jrd.git"))
             ,W.cfg_url = "http://jeremydrape.com"
             ,W.cfg_pwd = Nothing}

scramble :: [a] -> IO [a]
scramble k = R.evalRandIO (R.shuffleM k)

-- | If unset or empty scramble entire image set, else select first
-- image and store remainder.  This is rather fragile...
choose_image :: J.State -> C.CGI J.Image
choose_image st = do
  let im = J.st_img_set st
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

mk_front :: J.State -> W.Result
mk_front st = do
  (nm,tt) <- choose_image st
  W.utf8_html_output (J.mk_img st (nm,tt))

mk_front_ss :: J.State -> W.Result
mk_front_ss st = W.utf8_html_output (J.gen_slideshow st)

dispatch :: J.State -> W.Parameters -> W.Result
dispatch (opt,md,img_grp,_) (m,p,q) =
    let ix = read (W.q_default "s" "0" q)
        st = (opt,md,img_grp,ix)
        q' = W.q_remkey "s" q
    in case (m,p,q') of
         ("GET",_,[("e",d)]) -> W.edit_get d
         ("POST",_,[("e",_)]) -> W.edit_post e_config ""
         ("GET",_,[("i",i)]) ->
           case J.img_grp_lookup img_grp i of
             Nothing -> mk_front st
             Just (n,t) -> W.utf8_html_output (J.mk_img (opt,md,img_grp,n) (i,t))
         ("GET",_,[("m","ix")]) -> W.utf8_html_output (J.mk_ix st)
         ("GET",_,[("m","resize")]) ->
           C.liftIO J.proc_resize >>
           W.utf8_text_output "jrd: m=resize"
         ("GET",_,[("m","upload")]) -> W.upload_get "data/jpeg" ".jpeg"
         ("POST",_,[("m","upload")]) ->
           do r <- W.upload_post e_config
              C.liftIO J.proc_resize
              return r
         ("GET",_,[("m",mt)]) -> W.utf8_html_output (J.mk_md st mt)
         ("GET",_,[]) -> mk_front_ss st
         _ -> W.utf8_text_output "jrd: dispatch error"

main :: IO ()
main = J.load_st "." >>= \st -> W.run_cgi st dispatch

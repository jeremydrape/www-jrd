import qualified Network.CGI as C (liftIO) {- cgi -}

import qualified WWW.Minus.CGI as W {- www-minus -}
import qualified WWW.Minus.CGI.Editor as E {- www-minus -}

import qualified JRD as J

e_config :: E.Config
e_config =
    E.Config {E.cfg_vcs = Just (E.Git
                               ,("jeremy drape <jeremy.drape@gmail.com>"
                                ,Just "www-jrd.git"))
             ,E.cfg_url = "http://jeremydrape.com"
             ,E.cfg_pwd = Nothing}

mk_front_ss :: J.State -> W.Result
mk_front_ss st = W.utf8_html_output (J.mk_slideshow st)

dispatch :: J.State -> W.Parameters -> W.Result
dispatch (opt,md,img_grp,_) (m,p,q) =
    let s_ix = fmap read (W.q_lookup "s" q)
        st = (opt,md,img_grp,s_ix)
        q' = W.q_remkey "s" q
    in case (m,p,q') of
         ("GET",_,[("e",d)]) -> E.edit_get d
         ("POST",_,[("e",_)]) -> E.edit_post e_config ""
         ("GET",_,[("i",i)]) ->
           case J.img_grp_lookup img_grp i of
             Nothing -> mk_front_ss st
             Just (n,t) -> W.utf8_html_output (J.mk_img (opt,md,img_grp,Just n) (i,t))
         ("GET",_,[("m","ix")]) -> W.utf8_html_output (J.mk_ix st)
         ("GET",_,[("m","resize")]) ->
           C.liftIO J.proc_resize >>
           W.utf8_text_output "jrd: m=resize"
         ("GET",_,[("m","upload")]) -> E.upload_get "data/jpeg" ".jpeg"
         ("POST",_,[("m","upload")]) ->
           do r <- E.upload_post e_config
              C.liftIO J.proc_resize
              return r
         ("GET",_,[("m",mt)]) -> W.utf8_html_output (J.mk_md st mt)
         ("GET",_,[]) -> mk_front_ss st
         _ -> W.utf8_text_output "jrd: dispatch error"

main :: IO ()
main = J.load_st "." >>= \st -> W.run_cgi st dispatch

{-
import qualified Control.Monad.Random as R {- MonadRandom -}
import qualified System.Random.Shuffle as R {- random-shuffle -}

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
-}

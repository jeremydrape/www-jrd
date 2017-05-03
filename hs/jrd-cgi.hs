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

dispatch :: J.State -> W.Parameters -> W.Result
dispatch st (m,p,q) =
    let s_ix = W.q_default "s" "" q
        i_ix = W.q_default "x" "" q
        st' = st {J.st_ix = (s_ix,i_ix)}
        q' = W.q_remkeys ["s","x"] q
    in case (m,p,q') of
         ("GET",_,[("e",d)]) -> E.edit_get d
         ("POST",_,[("e",_)]) -> E.edit_post e_config ""
         ("GET",_,[("i",i)]) -> W.utf8_html_output (J.mk_slideshow st' (J.st_img_select_by_file i st'))
         ("GET",_,[("m","ix")]) -> W.utf8_html_output (J.mk_ix st')
         ("GET",_,[("m","resize")]) ->
           C.liftIO J.proc_resize >>
           W.utf8_text_output "jrd: m=resize"
         ("GET",_,[("m","upload")]) -> E.upload_get "data/jpeg" ".jpeg"
         ("POST",_,[("m","upload")]) ->
           do r <- E.upload_post e_config
              C.liftIO J.proc_resize
              return r
         ("GET",_,[("t",mt)]) -> W.utf8_html_output (J.mk_md st' mt)
         ("GET",_,[]) -> W.utf8_html_output (J.mk_slideshow st' (J.st_img_select_by_ix st'))
         _ -> W.utf8_text_output ("jrd: dispatch error: " ++ show (m,p,q'))

main :: IO ()
main = J.load_st "." >>= \st -> W.run_cgi st dispatch

import qualified WWW.Minus.CGI as CGI {- www-minus -}

import qualified JRD as J

{-

import qualified WWCGI.Minus.CGI.Editor as E {- www-minus -}

e_config :: E.Config
e_config =
    E.Config {E.cfg_vcs = Just (E.Git
                               ,("jeremy drape <jeremy.drape@gmail.com>"
                                ,Just "www-jrd.git"))
             ,E.cfg_url = "http://jeremydrape.com"
             ,E.cfg_pwd = Nothing}

         ("GET",_,[("e",d)]) -> E.edit_get d
         ("POST",_,[("e",_)]) -> E.edit_post e_config ""
         ("GET",_,[("m","upload")]) -> E.upload_get "data/jpeg" ".jpeg"
         ("POST",_,[("m","upload")]) ->
           do r <- E.upload_post e_config
              C.liftIO J.proc_resize
              return r

-}

dispatch :: J.State -> CGI.Parameters -> IO ()
dispatch st (m,q) =
    let s_ix = CGI.q_default "s" "" q
        i_ix = CGI.q_default "x" "" q
        st' = st {J.st_ix = (s_ix,i_ix)}
        q' = CGI.q_remkeys ["s","x"] q
    in case (m,q') of
         ("GET",[("i",i)]) -> CGI.utf8_html_output (J.mk_slideshow st' (J.st_img_select_by_file i st'))
         ("GET",[("m","ix")]) -> CGI.utf8_html_output (J.mk_ix st')
         ("GET",[("m","resize")]) ->
           J.proc_resize >>
           CGI.utf8_text_output "jrd: m=resize"
         ("GET",[("t",mt)]) -> CGI.utf8_html_output (J.mk_md st' mt)
         ("GET",[]) -> CGI.utf8_html_output (J.mk_slideshow st' (J.st_img_select_by_ix st'))
         _ -> CGI.utf8_text_output ("jrd: dispatch error: " ++ show (m,q'))

main :: IO ()
main = J.load_st "." >>= \st -> CGI.cgi_main (dispatch st)

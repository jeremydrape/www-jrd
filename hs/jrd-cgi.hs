import qualified Www.Minus.Cgi as Cgi {- www-minus -}

import qualified Jrd

{-

import qualified WWCgi.Minus.Cgi.Editor as E {- www-minus -}

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
              C.liftIO Jrd.proc_resize
              return r

-}

dispatch :: Jrd.State -> Cgi.Parameters -> IO ()
dispatch st (m,q) =
    let s_ix = Cgi.q_default "s" "_" q -- "_" is home
        i_ix = Cgi.q_default "x" "" q
        st' = st {Jrd.st_ix = (s_ix,i_ix)}
        q' = Cgi.q_remkeys ["s","x"] q
    in case (m,q') of
         ("GET",[("i",i)]) -> Cgi.utf8_html_output (Jrd.mk_slideshow st' (Jrd.st_img_select_by_file i st'))
         ("GET",[("m","ix")]) -> Cgi.utf8_html_output (Jrd.mk_ix st')
         ("GET",[("m","resize")]) ->
           Jrd.proc_resize >>
           Cgi.utf8_text_output "jrd: m=resize"
         ("GET",[("t",mt)]) -> Cgi.utf8_html_output (Jrd.mk_md st' mt)
         ("GET",[]) -> Cgi.utf8_html_output (Jrd.mk_slideshow st' (Jrd.st_img_select_by_ix st'))
         _ -> Cgi.utf8_text_output ("jrd: dispatch error: " ++ show (m,q'))

main :: IO ()
main = Jrd.load_st "." >>= \st -> Cgi.cgi_main (dispatch st)

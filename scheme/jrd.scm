(import (except (rnrs base) div)
        (except (rhs) id head span)
        (shtml-minimalist))

(define make-directory
  (lambda (s)
    (system (string-append "mkdir -p " s))))

(define std-meta
  (lambda (a d t s)
    (let ((t* (list a ": " d)))
      (list (title* noattr t*)
            (make-meta name "description" (if (string? t) t t*))
            (make-meta name "author" a)
            (make-meta http-equiv "expires" "-1")
            (make-meta http-equiv "pragma" "no-cache")
            (make-link "stylesheet" s "text/css")))))

(define std-html-attr
  (attr (xmlns "http://www.w3.org/1999/xhtml")
        (xml:lang "en")
        (lang "en")))

(define jd-site
  "http://www.jeremydrape.com/")

(define std-copyright
  (p (w/class "copyright")
     "&copy; "
     (a (attr (href jd-site)) "JEREMY DRAPE ")
     "2005-2007. "
     "NO UNAUTHORIZED COPYING OR REPRODUCTION. "
     (a (attr (href xhtml-validator)) "XHTML, ")
     (a (attr (href css-validator)) "CSS.")))

(define front
  (html
   std-html-attr
   (head
    noattr
    (std-meta
     "jeremy drape"
     "photographs about contact"
     "jeremy drape is a melbourne based photographer"
     "jrd.css"))
   (body
    (attr (class "front"))
    (p noattr
       (a (attr (href "portfolio/1/stairs/"))
          "W W W . J E R E M Y D R A P E . C O M")))))

(define mk-left-menu
  (lambda (top)
    (div
     noattr
     (p (w/class "menu")
        (a (attr (href (list top "portfolio/1/stairs/")))
           "PORTFOLIO")
        (br noattr))
     ;;        (a (attr (href (list top "portfolio/2/ferry/")))
     ;;           "PORTFOLIO 2"))
     ;;        (br noattr)
     ;;        (a (attr (href (list top "portfolio/3/annie/")))
     ;;           "PORTFOLIO 3"))
     (p (w/class "genera")
        (a (attr (href (list top "about/")))
           "CV")
        (br noattr)
        (a (attr (href (list top "contact/")))
           "CONTACT"))
     (p (w/class "validators")
        (a (attr (href xhtml-validator)) "XHTML, ")
        (a (attr (href css-validator)) "CSS.")))))

(define mk-text-page
  (lambda (nm top txt)
    (html
     std-html-attr
     (head
      noattr
      (std-meta
       "jeremy drape"
       nm
       #f
       (list top "jrd.css"))
      (body
       noattr
       (div
        (w/class "box")
        (div
         (w/class "header")
         (p
          (w/class "name")
          (a (attr (href top))
             "JEREMY DRAPE")))
        (div
         (w/class "left")
         (mk-left-menu top))
        (div
         (w/class "right")
         txt)
        (div
         (w/class "footer")
         )))))))

(define contact
  (mk-text-page
   "contact"
   "../"
   (p
    (w/class "contact")
    "JEREMY DRAPE<br/>"
    "EMAIL:JEREMY@JEREMYDRAPE.COM<br/>"
    "HTTP://WWW.JEREMYDRAPE.COM/<br/>"
    "TELEPHONE:0406 627 085<br/>")))

(define about
  (mk-text-page
   "about"
   "../"
   (div
    (w/class "about")
    (h1
     (attr (title "ABOUT")))
    (h2
     noattr
     "EDUCATION")
    (p
     noattr
     "BACHELOR OF FINE ART (HONOURS)<br/>"
     "MAJOR - PHOTOGRAPHY<br/>"
     "2000 - 2004<br/>"
     "VICTORIAN COLLEGE OF THE ARTS<br/>")
    (h2
     noattr
     "AWARDS")
    (p
     noattr
     "2003 - DR DAVID ROSENTHAL AWARD, VCA<br/>"
     "2001 - THEODOR URBACH AWARD, VCA<br/>")
    (h2
     noattr
     "SELECTED GROUP EXHIBITIONS")
    (p
     noattr
     "2007 - POLAR - MARGARET LAWRENCE GALLERY<br/>"
     "2007 - ALWAYS ON MY MIND - TCB GALLERY<br/>"
     "2004 - THE GRADUATE SHOW - MARGARET LAWRENCE GALLERY<br/>"
     "2004 - VCA PHOTOGRAPHY GRADUATES - SPAN GALLERIES<br/>"
     "2003 - THE GRADUATE SHOW - MARGARET LAWRENCE GALLERY<br/>"
     "2003 - ART OF PROTEST - BMW EDGE FEDERATION SQUARE<br/>"
     "2003 - ENTER VIA THE TRAIN LINE - HOPE STREET GALLERY<br/>"
     "2003 - EDITION 10 - HOMELESS GALLERY<br/>"
     "2002 - SIGNAGE - FOUND PROJECT SPACE<br/>"))))

(define portfolio
  (lambda (curr prev next txt top series prev-series next-series)
    (let* ((header
            (p
             (w/class "name")
             (a (attr (href top)) "JEREMY DRAPE")))
           (footer
            (list))
           (fwd
            (list "../../" next-series "/" next))
           (bck
            (list "../../" prev-series "/" prev))
           (right
            (a
             (attr
              (title "forward")
              (href fwd))
             (img
              (attr (class "photo")
                    (src (list top "images/portfolio/" series "/" curr ".jpeg"))
                    (alt (list curr ", photo by jeremy drape"))))))
           (left
            (div
             noattr
             (mk-left-menu top)
             (p
              (w/class "stepper")
              (a
               (attr (title "back")
                     (href bck))
               "&larr;")
              "&nbsp;"
              (a
               (attr (title "forward")
                     (href fwd))
               "&rarr;")))))
      (html
       std-html-attr
       (head
        noattr
        (std-meta
         "jeremy drape"
         "photographs"
         #f
         (string-append top "jrd.css")))
       (body
        noattr
        (div (w/class "box")
             (div (w/class "header") header)
             (div (w/class "left") left)
             (div (w/class "right") right)
             (div (w/class "footer") std-copyright)))))))

(define write-file
  (lambda (tree dir)
    (make-directory dir)
    (with-output-to-file
        (string-append dir "/index.html")
      (lambda ()
        (for-each
         display
         (flatten
          (list (?xml "1.0" "UTF-8")
                (!doctype
                 "html"
                 "PUBLIC"
                 "-//W3C//DTD XHTML 1.0 Strict//EN"
                 "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
                tree)))))))

(write-file front "../o")
(write-file contact "../o/contact")
(write-file about "../o/about")

(define series/portfolio/1
  (list
   (list "stairs" "" "1")
   (list "karly" "" "1")
   (list "ferry" "ferry (greece), 2005" "2")
   (list "hiding" "" "1")
   (list "anthony" "" "1")
   (list "tower" "tower, 2005" "2")
   (list "pool" "" "1")
   (list "ladder" "ladder, 2005" "2")
   (list "island" "" "2")
   (list "state-library" "" "1")
   ))

(define series/portfolio/2
  (list
   (list "ferry" "ferry (greece), 2005")
   (list "tower" "tower, 2005")
   (list "park"  "park (new york), 2005")
   (list "hose"  "hose, 2005")
   (list "doorway" "doorway #1, 2005")
   (list "ladder" "ladder, 2005")
   (list "family-looking" "family looking, 2005")
   ;;(list "metro" "untitled, 2005")
   (list "construction" "construction (berlin), 2005")
   (list "watching" "watching, 2005")
   (list "island" "")
   ;;(list "rail" "")
   (list "hotel" "")
   ))

(define series/portfolio/3
  (list
   (list "biting" "karly, 2006")
   (list "annie" "annie, 2006")
   (list "bed" "")
   ;;(list "call" "")
   ;;(list "concrete" "")
   (list "curtains" "")
   ;;(list "sky" "")
   (list "spirit" "")
   (list "julian" "julian, 2006")
   ))

(define (make-series series top)
  (let* ((m (length series))
         (z (lambda (j)
              (lambda (i)
                (list-ref
                 (list-ref series (modulo i m))
                 j))))
         (i (z 0))
         (t (z 1))
         (s (z 2)))
    (for-each (lambda (n)
                (write-file
                 (portfolio (i n)
                            (i (- n 1))
                            (i (+ n 1))
                            (t n)
                            top
                            (s n)
                            (s (- n 1))
                            (s (+ n 1)))
                 (string-append "../o/portfolio/" (s n) "/" (i n))))
              (enum-from-to 0 (- m 1)))))

(make-series series/portfolio/1 "../../../")
;;(make-series series/portfolio/2 "2" "../../../")
;;(make-series series/portfolio/3 "3" "../../../")

(exit)

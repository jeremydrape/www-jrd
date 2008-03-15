;; html.scm - (c) rohan drape, 2005-2008

(define ++ list)

(define (?xml version encoding)
  (++ "<?xml version=\"" version "\" encoding=\"" encoding "\"?>\n"))

(define (!doctype nm dm ds fn)
  (++ "<!DOCTYPE " nm " " dm " \"" ds "\"\n \"" fn "\">\n"))

;; Elements are nodes in the graph. Elements have attributes and
;; content. The element and attribute namespaces overlap. For
;; instance, title is both element and attribute. In such cases the
;; element constructor is capitalized.

(define-syntax define-attr
  (syntax-rules ()
    ((_ name) (define (name value) (list (quote name) value)))))

(define-attr xmlns)
(define-attr xml:lang)
(define-attr lang)
(define-attr name)
(define-attr content)
(define-attr rel)
(define-attr href)
(define-attr type)
(define-attr data)
(define-attr class)
(define-attr src)
(define-attr width)
(define-attr height)
(define-attr alt)
(define-attr title)
(define-attr id)
(define-attr http-equiv)
(define-attr border)
(define-attr colspan)
(define-attr rowspan)
(define-attr align)
(define-attr valign)

(define attr list)

;; Common attribute sets.

(define noattr (attr))
(define (w/class c) (attr (class c)))
(define (w/class+id c) (attr (class c) (id c)))

(define (build-attr a attr)
  (if (all (lambda (i) (member i attr)) (map car a))
      (map (lambda (key value) (list " " key "=\"" value "\""))
	   (map car a)
	   (map cadr a))
      (error "illegal attribute:" a " not in " attr)))

(define (sgml-element elem inline? content? allowable) 
  (let ((n (if inline? "" "\n")))
    (if content?
	(lambda (a . c)
	  (list "<" elem (build-attr a allowable) ">" n c n "</" elem ">" n))
	(lambda (a)
	  (list "<" elem (build-attr a allowable) " />" n)))))

(define-syntax define-elem
  (syntax-rules ()
    ((_ cons name inline? content? allowable)
     (define cons (sgml-element name inline? content? allowable)))))

(define-elem html "html" #f #t (quote (xmlns xml:lang lang)))
(define-elem head "head" #f #t (quote ()))
(define-elem title* "title" #f #t (quote (class)))
(define-elem meta "meta" #f #f (quote (name content http-equiv)))
(define-elem link "link" #f #f (quote (rel href name type)))
(define-elem body "body" #f #t (quote (class)))
(define-elem div "div" #f #t (quote (class)))
(define-elem span "span" #t #t (quote (class)))
(define-elem h1 "h1" #f #t (quote (class title)))
(define-elem h2 "h2" #f #t (quote (class title)))
(define-elem h3 "h3" #f #t (quote (class title)))
(define-elem p "p" #f #t (quote (class id)))
(define-elem dl "dl" #f #t (quote (class)))
(define-elem dt "dt" #f #t (quote (class)))
(define-elem dd "dd" #f #t (quote (class)))
(define-elem ol "ol" #f #t (quote (class id)))
(define-elem ul "ul" #f #t (quote (class id)))
(define-elem li "li" #f #t (quote (class)))
(define-elem a "a" #t #t (quote (class href name title)))
(define-elem img "img" #t #f (quote (class src alt height width)))
(define-elem em "em" #t #t (quote (class)))
(define-elem br "br" #f #f (quote (class)))
(define-elem table "table" #f #t (quote (class border)))
(define-elem tr "tr" #f #t (quote (class)))
(define-elem td "td" #f #t (quote (class colspan rowspan width height align valign)))
(define-elem th "th" #f #t (quote (class)))
(define-elem colgroup "colgroup" #f #t (quote (class span width align valign)))
(define-elem acronym "acronym" #t #t (quote (class title)))
(define-elem address "address" #f #t (quote (class title)))
(define-elem small "small" #t #t (quote (class)))
(define-elem strong "strong" #t #t (quote (class)))
(define-elem object "object" #t #t (quote (type name data)))

(define (meta* type name* content*)
  (meta (attr (type name*) (content content*))))

(define (link* rel* href* type*)
  (link (attr (rel rel*) (href href*) (type type*))))

(define xhtml-validator "http://validator.w3.org/check/referer")
(define css-validator "http://jigsaw.w3.org/css-validator/check/referer")

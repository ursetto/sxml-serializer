(module sxml-serializer
 (serialize-sxml        ;; looks like elaborate form of keyword processing

  sxml->xml
  sxml->xml/noindent
  sxml->html
  sxml->html/noindent

  display-sxml          ;; stupid name, it displays xml
  sxml->string

  conventional-ns-prefixes
  
  )

(import scheme chicken)
(require-library srfi-1)
(import (only srfi-1 filter))

(include "serializer.scm")

(define serialize-sxml srl:parameterizable)
(define sxml->xml srl:sxml->xml)
(define sxml->xml/noindent srl:sxml->xml-noindent)
(define sxml->html srl:sxml->html)
(define sxml->html/noindent srl:sxml->html-noindent)
(define display-sxml srl:display-sxml)
(define sxml->string srl:sxml->string)

;; override srl:conventional-ns-prefixes so that sxml->xml etc. use the extended list
(define srl:conventional-ns-prefixes
  '((admin . "http://webns.net/mvcb/")
    (atom . "http://www.w3.org/2005/Atom")
    (cc . "http://web.resource.org/cc/")
    (content . "http://purl.org/rss/1.0/modules/content/")
    (dc . "http://purl.org/dc/elements/1.1/")
    (feedburner . "http://rssnamespace.org/feedburner/ext/1.0")
    (fo . "http://www.w3.org/1999/XSL/Format")
    (geo . "http://www.w3.org/2003/01/geo/wgs84_pos#")
    (georss . "http://www.georss.org/georss")
    (itunes . "http://www.itunes.com/dtds/podcast-1.0.dtd")
    (media . "http://search.yahoo.com/mrss/")
    (rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (rng . "http://relaxng.org/ns/structure/1.0")
    (rss . "http://purl.org/rss/1.0/") 
    (slash . "http://purl.org/rss/1.0/modules/slash/")
    (sy . "http://purl.org/rss/1.0/modules/syndication/")
    (taxo . "http://purl.org/rss/1.0/modules/taxonomy/")
    (wiki . "http://purl.org/rss/1.0/modules/wiki/")
    (wfw . "http://wellformedweb.org/CommentAPI/")
    (xlink . "http://www.w3.org/1999/xlink")
    (xqx . "http://www.w3.org/2005/XQueryX")
    (xsd . "http://www.w3.org/2001/XMLSchema")
    (xsi . "http://www.w3.org/2001/XMLSchema-instance")
    (xsl . "http://www.w3.org/1999/XSL/Transform")))

(define conventional-ns-prefixes srl:conventional-ns-prefixes)

)

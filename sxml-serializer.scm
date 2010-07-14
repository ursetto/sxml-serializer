(module sxml-serializer
 (serialize-sxml        ;; looks like elaborate form of keyword processing

  sxml->xml
  sxml->xml/noindent
  sxml->html
  sxml->html/noindent

  display-sxml          ;; stupid name, it displays xml
  sxml->string
  
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

)

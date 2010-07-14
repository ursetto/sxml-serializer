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

(include "serializer.scm")


)

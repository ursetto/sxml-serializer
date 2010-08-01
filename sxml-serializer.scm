;; sxml-serializer -- Serialize SXML to XML and HTML4
;; Uses public domain code from sxml-tools by Dmitry Lizorkin
;; Chicken port Copyright (C) 2010 Jim Ursetto.  All Rights Reserved.
;; License: BSD.

;; Changes over stock:
;; Add allow-prefix-redeclarations? option.  Allows user to provide multiple namespace
;;   URIs that map to the same prefix.
;; Add fake namespace *default*.  

(module sxml-serializer
 (serialize-sxml
  conventional-ns-prefixes

  ;; These currently offer little benefit over plain serialize-sxml.
  ;; sxml->xml
  ;; sxml->xml/noindent
  ;; sxml->html
  ;; sxml->html/noindent
  
  )

(import scheme chicken)
(require-library srfi-1 srfi-13)
(import (only srfi-1 filter)
        (only srfi-13 string-concatenate))

(include "serializer.scm")

(define sxml->xml srl:sxml->xml)
(define sxml->xml/noindent srl:sxml->xml-noindent)
(define sxml->html srl:sxml->html)
(define sxml->html/noindent srl:sxml->html-noindent)
(define display-sxml srl:display-sxml)
(define sxml->string srl:sxml->string)

(define srl:apply-string-append string-concatenate)

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
(define allow-prefix-redeclarations? (make-parameter #f))

;; serialize-sxml: replacement for srl:parameterizable using keyword args
;; instead of (k . v) pairs.
;; Currently disallows xml-declaration emission because the interface is silly and
;; it doesn't provide an "encoding" option, and because if there is a (*PI* xml ...)
;; in the document it will either emit two, or omit only one.
(define (serialize-sxml sxml-obj
                        #!key
                        (output #f)
                        (cdata-section-elements '())
                        (indent "  ")
                        (method 'xml)
                        (ns-prefixes conventional-ns-prefixes)
                        (allow-prefix-redeclarations #f))  ; compatibility
  (let ((omit-xml-declaration #t)       ;; Force omission of xml-declaration
        (standalone 'omit)
        (version "1.0"))
    (parameterize ((allow-prefix-redeclarations? allow-prefix-redeclarations))
      (if output
          (srl:display-sxml sxml-obj output
                            cdata-section-elements indent
                            method ns-prefixes
                            omit-xml-declaration standalone version)
          (srl:sxml->string sxml-obj
                            cdata-section-elements indent
                            method ns-prefixes
                            omit-xml-declaration standalone version)
          ))))



;;; changes


(define (srl:qname->string prefix-string local-part)
  (if (and prefix-string
           (not (string=? prefix-string "*default*")))
      (string-append prefix-string ":" local-part)
      local-part))

(define (srl:namespace-decl->str-lst prefix-string namespace-uri)
  (if (string=? prefix-string "*default*")
      (list " xmlns" "=\""
            (srl:string->att-value namespace-uri) "\"")
      (list " xmlns:" prefix-string "=\""
            (srl:string->att-value namespace-uri) "\"")))

;; Similar to data-structures#alist-update!, but is non-destructive.
;; Returns a new list with (key . val) consed onto the front;
;; if KEY already exists in the alist, that pair is omitted from the
;; returned list.  Currently traverses the entire list and removes all matching keys.
(define (alist-update key val alist #!optional (cmp eqv?))
  (cons (cons key val)
        (let loop ((alist alist) (res '()))
          (cond ((null? alist)
                 (reverse res))
                ((cmp key (caar alist))
                 (loop (cdr alist) res))
                (else
                 (loop (cdr alist) (cons (car alist) res)))))))

;; Changes: When declaring a namespace prefix, remove any existing matching prefixes
;; from the declaration list, so new URIs shadow old ones with the same prefix.
;; Changes are marked with [+].
(define (srl:construct-start-end-tags
         elem method
         ns-prefix-assig namespace-assoc declared-ns-prefixes)
  (let ((ns-assoc-here (srl:namespace-assoc-for-elem elem))
        (empty? (srl:empty-elem? elem)))
    (let ((ns-prefix-assig
           (append
            (srl:extract-original-prefix-binding ns-assoc-here)
            ns-prefix-assig))
          (namespace-assoc
           (append ns-assoc-here namespace-assoc)))
      (call-with-values
       (lambda ()           
         (srl:name->qname-components  ; element name
          (car elem) ns-prefix-assig namespace-assoc declared-ns-prefixes))
       (lambda (elem-prefix elem-uri elem-local elem-decl-required?)
         (let loop ((attrs
                     (reverse
                      ((srl:select-kids 
                        (lambda (node)  ; not SXML 3.0 aux-list
                          (and (pair? node) (not (eq? (car node) '@)))))
                       ((srl:select-kids
                         (lambda (node)
                           (and (pair? node) (eq? (car node) '@))))
                        elem))))
                    (start-tag
                     (if
                      (or (not empty?)
                          (and (eq? method 'html)
                               (not elem-prefix)
                               (srl:member-ci
                                elem-local
                                ; ATTENTION: should probably move this list
                                ; to a global const
                                '("area" "base" "basefont" "br" "col"
                                  "frame" "hr" "img" "input" "isindex"
                                  "link" "meta" "param"))))
                      '(">") '(" />")))
                    (ns-prefix-assig ns-prefix-assig)
                    (namespace-assoc namespace-assoc)
                    (declared-ns-prefixes
                     ; As if element namespace already declared
                     (if elem-decl-required?
                         (alist-update elem-prefix elem-uri  ;; [+]
                                       declared-ns-prefixes
                                       string=?)
                         declared-ns-prefixes)))
           (if
            (null? attrs)  ; attributes scanned
            (let ((elem-name (srl:qname->string elem-prefix elem-local)))
              (values
               (cons "<"
                     (cons elem-name
                           (if
                            elem-decl-required?
                            (cons
                             (srl:namespace-decl->str-lst elem-prefix elem-uri)
                             start-tag)
                            start-tag)))
               (if empty? #f
                   (list "</" elem-name ">"))
               ns-prefix-assig
               namespace-assoc
               declared-ns-prefixes))
            (call-with-values
             (lambda ()
               (srl:name->qname-components
                (caar attrs)  ; attribute name
                ns-prefix-assig namespace-assoc declared-ns-prefixes))
             (lambda (attr-prefix attr-uri attr-local attr-decl-required?)
               (let ((start-tag
                      (cons
                       (srl:attribute->str-lst
                        attr-prefix attr-local
                        ; TODO: optimize for HTML output method
                        (if (null? (cdar attrs))  ; no attribute value
                            attr-local
                            (cadar attrs))
                        method)
                       start-tag)))
                 (loop
                  (cdr attrs)
                  (if attr-decl-required?
                      (cons (srl:namespace-decl->str-lst attr-prefix attr-uri)
                            start-tag)
                      start-tag)
                  ns-prefix-assig
                  namespace-assoc
                  (if attr-decl-required?                      
                      (cons (cons attr-prefix attr-uri) declared-ns-prefixes)
                      declared-ns-prefixes))))))))))))

;; Changes: check (allow-prefix-redeclarations) parameter before denying XML prefix
;; redeclarations.  Requires declared-ns-prefixes to contain unique keys (prefixes).
;; Also have empty namespace signal a declaration of "" is required if a non-empty
;; *default* namespace is defined.  Empty namespace declaration is considered
;; to be ("*default*" . "") so it overwrites any previous default declaration.
(define (srl:name->qname-components
         name ns-prefix-assig namespace-assoc declared-ns-prefixes)
  (let ((use-ns-id-or-generate-prefix
         (lambda (ns-id)
           (if
            (and ns-id  ; try to use namespace-id as a prefix
                 (not (assq (string->symbol ns-id) ns-prefix-assig))
                 (not (assoc ns-id declared-ns-prefixes))   ;; FIXME: maybe remove
                 )
            ns-id
            ; Otherwise - generate unique prefix
            ; Returns a prefix-string not presented in ns-prefix-assig and
            ; declared-ns-prefixes
            (let loop ((i 1))
              (let ((candidate (string-append "prfx" (number->string i))))
                (if (or (assoc candidate declared-ns-prefixes)
                        (assq (string->symbol candidate) ns-prefix-assig))
                    (loop (+ i 1))
                    candidate))))))
        (n-parts (srl:split-name name)))
    (cond
      ((not (car n-parts))  ; no namespace-id => no namespace
       (values "*default*" "" (cdr n-parts)  ; name as a string
               ;; declaration of empty namespace required if default currently non-empty
               (let ((def (assoc "*default*" declared-ns-prefixes)))
                 (and def (not (string=? "" (cdr def))))))
       ;; (values #f #f (cdr n-parts)  ; name as a string
       ;;         #f)
       )
      ((string-ci=? (car n-parts) "xml")  ; reserved XML namespace
       (values (car n-parts) "http://www.w3.org/XML/1998/namespace"
               (cdr n-parts) #f))
      (else
       (call-with-values
        (lambda ()
          (cond
            ((assq (string->symbol (car n-parts))  ; suppose a namespace-id
                   namespace-assoc)
             => (lambda (lst)
                  (values (cadr lst) (car n-parts))))
            (else  ; first part of a name is a namespace URI
             (values (car n-parts) #f))))
        (lambda (namespace-uri ns-id)
          (cond
            ((srl:assoc-cdr-string= namespace-uri declared-ns-prefixes)
             => (lambda (pair)
                  ; Prefix for that namespace URI already declared
                  (values (car pair) namespace-uri (cdr n-parts) #f)))
            (else  ; namespace undeclared
             (values
              (cond
                ((srl:assoc-cdr-string= namespace-uri ns-prefix-assig)
                 => (lambda (pair)
                      ; A candidate namespace prefix is supplied from the user
                      (let ((candidate (symbol->string (car pair))))
                        (if (and (not (allow-prefix-redeclarations?))
                                 (assoc candidate declared-ns-prefixes))
                            ;; The prefix already bound to a different namespace
                            ;; Avoid XML prefix re-declaration
                            (use-ns-id-or-generate-prefix ns-id)
                            candidate))))
                (else
                 (use-ns-id-or-generate-prefix ns-id)))
              namespace-uri
              (cdr n-parts)
              #t  ; in any case, prefix declaration is required
              )))))))))

)





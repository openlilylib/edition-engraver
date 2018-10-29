; -*- master: usage-examples/example-1.ly;
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%                                                                             %
;% This file is part of openLilyLib,                                           %
;%                      ===========                                            %
;% the community library project for GNU LilyPond                              %
;% (https://github.com/openlilylib)                                            %
;%              -----------                                                    %
;%                                                                             %
;% Library: edition-engraver                                                   %
;%          ================                                                   %
;%                                                                             %
;% openLilyLib is free software: you can redistribute it and/or modify         %
;% it under the terms of the GNU General Public License as published by        %
;% the Free Software Foundation, either version 3 of the License, or           %
;% (at your option) any later version.                                         %
;%                                                                             %
;% openLilyLib is distributed in the hope that it will be useful,              %
;% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
;% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
;% GNU General Public License for more details.                                %
;%                                                                             %
;% You should have received a copy of the GNU General Public License           %
;% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
;%                                                                             %
;% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
;% edition-engraver is maintained by Jan-Peter Voigt, jp.voigt@gmx.de          %
;% and others.                                                                 %
;%       Copyright Jan-Peter Voigt, Urs Liska, 2016                            %
;%                                                                             %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define-module (edition-engraver engine))

; use tree structure - but not alist-based!
(use-modules
 (lily)
 (oop goops)
 (srfi srfi-1)
 (oll-core tree)
 )

(ly:message "initializing edition-engraver ...")

; Here are some uses of '@@' which should be avoided!

; 1. If the EE will be integrated into LilyPond proper this will be not necessary anymore

; add context properties descriptions (private lambda in module 'lily')
((@@ (lily) translator-property-description) 'edition-id list? "edition id (list)")
((@@ (lily) translator-property-description) 'edition-anchor symbol? "edition-mod anchor for relative timing (symbol)")
((@@ (lily) translator-property-description) 'edition-engraver-log boolean? "de/activate logging (boolean)")

; callback for oll-core getOption ...
(define oll:getOption #f)
(define-public setOLLCallback #f)
(let ((callback #f))
  (set! setOLLCallback (define-void-function (cb)(procedure?) (set! callback cb)))
  (set! oll:getOption (lambda (path) (if (procedure? callback) (callback path) #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. The dynamic-tree will be integrated into (oll-core tree) to avoid this discouraged use of '@@'
;;; START dynamic-tree

(define <tree> (@@ (oll-core tree) <tree>))
(define children (@@ (oll-core tree) children))
(define get-key (@@ (oll-core tree) key))
(define value (@@ (oll-core tree) value))
(define has-value (@@ (oll-core tree) has-value))
(define has-value! (@@ (oll-core tree) has-value!))
(define set-value! (@@ (oll-core tree) set-value!))
(define type (@@ (oll-core tree) type))
(define stdsort (@@ (oll-core tree) stdsort))

; dynamic-tree class to allow for wildcard paths
(define-class <dynamic-tree> (<tree>))

; create dynamic-tree
(define-public (tree-create-dynamic . key)
  (let ((k (if (> (length key) 0)(car key) 'node)))
    (make <dynamic-tree> #:key k)
    ))

; adapted version of tree-set! to take care of procedures inside the path
(define-method (tree-set! (create <boolean>) (tree <dynamic-tree>) (path <list>) val)
  (if (= (length path) 0)
      ;; end of path reached: set value
      (let ((pred? (type tree)))
        (if pred?
            ;; if tree has a type defined check value against it before setting
            (if (pred? val)
                (begin
                 (set-value! tree val)
                 (has-value! tree #t))
                (begin
                 (ly:input-warning (*location*)
                   (format "TODO: Format warning about typecheck error in tree-set!
Expected ~a, got ~a" (procedure-name pred?) val))
                 (set! val #f)))
            ;; if no typecheck is set simply set the value
            (begin
             (set-value! tree val)
             (has-value! tree #t)
             )))
      ;; determine child
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey)))
        (if (not (tree? child))
            ;; create child node if option is set
            (if create
                (begin
                 (set! child (make <dynamic-tree> #:key ckey))
                 (hash-set! (children tree) ckey child))))
        (if (tree? child)
            ;; recursively walk path
            (tree-set! create child cpath val)
            (ly:input-warning (*location*)
              (format "TODO: Format missing path warning in tree-set!
Path: ~a" path)))))
  val)

; adapted version of tree-get-tree to take care of procedures inside path and/or tree
(define-method (tree-get-tree (tree <dynamic-tree>) (path <list>))
  (if (= (length path) 0)
      tree
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey)))
        (if (is-a? child <tree>)
            (tree-get-tree child cpath)
            (let* ((childs (hash-map->list cons (children tree) ))
                   (childs (map (lambda (p) (cons (car p) (cdr p))) childs))
                   (match #f))
              ;(ly:message "~A" childs)
              (for-each
               (lambda (pat)
                 (if (and (eq? #f match) (procedure? (car pat)) ((car pat) ckey))
                     (set! match (cons ckey (tree-get-tree (cdr pat) cpath)))
                     )) childs)
              (if (pair? match) (cdr match) #f)
              ))
        )))

; adapted version of tree-get-tree to take care of procedures inside path or tree
(define-method (display (tree <dynamic-tree>) port)
  (let ((tkey (get-key tree)))
    (tree-display tree
      `(port . ,port)
      `(pformat . ,(lambda (v)
                     (cond
                      ((procedure? v)
                       (let ((pn (procedure-name v))
                             (label (object-property v 'path-label)))
                         (if label label (format "<~A>" pn))))
                      (else (format "~A" v))
                      )))
      )))

; get all children with procedure inside path
; TODO we won't fetch all trees, if we mix plain paths with wildcards/regexs?
(define-method (tree-get-all-trees (tree <tree>) (path <list>))
   (if (= (length path) 0)
       (list tree)
       (let* ((ckey (car path))
              (cpath (cdr path))
              (childs (hash-map->list cons (children tree) ))
              (child (hash-ref (children tree) ckey)))

         (set! childs (map (lambda (p) (cons (car p) (cdr p))) childs))
         (set! childs
               (cond
                ((procedure? ckey)
                 (filter (lambda (child) (ckey (car child))) childs))
                ((is-a? child <tree>)
                 (list (cons ckey child)))
                (else
                 (map
                  (lambda (child)
                    (cons ckey (cdr child)))
                  (filter
                   (lambda (p)
                     (let* ((tree (cdr p))
                            (tkey (get-key tree)))
                       (and (procedure? tkey) (tkey ckey))
                       )) childs))
                 )))
         (concatenate
          (map
           (lambda (child)
             (tree-get-all-trees (cdr child) (cdr path)))
           childs))
         )))
; get all children with procedure inside path
(define-method (tree-get-all (tree <tree>) (path <list>))
  (map value (tree-get-all-trees tree path)))

; walk the tree and call callback for every node
(define-method (tree-walk (tree <dynamic-tree>) (path <list>) (callback <procedure>) . opts)
  (let ((dosort (assoc-get 'sort opts #f))
        (sortby (assoc-get 'sortby opts stdsort))
        (doempty (assoc-get 'empty opts #f)))
    (if (or doempty (has-value tree))
        (callback path (get-key tree) (value tree)))
    (for-each
     (lambda (p)
       (tree-walk (cdr p) `(,@path ,(car p)) callback `(sort . ,dosort) `(sortby . ,sortby) `(empty . ,doempty)))
     (if dosort
         (sort (hash-table->alist (children tree)) sortby)
         (hash-table->alist (children tree)) ))
    ))

; walk the tree and call callback for every node in sub-tree at path
(define-method (tree-walk-branch (tree <dynamic-tree>) (path <list>) (callback <procedure>) . opts)
  (let ((dosort (assoc-get 'sort opts))
        (sortby (assoc-get 'sortby opts stdsort))
        (doempty (assoc-get 'empty opts))
        (ctrees (tree-get-all-trees tree path)))
    (for-each
     (lambda (ctree)
       (tree-walk ctree path callback `(sort . ,dosort) `(sortby . ,sortby) `(empty . ,doempty)))
     ctrees)
    ))

;;; END dynamic-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public editionID
  (define-scheme-function (inherit path)((boolean? #t) list?)
    (if inherit
        #{ \with { edition-id = #`(,inherit-edition-id ,@path) } #}
        #{ \with { edition-id = $path } #}
        )))


; TODO: this maybe also a candidate for another module (IIRC there has been some kind of rhythmic location in lily ...)
; => this is https://github.com/openlilylib/snippets/blob/master/ly/utility/rhythmic-location.ily
; and we should think about where to move that in the new structure anyway, then we'll see whether
; it makes sense to move and merge the following code as well.

; a predicate for short input of ly:moment?s
(define (short-mom? v)(or (integer? v)(fraction? v)(rational? v)(ly:moment? v)))
; convert to a moment
(define (short-mom->moment m)
  (cond
   ((number? m)(ly:make-moment (inexact->exact m)))
   ((fraction? m)(ly:make-moment (/ (car m) (cdr m))))
   ((ly:moment? m) m)
   (else (ly:make-moment 0/4))))
; predicate for a pair of measure and short-mom
(define (mom-pair? v)
  (and (or (pair? v)
           (and (list? v)(= 2 (length v))))
       (integer? (car v))
       (short-mom? (cadr v))))

; a predicate for short input of lists of ly:moment-pairs (measure+moment)
(define (imom-list? v)
  (and (list? v)
       (every (lambda (p)
                (or (integer? p)
                    (mom-pair? p)))
         v)))
; convert to a list of measure/moment pairs
(define (imom-list v)
  (map (lambda (m)
         (cond
          ((integer? m)(cons m (ly:make-moment 0 0)))
          ((mom-pair? m)(cons (car m)(short-mom->moment (cadr m))))
          (else (cons 0 (ly:make-moment 0 4)))))
    v))



;%%%%%%%%%%%%% mod-classes

; we need to store some mods as arbitrary objects, to allow once

;;; property set as a class
(define-class <propset> ()
  (once #:init-value #t #:accessor is-once #:setter set-once! #:init-keyword #:once)
  (symbol #:accessor get-symbol #:setter set-symbol! #:init-keyword #:symbol)
  (value #:accessor get-value #:setter set-value! #:init-keyword #:value)
  (previous #:accessor get-previous #:setter set-previous! #:init-value #f)
  (context #:accessor get-context #:setter set-context! #:init-keyword #:context)
  )

; execute property set
(define-method (do-propset context (prop <propset>))
  (if (get-context prop)
      (let ((parent-context (ly:context-find context (get-context prop))))
        (if (ly:context? parent-context) (set! context parent-context))))
  (set-previous! prop (ly:context-property context (get-symbol prop)))
  (ly:context-set-property! context (get-symbol prop) (get-value prop))
  )
(export do-propset)

; execute property reset
(define-method (reset-prop context (prop <propset>))
  (if (get-context prop)
      (let ((parent-context (ly:context-find context (get-context prop))))
        (if (ly:context? parent-context) (set! context parent-context))))
  (ly:context-set-property! context (get-symbol prop) (get-previous prop))
  )
(export reset-prop)

; propset predicate
(define-public (propset? p)(is-a? p <propset>))
; propset -> string
(define-method (propset->string (ps <propset>))
  (format "~A\\set ~A = ~A" (if (is-once ps) "\\once " "") (string-append (if (get-context ps) (format "~A." (get-context ps)) "") (format "~A" (get-symbol ps))) (get-value ps)))
(export propset->string)
; display propset
(define-method (display (o <propset>) port) (display (propset->string o) port))



;;; property set as a class
(define-class <propunset> ()
  (once #:init-value #t #:accessor is-once #:setter set-once! #:init-keyword #:once)
  (symbol #:accessor get-symbol #:setter set-symbol! #:init-keyword #:symbol)
  (previous #:accessor get-previous #:setter set-previous! #:init-value #f)
  (context #:accessor get-context #:setter set-context! #:init-keyword #:context)
  )

; execute property set
(define-method (do-propunset context (prop <propunset>))
  (if (get-context prop)
      (let ((parent-context (ly:context-find context (get-context prop))))
        (if (ly:context? parent-context) (set! context parent-context))))
  (set-previous! prop (ly:context-property context (get-symbol prop)))
  ;(ly:message "~A" prop)
  (ly:context-unset-property context (get-symbol prop))
  )
(export do-propunset)

; execute property reset
(define-method (reunset-prop context (prop <propunset>))
  (if (get-context prop)
      (let ((parent-context (ly:context-find context (get-context prop))))
        (if (ly:context? parent-context) (set! context parent-context))))
  (ly:context-set-property! context (get-symbol prop) (get-previous prop))
  )
(export reunset-prop)

; propset predicate
(define-public (propunset? p)(is-a? p <propunset>))
; propset -> string
(define-method (propunset->string (ps <propunset>))
  (format "~A\\unset ~A" (if (is-once ps) "\\once " "")
    (string-append (if (get-context ps) (format "~A." (get-context ps)) "") (format "~A" (get-symbol ps)))))
(export propunset->string)
; display propset
(define-method (display (o <propunset>) port) (display (propunset->string o) port))



;;; apply-context as a class
(define-class <apply-context> ()
  (proc #:accessor procedure #:setter set-procedure! #:init-keyword #:proc)
  )

; execute apply-context!
(define-method (do-apply context (a <apply-context>))
  ((procedure a) context))
(export do-apply)
; apply-context as a class
(define-public (apply-context? a)(is-a? a <apply-context>))



;;; override as a class
; TODO: temporary override?
; TODO: is this needed?
(define-class <override> ()
  (once #:init-value #t #:accessor is-once #:setter set-once! #:init-keyword #:once)
  (revert #:init-value #f #:accessor is-revert #:setter set-revert! #:init-keyword #:revert)
  (grob #:accessor get-grob #:setter set-grob! #:init-keyword #:grob)
  (prop #:accessor get-prop #:setter set-prop! #:init-keyword #:prop)
  (value #:accessor get-value #:setter set-value! #:init-keyword #:value)
  (context #:accessor get-context #:setter set-context! #:init-keyword #:context)
  )

; override -> string
(define-method (oop->string (o <override>))
  (let* ((context-name (get-context o))
         (context-sname (if context-name (format "~A." context-name) "")))
    (if (is-revert o)
        (string-append "\\revert " context-sname (format "~A " (get-grob o)) (format "#'~A" (get-prop o)))
        (string-append (if (is-once o) "\\once " "") "\\override " context-sname (format "~A " (get-grob o)) (format "#'~A" (get-prop o)) " = " (format "~A" (get-value o)))
        )))
(export oop->string)
; display override
(define-method (display (o <override>) port) (display (oop->string o) port))
; override predicate
(define-public (override? o)(is-a? o <override>))

; execute override
(define-method (do-override context (mod <override>))
  (if (get-context mod)
      (let ((parent-context (ly:context-find context (get-context mod))))
        (if (ly:context? parent-context) (set! context parent-context))))
  (ly:context-pushpop-property context (get-grob mod) (get-prop mod) (get-value mod)))
(export do-override)
; revert override
(define-method (do-revert context (mod <override>))
  (if (get-context mod)
      (let ((parent-context (ly:context-find context (get-context mod))))
        (if (ly:context? parent-context) (set! context parent-context))))
  (ly:context-pushpop-property context (get-grob mod) (get-prop mod)))
(export do-revert)

;%%%%%%%%%%%%%%%%%%%%%%%%%


; store active edition-targets
(define edition-targets '())
; store mods in a tree - to be accessed by path
(define mod-tree (tree-create-dynamic 'edition-mods))

; add/activate edition-target
(define-public (add-edition edition-target)
  (if (not (memq edition-target edition-targets))
      (set! edition-targets `(,@edition-targets ,edition-target))))
(define-public addEdition
  (define-void-function (edition-target)(symbol?)
    (add-edition edition-target)))

; remove/deactivate edition-target
(define-public (remove-edition edition-target)
  (set! edition-targets (filter (lambda (t) (equal? t edition-target)) edition-targets)))
(define-public removeEdition
  (define-void-function (edition-target)(symbol?)
    (remove-edition edition-target)))

; set list of edition-targets (copy list)
(define-public (set-edition-list edition-list) (set! edition-targets `(,@edition-list)))
(define-public setEditions
  (define-void-function (edition-list)(symbol-list?)
    (set-edition-list edition-list)))
; get list of edition-targets (copy list)
(define-public (get-edition-list) `(,@edition-targets))
(define-public getEditions
  (define-scheme-function ()()
    (get-edition-list)))

(define (for-some-music-with-elements-callback stop? music)
  "Like @var{for-some-music}, but also processes @var{elements-callback},
  which is used by TimeSignatureMusic, SequentialMusic, and a few others"
  (let loop ((music music))
    (if (not (stop? music))
        (let ((callback (ly:music-property music 'elements-callback)))
          (if (procedure? callback)
              (for-each loop (callback music))
              (begin
               (let ((elt (ly:music-property music 'element)))
                 (if (ly:music? elt)
                     (loop elt)))
               (for-each loop (ly:music-property music 'elements))
               (for-each loop (ly:music-property music 'articulations))
               ))))))

; collect mods, accepted by the engraver, from a music expression
; TODO should mods be separated by engraver-slot? (e.g. start-timestep - process-music - acknowledger - listener)
(define (collect-mods music context)
  (let ((collected-mods '()))
    (for-some-music-with-elements-callback
     (lambda (m)
       (let ((music-name (ly:music-property m 'name)))
         ;(if (ly:duration? (ly:music-property m 'duration))
         ;    (ly:music-warning music "Music unsuitable for edition mod"))
         (cond
          ; specified context like in \set Timing.whichBar = "||"
          ((eq? 'ContextSpeccedMusic music-name)
           (let* ((ct (ly:music-property m 'context-type))
                  (elm (ly:music-property m 'element)))
             (if (eq? 'Bottom ct)
                 #f
                 (begin
                  (set! collected-mods (append collected-mods (collect-mods elm ct)))
                  #t)
                 )
             ))

          ; \override Grob.property =
          ((eq? 'OverrideProperty music-name)
           (let* ((once (ly:music-property m 'once #f))
                  (grob (ly:music-property m 'symbol))
                  (prop (ly:music-property m 'grob-property))
                  (prop (if (symbol? prop)
                            prop
                            (car (ly:music-property m 'grob-property-path))))
                  (value (ly:music-property m 'grob-value))
                  (mod (make <override> #:once once #:grob grob #:prop prop #:value value #:context context)))
             ; (ly:message "mod ~A" mod)
             (set! collected-mods `(,@collected-mods ,mod)) ; alternative (cons mod collected-mods)
             #t
             ))
          ; \revert ...
          ((eq? 'RevertProperty music-name)
           (let* ((grob (ly:music-property m 'symbol))
                  (prop (ly:music-property m 'grob-property))
                  (prop (if (symbol? prop)
                            prop
                            (car (ly:music-property m 'grob-property-path))))
                  (mod (make <override> #:once #f #:revert #t #:grob grob #:prop prop #:value #f #:context context)))
             (set! collected-mods `(,@collected-mods ,mod))
             #t
             ))
          ; \set property = ...
          ((eq? 'PropertySet music-name)
           (let* ((once (ly:music-property m 'once #f))
                  (symbol (ly:music-property m 'symbol))
                  (value (ly:music-property m 'value))
                  (mod (make <propset> #:once once #:symbol symbol #:value value #:context context)))
             (set! collected-mods `(,@collected-mods ,mod))
             #t
             ))

          ; \unset property = ...
          ((eq? 'PropertyUnset music-name)
           (let* ((once (ly:music-property m 'once #f))
                  (symbol (ly:music-property m 'symbol))
                  (mod (make <propunset> #:once once #:symbol symbol #:context context)))
             (set! collected-mods `(,@collected-mods ,mod))
             #t
             ))

          ; \applyContext ...
          ((eq? 'ApplyContext music-name)
           (let* ((proc (ly:music-property m 'procedure))
                  (mod (make <apply-context> #:proc proc)))
             (set! collected-mods `(,@collected-mods ,mod))
             #t
             ))

          ; TimeSignature
          ((memq music-name '(TimeSignatureMusic))
           ;(set! collected-mods `(,@collected-mods ,m))
           (let ((callback (ly:music-property m 'elements-callback)))
             (if (procedure? callback)
                 (for-each (lambda (m) (collect-mods m context)) (callback m)))
             #f))

          ; any other
          ((memq music-name
             (filter
              (lambda (e)
                (not (memq e '(SequentialMusic SimultaneousMusic EventChord))))
              (map car music-descriptions)))
           (set! collected-mods `(,@collected-mods ,m))
           #t)

          (else #f) ; go ahead ...
          ))) music)
    collected-mods))

(define (create-mod-path edition-target measure moment context-edition-id)
  `(,@context-edition-id ,measure ,moment ,edition-target))

; add modification(s)
(define-public (edition-mod edition-target measure moment context-edition-id mods)
  (cond
   ((ly:context-mod? mods) (set! mods (list mods))) ; apply context-mod
   ((ly:music? mods) (set! mods (collect-mods mods #f))) ; collect mods from music expression
   )
  (let* ((mod-path (create-mod-path edition-target measure moment context-edition-id))
         (tmods (tree-get mod-tree mod-path))
         (tmods (if (list? tmods) tmods '())))
    (define (wildcard2regex in)
      (let ((regex-string
             (list->string
              `(#\^
                ,@(apply append
                    (map
                     (lambda (c)
                       (cond
                        ((eq? c #\?) (list #\.))
                        ((eq? c #\*) (list #\. #\*))
                        (else (list c))
                        )) in))
                #\$))))
        (regex-match regex-string)
        ))
    (define (regex-match in)
      (let ((regex (make-regexp in regexp/icase)))
        (lambda (s)
          ;(ly:message "/~A/: ~A" in s)
          (regexp-exec regex
            (cond
             ((string? s) s)
             ((symbol? s) (symbol->string s))
             (else (format "~A" s))
             )))
        ))
    ; fetch procedures from path
    ; TODO build procedures from wildcard string
    (define (explode-mod-path mod-path)
      (if (symbol? mod-path)
          (let* ((mod-string (symbol->string mod-path))
                 (mod-cl (string->list mod-string)))
            (cond
             ((and
               (eq? #\{ (first mod-cl))
               (eq? #\} (last mod-cl))
               ) (wildcard2regex (list-tail (list-head mod-cl (1- (length mod-cl))) 1) ))
             ((and
               (eq? #\/ (first mod-cl))
               (eq? #\/ (last mod-cl))
               ) (regex-match (substring mod-string 1 (1- (string-length mod-string)))))
             ((and
               (eq? #\< (first mod-cl))
               (eq? #\> (last mod-cl)))
              (let* ((proc-name (string->symbol (substring mod-string 1 (1- (string-length mod-string)))))
                     (proc (ly:parser-lookup proc-name)))
                (if (procedure? proc) proc mod-path)))
             (else mod-path)))
          mod-path
          ))
    ; (ly:message "mods ~A" mods)
    (tree-set! mod-tree (map explode-mod-path mod-path) (append tmods mods))
    ))
; predicate for music or context-mod
(define-public (music-or-context-mod? v) (or (ly:music? v)(ly:context-mod? v)))
(define-public editionMod
  (define-void-function
   (edition-target measure moment context-edition-id mods)
   (symbol? integer? short-mom? list? music-or-context-mod?)
   (edition-mod edition-target measure (short-mom->moment moment) context-edition-id mods)))

; add modification(s) on multiple times
(define-public (edition-mod-list edition-target context-edition-id mods mom-list)
  (for-each
   (lambda (mom) (edition-mod edition-target (car mom) (cdr mom) context-edition-id mods))
   (imom-list mom-list)))
(define-public editionModList
  (define-void-function
   (edition-target context-edition-id mods mom-list)
   (symbol? list? ly:music? imom-list?)
   (edition-mod-list edition-target context-edition-id mods mom-list)))

; TODO development1.ly Start/Stop/Add-ModList

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The edition-engraver

(define-public inherit-edition-id 'ICEID) ; Inherit Context-Edition-ID

(define context-counter (tree-create 'context-counter))

;;;; TODO where to put this???
(define (base26list nr suf)
  (if (>= nr 26)
      (append (base26list (quotient nr 26) suf) (list (remainder nr 26)))
      `(,nr ,@suf)))
(define (base26 nr)
  (let ((A (char->integer #\A)))
    (list->string (map (lambda (i) (integer->char (+ i A))) (base26list nr '())))
    ))
;;;;

; the edition-engraver
(define-public (edition-engraver context) ; TODO better use make-engraver macro?
  (let ( (context-edition-id '()) ; it receives the context-edition-id from a context-property while initializing
         (context-edition-number 0)
         (context-name (ly:context-name context)) ; the context name (Voice, Staff or else)
         (context-id
          (let ((cid (ly:context-id context))) ; the context-id assigned by \new Context = "the-id" ...
            (if (> (string-length cid) 0)
                (string->symbol cid)
                #f)))
         (context-mods #f)
         (once-mods '())
         (start-translation-timestep-moment #f)
         (track-mod-move #f)
         )

    ; log slot calls
    (define (log-slot slot) ; TODO: option verbose? oll logging function?
      (if (and (eq? (ly:context-property-where-defined context 'edition-engraver-log) context)
               (eq? #t (ly:context-property context 'edition-engraver-log #f)))
          (ly:message "edition-engraver ~A ~A = \"~A\" : ~A @ ~A (~A@~A)"
            context-edition-id
            context-name
            (if (symbol? context-id) (symbol->string context-id) "")
            slot
            (ly:context-current-moment context)
            (ly:context-property context 'currentBarNumber)
            (ly:context-property context 'measurePosition)
            )))

    ; find mods for the current time-spec
    (define (find-mods)
      (log-slot "find-mods")
      (let* ((moment (ly:context-current-moment context))
             (timing (ly:context-find context 'Timing))
             (measure (ly:context-property timing 'currentBarNumber))
             (measurePos (ly:context-property timing 'measurePosition))
             (current-mods (tree-get context-mods (list measure measurePos))))

        (if (list? current-mods) current-mods '())

        ))
    (define (propagate-mods)
      (log-slot "propagate-mods")
      (let* ((moment (ly:context-current-moment context))
             (timing (ly:context-find context 'Timing))
             (measure (ly:context-property timing 'currentBarNumber))
             (measurePos (ly:context-property timing 'measurePosition))
             (measure-length (ly:context-property timing 'measureLength))
             (positions (tree-get-keys context-mods (list measure))))
        ; propagate mods into the next measure, if the moment exceeds measure-length
        ; TODO this works as long there are no cadenza parts! (look for Timing.timing = #f)
        (if (and (list? positions) ; do we have any mods in this measure?
                 (or (not (integer? track-mod-move)) ; do it once per measure
                     (not (= track-mod-move measure))
                     ))
            (begin
             (for-each
              (lambda (pos)
                (let ((omeasure (1+ measure))
                      (opos (ly:moment-sub pos measure-length))
                      (omods (tree-get-tree context-mods (list measure pos))))
                  (tree-walk omods (list measure pos)
                    (lambda (path nkey value)
                      (let* ((ospot (list omeasure opos))
                             (omods (tree-get context-mods ospot)))
                        ; TODO format message
                        (ly:message "~A (~A): ~A ---> ~A" context-edition-id context-name path ospot)
                        (if (not (list? omods)) (set! omods '()))
                        (tree-set! context-mods ospot (append omods value))
                        (tree-unset! context-mods path)
                        )))
                  ))
              (filter
               (lambda (pos)
                 (and (ly:moment? pos)
                      (or (equal? pos measure-length)
                          (ly:moment<? measure-length pos))))
               positions)
              ))
            (set! track-mod-move measure))
        ))

    (define (broadcast-music mod clsevent)
      (ly:broadcast (ly:context-event-source context)
        (ly:make-stream-event
         (ly:make-event-class clsevent)
         (ly:music-mutable-properties mod))
        ))
    ; define start-translation-timestep to use it in initialize if needed
    (define (start-translation-timestep trans)
      (log-slot "start-translation-timestep")
      (if (or (not start-translation-timestep-moment)
              (ly:moment<? start-translation-timestep-moment (ly:context-now context)))
          (for-each
           (lambda (mod)
             (let ((mod-name (if (ly:music? mod) (ly:music-property mod 'name))))
               (cond
                ((override? mod)
                 (if (is-revert mod)
                     (do-revert context mod)
                     (do-override context mod))
                 ; if it is once, add to once-list
                 (if (is-once mod) (set! once-mods (cons mod once-mods)))
                 )
                ((propset? mod)
                 (do-propset context mod)
                 (if (is-once mod) (set! once-mods (cons mod once-mods)))
                 )
                ((propunset? mod)
                 (do-propunset context mod)
                 (if (is-once mod) (set! once-mods (cons mod once-mods)))
                 )

                ((apply-context? mod) (do-apply context mod))
                ((and (ly:music? mod)(eq? 'CrescendoEvent mod-name))
                 (broadcast-music mod 'crescendo-event))
                ((and (ly:music? mod)(eq? 'DecrescendoEvent mod-name))
                 (broadcast-music mod 'decrescendo-event))

                ((and (ly:music? mod)
                      (not (memq mod-name '(TextScriptEvent)))
                      (memq mod-name (map car music-descriptions)))
                 (ly:message "trying ~A" (with-output-to-string (lambda () #{ \displayLilyMusic #mod #})))
                 (ly:broadcast (ly:context-event-source context)
                   (ly:make-stream-event
                    (ly:assoc-get 'types (ly:assoc-get mod-name music-descriptions '()) '())
                    (ly:music-mutable-properties mod)))
                 )

                ((ly:music? mod) (ly:context-mod-apply! context (context-mod-from-music mod)))
                ))) (find-mods)))
      (set! start-translation-timestep-moment #f)
      )


    ;(ly:message "~A ~A" (ly:context-id context) context-id)
    `( ; TODO slots: listeners, acknowledgers, end-acknowledgers, process-acknowledged

       (must-be-last . #t)
       ; initialize engraver with its own id
       (initialize .
         ,(lambda (trans)
            (define (find-edition-id context)
              (if (ly:context? context)
                  (let ((edition-id (ly:context-property context 'edition-id #f))
                        (id-source (ly:context-property-where-defined context 'edition-id))
                        (parent-context (ly:context-parent context)))
                    (if (and (list? edition-id)(> (length edition-id) 0)) ; we have an edition-id
                        (if (eq? (car edition-id) inherit-edition-id) ; inherit parent id? (wildcard found)
                            (if (eq? context id-source) ; don't replace wildcard if it comes from parent context
                                (let* ((parent-edition-id (find-edition-id parent-context))
                                       (edition-id (if (> (length edition-id) 1)
                                                       (append parent-edition-id (cdr edition-id))
                                                       parent-edition-id))) ; replace inherit-token with parent-id
                                  (ly:context-set-property! context 'edition-id edition-id)
                                  edition-id
                                  )
                                (find-edition-id parent-context)
                                )
                            edition-id) ; no inherit
                        (find-edition-id (ly:context-parent context)))) ; no edition-id
                  '())) ; if context

            (set! context-edition-id (find-edition-id context))
            (set! context-edition-number
                  (let ((nr (tree-get context-counter `(,@context-edition-id ,context-name))))
                    (if (and (pair? nr)(integer? (car nr))) (+ (car nr) 1) 0)
                    ))
            (tree-set! context-counter
              `(,@context-edition-id ,context-name)
              (cons context-edition-number context-id))
            (tree-set! context-counter
              `(,@context-edition-id ,context-name
                 ,(string->symbol (base26 context-edition-number))) ; we need a symbol for the path
              (if context-id (symbol->string context-id) "")) ; we need a string here

            ; copy all mods into this engravers mod-tree
            (set! context-mods
                  (tree-create (string->symbol
                                (string-join
                                 (map
                                  (lambda (s)
                                    (format "~A" s))
                                  context-edition-id) ":"))))
            ;(ly:message "init ~A \"~A\"" context-edition-id (ly:context-id context))
            (for-each
             (lambda (context-edition-sid)
               ;(ly:message "~A" context-edition-sid)
; TODO we won't fetch all trees, if we mix plain paths with wildcards/regexs?
               (let ((mtrees (tree-get-all-trees mod-tree context-edition-sid)))
                 (for-each
                  (lambda (mtree)
                    (tree-walk mtree '()
                      (lambda (path k val)
                        (let ((plen (length path)))
                          (if (and (= plen 3)(list? val)
                                   (integer? (list-ref path 0))
                                   (member (list-ref path 2) edition-targets))
                              (let* ((subpath (list (list-ref path 0)(list-ref path 1)))
                                     (submods (tree-get context-mods subpath)))
                                (tree-set! context-mods subpath
                                  (if (list? submods) (append submods val) val))
                                ))))
                      )) mtrees)))
             `((,@context-edition-id ,context-name)
               ,@(if context-id `(
                                   (,@context-edition-id ,context-id)
                                   (,@context-edition-id ,context-name ,context-id)
                                   ) '())
               (,@context-edition-id ,context-name ,(string->symbol (base26 context-edition-number)))
               ))

            (log-slot "initialize")
            ; if the now-moment is greater than 0, this is an instantly created context,
            ; so we need to call start-translation-timestep here.
            (let ((now (ly:context-now context))
                  (partial (ly:context-property context 'measurePosition)))
              (if (or
                   ; start-translation-timestep is not called for instant Voices
                   (ly:moment<? (ly:make-moment 0/4) now)
                   ; start-translation-timestep is not called on upbeats!
                   (and (ly:moment? partial)(< (ly:moment-main partial) 0)))
                  (begin
                   (log-slot "initialize->start-translation-timestep")
                   (start-translation-timestep trans)
                   ))
              (set! start-translation-timestep-moment now))
            ))

       ; paper columns --> breaks
       (acknowledgers ; TODO add acknowledgers from mods
         (paper-column-interface .
           ,(lambda (engraver grob source-engraver)
              (if (eq? #t (ly:grob-property grob 'non-musical))
                  (for-each
                   (lambda (mod)
                     (cond
                      ((and (ly:music? mod) (eq? 'LineBreakEvent (ly:music-property mod 'name)))
                       (set! (ly:grob-property grob 'line-break-permission) (ly:music-property mod 'break-permission)))
                      ((and (ly:music? mod) (eq? 'PageBreakEvent (ly:music-property mod 'name)))
                       (set! (ly:grob-property grob 'page-break-permission) (ly:music-property mod 'break-permission)))
                      ((and (ly:music? mod) (eq? 'PageTurnEvent (ly:music-property mod 'name)))
                       (set! (ly:grob-property grob 'page-turn-permission) (ly:music-property mod 'break-permission)))
                      ((and (ly:music? mod) (eq? 'ApplyOutputEvent (ly:music-property mod 'name)))
                       (let ((proc (ly:music-property mod 'procedure)))
                         (proc grob context context)
                         ))
                      )) (find-mods))
                  )))
         )
       ; start timestep
       (start-translation-timestep . ,start-translation-timestep)
       ; stop/finish translation timestep
       (stop-translation-timestep .
         ,(lambda (trans)
            (log-slot "stop-translation-timestep")
            ; we have to propagate measure-length exceeding mods here to correctly follow time sigs
            (propagate-mods)
            (for-each ; revert/reset once override/set
              (lambda (mod)
                (cond
                 ((propset? mod) (reset-prop context mod))
                 ((override? mod) (do-revert context mod))
                 ((ly:context-mod? mod) (ly:context-mod-apply! context mod))
                 ))
              once-mods)
            (set! once-mods '()) ; reset once-mods
            ))
       ; process music
       (process-music .
         ,(lambda (trans)
            (log-slot "process-music")
            (for-each ; revert/reset once override/set
              (lambda (mod)
                (let ((music-name (if (ly:music? mod) (ly:music-property mod 'name) #f)))
                  (cond
                   ((eq? 'TextScriptEvent music-name)
                    (let ((grob (ly:engraver-make-grob trans 'TextScript
                                  (ly:make-stream-event '(event)
                                    `((origin . ,(ly:music-property mod 'origin))
                                      (tweaks . ,(ly:music-property mod 'tweaks))
                                      (music-cause . mod)))))
                          (direction (ly:music-property mod 'direction #f))
                          (text (ly:music-property mod 'text #f)))
                      (ly:grob-set-property! grob 'text text)
                      (if direction (ly:grob-set-property! grob 'direction direction))
                      ))
                   )))
              (find-mods))
            ))
       ; finalize engraver
       (finalize .
         ,(lambda (trans)
            ;(log-slot "finalize")
            (if (eq? 'Score context-name)
                (let* ((timing (ly:context-find context 'Timing))
                       (current-moment (ly:context-current-moment context))
                       (current-measure (ly:context-property timing 'currentBarNumber))
                       (measure-position (ly:context-property timing 'measurePosition)))
                  (ly:message "finalize ~A with ~A @ ~A / ~A-~A"
                    context-edition-id edition-targets current-moment current-measure measure-position)
                  ; TODO filename, option-name
                  ; TODO format <file>.edition.log
                  (if (oll:getOption '(edition-engraver write-log))
                      (let ((filename (string-append (ly:parser-output-name (*parser*)) ".edition.log")))
                        (ly:message "write '~A' ..." filename)
                        (with-output-to-file
                         filename
                         (lambda ()
                           (tree-display context-counter)
                           (tree-walk context-counter '()
                             (lambda (p k val)
                               (if (string? val) (format #t "~A \"~A\"\n" p val))
                               ) '(sort . #t))
                           ))))
                  ))))

       ) ; /make-engraver
    ))


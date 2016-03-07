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
; TODO: tree.scm should be placed in another more generic module (oll-core?)
(use-modules
 (lily)
 (oop goops)
 (edition-engraver tree)
 )

(ly:message "initializing edition-engraver ...")

; TODO: "mom?" should be named more clearly
; TODO: "mom?" or "mom-pair?" ...
; TODO: this maybe also a candidate for another module (IIRC there has been some kind of rhythmic location in lily ...)

; a predicate for short input of ly:moment?s
(define (mom? v)(or (integer? v)(fraction? v)(ly:moment? v)))
; convert to a moment
(define (mom->moment m)
  (cond
   ((integer? m)(ly:make-moment m/4))
   ((fraction? m)(ly:make-moment (car m) (cdr m)))
   ((ly:moment? v) v)
   (else (ly:make-moment 0/4))))

; a predicate for short input of lists of ly:moment-pairs (measure+moment)
(define (imom-list? v)(and (list? v)
                           (every (lambda (p)
                                    (or (integer? p)
                                        (and (pair? p)
                                             (integer? (car p))
                                             (mom? (cdr p))))
                                    v))))
; convert to a list of measure/moment pairs
(define (imom-list v)
  (map (lambda (m)
         (cond
          ((integer? m)(cons m (ly:make-moment 0 0)))
          ((and (pair? m)(integer? (car m))(mom? (cdr m)))(cons (car m)(mom->moment (cdr m))))
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
  (format "~A\\set ~A = ~A" (if (is-once ps) "once " "") (string-append (if (get-context ps) (format "~A." (get-context ps)) "") (format "~A" (get-symbol ps))) (get-value ps)))
(export propset->string)
; display propset
(define-method (display (o <propset>) port) (display (propset->string o) port))



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
(define mod-tree (tree-create 'edition-mods))

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

; add modification(s)
(define-public (edition-mod edition-target measure moment context-edition-id mods)
  (let* ((mod-path `(,edition-target ,measure ,moment ,@context-edition-id))
         (tmods (tree-get mod-tree mod-path))
         (tmods (if (list? tmods) tmods '())))
    (ly:message "mods ~A" mods)
    (tree-set! mod-tree mod-path (append tmods mods))
    ))
(define-public (music-or-contextmod? v) (or (ly:music? v)(ly:context-mod? v)))
(define-public editionMod
  (define-void-function
   (edition-target measure moment context-edition-id mods)
   (symbol? integer? mom? list? music-or-contextmod?)
   (cond
    ((ly:context-mod? mods) (edition-mod edition-target measure (mom->moment moment) context-edition-id (list mods)))
    ((ly:music? mods)
     (let ((collected-mods '()))
       (for-some-music
        (lambda (m)
          (cond

           ((eq? 'OverrideProperty (ly:music-property m 'name))
            (let* ((once (ly:music-property m 'once #f))
                   (grob (ly:music-property m 'symbol))
                   (prop (ly:music-property m 'grob-property))
                   (prop (if (symbol? prop)
                             prop
                             (car (ly:music-property m 'grob-property-path))))
                   (value (ly:music-property m 'grob-value))
                   (mod (make <override> #:once once #:grob grob #:prop prop #:value value #:context #f)))
              (ly:message "mod ~A" mod)
              (set! collected-mods `(,@collected-mods ,mod)) ; alternative (cons mod collected-mods)
              #t
              ))

           ((eq? 'RevertProperty (ly:music-property m 'name))
            (let* ((grob (ly:music-property m 'symbol))
                   (prop (ly:music-property m 'grob-property))
                   (prop (if (symbol? prop)
                             prop
                             (car (ly:music-property m 'grob-property-path))))
                   (mod (make <override> #:once #f #:revert #t #:grob grob #:prop prop #:value #f #:context #f)))
              (set! collected-mods `(,@collected-mods ,mod))
              #t
              ))

           ((eq? 'PropertySet (ly:music-property m 'name))
            (let* ((once (ly:music-property m 'once #f))
                   (symbol (ly:music-property m 'symbol))
                   (value (ly:music-property m 'value))
                   (mod (make <propset> #:once once #:symbol symbol #:value value #:context #f)))
              (set! collected-mods `(,@collected-mods ,mod))
              #t
              ))

           ((eq? 'ApplyContext (ly:music-property m 'name))
            (let* ((proc (ly:music-property m 'procedure))
                   (mod (make <apply-context> #:proc proc)))
              (set! collected-mods `(,@collected-mods ,mod))
              #t
              ))

           (else #f)
           )) mods)
       (edition-mod edition-target measure (mom->moment moment) context-edition-id collected-mods)))
    )))

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

; the edition-engraver
(define-public (edition-engraver context)
  (let ( (context-edition-id '()) ; it receives the context-edition-id from a context-property
         (context-name (ly:context-name context)) ; the context name (Voice, Staff or else)
         (context-id (ly:context-id context)) ; the context-id assigned by \new Context = "the-id" ...
         (once-mods '())
         )

    ; log slot calls
    (define (log-slot slot) ; TODO: option verbose? oll logging function?
      (ly:message "edition-engraver ~A = \"~A\" : ~A" context-name context-id slot))

    ; find mods for the current time-spec
    (define (find-mods)
      (let ((current-mods '())
            (moment (ly:context-current-moment context))
            (measure (ly:context-property context 'currentBarNumber))
            (measurePos (ly:context-property context 'measurePosition))
            )
        (for-each
         (lambda (tag)
           (let ((mods (tree-get mod-tree `(,tag ,measure ,measurePos ,@context-edition-id ,context-name))))
             (if (and (list? mods)(> (length mods) 0))
                 (set! current-mods `(,@current-mods ,@mods)))
             )) edition-targets)
        current-mods))

    `( ; TODO better make-engraver macro?
       ; TODO slots: listeners, acknowledgers, end-acknowledgers, process-acknowledged
       ; initialize engraver with its own id
       (initialize .
         ,(lambda (trans)
            (define (find-edition-id context)
              (if (ly:context? context)
                  (let ((edition-id (ly:context-property context 'edition-id #f)))
                    (if (and (list? edition-id)(> (length edition-id) 0))
                        edition-id
                        (find-edition-id (ly:context-parent context)))
                    )
                  '()))
            (log-slot "initialize")
            (set! context-edition-id (find-edition-id context))
            (ly:message "edition-engraver: ~A ~A \"~A\"" context-edition-id context-name context-id)
            ))
       ; paper columns --> breaks
       (paper-column-interface .
         ,(lambda (engraver grob source-engraver)
            (log-slot "paper-column-interface")
            ))
       ; start timestep
       (start-translation-timestep .
         ,(lambda (trans)
            (log-slot "start-translation-timestep")
            (for-each
             (lambda (mod)
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
                ((ly:music? mod) (ly:context-mod-apply! context (context-mod-from-music mod)))
                )
               ) (find-mods))
            ))
       ; stop/finish translation timestep
       (stop-translation-timestep .
         ,(lambda (trans)
            (log-slot "stop-translation-timestep")
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
       (process-music .
         ,(lambda (trans)
            (log-slot "process-music")
            ))
       (finalize .
         ,(lambda (trans)
            (log-slot "finalize")
            ; TODO edition.log
            ))

       ) ; /make-engraver
    ))


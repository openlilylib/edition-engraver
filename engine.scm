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
(use-modules (edition-engraver tree)(lily))

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
   ((fraction? m)(ly:make-moment m))
   ((ly:moment? v) v)
   (else (ly:make-moment 0/4))))

; a predicate for short input of lists of ly:moment-pairs (measure+moment)
(define (mom-list? v)(and (list? v)
                          (every (lambda (p)
                                   (or (integer? p)
                                       (and (pair? p)
                                            (integer? (car p))
                                            (mom? (cdr p))))
                                   v))))
; convert to a list of measure/moment pairs
(define (mom-list v)
  (map (lambda (m)
         (cond
          ((integer? m)(cons m (ly:make-moment 0 0)))
          ((and (pair? m)(integer? (car m))(mom? (cdr m)))(cons (car m)(mom->moment (cdr m))))
          (else (cons 0 (ly:make-moment 0 4)))))
    v))

; predefine functions - to be implemented in a closure

; the edition-engraver
(define-public edition-engraver (lambda (context) (list)))

; add/activate edition-tag
(define-public (add-edition edition-name) #f)
; remove/deactivate edition-tag
(define-public (remove-edition edition-name) #f)
; set list of edition-tags
(define-public (set-edition-list edition-list) #f)
; get list of edition-tags
(define-public (get-edition-list) '())
; add modification(s)
(define-public (edition-mod edition-tag measure moment context-edition-id mods) #f)
; add modification(s) on multiple times
(define-public (edition-mod-list edition-tag context-edition-id mods mom-list) #f)

; the closure to store tags and mods
(let ((tags '())
      (mod-tree (tree-create 'edition-mods)))

  ; add edition-tag (name target?)
  (set! add-edition (lambda (tag) (if (not (memq tag tags)) (set! tags `(,@tags ,tag)))))
  ; remove edition-tag
  (set! remove-edition (lambda (tag) (set! tags (remove (lambda (e) (eq? e tag)) tags))))
  ; set list of edition-tags
  (set! set-edition-list
        (lambda (edition-list)
          (if (list? edition-list)
              (set! tags (filter (lambda (v) (symbol? v)) edition-list))
              (set! tags '()))
          ))
  ; get list of edition-tags
  (set! get-edition-list (lambda () `(,@tags)))

  ; add edition-mod
  ; TODO alternative triggers (not only measure/moment)
  (set! edition-mod
        (lambda (edition-tag measure moment context-edition-id mods)
          (let* ((mod-path `(,edition-tag ,measure ,moment ,@context-edition-id))
                 (tmods (tree-get mod-tree mod-path))
                 (tmods (if (list? tmods) tmods '())))
            (tree-set! mod-tree mod-path (append tmods mods))
            ))
        )

  ; the edition-engraver
  (set! edition-engraver
        (lambda (context)
          (let ( (context-edition-id '()) ; it receives the context-edition-id from a context-property
                 (context-name (ly:context-name context)) ; the context name (Voice, Staff or else)
                 (context-id (ly:context-id context)) ; the context-id assigned by \new Context = "the-id" ...
                 )
            ; find mods for the current time-spec
            (define (find-mods)
              (let ((current-mods '())
                    (measure (ly:context-property context 'currentBarNumber))
                    (measurePos (ly:context-property context 'measurePosition))
                    )
                (for-each
                 (lambda (tag)
                   (let ((mods (tree-get mod-tree `(,tag ,measure ,measurePos ,@context-edition-id ,context-name))))
                     (if (and (list? mods)(> (length mods) 0))
                         (set! current-mods `(,@current-mods ,@mods)))
                     )) tags)
                current-mods))

            `( ; better make-engraver macro?
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
                    (set! context-edition-id (find-edition-id context))
                    (ly:message "edition-engraver: ~A ~A \"~A\"" context-edition-id context-name context-id)
                    ))
               ; start timestep
               (start-translation-timestep .
                 ,(lambda (trans)
                    (for-each
                     (lambda (mod)
                       (if (ly:music? mod) (ly:context-mod-apply! context (context-mod-from-music mod)))
                       ) (find-mods))
                    ))
               ) ; /make-engraver
            )) ; /lambda/let
        ) ; /set!
  )

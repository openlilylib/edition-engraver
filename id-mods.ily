%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of openLilyLib,                                           %
%                      ===========                                            %
% the community library project for GNU LilyPond                              %
% (https://github.com/openlilylib)                                            %
%              -----------                                                    %
%                                                                             %
% Library: edition-engraver                                                   %
%          ================                                                   %
%                                                                             %
% openLilyLib is free software: you can redistribute it and/or modify         %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% openLilyLib is distributed in the hope that it will be useful,              %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU General Public License for more details.                                %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
%                                                                             %
% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
% edition-engraver is maintained by Jan-Peter Voigt, jp.voigt@gmx.de          %
% and others.                                                                 %
%       Copyright Jan-Peter Voigt, Urs Liska, 2016                            %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\registerOption ee.id-mods #'()

#(set-object-property! 'eid 'backend-type? symbol?)

#(define (key-value-pair? obj)
   (and
    (pair? obj)
    (symbol? (car obj))))

#(define (ee-prop-list? obj)
   (and (list? obj)
        (every key-value-pair? obj)))

% Apply a mod to an individual ID.
% Arguments:
% - id: a symbol
% - prop: property name
% - val: the new value
% prop and val are checked to be present and matching
idMod =
#(define-void-function (id prop val) (symbol? symbol? scheme?)
   (setChildOption #t `(ee id-mods ,id) prop val))

% Apply multiple mods to an individual ID.
% Arguments:
% - id: a symbol
% - items: a list of key-value pairs
idMods =
#(define-void-function (id items) (symbol? ee-prop-list?)
   (for-each
    (lambda (item)
      (idMod id (car item) (cdr item)))
    items))

% Apply a mod to a list of IDs.
% Arguments:
% - prop: property name
% - val: value to be applied to all IDs
% - ids: list of IDs (symbol-list?)
% prop and val are checked to be present and matching
idModList =
#(define-void-function (prop val ids) (symbol? scheme? symbol-list?)
   (for-each
    (lambda (id)
      (idMod id prop val))
    ids))

% Apply (variable) values to a list of IDs
% Arguments:
% - prop: property name
% - default: default value
% - items: list of items, where each item is one of
%   - id (symbol?)
%   - list of id and value
% prop, default and each val are checked
% if items is a two-element list the first is the ID and
% the second the value. If it is a single symbol this is
% the ID, and the default value is applied.
idVarModList =
#(define-scheme-function (prop default items) (symbol? scheme? list?)
   (display items)
   (for-each
    (lambda (item ind)
      (let ((id (if (list? item)
                    (first item)
                    item))
            (val (if (list? item)
                     (cdr item)
                     default)))
        (idMod id prop val)))
    items (iota (length items))))

#(define (id-mods grob)
   "Check if ID mods are registered for this grob and
    apply them as property overrides.
    Special treatment is done to the force-right-nc mod."
   (let*
    ((_id-mods
      (or
       (assq-ref (getOption '(ee id-mods)) (ly:grob-property grob 'eid))
       '())))
    (for-each
     (lambda (mod)
       (ly:grob-set-property! grob (car mod) (cdr mod)))
     _id-mods)))

% Function to "install" the ID mod functionality to multiple grob types
% Expects a symbol-list? with grob names.
activateIdMods =
#(define-scheme-function (grobs)(symbol-list?)
   #{
     \layout {
       \context {
         \Score
         #@(map
         (lambda (grob)
         #{
           \override #`(,grob before-line-breaking) = #id-mods
           #})
         grobs)
       }
     }
   #})


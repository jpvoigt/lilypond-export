;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%                                                                             %
;% This file is part of openLilyLib,                                           %
;%                      ===========                                            %
;% the community library project for GNU LilyPond                              %
;% (https://github.com/openlilylib)                                            %
;%              -----------                                                    %
;%                                                                             %
;% Library: lilypond-export                                                    %
;%          ===============                                                    %
;%                                                                             %
;% export foreign file formats with LilyPond                                   %
;%                                                                             %
;% lilypond-export is free software: you can redistribute it and/or modify     %
;% it under the terms of the GNU General Public License as published by        %
;% the Free Software Foundation, either version 3 of the License, or           %
;% (at your option) any later version.                                         %
;%                                                                             %
;% lilypond-export is distributed in the hope that it will be useful,          %
;% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
;% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
;% GNU General Public License for more details.                                %
;%                                                                             %
;% You should have received a copy of the GNU General Public License           %
;% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
;%                                                                             %
;% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
;% lilypond-export is maintained by Jan-Peter Voigt, jp.voigt@gmx.de           %
;%                                                                             %
;%       Copyright Jan-Peter Voigt, Urs Liska, 2017                            %
;%                                                                             %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; The code in this module is originally from the GNU Guile (sxml simple) module:
;; https://git.savannah.gnu.org/gitweb/?p=guile.git;a=blob_plain;f=module/sxml/simple.scm
;; which is free software and can be redistributed or modified under the terms
;; of the GNU Lesser General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or any later version.  See the
;; module's source code for more details.

;; Not all of the code from the (sxml simple) module is used in this module.
;; Why not just use the (sxml simple) module?  LilyPond currently uses
;; Guile 1.8 and the (sxml simple) module is not available in Guile 1.8.
;; If and when LilyPond moves to a newer version of Guile then this module will
;; likely have reached the end of its utility.

(define-module (lilypond-export sxml-to-xml)
  #:use-module (ice-9 optargs)
  #:export (sxml->xml))

(define check-name
  (let ((*good-cache* (make-hash-table)))
    (lambda (name)
      (if (not (hashq-ref *good-cache* name))
          (let* ((str (symbol->string name))
                 (i (string-index str #\:))
                 (head (or (and i (substring str 0 i)) str))
                 (tail (and i (substring str (1+ i)))))
            (and i (string-index (substring str (1+ i)) #\:)
                 (error "Invalid QName: more than one colon" name))
            (for-each
             (lambda (s)
               (and s
                    (or (char-alphabetic? (string-ref s 0))
                        (eq? (string-ref s 0) #\_)
                        (error "Invalid name starting character" s name))
                    (string-for-each
                     (lambda (c)
                       (or (char-alphabetic? c) (string-index "0123456789.-_" c)
                           (error "Invalid name character" c s name)))
                     s)))
             (list head tail))
            (hashq-set! *good-cache* name #t))))))

;; The following two functions serialize tags and attributes. They are
;; being used in the node handlers for the post-order function, see
;; below.

(define (attribute-value->xml value port)
  (cond
   ((pair? value)
    (attribute-value->xml (car value) port)
    (attribute-value->xml (cdr value) port))
   ((null? value)
    *unspecified*)
   ((string? value)
    (string->escaped-xml value port))
   ((procedure? value)
    (with-output-to-port port value))
   (else
    (string->escaped-xml
     (call-with-output-string (lambda (port) (display value port)))
     port))))

(define (attribute->xml attr value port)
  (check-name attr)
  (display attr port)
  (display "=\"" port)
  (attribute-value->xml value port)
  (display #\" port))

(define (element->xml tag attrs body port)
  (check-name tag)
  (display #\< port)
  (display tag port)
  (if attrs
      (let lp ((attrs attrs))
        (if (pair? attrs)
            (let ((attr (car attrs)))
              (display #\space port)
              (if (pair? attr)
                  (attribute->xml (car attr) (cdr attr) port)
                  (error "bad attribute" tag attr))
              (lp (cdr attrs)))
            (if (not (null? attrs))
                (error "bad attributes" tag attrs)))))
  (if (pair? body)
      (begin
       (display #\> port)
       (let lp ((body body))
         (cond
          ((pair? body)
           (sxml->xml (car body) port)
           (lp (cdr body)))
          ((null? body)
           (display "</" port)
           (display tag port)
           (display ">" port))
          (else
           (error "bad element body" tag body)))))
      (display " />" port)))

;; FIXME: ensure name is valid
(define (entity->xml name port)
  (display #\& port)
  (display name port)
  (display #\; port))

;; FIXME: ensure tag and str are valid
(define (pi->xml tag str port)
  (display "<?" port)
  (display tag port)
  (display #\space port)
  (display str port)
  (display "?>" port))

(define* (sxml->xml tree #:optional (port (current-output-port)))
  "Serialize the sxml tree @var{tree} as XML. The output will be written
    to the current output port, unless the optional argument @var{port} is
    present."
  (cond
   ((pair? tree)
    (if (symbol? (car tree))
        ;; An element.
        (let ((tag (car tree)))
          (case tag
            ((*TOP*)
             (sxml->xml (cdr tree) port))
            ((*ENTITY*)
             (if (and (list? (cdr tree)) (= (length (cdr tree)) 1))
                 (entity->xml (cadr tree) port)
                 (error "bad *ENTITY* args" (cdr tree))))
            ((*PI*)
             (if (and (list? (cdr tree)) (= (length (cdr tree)) 2))
                 (pi->xml (cadr tree) (caddr tree) port)
                 (error "bad *PI* args" (cdr tree))))
            (else
             (let* ((elems (cdr tree))
                    (attrs (and (pair? elems) (pair? (car elems))
                                (eq? '@ (caar elems))
                                (cdar elems))))
               (element->xml tag attrs (if attrs (cdr elems) elems) port)))))
        ;; A nodelist.
        (for-each (lambda (x) (sxml->xml x port)) tree)))
   ((string? tree)
    (string->escaped-xml tree port))
   ((null? tree) *unspecified*)
   ((not tree) *unspecified*)
   ((eqv? tree #t) *unspecified*)
   ((procedure? tree)
    (with-output-to-port port tree))
   (else
    (string->escaped-xml
     (call-with-output-string (lambda (port) (display tree port)))
     port))))

(define (make-char-quotator char-encoding)
  (let ((bad-chars (list->char-set (map car char-encoding))))

    ;; Check to see if str contains one of the characters in charset,
    ;; from the position i onward. If so, return that character's index.
    ;; otherwise, return #f
    (define (index-cset str i charset)
      (string-index str charset i))

    ;; The body of the function
    (lambda (str port)
      (let ((bad-pos (index-cset str 0 bad-chars)))
        (if (not bad-pos)
            (display str port)          ; str had all good chars
            (let loop ((from 0) (to bad-pos))
              (cond
               ((>= from (string-length str)) *unspecified*)
               ((not to)
                (display (substring str from (string-length str)) port))
               (else
                (let ((quoted-char
                       (cdr (assv (string-ref str to) char-encoding)))
                      (new-to
                       (index-cset str (+ 1 to) bad-chars)))
                  (if (< from to)
                      (display (substring str from to) port))
                  (display quoted-char port)
                  (loop (1+ to) new-to))))))))))

;; Given a string, check to make sure it does not contain characters
;; such as '<' or '&' that require encoding. Return either the original
;; string, or a list of string fragments with special characters
;; replaced by appropriate character entities.

(define string->escaped-xml
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;"))))

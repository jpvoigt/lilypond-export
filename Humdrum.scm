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

;% TODO ties, slurs, grace notes

;(define-module (lilypond-export Humdrum))

(use-modules
 (oll-core tree)
 (oll-core internal music-tools)
 (lilypond-export api)
 (lily))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; humdrum export

(define-public (export-lilypond musicexport filename . options)
  ;(display musicexport)
  (let ((grid (tree-create 'grid))
        (bar-list (sort (filter integer? (tree-get-keys musicexport '())) (lambda (a b) (< a b))) )
        (finaltime (tree-get musicexport '(finaltime))))
    (define namesU '(#\c #\d #\e #\f #\g #\a #\b))
    (define namesL '(#\C #\D #\E #\F #\G #\A #\B))
    ; serialize pitch in  humdrum-format
    (define (hum-pitch p)
      (if (ly:pitch? p) ; rests don't have a pitch
          (let ((nn (ly:pitch-notename p))
                (oo (ly:pitch-octave p))
                (aa (ly:pitch-alteration p)))
            (define (hum-note ch oo)
              (if (or (= -1 oo)(= 0 oo))
                  (cons ch '())
                  (cons ch (hum-note ch (if (< oo 0) (1+ oo) (1- oo))))
                  ))
            (string-append
             (list->string (hum-note (list-ref (if (< oo 0) namesL namesU) nn) oo))
             (cond
              ((= 1/2 aa) "#")
              ((= -1/2 aa) "-")
              (else "")
              ))) "r"))
    ; list OR returns true, if one of its elements is true
    (define (lor l) (or (car l) (if (> (length l) 1) (lor (cdr l)) #f)))

    ;(ly:message "Takte: ~A" bar-list)

    ; extract all staffs and all voices

    (tree-walk musicexport '()
      (lambda (path key value)
        (if (= 4 (length path))
            (let ((staff (caddr path))
                  (voice (cadddr path)))
              (if (and (integer? staff)(integer? voice))
                  (tree-set! grid (list staff voice) #t))
              )
            )))
    ;(tree-display grid)

    ; write krn-file
    (with-output-to-file filename
      (lambda ()
        (let ((staff-list (sort (tree-get-keys grid (list)) (lambda (a b) (< b a)))))
          ; eine Zeile im humdrum Format (mit allen splines)
          (define (spline-entry callback bar moment)
            (string-join
             (map
              (lambda (staff)
                (let ((voice-list (sort (tree-get-keys grid (list staff)) (lambda (a b) (> b a)))))
                  (string-join
                   (map
                    (lambda (voice)
                      ; callback wird übergeben und für jeden spline aufgerufen
                      (callback bar moment staff voice)
                      ) voice-list) "\t" 'infix)
                  )) staff-list) "\t" 'infix)
            )

          ; init kern
          ;(display (spline-entry (lambda (b m s v) "**kern") #f #f))
          (display (string-join (map (lambda (s) "**kern") staff-list) "\t" 'infix))
          (newline)
          (display (string-join (map (lambda (s) (format "*staff~A" s)) staff-list) "\t" 'infix))
          (newline)

          ; we need to split splines to have multiple voices in one score
          (let ((vl (map (lambda (s) (length (tree-get-keys grid (list s)))) staff-list)))
            ; recursive function
            (define (spline-voice vl)
              ; every member with a voice-count > 1 is true
              (let ((spline-split (map (lambda (v) (> v 1)) vl)))
                ; create next line with extra spline ("*^")
                (define (explode li)
                  (let ((i (car li))
                        (l (cdr li)))
                    (if (> (length l) 0)
                        (set! l (explode l)))
                    (if (> i 1)
                        (cons 1 (cons (1- i) l))
                        (cons 1 l)
                        )
                    ))
                (if (lor spline-split)
                    (begin
                     (display (string-join (map (lambda (split) (if split "*^" "*")) spline-split) "\t" 'infix))
                     (newline)
                     (spline-voice (explode vl))
                     ))))
            (spline-voice vl)
            )

          ; für jeden Takt ...
          (for-each
           (lambda (bar)
             (let ((mom-list (sort (tree-get-keys musicexport (list bar)) ly:moment<?)))
               (display (spline-entry (lambda (b m s v) (format "=~A" b)) bar #f))
               (newline)

               ; für jeden Moment im Takt
               (for-each
                (lambda (moment)

                  (let ((timesigs (map (lambda (s) (tree-get musicexport (list bar moment s 'timesig))) staff-list))
                        (keysigs (map (lambda (s) (tree-get musicexport (list bar moment s 'keysig))) staff-list))
                        (clefs (map (lambda (s)
                                      (let ((clefGlyph (tree-get musicexport (list bar moment s 'clefGlyph)))
                                            (clefPosition (tree-get musicexport (list bar moment s 'clefPosition))))
                                        (and (integer? clefPosition)(string? clefGlyph))
                                        )) staff-list)))

                    (if (lor clefs)
                        (begin
                         (display
                          (spline-entry
                           (lambda (b m s v)
                             (let ((clefGlyph (tree-get musicexport (list b m s 'clefGlyph)))
                                   (clefPosition (tree-get musicexport (list b m s 'clefPosition)))
                                   (clefTransposition (tree-get musicexport (list b m s 'clefTransposition))))
                               (define (ottava ct)
                                 (cond
                                  ((and (> ct 0)(= 0 (modulo ct 7))) (string-append "^" (ottava (- ct 7))))
                                  ((and (< ct 0)(= 0 (modulo ct 7))) (string-append "v" (ottava (+ ct 7))))
                                  (else "")
                                  ))
                               ;(ly:message "clef ~A ~A ~A" clefGlyph clefPosition clefTransposition)
                               (if (and (integer? clefPosition)(string? clefGlyph))
                                   (format "*clef~A~A~A" (list-ref (string-split clefGlyph #\.) 1)
                                     (ottava clefTransposition) (+ 3 (/ clefPosition 2)))
                                   "*clefG2")
                               )) bar moment))
                         (newline)))

                    (if (lor (map number-pair? timesigs))
                        (let ((gtimesig (car (filter number-pair? timesigs))))
                          (display
                           (spline-entry
                            (lambda (b m s v)
                              (let ((meta (tree-get musicexport (list b m s 'timesig))))
                                (if (number-pair? meta)
                                    (format "*M~A/~A"
                                      (car meta)(cdr meta))
                                    (format "*M~A/~A"
                                      (car gtimesig)(cdr gtimesig))
                                    ))) bar moment))
                          (newline)))

                    (if (lor (map (lambda (k) (music-is? k 'KeyChangeEvent)) keysigs))
                        (begin
                         (display
                          (spline-entry
                           (lambda (b m s v)
                             (let ((meta (tree-get musicexport (list b m s 'keysig))))
                               (if (music-is? meta 'KeyChangeEvent)
                                   (let ((pitch-alist (ly:music-property meta 'pitch-alist)))
                                     (define (acc p)
                                       (cond
                                        ((= 1/2 (cdr p)) (format "~A#" (list-ref namesU (car p))))
                                        ((= -1/2 (cdr p)) (format "~A-" (list-ref namesU (car p))))
                                        ((= 1 (cdr p)) (format "~A##" (list-ref namesU (car p))))
                                        ((= -1 (cdr p)) (format "~A--" (list-ref namesU (car p))))
                                        (else #f)))
                                     (format "*k[~A]" (string-join (filter (lambda (v) v) (map acc pitch-alist)) "" 'infix))
                                     )
                                   "*"))) bar moment))
                         (newline)))

                    (let ((mx (spline-entry
                               (lambda (b m s v)
                                 (let ((music (tree-get musicexport (list b m s v))))
                                   (if (ly:music? music) "m" "")))
                               bar moment)))
                      (if (memq #\m (string->list mx)) ; only write lines containing music!
                          (begin
                           (display
                            (spline-entry
                             (lambda (b m s v)
                               (let ((music (tree-get musicexport (list b m s v))))
                                 (if (ly:music? music) ; wenn Musik, setzen, sonst '.'
                                     (let ((dur (ly:music-property music 'duration))
                                           (tom (ly:music-property music 'name))
                                           (beam (tree-get musicexport (list b m s v 'beam))))
                                       ;(ly:message "scale ~A" (ly:duration-scale dur))
                                       (format "~A~A~A~A"
                                         (/ (expt 2 (ly:duration-log dur)) (ly:duration-scale dur))
                                         (cond
                                          ((= 1 (ly:duration-dot-count dur)) ".")
                                          ((= 2 (ly:duration-dot-count dur)) "..")
                                          (else ""))
                                         (cond
                                          ((eq? 'NoteEvent tom) (hum-pitch (ly:music-property music 'pitch)))
                                          ((eq? 'EventChord tom)
                                           (string-join
                                            (filter (lambda (v) v)
                                                    (map
                                                     (lambda (me) (if (eq? 'NoteEvent (ly:music-property me 'name))
                                                                      (hum-pitch (ly:music-property me 'pitch)) #f))
                                                     (ly:music-property music 'elements))) " " 'infix))
                                          (else "r")
                                          )
                                         (cond
                                          ((eq? 'start beam) "L")
                                          ((eq? 'end beam) "J")
                                          (else "")
                                          )
                                         ))
                                     "."))) bar moment))
                           (newline))))
                    )) mom-list)


               )) bar-list)

          (if (equal? (ly:make-moment 0) (cdr finaltime))
              (begin
               (display (spline-entry (lambda (b m s v) (format "=~A" b)) (car finaltime) #f))(newline)
               ))
          (display (spline-entry (lambda (b m s v) "*-") #f #f))(newline)(newline)
          )
        ))
    ))

(set-object-property! export-lilypond 'file-suffix "krn")


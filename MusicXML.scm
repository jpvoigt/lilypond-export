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
;%       Contributions from Alex Roitman 2018                                  %
;%                                                                             %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define-module (lilypond-export MusicXML))

(use-modules
 (srfi srfi-1)
 (oll-core tree)
 (lilypond-export api)
 (lilypond-export sxml-to-xml)
 (lily))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; musicXML export

(define (duration-factor dur)
  (*
   (/ 4 (expt 2 (ly:duration-log dur)))
   (duration-dot-factor (ly:duration-dot-count dur))
   (ly:duration-scale dur)
   ))

(define notenames '(C D E F G A B))
(define types '(breve breve whole half quarter eighth 16th 32nd 64th 128th))

(define (make-pitch p)
  (if (ly:pitch? p)
      (let ((notename (list-ref notenames (ly:pitch-notename p)))
            (alter (* 2 (ly:pitch-alteration p)))
            (octave (+ 4 (ly:pitch-octave p))))
        `(pitch
          (step ,notename)
          ,(if (not (= 0 alter)) `(alter ,alter) '())
          (octave ,octave)))
      '(rest)))

(define (make-keyblock pitch-alt)
  ;; This is alternative to the traditional keys like Gm and F
  ;; https://usermanuals.musicxml.com/MusicXML/Content/EL-MusicXML-key.htm
  (let ((notename (list-ref notenames (car pitch-alt)))
        (alt (cdr pitch-alt)))
    `((key-step ,notename)
      (key-alter ,(* 2 alt))
      (key-accidental ,(acctext alt)))))

(define (fifths pitch-alist)
  (let ((flats (length (filter (lambda (pa) (= -1/2 (cdr pa))) pitch-alist)))
        (sharps (length (filter (lambda (pa) (= 1/2 (cdr pa))) pitch-alist)))
        (others (length (filter (lambda (pa) (not (memv (cdr pa) '(-1/2 1/2)))) pitch-alist))))
    (cond
     ((> others 0) #f)
     ((and (> flats 0) (> sharps 0)) #f)
     ((> flats 0) (- flats))
     (else sharps))
    ))

(define (make-key pitch-alist)
  (if pitch-alist
      (let* ((non-zero-pitch-alts (filter (lambda (p-a) (not (= 0 (cdr p-a)))) pitch-alist))
             (fifths-val (fifths non-zero-pitch-alts)))
        `(key
          ,(if fifths-val
               `((fifths ,fifths-val) (mode "none"))
               (map make-keyblock non-zero-pitch-alts) ; alternative to traditional keys
               )))
      '()))

(define (make-duration dur moment divisions)
  (if (and (ly:duration? dur) (= 0 (ly:moment-grace moment)))
      (let ((divlen (* (duration-factor dur) divisions))
            (divmom (* divisions 4 (ly:moment-main moment)))
            (addskew 0))
        (if (not (integer? divmom))
            (let* ((num (numerator divmom))
                   (den (denominator divmom))
                   (rest (modulo num den))
                   (div (/ (- num rest) den)))
              ;(ly:message "mom: ~A ~A" (/ div rest) rest)
              (set! addskew (/ rest den))
              ))
        ;(ly:message "dur: ~A" (* divlen divisions))
        (if (not (integer? divlen))
            (let* ((len (inexact->exact divlen))
                   (num (numerator len))
                   (den (denominator len))
                   (rest (modulo num den))
                   (dur (/ (- num rest) den))
                   (adddur (+ addskew (/ rest den))))
              (while (>= adddur 1)
                (set! dur (1+ dur))
                (set! adddur (1- adddur)))
              ;(ly:message "time: ~A:~A ... ~A" num den rest)
              (set! divlen dur)
              ))
        `(duration ,divlen)
        )
      '()))

(define (make-type dur)
  (if (ly:duration? dur)
      `(type ,(list-ref types (+ 2 (ly:duration-log dur))))
      '()))

(define (make-dots d)
  (if (> d 0)
      (cons '(dot) (make-dots (1- d)))
      '()))

(define (make-timemod dur)
  (if (and (ly:duration? dur) (not (integer? (ly:duration-scale dur))))
      (let ((num (numerator (ly:duration-scale dur)))
            (den (denominator (ly:duration-scale dur))))
        `(time-modification
          (actual-notes ,den)
          (normal-notes ,num)))
      '()))

(define (make-tuplet tuplet)
  (if (pair? tuplet)
      `(tuplet (@ (number 1)
                  (placement "above")
                  (type ,(car tuplet))))
      '()))

(define art-map ; articulations
  '((accent . accent)
    (marcato . strong-accent)
    (portato . detached-legato)
    (staccatissimo . staccatissimo)
    (staccato . staccato)
    (tenuto . tenuto)))

(define orn-map ; ornaments
  '((reverseturn . inverted-turn)
    (mordent . mordent)
    (prall . shake)
    (trill . trill-mark)
    (turn . turn)))

(define onot-map '((fermata . fermata))) ; other notaions

(define picker
  (lambda (the-map)
    (lambda (atype)
      (if (null? atype)
          #f
          (let ((art-pair (assq (string->symbol atype) the-map)))
            (if art-pair (cdr art-pair) #f))))))

(define (make-articulations art-types)
  (if art-types
      (let ((arts (filter identity (map (picker art-map) art-types)))
            (orns (filter identity (map (picker orn-map) art-types)))
            (onots (filter identity (map (picker onot-map) art-types))))
        `(,(if (not (null? arts)) `(articulations ,(map list arts)) '())
          ,(if (not (null? orns)) `(ornaments ,(map list orns)) '())
          ,(map list onots)))
      '()))

(define slurs 0)

(define (make-slurs slur-num stop-num start-num)
  (if (and stop-num (> stop-num 0) (> slur-num 0))
      `((slur (@ (number ,slur-num)
                 (type "stop")))
        ,(make-slurs (- slur-num 1) (- stop-num 1) start-num))
      (if (and start-num (> start-num 0))
          `((slur (@ (number ,slurs)
                     (type "start")))
            ,(make-slurs (+ slurs 1) stop-num (- start-num 1)))
          (begin
            (set! slurs slur-num)
            '()))))

(define (make-notations chord tuplet art-types slur-start slur-stop tie-start tie-stop)
  (if (or (pair? tuplet)
          (and (not chord)
               (or art-types slur-start slur-stop))
          tie-start
          tie-stop)
      `(notations
        ,(make-tuplet tuplet)
        ,(if tie-start `(tied (@ (type "start"))) '())
        ,(if tie-stop `(tied (@ (type "stop"))) '())
        ,(if chord
             '()
             `(,(make-articulations art-types)
               ,(make-slurs slurs slur-stop slur-start))))
      '()))

(define (acctext accidental)
  (case accidental
    ((0) "natural")
    ((-1/2) "flat")
    ((1/2) "sharp")
    ((-1) "flat-flat")
    ((1) "double-sharp")
    (else "")))

(define (make-direction abs-dynamic span-dynamic)
  `(direction
    ,(if abs-dynamic
         `(direction-type
           (dynamics ,(list abs-dynamic)))
           '())
    ,(if span-dynamic
         `(direction-type
           (wedge (@ (type ,(if (eqv? span-dynamic 'decrescendo)
                                'diminuendo
                                span-dynamic)))))
          '())))

(define (writemusic m staff voice divisions . opts)
  (let* ((dur (ly:music-property m 'duration))
         (chord (ly:assoc-get 'chord opts #f #f))
         (pitch-acc (ly:assoc-get 'pitch-acc opts #f #f))
         (slur-start (ly:assoc-get 'slur-start opts #f #f))
         (slur-stop (ly:assoc-get 'slur-stop opts #f #f))
         (tie-start-pitches (ly:assoc-get 'tie-start-pitches opts #f #f))
         (tie-stop-pitches (ly:assoc-get 'tie-stop-pitches opts #f #f))
         (abs-dynamic (ly:assoc-get 'abs-dynamic opts #f #f))
         (span-dynamic (ly:assoc-get 'span-dynamic opts #f #f))
         (beam (ly:assoc-get 'beam opts))
         (tuplet (ly:assoc-get 'tuplet opts))
         (art-types (ly:assoc-get 'art-types opts #f))
         (lyrics (ly:assoc-get 'lyrics opts))
         (moment (ly:assoc-get 'moment opts))
         (music-name (ly:music-property m 'name))
         (dynamic-element (if (and (equal? music-name 'NoteEvent)
                                   (not chord)
                                   (or abs-dynamic span-dynamic))
                              (make-direction abs-dynamic span-dynamic)
                              '())))
    ;(ly:message "-----> lyrics ~A" lyrics)
    (case music-name

      ((NoteEvent)
       (let* ((pitch (ly:music-property m 'pitch))
              (tie-start (if tie-start-pitches (lset<= equal? (list pitch) tie-start-pitches) #f))
              (tie-stop (if tie-stop-pitches (lset<= equal? (list pitch) tie-stop-pitches) #f)))
         `(
           ,dynamic-element
           (note
            ,(if chord '(chord) '())
            ,(if (= 0 (ly:moment-grace moment)) '() '(grace))
            ,(make-pitch pitch)
            ,(make-duration dur moment divisions)
            ,(if tie-start `(tie (@ (type "start"))) '())
            ,(if tie-stop `(tie (@ (type "stop"))) '())
            (voice ,voice)
            ,(make-type dur)
            ,(make-dots (if (ly:duration? dur) (ly:duration-dot-count dur) 0))
            ,(if pitch-acc
                 (let ((my-p-a (filter
                                (lambda (p-a) (and p-a (eqv? pitch (car p-a))))
                                pitch-acc)))
                   (if (not (null? my-p-a))
                       `(accidental ,(acctext (cadar my-p-a)))
                       '()))
                 '())
            ,(if (symbol? beam)
                 `(beam (@ (number 1)) ,beam)
                 '())
            ,(make-timemod dur)
            ,(make-notations chord tuplet art-types slur-start slur-stop tie-start tie-stop)
            ,(if (and (not chord) (list? lyrics))
                 (map (lambda (lyric)
                        `(lyric
                          (syllabic "single")
                          (text ,lyric)))
                      lyrics)
                 '()))
           )))

      ((RestEvent)
       `(note
         (rest)
         ,(make-duration dur moment divisions)
         (voice ,voice)
         ,(make-type dur)
         ,(make-dots (if (ly:duration? dur) (ly:duration-dot-count dur) 0))
         ,(make-timemod dur)
         ,(make-notations chord tuplet art-types slur-start slur-stop #f #f)
         ))

      ((EventChord)
       (let* ((elements (ly:music-property m 'elements))
              (notes (filter (lambda (m) (music-is? m 'NoteEvent)) elements))
              (artics (filter (lambda (m) (not (music-is? m 'NoteEvent))) elements)))
         (if (not (null? notes))
             (cons
              (apply writemusic (car notes) staff voice divisions opts)
              (map
               (lambda (n)
                 (apply writemusic n staff voice divisions (cons '(chord . #t) opts)))
               (cdr notes)))
             '())
         ))

      (else '())
      )))

(define (make-clef musicexport measure moment staff doattr)
  (let ((clefGlyph (tree-get musicexport (list measure moment staff 'clefGlyph)))
        (clefPosition (tree-get musicexport (list measure moment staff 'clefPosition)))
        (clefTransposition (tree-get musicexport (list measure moment staff 'clefTransposition))))
    (if (and (string? clefGlyph)(integer? clefPosition))
        (let* ((sign (list-ref (string-split clefGlyph #\.) 1))
               (line (+ 3 (/ clefPosition 2)))
               (octave-change (if (and (not (= 0 clefTransposition))
                                       (= 0 (modulo clefTransposition 7)))
                                  `(clef-octave-change ,(/ clefTransposition 7))
                                  '()))
               (clef-tag `(clef
                           (sign ,sign)
                           (line ,line)
                           ,octave-change
                           )))
          (if doattr `(attributes ,clef-tag) clef-tag))
        '())))

(define backup 0)

(define (make-moment-function musicexport measure staff voice divisions first-moment)
  (let ((beamcont #f))
    (lambda (moment)
      (let ((music (tree-get musicexport
                     (list measure moment staff voice)))
            (clef-element
             (if (not (equal? moment (ly:make-moment 0)))
                 (make-clef musicexport measure first-moment staff #t)
                 '())))

        (if (ly:music? music)
            (let ((dur (ly:music-property music 'duration))
                  (beam (tree-get musicexport (list measure moment staff voice 'beam)))
                  (pitch-acc (tree-get musicexport (list measure moment staff voice 'pitch-acc)))
                  (art-types (tree-get musicexport (list measure moment staff voice 'art-types)))
                  (slur-start (tree-get musicexport (list measure moment staff voice 'slur-start)))
                  (slur-stop (tree-get musicexport (list measure moment staff voice 'slur-stop)))
                  (tie-start-pitches (tree-get musicexport (list measure moment staff voice 'tie-start-pitches)))
                  (tie-stop-pitches (tree-get musicexport (list measure moment staff voice 'tie-stop-pitches)))
                  (abs-dynamic (tree-get musicexport (list measure moment staff voice 'abs-dynamic)))
                  (span-dynamic (tree-get musicexport (list measure moment staff voice 'span-dynamic)))
                  (tuplet (tree-get musicexport (list measure moment staff voice 'tuplet)))
                  (lyrics (tree-get musicexport (list measure moment staff voice 'lyrics)))
                  )
              (case beam
                ((start) (set! beamcont 'continue))
                ((end) (set! beamcont #f))
                )

              (if (ly:duration? dur)
                  (set! backup (+ backup (* (duration-factor dur) divisions))))

              ; TODO staff grouping!
              (list clef-element
                (writemusic music 1 voice divisions
                  `(beam . ,(cond
                             ((eq? 'start beam) 'begin)
                             ((symbol? beam) beam)
                             ((symbol? beamcont) beamcont)))
                  `(pitch-acc . ,pitch-acc)
                  `(art-types . ,art-types)
                  `(slur-start . ,slur-start)
                  `(slur-stop . ,slur-stop)
                  `(tie-start-pitches . ,tie-start-pitches)
                  `(tie-stop-pitches . ,tie-stop-pitches)
                  `(abs-dynamic . ,abs-dynamic)
                  `(span-dynamic . ,span-dynamic)
                  `(moment . ,moment)
                  `(tuplet . ,tuplet)
                  `(lyrics . ,lyrics))))
            '())
        ))))

(define (make-voice-function musicexport measure staff divisions moment-list first-moment)
  (lambda (voice)
    (let ((backup-element (if (> backup 0)
                              `(backup (duration ,backup))
                              '())))
      (set! backup 0)
      (list backup-element
        (map-in-order (make-moment-function musicexport measure staff voice divisions first-moment)
          moment-list)))))

(define (make-measure-function musicexport staff divisions grid)
  (lambda (measure)
    (set! backup 0)
    (let* ((unsorted-moments (filter ly:moment?
                                     (tree-get-keys musicexport
                                       (list measure))))
           (moment-list (sort unsorted-moments ly:moment<?))
           (first-moment (if (> (length moment-list) 0)
                             (car moment-list)
                             (ly:make-moment 0)))
           (voices (sort (tree-get-keys grid (list staff))
                     (lambda (a b) (< a b))))
           (attributes-element
            `(attributes
              (divisions ,divisions) ; divisions by measure?
              ,(make-key (tree-get musicexport
                           (list measure first-moment staff 'key-pitch-alist)))
              ,(let ((meter (tree-get musicexport
                              (list measure first-moment staff 'timesig))))
                 (if (number-pair? meter)
                     `(time (beats ,(car meter)) (beat-type ,(cdr meter)))
                     '()))
              ,(make-clef musicexport measure first-moment staff #f))))

      `(measure (@ (number ,measure))
         ,(list attributes-element
                (map-in-order
                 (make-voice-function musicexport measure staff divisions moment-list first-moment)
                 voices)))
      )))

(define (make-staff-function musicexport divisions grid)
  (lambda (staff)
    (let ((measures (sort (filter integer? (tree-get-keys musicexport '()))
                          (lambda (a b) (< a b))))
          (measure-function (make-measure-function musicexport staff divisions grid))
          (part-id (format #f "P~A" staff)))

      `(part (@ (id ,part-id))
             ,(map-in-order measure-function measures))
      )))

(define (make-score-parts staff-list)
  (map (lambda (staff-number)
         (let ((id (format #f "P~A" staff-number))
               (part-name (format #f "Part ~A" staff-number)))
           `(score-part
             (@ (id ,id))
             (part-name ,part-name))))
       staff-list))

(define-public (exportMusicXML musicexport filename . options)
  (let* ((grid (tree-create 'grid))
         (bar-list (sort (filter integer? (tree-get-keys musicexport '())) (lambda (a b) (< a b))) )
         (finaltime (tree-get musicexport '(finaltime)))
         (division-dur (tree-get musicexport '(division-dur)))
         (divisions (if (ly:duration? division-dur)
                        (/ 64 (duration-factor division-dur))
                        1)))

    (ly:message "divisions: ~A" divisions)

    (tree-walk musicexport '()
      (lambda (path key value)
        (if (= 4 (length path))
            (let ((staff (caddr path))
                  (voice (cadddr path)))
              (if (and (integer? staff)(integer? voice))
                  (tree-set! grid (list staff voice) #t))
              ))))

    (let ((staff-list (sort (tree-get-keys grid '())
                        (lambda (a b) (< a b)))))
      (with-output-to-file filename
        (lambda ()
          (display "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>")
          (display "<!DOCTYPE score-partwise PUBLIC \"-//Recordare//DTD MusicXML 3.0 Partwise//EN\" \"http://www.musicxml.org/dtds/partwise.dtd\">")
          (sxml->xml
           `(score-partwise
             (@ (version "3.0"))
             (part-list ,(make-score-parts staff-list))
             ,(map-in-order
               (make-staff-function musicexport divisions grid)
               staff-list)))
          )))
    ))

(set-object-property! exportMusicXML 'file-suffix "xml")

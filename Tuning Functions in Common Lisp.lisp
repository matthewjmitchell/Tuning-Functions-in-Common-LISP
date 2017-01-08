;matt mitchell 
;tuning functions in common lisp 2017
;mattmitchellguitars@gmail.com


;;;;;;;contents;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;ratio functions ;;;;;;;;;;;;;;;
;octave-reduce
;transpose-ratio-by-octave
;transpose-by-ratio
;cycle-ratio
;convert-to-adjacencies
;separate-ratio
;invert-ratio
;invert-ratios
;invert-ratio-list
;add-cents-to-ratio

;;;translation functions;;;;;;;;;;;;
;ratio-to-cents
;ratios-to-cents
;cents-to-ratio
;cents-list-to-ratio-list
;ratio-to-semitones
;ratios-to-pitch-class-map
;midi-to-freq
;freqs-to-ratios
;freqs-to-cents
;freq-to-midi
;freqs-to-midi
;n-edo-interval-to-ratio
;equal-tempered-ratio-list

;;;building and parsing rational scales;;;;;;;;;;;;;;;;;;;
;modes-with-this-chord
;has-this-chord
;harmonics-of-ratio
;pythagorean-chromatic
;ratio-cycle
;cyclic-scale
;odentities
;udentities
;identities
;tonality-diamond
;odd-list (helper)
;rational-permutations
;find-rational-permutations (helper)
;non-identities
;available-identities
;subset-with-this-factor
;all-possible-dyads
;all-distinct-elements (helper)
;reduce-to-tuning-system 

;;;;;modes of ratios;;;;;;;;;;;;;;;;;;;;;;;;;;
;modes-of-ratio-list
;list-modes-of-ratio-list
;nth-mode-of-ratio-list
;mode-of-ratio-list
;relate-ratio-list-to-new-tonic (helper for mode-of-ratio-list)

;;;modes of cents;;;;;;;;;;;;;;;;;;;;;;;;;
;mode-of-cents-list

;;;common tones upon rotation;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ratios-in-common-upon-rotation
;number-of-ratios-in-commmon-for-each-mode <-- check this one
;number-of-common-tones-for-this-mode
;number-of-common-tones-for-each-mode

;;;;;harmonic distance;;;;;;;;;;;;;;;;;;;;
;tenney-hd
;tenney-hd-of-modes
;mean-tenney-hd-from-tonic
;tenney-hd-list
;sort-by-tenney-hd
;lesser-tenney-hd
;sort-modes-by-tenney-hd
;sort-modes-by-tenney-hd-and-include-hd-value
;home-mode-by-tenney-hd
;rational-interval-class
;rational-set-class <-- work in progress

;;;harmonic nodes;;;;;;;;;;;;;;;;;;;;;;;;;;
;harmonic-node-distances
;harmonic-node-distances-in-frets
;node-distance-in-decimal-frets
;harmonics-in-common
;gamut-of-nodes
;harmonics-of-ratio
;sub-harmonics-of-ratio

;;;;;fret location;;;;;;;;;;;;;;;;;;;;;;;;
;rational-fret-locator
;temperamental-fret-locator
;n-edo-fret-locator
;n-ed-x-fret-locator
;n-edo-fretmap

;;;;;;;misc;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;dancing-problem
;sum-list
;mean-of-list
;rotate-list
;subtract-lists
;list-first-elements
;list-common-elements
;list-common-elements-of-sublists
;in-all-lists
;list-first-elements
;collapse
;primep
;trial-division (a helper for primep)
;second-element-greater
;second-element-lesser
;nth-element-greater




;some scales:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant *septagorean* '(16807/16384 282475249/268435456 2401/2048 40353607/33554432 
343/256 5764801/4194304 49/32 823543/524288 13841287201/8589934592 7/4 117649/65536 1977326743/1073741824))
;(sort (ratio-cycle 7/4 12) '<))

(defconstant *partch-scale* '(1/1 81/80 33/32 21/20 16/15 12/11 11/10 10/9 9/8 8/7  	
7/6 32/27 6/5 11/9 5/4 14/11 9/7 21/16 4/3 27/20 11/8 7/5 10/7 16/11 40/27 3/2 32/21
14/9 11/7 8/5 18/11 5/3 27/16 12/7 7/4 16/9 9/5 20/11 11/6 15/8 40/21 64/33 160/81))

(defconstant *5-limit-chromatic-scale* '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8))

(defconstant *5-limit-chromatic-scale-2* '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 16/9 15/8))

(defconstant *5-limit-major-scale* '(1/1 9/8 5/4 4/3 3/2 5/3 15/8))

(defconstant *wtp* '(1/1 567/512 9/8 147/128 21/16 1323/1024 189/128 3/2 49/32 7/4 441/256 63/32))
;the tuning of La Monte Young's Well Tuned Piano
;
;according to Kyle Gann
;http://www.kylegann.com/wtp.html
;Kyle Gann. "La Monte Young's The Well-Tuned Piano" in Perspectives of New Music Vol. 31 No. 1 (Winter 1993), pp. 134-162.
;
;this mode of this system appears to be the most, not least complex, by mean tenney hd
;(sort-modes-by-tenney-hd *wtp*)
;the simplest is:
;(mode-of-ratio-list *well-tuned-piano-scale* 21/16)

(defconstant *my-septimal-heptatonic*
'(1 15/14 5/4 7/5 3/2 12/7 7/4))
;a subset of the scale below

(defconstant *septimonian-scale* '(1 15/14 8/7 7/6 5/4 4/3 7/5 3/2 8/5 12/7 7/4 28/15))
;a 7-limit chromatic scale,
;Gene Smith tells me that this scale is not a Fokker block, but is just a twerk away.
;it's named for Nico Simonian, who is also just a twerk away from many things.


;;;;;;;;ratio functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;from Larry's harmony primer 
;http://eamusic.dartmouth.edu/~larry/misc_writings/talks/harmony_primer.pdf pages 2-3
(defun octave-reduce (ratio)
"takes a ratio and expresses it as between 1/1 and (not including) 2/1"
(if (< ratio 1)
  (octave-reduce (* ratio 2))
  (if (>= ratio 2)
    (octave-reduce (/ ratio 2))
    ratio)))
;(octave-reduce 9/1)

(defun transpose-ratio-by-octave (ratio octave-displacement)
"transposes a ratio by the given number of octaves (positive or negative)"
  (* ratio (expt 2 octave-displacement)))
;(transpose-ratio-by-octave 9/8 -1)

(defun transpose-by-ratio (ratio ratio-list)
"transposes ratio-list by ratio"
  (if (null ratio-list)()
    (cons (* ratio (first ratio-list))
          (transpose-by-ratio ratio (rest ratio-list)))))
;(transpose-by-ratio 16/15 '(1/1 9/8 5/4 4/3 3/2 5/4 15/8))

(defun cycle-ratio (ratio times)
"returns the octave-reduced result of cycling a ratio n times" 
 (octave-reduce (expt ratio times)))
;(cycle-ratio 3/2 3)
;(cycle-ratio 5/4 2)

;if you have a scale, you might want to know the sizes of the steps
;
;this function converts a list of ratios relating to 1/1 into a list in which each
;relates to the previous
;the distance between ratio-one and ratio-two: (/ ratio-two ratio-one)
;
;to find the adjacencies of an octave-repeating system, use with second argument 2 
;that way, it also returns the adjacency between (last ratio-list) and (* 2 (first ratio-list))
;to simply list the ratios between elements of ratio-list, use without a second argument 
;
(defun convert-to-adjacencies (ratio-list &optional (repeat-factor))
"returns a list of the ratios between adjecent elements of ratio-list"
  (if repeat-factor (convert-to-adjacencies (append ratio-list (list (* repeat-factor (first ratio-list)))))
  ;if a repeat-factor is provided, recurse, and tack (first-element * repeat-factor) on to the end of ratio-list
    (if (< (length ratio-list) 2) ();if there are < 2 ratios in the list, there are no more adjacency ratios
      (cons
       (/ (second ratio-list) (first ratio-list))
       (convert-to-adjacencies (rest ratio-list))))))
;(convert-to-adjacencies '(1/1 6/5 4/3 3/2 9/5) 2)
;(convert-to-adjacencies '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 16/9 15/8) 2)
;(convert-to-adjacencies '(3/4 15/16 1/1 9/8 4/3 5/4 9/8))
;(ratios-to-cents (convert-to-adjacencies '(3/4 15/16 1/1 9/8 4/3 5/4 9/8)))

(defun separate-ratio (ratio)
"takes a ratio and returns the numerator and denominator of its simple form in a list"
(list (numerator ratio) (denominator ratio)))
;(separate-ratio 5/4)

(defun invert-ratio (ratio)
"returns the octave-reduced inversion of a ratio"
 (octave-reduce (/ 1 ratio)))
;(invert-ratio 3/2)

(defun invert-ratios (ratio-list)
"returns the octave-reduced inversions of a list of ratios"
  (if (null ratio-list) ()
    (cons (invert-ratio (first ratio-list))
          (invert-ratios (rest ratio-list))
          )))
;(invert-ratios '(1/1 9/8 5/4 4/3 3/2 5/3 15/8))

(defun invert-ratio-list (ratio-list)
"returns the octave-reduced inversions of a list of ratios, reordered from smallest to largest"
  (sort (invert-ratios ratio-list) #'<))
;(invert-ratio-list '(1/1 9/8 5/4 4/3 3/2 5/3 15/8))
;
;the partch scale is inversionally symmetrical:
;(equalp *partch-scale* (invert-ratio-list *partch-scale*))

(defun add-cents-to-ratio (ratio cents)
"returns the ratio that results from adjusting the given ratio by the given number of cents"
(cents-to-ratio (+ cents (ratio-to-cents ratio))))
;(add-cents-to-ratio 5/4 13.68631)
;(ratio-to-cents (add-cents-to-ratio 5/4 13.68631))
;(add-cents-to-ratio 3/2 -2)


;;;translation functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ratio-to-cents (ratio)
"converts ratio to cents" 
      (* 1200 (log ratio 2)))
;(ratio-to-cents 3/2)

(defun ratios-to-cents (ratios)
  "converts a list of ratios into a list of cents values"
  (if (null ratios) ()
    (cons
     (ratio-to-cents (first ratios))
     (ratios-to-cents (rest ratios)))))
;(ratios-to-cents '(1 9/8 5/4 4/3 3/2 5/3 15/8))

(defun cents-to-ratio (cents)
"converts a cents value to a ratio expressed as a float"
  (expt 2 (/ cents 1200)))
;(cents-to-ratio 969)
;
;the ratio of one cent is
;(expt 2 1/1200)
;
;rounding error is an issue
;(rational (cents-to-ratio 386.3137)) returns 5/4, but
;(rational (cents-to-ratio 386.3136)) returns 10485759/8388608
;a "quantize-to-simple-ratio" function would be useful


(defun cents-list-to-ratio-list (cents-list)
"converts a cents-list to a list of ratios expressed as floats"
  (if (null cents-list)()
    (cons (cents-to-ratio (first cents-list))
          (cents-list-to-ratio-list (rest cents-list)))))
;(cents-list-to-ratio-list '(0 204 386 498 702 884 1088))

(defun ratio-to-semitones (ratio)
"takes a ratio, and returns decimal semitones" 
  (* 12 (log ratio 2)))
;(ratio-to-semitones 3/2)


(defun ratios-to-pitch-class-map (ratio-list)
  "expresses a list of ratios in terms of decimal 12-tet semitones"
  (if (null ratio-list) ()
    (cons (ratio-to-semitones (first ratio-list))
          (ratios-to-pitch-class-map (rest ratio-list)))))
;(ratios-to-pitch-class-map '(1/1 16/15 8/7 4/3 3/2 7/4 15/8 2))
;(sort (ratios-to-pitch-class-map (pythagorean-chromatic)) '<) ;pythagorean scale


(defun midi-to-freq (midi-number &optional (a-reference 440))
"converts a midi-pitch into a frequency"
  (* a-reference (expt 2 (/ (- midi-number 69) 12))))
;(midi-to-freq 73)
;(midi-to-freq 73 435)

(defun freqs-to-ratios (freq-list tonic-freq)
"converts a list of frequencies to a list of ratios from the frequency givan as tonic-freq"
  (if (null freq-list) ()
    (cons
     (/ (first freq-list) tonic-freq)
     (freqs-to-ratios (rest freq-list) tonic-freq))))
;(freqs-to-ratios '(136.1 140.25 144.72 147.27 147.85 183.58 206.36 211.44 221.23) 136.1)

(defun freqs-to-cents (freq-list tonic-freq)
"converts a list of frequencies to a list of cents from the frequency given as tonic-freq"
  (ratios-to-cents (freqs-to-ratios freq-list tonic-freq)))
;(freqs-to-cents '(136.1 140.25 144.72 147.27 147.85 183.58 206.36 211.44 221.23) 136.1)


(defun freq-to-midi (freq &optional (a-reference 440))
"takes a frequency and returns a decimal midi pitch number"
(+ (/ (ratio-to-cents (/ freq a-reference)) 100) 69))
;(freq-to-midi 440)
;(freq-to-midi (* 440 (expt 2 1/12)))
;(freq-to-midi 223.6)
;
;(ratio-to-cents (/ freq 440)) = cents from A440
;(/ (ratio-to-cents (/ freq 440)) 100) is the same value converted to decimal semitones
;A440 is midi pitch number 69
;since A440 is our reference point, we add 69 to the above expression to find the correct midi pitch number

(defun freqs-to-midi (freqs)
"converts a list of frequencies to a list of decimal midi pitch values"
  (if (null freqs)()
    (cons
     (freq-to-midi (first freqs))
     (freqs-to-midi (rest freqs)))))
;(freqs-to-midi '(223.6 354.9 764.3))

;interval is expressed in units of its smallest interval of the temperament
(defun n-edo-interval-to-ratio (n interval)
"expresses as a float the ratio of interval steps in a system that divides the octave into n parts"
  (expt 2 (/ interval n)))
;(rational (n-edo-interval-to-ratio 19 1))
;(n-edo-interval-to-ratio 12 -1)
;
;this can also be applied to rhythm
;an equal tempered major third above 120bpm:
;(* 120 (n-edo-interval-to-ratio 12 4))
;
;to find the ratio-list for 12 edo: 
;'((expt 2 0/12)(expt 2 1/12)(expt 2 2/12)...(expt 2 11/12))
;and, more generally:
;'((expt repeat-factor (/ 0 cardinality)) (expt repeat-factor (/ 1 cardinality))...(expt repeat-factor (/ (- cardinality 1)) cardinality))
(defun equal-tempered-ratio-list (repeat-factor cardinality &optional (counter 0))
"returns the list of ratios associated with 'cardinality' divisions the interval 'repeat-factor'"
    (if (>= counter cardinality)()
      (cons (expt repeat-factor (/ counter cardinality))
            (equal-tempered-ratio-list repeat-factor cardinality (1+ counter)))))
;(equal-tempered-ratio-list 2 12)
;(equal-tempered-ratio-list 3 13)


;;;building and parsing rational scales;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun modes-with-this-chord (chord ratio-list &optional (counter 0))
"lists the modes of ratio-list that contain chord"
  (if (>= counter (length ratio-list))();if we've gone through all the modes, we're done. otherwise,
    (if (has-this-chord chord (nth-mode-of-ratio-list counter ratio-list)); if chord is in this mode,
      (cons (nth counter ratio-list);cons the tonic if this mode
            (modes-with-this-chord chord ratio-list (1+ counter)));and check the next mode
      (modes-with-this-chord chord ratio-list (1+ counter)))));otherwise, recurse without consing anything
;(modes-with-this-chord '(1 5/4 3/2) *5-limit-chromatic-scale*)
;(modes-with-this-chord '(1 6/5 3/2) *5-limit-chromatic-scale*)
;(modes-with-this-chord '(1 5/4 3/2) *septimonian-scale*)
;(modes-with-this-chord '(1 6/5 3/2) *septimonian-scale*)
;(modes-with-this-chord '(1 5/4 7/4) *septimonian-scale*)

(defun has-this-chord (chord ratio-list)
"returns true if all members of chord are in ratio-list"
  (if (null chord) t ;if we've gone through all of chord, return true
    (if (find (first chord) ratio-list) ;if this element of chord is in ratio-list 
      (has-this-chord (rest chord) ratio-list);check the next element
      ())));otherwise, return nil


;the elements of this scale upon which you can build just minor triads are the inversions 
;of those upon which you can build just minor triads:
#|;
(equalp 
 (modes-with-this-chord '(1 5/4 3/2) *5-limit-chromatic-scale*)
 (invert-ratio-list (modes-with-this-chord '(1 6/5 3/2) *5-limit-chromatic-scale*)))
|#

(defun harmonics-of-ratio (ratio limit &optional (counter 1))
 "lists the harmonics of ratio from 1 to limit"
  (if (>= counter limit)()
    (cons (* ratio counter)
          (harmonics-of-ratio ratio limit (1+ counter)))))
;(harmonics-of-ratio 1 9)
;(harmonics-of-ratio 4/3 9)

(defun pythagorean-chromatic (&optional (counter -5))
"generates a pythagorean chromatic scale"
  (if (> counter 6) ()
    (cons (octave-reduce (expt 3 counter))
          (pythagorean-chromatic (1+ counter)))))
;(pythagorean-chromatic)
;(convert-to-adjacencies (pythagorean-chromatic))
;(convert-to-adjacencies (pythagorean-chromatic) 2)
;(sort (pythagorean-chromatic) '<)
;(convert-to-adjacencies (sort (pythagorean-chromatic) '<))
;(sort-by-tenney-hd (pythagorean-chromatic))
;(equalp (sort-by-tenney-hd (pythagorean-chromatic)) (sort-modes-by-tenney-hd (pythagorean-chromatic)))

(defun ratio-cycle (ratio times &optional (counter 1))
"returns a list of succesive powers of ratio, octave reduced"
  (if (> counter times) ()
    (cons (octave-reduce (expt ratio counter))
          (ratio-cycle ratio times (1+ counter)))))
;(ratio-cycle 3/2 4)

(defun cyclic-scale (ratio min max)
"generates a list from ratio to the power of min to ratio to the power of max, octave-reduced"
  (if (> min max) ()
    (cons (octave-reduce (expt ratio min))
          (cyclic-scale ratio (1+ min) max))))
;(cyclic-scale 7 -12 13) 
;
;((ratios-to-cents (ratio-cycle 7 26))

;this is the same as harmonics-of-ratio, except that the results are octave-reduced
;http://en.wikipedia.org/wiki/Limit_%28music%29
;or for a more cryptic explanation, see Genesis of a Music page 72
(defun odentities (odd-limit &optional (ratio 1)(counter 1))
"lists the octave-reduced equivalents of the set of harmonics of ratio up to odd-limit"
  (if (or (> counter odd-limit)(< counter 0)) ();odd-limit shouldn't be negative
    (cons (octave-reduce (* ratio counter))
          (odentities odd-limit ratio (+ 2 counter)))))
;(odentities 13)
;(odentities 11 4/3)
;(odentities 7 12/7)

;http://en.wikipedia.org/wiki/Limit_%28music%29
;or for a more cryptic explanation, see Genesis of a Music page 72
(defun udentities (odd-limit &optional (ratio 1)(counter 1))
"lists the octave-reduced equivalents of the set of sub-harmonics of ratio up to odd-limit"
  (if (or (> counter odd-limit)(< counter 0)) ();odd-limit shouldn't be negative
    (cons (octave-reduce (/ ratio counter))
          (udentities odd-limit ratio (+ 2 counter)))))
;(udentities 11)
;(udentities 7 12/7)

;this lists the identities of the otonalities and utonalities of ratio up to odd-limit
(defun identities (odd-limit &optional (ratio 1))
"lists the octave-reduced equivalents of the set of harmonics and sub-harmonics of ratio up to odd-limit, ordered by '<"
  (sort 
   (remove-duplicates
    (append 
     (odentities odd-limit ratio) 
     (udentities odd-limit ratio)))
   '<))
;(identities 11)
;(identities 7 5/4)

;odd-limits and the tonality diamond:
;a tonality diamond is made up of the set of octave-reduced ratios that can be generated 
;by some combination of odd numbers up to and including the odd-limit
;with no odd greater than odd-limit in either the numerator of the denominator
;for the 5-limit
;take the numbers '(1 3 5)
;and work through the permutations
;
;1/1 5/1 3/1
;1/5 5/5 3/5
;1/3 5/3 3/3
;
;octave-reduce and simplify:
;
;1/1 5/4 3/2
;8/5 1/1 6/5
;4/3 5/3 1/1
;
;major triads - left to right
;minor triads - bottom to top
;
;eliminate redundancies and sort by '<
;
;'(1/1 6/5 5/4 4/3 3/2 8/5 5/3)
;
;
;Partch's "Genesis of a Music" pages 109-118
;also: http://en.wikipedia.org/wiki/Tonality_diamond
(defun tonality-diamond (odd-limit)
"returns the list of members of the tonality diamond for the given odd-limit"
 (rational-permutations (odd-list odd-limit)))
;(tonality-diamond 3)
;(tonality-diamond 5)
;(tonality-diamond 7)
;(tonality-diamond 9)
;(tonality-diamond 11)
;(tonality-diamond 13)


;a helper for tonality-diamond
;this makes an element-list for rational-permutations
(defun odd-list (odd-limit &optional (counter 1))
"lists all positive odd numbers up to odd limit"
  (if (> counter odd-limit)()
    (cons counter 
          (odd-list odd-limit (+ counter 2)))))
(odd-list 13)
   
(defun rational-permutations (element-list)
"lists all octave-reduced ratios that can be made by multiplying or dividing each element in list by another element in list"
  (sort (remove-duplicates (find-rational-permutations element-list)) '<))
;(rational-permutations '(1 3 5))
;(rational-permutations '(1 3 5 9))
;(rational-permutations '(1 3 5 9 15))
;(rational-permutations '(1 3 5 9 15 45))

;(rational-permutations '(1 3 5 7))
;(ratios-to-cents (rational-permutations '(1 3 5 7)))


(defun find-rational-permutations (element-list &optional (num-counter 0)(den-counter 0))
"a helper for rational-permutations"
  (if (and (>= den-counter (1- (length element-list)));if we're on the last denominator,
           (> num-counter (1- (length element-list))))();and we've gone through all the numerators for this denominator, we're done
    (if (> num-counter (1- (length element-list)));otherwise, if we've gone through all the numerators,
      (find-rational-permutations element-list 0 (1+ den-counter)); move on to the next denominator, and start with the first numerator
      (cons (octave-reduce (/ (nth num-counter element-list)(nth den-counter element-list)));otherwise, cons this-numerator/this-denominator
            (find-rational-permutations element-list (1+ num-counter) den-counter))))) ;and recurse, moving on to the next numerator
;(find-rational-permutations '(1 3 5 9 15 45))
;(find-rational-permutations '(3 7 23))

(defun non-identities (odd-limit)
"lists the members of the given odd-limit that are not harmonics or sub-harmonics"
  (subtract-lists (tonality-diamond odd-limit) (identities odd-limit)))
;(non-identities 7)
;(non-identities 11)
;(identities 11)
;
;identities + non-identities = tonality-diamond
;(equalp (tonality-diamond 11) (sort (append (identities 11) (non-identities 11)) '<))


;what members of ratio-list are an odentity or udentity of ratio
(defun available-identities (odd-limit ratio ratio-list)
"lists the elements of ratio-list that are related by harmonic to ratio, up to odd-limit"
  (sort (intersection ratio-list (identities odd-limit ratio))
   '<))
;(available-identities 11 81/80 *partch-scale*)
;(available-identities 11 3/2 *partch-scale*)

(defun subset-with-this-factor (ratio-list factor)
"lists those ratios in ratio list that contain factor"
  (if (null ratio-list) ()
    (if (or (zerop (rem (numerator (first ratio-list)) factor)) 
            (zerop (rem (denominator (first ratio-list)) factor)))
      (cons (first ratio-list)
            (subset-with-this-factor (rest ratio-list) factor))
      (subset-with-this-factor (rest ratio-list) factor))))
;(subset-with-this-factor *partch-scale* 11)
;(subset-with-this-factor *partch-scale* 7)
;(subset-with-this-factor *partch-scale* 5)
;(subset-with-this-factor *partch-scale* 3)

(defun all-possible-dyads (ratio-list)
"lists all distinct ratios that exist between members of ratio-list"
  (all-distinct-elements (list-modes-of-ratio-list ratio-list)))
;(all-possible-dyads (tonality-diamond 11))
;(all-possible-dyads (rational-permutations '(1 3 5 9 15 45)))
;(all-possible-dyads *partch-scale*)
;(all-possible-dyads *wtp*)
;
;(sort-by-tenney-hd (all-possible-dyads *partch-scale*))
;(sort-by-tenney-hd (all-possible-dyads (tonality-diamond 11)))

(defun all-distinct-elements (list-of-lists)
"reduces a list of lists to a single list containing one of each distinct element, ordered by '<"
  (sort (remove-duplicates (collapse list-of-lists)) '<))
;(all-distinct-elements '((2 4 6 8)(9 8 7 6 5)(1 2 3 4)))

(defun reduce-to-tuning-system (ratio-list)
"removes duplicates, octave reduces and sorts ratio-list by '<"
  (sort
   (remove-duplicates
    (mapcar 'octave-reduce
            ratio-list))
  '<))
;(reduce-to-tuning-system '(3/1 9/4 2/1 15/4 5/2 8/3 5/6))



;are there any elements of the partch scale that can not be obtained by rotating the 11-limit tonality diamond?
;(equalp *partch-scale* (list-common-elements *partch-scale* (all-possible-dyads (tonality-diamond 11))))
;no, that's not surprising.

;what ratios obtainable by rotating the 11-limit diamond are not part of the partch scale?
;and could you sort those by complexity please?
;(sort-by-tenney-hd (subtract-lists (all-possible-dyads (tonality-diamond 11)) *partch-scale*))



;;;;;modes of ratios;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun modes-of-ratio-list (ratio-list &optional (counter 0))
  "returns all modes of ratio-list, with each preceded by its tonic"
  (if (>= counter (length ratio-list)) ()
    (cons 
     (list (nth counter ratio-list)
           (nth-mode-of-ratio-list counter ratio-list))
     (modes-of-ratio-list ratio-list (1+ counter)))))
;(modes-of-ratio-list '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8))


;this is the same as modes-of-ratio-list
;except that it returns a simple list of lists
;instead of listing the "new-tonic" before each mode
(defun list-modes-of-ratio-list (ratio-list &optional (counter 0))
  "returns all modes of ratio-list as a list of lists"
  (if (>= counter (length ratio-list)) ()
    (cons (nth-mode-of-ratio-list counter ratio-list)
     (list-modes-of-ratio-list ratio-list (1+ counter)))))
;(list-modes-of-ratio-list '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8))

(defun nth-mode-of-ratio-list (n ratio-list)
"returns the nth mode of ratio-list"
  (mode-of-ratio-list ratio-list (nth n ratio-list)))
;(nth-mode-of-ratio-list 17 *partch-scale*)

(defun mode-of-ratio-list (ratio-list new-tonic)
  "returns the mode of ratio-list with new-tonic as 1/1"
  (sort (relate-ratio-list-to-new-tonic ratio-list new-tonic) #'<))
;(mode-of-ratio-list '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8) 3/2)

(defun relate-ratio-list-to-new-tonic (ratio-list new-tonic)
  "returns the ratio-list reckoned from new-tonic"
  (if (null ratio-list) ()
    (cons
     (octave-reduce (/ (first ratio-list) new-tonic))
     (relate-ratio-list-to-new-tonic (rest ratio-list) new-tonic))))
;(relate-ratio-list-to-new-tonic '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8) 3/2)

(defun modes-of-ratio-list-in-cents (ratio-list)
"returns the modes of ratio-list, expressed in cents"
       (mapcar 'ratios-to-cents (list-modes-of-ratio-list ratio-list)))


;;;modes of cents;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mode-of-cents-list (new-tonic cents-list)
"returns the elements of cents-list as reckoned from new-tonic (mod 1200), ordered by '<"
  (sort 
   (if (null cents-list) ()
     (cons
      (abs (mod (- (first cents-list) new-tonic) 1200))
      (mode-of-cents-list new-tonic (rest cents-list))))
   '<))
;(mode-of-cents-list 702 '(0 204 386 498 702 884 1088))



;common tones upon rotation;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;to make:
;(sort-modes-by-ratios-in-common
;how would this ranking compare with harmonic distance?

;returns a list in the form: ( (mode-1)(ratios-in-common-with-mode-0)... (mode-n)(ratios-in-common-with-mode-0) )
(defun ratios-in-common-upon-rotation (ratio-list &optional (counter 1))
"For each mode of ratio-list, lists ratios in common with mode 0"
  (if (>= counter (length ratio-list))()
    (cons
     (cons (nth counter ratio-list)
           (list
            (list-common-elements 
             ratio-list 
             (mode-of-ratio-list ratio-list (nth counter ratio-list)))))
     (ratios-in-common-upon-rotation ratio-list (1+ counter)))))
;(ratios-in-common-upon-rotation '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8))
;(ratios-in-common-upon-rotation *partch-scale*)
;(ratios-in-common-upon-rotation *septimonian-scale*)
;(ratios-in-common-upon-rotation *wtp*)


;check this
(defun number-of-ratios-in-commmon-for-each-mode (ratio-list &optional (counter 0))
"returns the number of ratios that each mode of ratio-list has in common with mode 0" 
  (if (>= counter (length ratio-list)) ()
    (cons (length (second (nth counter (ratios-in-common-upon-rotation ratio-list))))
          (number-of-ratios-in-commmon-for-each-mode ratio-list (1+ counter)))))
;(number-of-ratios-in-commmon-for-each-mode *partch-scale*)
;(number-of-ratios-in-commmon-for-each-mode '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 16/9 15/8))
;(number-of-ratios-in-commmon-for-each-mode '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8))

(defun number-of-common-tones-for-this-mode (ratio-list mode)
"lists the number of ratios in common between ratio-list and mode of ratio-list"
       (length (list-common-elements ratio-list (mode-of-ratio-list ratio-list (nth mode ratio-list)))))
;(number-of-common-tones-for-this-mode '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8) 7)
;(number-of-common-tones-for-this-mode *partch-scale* 18)

(defun number-of-common-tones-for-each-mode (ratio-list)
"returns a list of lists with these two elements: ratio and number of tones its mode has in common with mode 0"
       (modal-common-tone-helper (ratios-in-common-upon-rotation ratio-list)))
;(number-of-common-tones-for-each-mode *5-limit-chromatic-scale*)

(defun modal-common-tone-helper (list-of-common-tones)
  (if (null list-of-common-tones) ()
    (cons 
     (list
      (first (first list-of-common-tones))
      (length (second (first list-of-common-tones))))
     (modal-common-tone-helper (rest list-of-common-tones)))))
;(number-of-common-tones-for-each-mode *partch-scale*)

;;;;;harmonic distance;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;for ratio x/y, log(xy)
(defun tenney-hd (ratio)
"James Tenney's harmonic distance function"
  (+ (log (* (numerator (octave-reduce ratio)) (denominator (octave-reduce ratio))) 10)))
;(tenney-hd 3/2)
;(tenney-hd 81/80)
;
;"John Cage and the theory of harmony" James Tenney (page 24)
;http://www.plainsound.org/pdfs/JC&ToH.pdf 
;
;also, "Harmony Primer" Larry Polansky 
;http://eamusic.dartmouth.edu/~larry/misc_writings/talks/harmony_primer.pdf
;
;something to be aware of:
;(tenney-hd 3/2)
;(tenney-hd (rational (add-cents-to-ratio 3/2 .01)))


;returns a list of two-item lists. the first item is a ratio. 
;The second item is the mean of the harmonic distances between that ratio and the others in the list. 
(defun tenney-hd-of-modes (ratio-list &optional (counter 0))
 "lists the means of the harmonic distances between each ratio and all the other ratios in the list"
 (if (>= counter (length ratio-list)) ()
   (cons 
    (list (first ratio-list) (mean-tenney-hd-from-tonic (mode-of-ratio-list ratio-list (first ratio-list))))
    (tenney-hd-of-modes (rotate-list ratio-list) (1+ counter)))))
;(tenney-hd-of-modes *5-limit-chromatic-scale*)
;(tenney-hd-of-modes '(1/1 6/5 4/3 3/2 9/5))
;(tenney-hd-of-modes *wtp*)

(defun mean-tenney-hd-from-tonic (ratio-list)
  "returns the average Tenney Harmonic Distance from 1/1 for the given ratio list"
  (mean-of-list (tenney-hd-list ratio-list)))
;(mean-tenney-hd-from-tonic *5-limit-chromatic-scale*)
;(mean-tenney-hd-from-tonic *septimal-chromatic-scale*)

;a helper function for mean-tenney-hd-from-tonic
(defun tenney-hd-list (ratio-list)
  "returns a list of Tenney Harmonic Distance values for the given ratio-list"
  (if (null ratio-list) ()
    (cons
     (tenney-hd (first ratio-list))
     (tenney-hd-list (rest ratio-list)))))
;(tenney-hd-list '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8))

;copy-list is neccessary in order to make this non-destructive
;don't do this: (sort *list-that-i-don't-want-to-overwrite* 'predicate-function)
(defun sort-by-tenney-hd (ratio-list)
 "sorts ratio-list in ascending order of tenney-hd"
  (sort (copy-list ratio-list) 'lesser-tenney-hd))
;(sort-by-tenney-hd *partch-scale*)


(defun lesser-tenney-hd (ratio-1 ratio-2)
"returns true if tenney-hd of ratio-1 is less than that of ratio-2"
  (if (< (tenney-hd ratio-1)(tenney-hd ratio-2)) T))
;(lesser-tenney-hd 3/2 5/4)


(defun sort-modes-by-tenney-hd (ratio-list)
"ranks the modes of ratio-list by mean tenney-hd"
  (list-first-elements (sort-modes-by-tenney-hd-and-include-hd-value ratio-list)))
;(sort-modes-by-tenney-hd *5-limit-major-scale*)
;(sort-modes-by-tenney-hd *partch-scale*)
;(sort-modes-by-tenney-hd *my-septimal-heptatonic*)
;this one's interesting:
;(sort-modes-by-tenney-hd *wtp*)

(defun sort-modes-by-tenney-hd-and-include-hd-value (ratio-list)
"sorts the output of (tenney-hd-of-modes by tenney-hd value"
  (sort (tenney-hd-of-modes ratio-list) 'second-element-lesser))
;(sort-modes-by-tenney-hd-and-include-hd-value *partch-scale*)

;what is the relationship between the tenney-hd of a ratio
;and the mean tenney-hd of the mode built upon that ratio? 
;and does this say something about the structure of the scale?
;
;these are not the same. what is the relationship between them?
;(sort-by-tenney-hd *partch-scale*)
;(sort-modes-by-tenney-hd *partch-scale*)
;
;how about these?:
;(sort-by-tenney-hd *wtp*)
;(sort-modes-by-tenney-hd *wtp*)

(defun home-mode-by-tenney-hd (ratio-list)
"returns the member of ratio-list with the lowest 
mean harmonic distance from the other members"
  (first (sort-modes-by-tenney-hd ratio-list)))

(defun rational-interval-class (ratio)
  (if ( >= (octave-reduce (numerator ratio))
           (octave-reduce (denominator ratio)))
      (octave-reduce ratio)
       (octave-reduce (/ 1 ratio))))
;(rational-interval-class 4/3)
;(rational-interval-class 5/8)
;(rational-interval-class 16/15)
;(rational-interval-class 10/7)


;a "canonical" form of a rational structure
;use mean harmonic distance to find the best candidate for tonic,
;then transpose that mode to tonic = 1
(defun rational-set-class (ratio-list)
"returns the mode of ratio-list with the lowest mean harmonic distance
from tonic, transposed to tonic = 1/1" 
(sort 
 (mode-of-ratio-list ratio-list (home-mode-by-tenney-hd ratio-list))
 '<))
;(rational-set-class *wtp*)
;(rational-set-class '(3/2 15/8 9/8)
;(rational-set-class '(15/8 3/2 9/8))
;(rational-set-class '(35/16 7/1 21/16))
;(rational-set-class '(1/1 6/5 3/2))
;(rational-set-class '(7/8 6/1 5/2 1/2))

;;;;;;;;;;;;harmonic nodes;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the distance (from the nut) for node n of harmonic h is (* scale-length (/ n h))
;
;e.g. for a scale-length of 500 the nodes of the fifth harmonic will be at '(100 200 300 400)
;
;for non-prime harmonics, it's a little more complicated, since we want to exclude those nodes that are shared with 
;a lower harmonic 
;
;e.g. there is a node of the fourth harmonic at 2/4 the scale length, but this is also a node of the second harmonic (1/2)
;
;to find the nodes that correspond to the fourth harmonic: 
;'( (* 1/4 scale-length)(* 2/4 scale-length)(* 3/4 scale-length) )
;discard (* 2/4 scale-length) since it is equal to (* 1/2 scale-length)
;which corresponds to the second harmonic
;
;the function below makes use of lisp's automatic simplification of ratios
;to discard ratios which are reducible, and therefore belong to a lower harmonic
;(denominator 2/4) returns 2, the denominator of the ratio after it has been reduced
;
;the distance (from the nut) for node n of harmonic h is (* scale-length (/ n h))
;if (/ n h) can be reduced to a fraction with a denominator that is less than h ...
;this node belongs to a lower harmonic

(defun harmonic-node-distances (harmonic scale-length &optional (counter 0))
"returns a list of locations of harmonic nodes, expressed as distance from nut"
  (if (>= counter harmonic)()
    (if (< (denominator (/ counter harmonic)) harmonic);if the fraction counter/harmonic is reducible
      (harmonic-node-distances harmonic scale-length (1+ counter));recurse without consing anything
      (cons (float (* scale-length (/ counter harmonic)));otherwise, cons the distance for this node, 
            (harmonic-node-distances harmonic scale-length (1+ counter))))));then recurse
;(harmonic-node-distances 8 650)
;(harmonic-node-distances 13 650)


;or, if you don't have your ruler handy...


(defun harmonic-node-distances-in-frets (harmonic &optional (counter 0))
"returns a list of locations of harmonic nodes, expressed as 12-edo decimal fret numbers"
  (if (>= counter harmonic)()
    (if (< (denominator (/ counter harmonic)) harmonic);if the fration counter/harmonic is reducible
      (harmonic-node-distances-in-frets harmonic (1+ counter));recurse without consing anything
      (cons (node-distance-in-decimal-frets counter harmonic);otherwise cons the distance for this node (in decminal frets)
            (harmonic-node-distances-in-frets harmonic (1+ counter))))));and recurse
;(harmonic-node-distances-in-frets 5)
;(harmonic-node-distances-in-frets 7)
;(harmonic-node-distances-in-frets 11)


;http://en.wikipedia.org/wiki/Guitar_harmonics
(defun node-distance-in-decimal-frets (node-number harmonic) 
  (log (/ harmonic (- harmonic node-number)) (expt 2 (/ 1 12))))
;(node-distance-in-decimal-frets 1 3)

;check this
;'((harmonic-of-ratio-one harmonic-of-ratio-two)...)
(defun harmonics-in-common (ratio-one ratio-two &optional (limit 13))
"lists harmonics (up to limit) in common between ratio-one and ratio-two"
(intersection (harmonics-of-ratio ratio-one limit)(harmonics-of-ratio ratio-two limit)))
;(harmonics-in-common 5/1 3/1)
;(harmonics-in-common 12/7 15/14)
;(harmonics-in-common 1 3/2)



#|
make:
(defun shared-harmonics (ratio-list limit)
"find harmonics that are shared by any members of ratio-list"

(defun find ratio-in-harmonic-series-of-strings (ratio '(string-ratio-list))
"returns string-ratio and harmonic number of any occurence of ratio in the harmonic series of any ratio in string-ratio-list"
|#


(defun gamut-of-nodes (string-ratio-list &optional (limit 9))
"returns the list of the ratios of the harmonics that are available 
on a set of strings tuned to the ratios of string-ratio-list"
  (sort (remove-duplicates
   (if (null string-ratio-list)()
     (append (harmonics-of-ratio (first string-ratio-list) limit)
             (gamut-of-nodes (rest string-ratio-list))))) '<))
;(gamut-of-nodes '(1 5/3 9/4 3/1 15/4 5/1) 5)
;(ratios-to-cents (gamut-of-nodes '(1 5/3 9/4 3/1 15/4 5/1)))

(defun harmonics-of-ratio (ratio limit &optional (counter 2))
"returns the integer multiples of ratio, up to limit"
  (if (> counter limit)()
    (cons (* ratio counter)
          (harmonics-of-ratio ratio limit (1+ counter)))))
;(harmonics-of-ratio 3 13)
;(harmonics-of-ratio 4/3 9)
;(harmonics-of-ratio 7/4 9)

(defun sub-harmonics-of-ratio (ratio limit &optional (counter 2))
"returns the reciprocals of the integer multiples of ratio, up to limit"
  (if (> counter limit)()
    (cons (/ ratio counter)
          (sub-harmonics-of-ratio ratio limit (1+ counter)))))
;(sub-harmonics-of-ratio 1 9)
;(sub-harmonics-of-ratio 7/4 13)

;;;;;fret location;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rational-fret-locator (ratio scale-length &optional (string-ratio 1/1))
"returns fret distance from nut for the given ratio"
  (float (- scale-length (* (/ (octave-reduce string-ratio) ratio) scale-length))))
;(rational-fret-locator 7/4 650 3/2)

;needs to do mod 1200 so string cents can be greater than cents and still return positive distance
(defun temperamental-fret-locator (cents scale-length &optional (string-cents 0))
"returns fret distance from nut for the given cents value"
(- scale-length (* scale-length (/ 1 (expt 2 (/ (- cents string-cents) 1200))))))
;(temperamental-fret-locator 701.9 650 498)


(defun n-edo-fret-locator (fret-number scale-length &optional (n 12))
"returns fret distance from nut in an n-edo system (defaulting to 12-tet if n is not specified)"
  (- scale-length (* (/ 1 (expt 2 (/ fret-number n))) scale-length)))
;(n-edo-fret-locator 3 650 31)


(defun n-ed-x-fret-locator (fret-number scale-length &optional (n 12) (x 2))
"returns fret distance from nut in an n-ed-x system (defaulting to 12-ed2 if n, x are not specified)"
  (- scale-length (* (/ 1 (expt x (/ fret-number n))) scale-length)))
;(n-ed-x-fret-locator 3 650 13 3)

;check this
(defun saddle-compensation (scale-length cents-off-at-octave)
  "returns the amount of saddle compensation needed to match octave harmonic with fretted octave"
  (- (float (* (cents-to-ratio cents-off-at-octave) scale-length)) scale-length))
;(saddle-compensation 592 7)

(defun n-edo-fretmap (scale-length fingerboard-length &optional (cardinality 12) (fret-counter 1) (length-counter 0))
  "returns the list of fret distances from the nut for the given n-equal-divisions-of-the-octave system"
  (if (>= length-counter fingerboard-length)()
    (cons 
     (n-edo-fret-locator fret-counter scale-length cardinality);calculate distance for this fret
     (n-edo-fretmap ;recurse...
      scale-length 
      fingerboard-length
      cardinality
      (1+ fret-counter);increment fret number
      (n-edo-fret-locator (1+ fret-counter) scale-length cardinality);update length-counter to test exit condition upon recursion
      ))))
;(n-edo-fretmap 650 440 12)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Miscellaneous;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun dancing-problem (n)
"calculates the number of possible pairings among a group of n members"
  (/ (- (expt n 2) n) 2))
;(dancing-problem 7)


(defun sum-list (number-list &optional (tally 0))
  "sums the elements in the list"
  (if (null number-list) tally
    (sum-list (rest number-list) (+ tally (first number-list)))))
;(sum-list '(1 2 3 4))  


(defun mean-of-list (number-list)
  "returns the arithmetic mean of a list of numbers"
  (float (/ (sum-list number-list) (length number-list))))
;(mean-of-list '(1 2 3 4))

;a helper function for tenney-hd-of-modes
(defun rotate-list (the-list)
  "moves the first item in list to the end of the list"
  (append (rest the-list) (list (first the-list))))   
;(rotate-list '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8))

(defun subtract-lists (list-1 list-2)
"returns the items in list-1 that are not in list-2"
  (if (null list-1)()
    (if (not (search (list (first list-1)) list-2));if this item in list-1 is not also in list-2
      (cons (first list-1);cons it
            (subtract-lists (rest list-1) list-2));and recurse
      (subtract-lists (rest list-1) list-2))));otherwise recurse without consing it
;(subtract-lists '(0 1 2 3 4 5 6 7 8 9 10) '(1 3 5 7 9))

(defun list-first-elements (list-of-lists)
 "takes a list of lists and returns a list of the first elements of each of the sublists"
 (if (null list-of-lists)()
   (cons (first (first list-of-lists))
         (list-first-elements (rest list-of-lists)))))
;(list-first-elements '((0 4 8 12)(1 5 9 13)(2 6 10 14)(3 7 11 15)))

(defun list-common-elements (list-1 list-2)
"lists the common elements between two lists"
  (if (null list-1)()
    (if (search (list (first list-1)) list-2);if first item in list-1 is also in list-2
      (cons (first list-1);cons it to the list
            (list-common-elements (rest list-1) list-2));then recurse
      (list-common-elements (rest list-1) list-2))));if no match, recurse without consing anything
;(list-common-elements *partch-scale* (mode-of-ratio-list *partch-scale* 11/8))

(defun list-common-elements-of-sublists (list-of-lists &optional (counter 0))
"lists any elements that all sublists of list-of-lists have in common"
  (if (>= counter (length (first list-of-lists))) ();if we've tested all the elements in the first list, we're done
    (if (in-all-lists (nth counter (first list-of-lists)) list-of-lists);if this element is in all the other lists...
      (cons (nth counter (first list-of-lists));cons it
            (list-common-elements-of-sublists list-of-lists (1+ counter)));and recurse
      (list-common-elements-of-sublists list-of-lists (1+ counter)))));otherwise, recurse without consing            
;(list-common-elements-of-sublists '((1 2 3 4)(2 4 6 8)(2 4 8 16)))

(defun in-all-lists (element list-of-lists)
"returns true if element is in all sublists of list of lists"
  (if (null list-of-lists) t;if we've made it all the way to the end of list-of-lists, return true
    (if (search (list element) (first list-of-lists));search list for element
      (in-all-lists element (rest list-of-lists));if it's there, recurse
      ();otherwise return nil
      )))
;(in-all-lists 4 '((1 2 3 4)(2 4 6 8)(2 4 8 16)))
;(in-all-lists 4 '((1 2 3 4)(2 4 6 8)(3 6 9 12)))

(defun list-first-elements (list-of-lists)
 "takes a list of lists and returns a list of the first elements of each of the sublists"
 (if (null list-of-lists)()
   (cons (first (first list-of-lists))
         (list-first-elements (rest list-of-lists)))))

(defun second-element-greater (list-1 list-2)
"returns true if second element of list-1 is greater than second element of list-2"
  (if (> (second list-1) (second list-2)) t))

(defun second-element-lesser (list-1 list-2)
"returns true if second element of list-1 is greater than second element of list-2"
  (if (< (second list-1) (second list-2)) t))

(defun nth-element-greater (list-1 list-2 n)
"returns true if nth element of list-1 is greater than nth element of list-2"
  (if (> (nth n list-1) (nth n list-2)) t))
;(nth-element-greater '(1 2 3) '(0 2 3) 0)

(defun collapse (list-of-lists)
"collapses a list of lists to a single list"
  (if (null list-of-lists)()
    (append (first list-of-lists)
            (collapse (rest list-of-lists)))))
;(collapse '((1 2 3)(5 7)(6 8 9)))

;http://www.wikihow.com/Check-if-a-Number-Is-Prime
(defun primep (n)
"tests if a number is prime"
  (if (and (oddp n)(trial-division n)) t ()))
;(primep 21317)
;(primep 5729)
;
;a helper for primep
;does not check for evenness, just for use within primep
(defun trial-division (n &optional (counter 3))
"tests if an odd number is prime"
  (if (> counter (ceiling (sqrt n)))t;if we've tried everything less than sqrt of n, return t
    (if (zerop (rem n counter))();otherwise, if n is divisible by counter, return nil
      (trial-division n (+ 2 counter)))));otherwise, recurse and try the next odd divisor (ideally, this should be the next prime divisor)
;(trial-division 3)
;(trial-division 15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mean (a b)
  (/ (+ a b) 2))

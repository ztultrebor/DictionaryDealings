;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname dictionarydealings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)



; data definitions

; A Dictionary is a [ListOf String]
; it is presumed to already be sorted alphabetically
#;;
(define (fn-wit-dictionary d)
  (cond
    [(empty? d) ...]
    [else ... (first d) ... (fn-wit-dictionary (rest d))]))


(define-struct letter-count [letter count])
; A LetterCount is a [1String Natural]
#;
(define (fn-wit-letter-count lc)
  (... (fn-on-1str (letter-count-letter lc))
       ... (fn-on-natural (letter-count-count lc))))


; A [ListOf LetterCount] is one of
;     - '()
;     - (cons LetterCount [ListOf LetterCount])
#;
(define (fn-wit-lolc llc)
  (cond
    [(empty? llc) ...]
    [else  (fn-wit-letter-count (first llc)) ... (fn-wit-lolc (rest llc))]))



; constants

(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))



; abstract functions

(define (recurse-over-dict dicto f-changes f-sameness)
  ; [ListOf String]
  ; [String [ListOf X] -> [ListOf X]]
  ; [String [ListOf X] -> [ListOf X]]
  ; -> [ListOf X]
  ; assembles a [ListOf X] associated with a given Dictionary
  (cond
    [(empty? (rest dicto)) (f-changes (first dicto) '())]
    [else (local (
                  (define list-of-results
                    ; this is the [ListOf X], constructed from right to left
                    (recurse-over-dict (rest dicto) f-changes f-sameness)))
            ; - IN -
            (cond
              [(starts-with=? (first dicto) (second dicto))
               (f-sameness (first dicto) list-of-results)]
              [else (f-changes (first dicto) list-of-results)]))]))


(define (starts-with=? str1 str2)
  ; String String -> Boolean
  ; determines if two strings srart with the same letter
  (string-ci=? (string-ith str1 0) (string-ith str2 0)))
; checks
(check-expect (starts-with=? "walla" "washington") #t)
(check-expect (starts-with=? "a" "beta") #f)
(check-expect (starts-with=? "the" "Thing") #t)



; functions

(define (starts-with# c dicto)
  ;Letter Dictionary -> Natural
  ; counts how many words in Dictionary begin with Letter
  (cond
    [(empty? dicto) 0]
    [(string-ci<? c (string-ith (first dicto) 0)) 0]
    [(string-ci=? c (string-ith (first dicto) 0))
     (+ 1 (starts-with# c (rest dicto)))]
    [else (starts-with# c (rest dicto))]))
(check-expect (starts-with# "c" (list "cat" "cheese" "dog" )) 2)
(check-expect (starts-with# "c" (list "cheese" "cat" "dog" )) 2)
(check-expect (starts-with# "c" (list "cat" "dog" "cheese")) 1)
(check-expect (starts-with# "c" (list "dog" "cat" "cheese")) 0)


(define (most-frequent dicto)
  ; [ListOf String] -> LetterCount
  ; determines the number of words that start with of the most
  ;     common initial letter
  (max-count (count-by-letter dicto)))


(define (count-by-letter dicto);
  ;[ListOf String] -> [ListOf LetterCount]
  ; count the number of words in dictionary that start with each
  ; letter of the alphabet, and output the result as a list of letter-counts
  (recurse-over-dict dicto new-letter-count add-to-letter-count))
; checks
(check-expect (count-by-letter (list "a"))
              (list (make-letter-count "a" 1)))
(check-expect (count-by-letter (list "a" "ab" "abc"))
              (list (make-letter-count "a" 3)))
(check-expect (count-by-letter (list "a" "ab" "Abc" "b" "bb"))
              (list (make-letter-count "a" 3) (make-letter-count "b" 2)))


(define (words-by-leading-letter dicto)
  ; [ListOf String]
  ; [String [ListOf ListOf String] -> [ListOf ListOf String]]
  ; [String [ListOf ListOf String] -> [ListOf ListOf String]]
  (recurse-over-dict dicto init-subdictionary compile-subdictionaries))
; checks
(check-expect (words-by-leading-letter (list "a"))
              (list (list "a")))
(check-expect (words-by-leading-letter (list "a" "ab"))
              (list (list "a" "ab")))
(check-expect (words-by-leading-letter (list "a" "ab" "abc"))
              (list (list "a" "ab" "abc")))
(check-expect (words-by-leading-letter (list "a" "ab" "Abc" "b" "bb"))
              (list (list "a" "ab" "Abc") (list "b" "bb")))


(define (new-letter-count word llc)
  ; String [ListOf LetterCount] -> [ListOf LetterCount]
  ; initialize a new LetterCount object on transitioning from one
  ; starting letter to the previous
  (cons (make-letter-count
         (string-downcase (string-ith word 0)) 1) llc))


(define (add-to-letter-count _ llc)
  ; String [ListOf LetterCount] -> [ListOf LetterCount]
  ; increases the count of the first LetterCount by 1
  (cons (make-letter-count
         (letter-count-letter (first llc))
         (+ (letter-count-count (first llc)) 1))
        (rest llc)))


(define (init-subdictionary word llc)
  ; String [ListOf ListOf String] -> [ListOf ListOf String]
  (cons (list word) llc))


(define (compile-subdictionaries word llc)
  ; String [ListOf ListOf String] -> [ListOf ListOf String]
  ; adds a String entry to the leading dictionary
  (cons (cons word (first llc)) (rest llc)))


(define (max-count llc)
  ; [ListOf LetterCount] -> LetterCount
  ; extracts the maximum LetterCount according to count
  (cond
    [(empty? (rest llc)) (first llc)]
    [(> (letter-count-count (first llc))
        (letter-count-count (max-count (rest llc)))) (first llc)]
    [else (max-count (rest llc))]))
; checks
(check-expect (max-count
               (list (make-letter-count "a" 5)
                     (make-letter-count "c" 1)
                     (make-letter-count "b" 7)))
              (make-letter-count "b" 7))



; actions

(most-frequent AS-LIST)
(length (first (words-by-leading-letter AS-LIST)))
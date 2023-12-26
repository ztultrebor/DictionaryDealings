;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname dictionarydealings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A Dictionary is a List-of-strings.
#;;
(define (fn-wit-dictionary d)
  (cond
    [(empty? d) ...]
    [else ... (first d) ... (fn-wit-dictionary (rest d))]))


; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
(define (letter? c)
  (member? c LETTERS))
; checks
(check-expect (letter? "h") #t)
(check-expect (letter? " ") #f)
(check-expect (letter? 8) #f)
(check-expect (letter? "ph") #f)
(check-expect (letter? "M") #f)
#;
(define (fn-wit-letter c)
  (cond
    [(not (letter? c)) (error "not a letter")]
    [else ... c]))


(define-struct letter-count [letter count])
; A LetterCount is a (make-letter-count [Letter Natural])

; A ListOfLetterCounts is one of
;     - (list)
;     - (cons LetterCount ListOfLetterCounts)
(define (list-of-letter-counts? llc)
  (or
   (empty? llc)
   (and
    (string? (letter-count-letter (first llc)))
    (number? (letter-count-count (first llc)))
    (list-of-letter-counts? (rest llc)))))
#;
(define (fn-wit-letter-count llc)
  (cond
    [(empty? llc) ...]
    [else  (letter-count-letter (first llc)) (letter-count-count (first llc))
           ... (fn-wit-letter-count (rest llc))]))


; abstract functions


(define (recursive dicto f1 f2)
  ; [ListOf String] -> [ListOf X]
  ; assembles a [ListOf X] associated with a given Dictionary
  (local (
          (define list-of-results
            ; this is the [ListOf X], constructed from right to left
            (cond
              [(empty? (rest dicto)) '()]
              [else (recursive (rest dicto) f1 f2)])))
    ; - IN -
    (cond
      [(empty? (rest dicto)) (f1 (first dicto) list-of-results)]
      [(starts-with=? (first dicto) (second dicto))
       (f2 (first dicto) list-of-results)]
      [else (f1 (first dicto) list-of-results)])))



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


(define (count-by-letter dicto)
  ; !!! abstract this!
  ; Dictionary -> ListOfLetterCounts
  ; assembles a ListOfLetterCounts associated with a given Dictionary
  (cond
    ; We begin construction of the ListOfLetterCounts only after completely
    ;     recursing through the Dictionary. The first step is to initialize
    ;     the LetterCount for the leading (lowercase) letter of the last
    ;     element of the Dictionary and set its count to 1.
    [(empty? (rest dicto))
     (list (make-letter-count
            (string-downcase (string-ith (first dicto) 0)) 1))]
    ; If the leading letter of the first and second elements remaining in the
    ;     Dictionary are equal, we simply increment the count of the active
    ;     LetterCount by 1. We do this in an auxiliary function, and1
    [(starts-with=? (first dicto) (second dicto))
     (and1 (count-by-letter (rest dicto)))]
    ; Else the leading letter of the first and second elements remaining
    ;     in the Dictionary are different. We now need to close out the
    ;     current LetterCount and initialize a new one for the prior
    ;     (lowercase) letter in reverse alphabetical order,
    ;     setting its count to 1
    [else  (cons (make-letter-count
                  (string-downcase (string-ith (first dicto) 0)) 1)
                 (count-by-letter (rest dicto)))]))
; checks
(check-expect (count-by-letter (list "a"))
              (list (make-letter-count "a" 1)))
(check-expect (count-by-letter (list "a" "ab" "abc"))
              (list (make-letter-count "a" 3)))
(check-expect (count-by-letter (list "a" "ab" "Abc" "b" "bb"))
              (list (make-letter-count "a" 3) (make-letter-count "b" 2)))


(define (most-frequent dicto)
  ; Dictionary -> LetterCount
  ; determines the number of words that start with of the most
  ;     common initial letter
  (max-count (count-by-letter dicto)))


(define (words-by-leading-letter dicto)
  ; [ListOf String]
  ; [String [ListOf ListOf String] -> [ListOf ListOf String]]
  ; [String [ListOf ListOf String] -> [ListOf ListOf String]]
  (recursive dicto in init-subdictionary compile-subdictionaries))
; checks
(check-expect (words-by-leading-letter (list "a"))
              (list (list "a")))
(check-expect (words-by-leading-letter (list "a" "ab"))
              (list (list "a" "ab")))
(check-expect (words-by-leading-letter (list "a" "ab" "abc"))
              (list (list "a" "ab" "abc")))
(check-expect (words-by-leading-letter (list "a" "ab" "Abc" "b" "bb"))
              (list (list "a" "ab" "Abc") (list "b" "bb")))


(define (init-subdictionary word llc)
  ; String [ListOf ListOf String] -> [ListOf ListOf String]
  (cons (list word) llc))


(define (compile-subdictionaries word llc)
  ; String [ListOf ListOf String] -> [ListOf ListOf String]
  ; adds a String entry to the leading dictionary
  (cons (cons word (first llc)) (rest llc)))


(define (starts-with=? str1 str2)
  ; String String -> Boolean
  ; determines if two strings srart with the same letter
  (string-ci=? (string-ith str1 0) (string-ith str2 0)))
; checks
(check-expect (starts-with=? "walla" "washington") #t)
(check-expect (starts-with=? "a" "beta") #f)
(check-expect (starts-with=? "the" "Thing") #t)


(define (and1 llc)
  ; ListOfLetterCounts -> ListOfLetterCounts
  ; increases the count of the first LetterCount by 1
  (cons (make-letter-count
         (letter-count-letter (first llc))
         (+ (letter-count-count (first llc)) 1))
        (rest llc)))

#;
(define (compile-subdictionaries word llc)
  ; ListOfDictionaries -> ListOfDictionaries
  ; adds a String entry to the leading dictionary
  (cons (cons word (first llc)) (rest llc)))


(define (max-count llc)
  ; ListOfLetterCounts -> LetterCount
  ; extracts the maximum LetterCount according to count
  (cond
    [(empty? (rest llc)) (first llc)]
    [(> (letter-count-count (first llc))
        (letter-count-count (max-count (rest llc)))) (first llc)]
    [else (max-count (rest llc))]))
(check-expect (max-count
               (list (make-letter-count "a" 5)
                     (make-letter-count "c" 1)
                     (make-letter-count "b" 7)))
              (make-letter-count "b" 7))



; constants
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))
(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))


; actions

(most-frequent AS-LIST)
(length (first (words-by-leading-letter AS-LIST)))
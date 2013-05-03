;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname uebung02-final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; cpat defines the compatibility (cpat) of one student with other student
;; name: symbol - name of the second student
;; cvalue: number - compatibility value between -5 and 5
(define-struct cpat (name cvalue))

;; student defines a student together with his/her list of compatibilities
;; name: symbol - name of the student
;; cpats: (listof cpat) - list of compatibilities of a student
(define-struct student (name cpats))

;; example values
(define datac0 (make-cpat 'John 2))
(define datac1 (make-cpat 'Arthur 5))
(define datac2 (make-cpat 'Peter 2))
(define datac3 (make-cpat 'Bernhard 1))
(define datac4 (make-cpat 'Peter 4))
(define datac5 (make-cpat 'John -2))

;; example students
(define Peter (make-student 'Peter (list datac0 datac1)))
(define John (make-student 'John (list datac2 datac3)))
(define Arthur (make-student 'Arthur (list datac4 datac5)))

;; 9.1 compare-cpat

;; contract: compare-cpat: cpat cpat -> cpat 
;; compares two cpat structures and gives the name back with the higher rating
;; example: (compare-cpat datac0 datac1)
;; = datac1
(define  (compare-cpat cpat1 cpat2)
  (cond
    [(and (empty? cpat1) (empty? cpat2)) (error 'compare-cpat "empty structures")]
    [(empty? cpat1) cpat2]
    [(empty? cpat2) cpat1]
    [else (cond
            [(> (cpat-cvalue cpat2) (cpat-cvalue cpat1)) cpat2]
            [else cpat1])]))



;; Tests for compare-cpat
(check-expect (compare-cpat empty datac0)
              datac0)
(check-error (compare-cpat empty empty)
             "compare-cpat: empty structures")
;; own tests for compare-cpat
(check-expect (compare-cpat datac0 empty)
              datac0)
(check-expect (compare-cpat datac5 datac3)
              datac3)

;; 9.2 best-cpat-in-list

;; contract: best-cpat-in-list: (listof cpat) -> cpat
;; consume a list of cpat structures and gives the cpat structure with the highest rating 
;; example: (best-cpat-in-list (list datac0 datac1 datac2))
;; = datac1
(define (best-cpat-in-list cpats)
  (cond
    [(empty? cpats) (error 'best-cpat-in-list "empty list")]
    [(empty? (rest cpats)) (first cpats)]
    [else (best-cpat-in-list (cons (compare-cpat (first cpats) (first (rest cpats))) (rest(rest cpats))))]))
    
;; Tests for best-cpat-in-list
(check-expect (best-cpat-in-list (list datac0 datac2))
              datac0)
(check-error (best-cpat-in-list empty)
             "best-cpat-in-list: empty list")
;; own tests for best-cpat-in-list
(check-expect (best-cpat-in-list (list datac0 datac1 datac2 datac3))
              datac1)
(check-expect (best-cpat-in-list (list datac5))
              datac5)

                    
;; 9.3 best-for-each

;; contract: best-for-each: (listof student) -> (listof (listof symbol symbol))
;; This procedure consumes a list of students and gives a list of a list of two symbols
;; example: (best-for-each (list Peter John Arthur)) 
;; = (list (list 'Peter 'Arthur) (list 'John 'Peter) (list 'Arthur 'Peter))
(define (best-for-each students)
  (cond
    [(empty? students) empty]
    [else (cons (list (student-name (first students))
                      (cpat-name (best-cpat-in-list (student-cpats (first students)))))
                (best-for-each (rest students)))]))
                
;; Tests for best-for-each
(check-expect (best-for-each empty) empty)
(check-expect (best-for-each (list John))
              (list (list 'John 'Peter)))
(check-expect (best-for-each (list Peter John Arthur))
              (list (list 'Peter 'Arthur)(list 'John 'Peter)(list 'Arthur 'Peter)))

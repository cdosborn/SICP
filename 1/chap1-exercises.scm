; Ex. 1.1
; 10
; 12
; 8
; 3
; 6
; 19
; #f
; 4
; 16
; 6
; 16
;
; Ex. 1.2
; (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3))))) (* 3 (- 6 2) (- 2 7)))
;
; Ex. 1.3
(define square (lambda (x) (* x x)))
(define (big-square-sum a b c)
  (cond ((and (> a b) (> c b)) (+ (square a) (square c)))
        ((and (> b a) (> c a)) (+ (square b) (square c)))
        (else (+ (square a) (square b)))))

; Ex. 1.4
; The evaluation of the compound expression (the if clause) results in an operator which is applied to the a and b operands.

; Ex. 1.5
; Normal-order evaluation will evaluate both operands, whereas applicative-order would delay evaluation of operands until needed. In applicative-order the evaluation of y would never occur, due to the if clause.

; Ex. 1.6
; It results in infinite recursion, because the operands are evaluated first.

(define (pow a b)
  (pow_ a b 1)) 

(define (pow_ a b cum) 
  (if (> b 0) (pow_ a (- b 1) (* cum a)) cum))

(define (close-enough? x guess n)
  (begin (display (abs (- (square guess) x)))
         (newline)
         (< (abs (- (square guess) x)) (/ 1.0 (pow 10 n)))))

(define (next x guess)
  (/ (+ (/ x guess) guess) 2))

(define (loop x guess n)
  (if (close-enough? x guess n) 
    guess
    (loop x (next x guess) n)))

(define (sqrt-to-n-decimals x n)
  (loop x 1.0 n))

(define (sqrt x) (sqrt-to-n-decimals x 3))

; Ex. 1.7
; Small numbers need a different tolerance than large numbers. (sqrt .001) is accurate only to the hundredth's place. The (sqrt 1000000000) is to large a number to probably care about accuracy to the hundredth's place. It accepts greater innaccuracy for large numbers.

(define (close-enough2? x guess)
  (begin (display (abs (- (square guess) x)))
         (newline)
         (< (abs (- (square guess) x)) (* .0001 x))))

(define (loop2 x guess)
  (if (close-enough2? x guess) 
    guess
    (loop2 x (next x guess))))

(define (sqrt2 x)
  (loop2 x 1.0))

;; Ex. 1.8
(define (close-enough3? x guess)
  (begin (display (abs (- (pow guess 3) x)))
         (newline)
         (< (abs (- (pow guess 3) x)) (* .0001 x))))

(define (next x guess)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

; Ex. 1.8
(define (loop3 x guess)
  (if (close-enough3? x guess) 
    guess
    (loop3 x (next x guess))))

(define (cbrt x)
  (loop3 x 1.0))

; Ex. 1.9
; 
; The first method (iterative):
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9

; The second method (recursive):
; (+ 4 5) 
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (+ 0 5))))
; (inc (inc (inc 5)))
; (inc (inc 6))
; (inc 8)
; 9

; Ex. 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) 
                 (A x (- y 1))))))

; (A 1 10)
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1))))))))))
;                                              2
;                                          2 * 2
;                                     2  * 2 * 2
;                                2 *  2  * 2 * 2
;                           2 *  2 *  2  * 2 * 2
; (A 2 4)
; (A 1 (A 1 (A 1 (A 1 1))))
;                 2
;           (A 0 (A 0 1))
;            4
;      (A 0 (...))
;      16
; (A 1 16)
; 65536
;           
; (A 1 10) = 2↑10 = 1024
; (A 2 4) = 2↑↑4 = 2↑(2↑(2↑2)) = 2^(2^(2^2))) = 2^2^2^2 = 2^16 = 65536
; (A 3 3) = 2↑↑↑3 = 2↑↑(2↑↑2) = 2↑↑(tower of 2's 2 high) = tower of 2's (a tower of 2's) high = 2^16

; (define (f n) (A 0 n)) = 2*n
; (define (g n) (A 1 n)) = 2^n
; (define (h n) (A 2 n)) = 2↑↑n = A "tower" of 2's n high. (2↑↑3 2^2^2)

; Ex 1.11
(define (f1 n)
  (if (< n 3) 
    n
    (+ (f1 (- n 1))
       (* 2 (f1 (- n 2)))
       (* 3 (f1 (- n 3))))))

(define (f2 n)
  (aux n 1 0))
 
(define (aux n factor sum)
  (if (< n 3)
    (+ sum (* factor n))
    (aux (- n 1) factor (aux (- n 2) (* 2 factor) (aux (- n 3) (* 3 factor) sum)))))

(define (f n) 
  (define (f-iter n a b c) 
    (if (< n 4) 
      (+ (* a (- n 1) ) 
         (* b (- n 2)) 
         (* c (- n 3))) 
      (f-iter (- n 1) (+ b a) (+ c (* 2 a)) (* 3 a)))) 
  (f-iter n 1 2 3))

; Ex 1.12
(define (pascal x y)
  (if (or (= x y) (= x 1))
    1
    (+ (pascal (- x 1) (- y 1)) (pascal x (- y 1)))))

; Ex 1.13
;
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; Ex. 1.14
(define (int-div a b)
  (if (< (- a b) 0)
    0
    (+ 1 (int-div (- a b) b))))
    
(define (steps-for-cc amount)
  (+ (pow 2 (- (int-div amount 50)))
     (pow 2  (int-div amount 25))
     (pow 2 (int-div amount 10))
     (pow 2 (int-div amount 5))
     (pow 2 amount)))

; Ex. 1.15
; a. 5
; b. (angle * 10 / 3 + 1) * 2 steps
; (angle * 10 / 3) space, 
;
;
(define (exp-fst b n)
  (cond ((= n 0) 1)
        ((= n 1) b)
        ((odd? n) (exp-fst (* (square b) b) (- n 3)))
        (else (exp-fst (square b) (/ n 2)))))

; Ex. 1.17
(define (double x)
  (+ x x))

(define (half x)
  (/ x 2))

; Ex. 1.18
(define (*-slw a b)
  (cond ((= b 0) 0)
        ((= b 1) a) 
        ((odd? b) (+ a (*-slw a (- b 1))))
        (else (*-slw (double a) (half b)))))

(define (*-fst a b)
    (define (aux a b inv)
      (cond ((= b 0) 0)
            ((= b 1) (+ a inv))
            ((odd? b) (aux a (- b 1) (+ inv a)))
            (else (aux (double a) (half b) inv)))) 
    (aux a b 0))

; Ex. 1.19

(define (fib n)
    (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a b 
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; Ex. 1.20

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

; (gcd a b)    = (gcd b r) 
;
; Applicative Approach
; (gcd 206 40) =
; (gcd 40 6)   
; (gcd 6 4) 
; (gcd 4 2)
; (gcd 2 0)
; 2
;
; remainder steps: 4
;
; Normal-Order Approach (Substitutive)
; (gcd 206 40) -> (gcd 40 (remainder 206 40)) A=0,B=1,R=1
; (gcd 40 6) -> (gcd (remainder 206 40) (remainder 40 (remainder 206 40))) A=1,B=2,R=3
; (gcd 6 4) -> (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) A=2,B=4,R=6
; (gcd 4 2) -> A=4,B=7,R=11
; (gcd 2 0) ->
;
; remainder steps: 14 + 4
;
; Ex. Towers of Hanoi 
(define (move n from to spare)
  (cond ((and (= from 0) (= spare 0)) 

;; 1.1.4 合成手続き
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;; 1.1.6 条件式と述語
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y)
  (or (> x y) (= x y)))

(define (>= x y)
  (not (< x y)))

;; 1.1.7 ニュートン法による平方根
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) .001))
(define (sqrt x)
  (sqrt-iter 1 x))

;; 1.1.8 ブラックボックス抽象化としての手続き
(define (square x) (* x x))
(define (square x)
  (exp (double (log x))))
(define (double x) (+ x x))

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) .001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1 x))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) .001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1))

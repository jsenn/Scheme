(define (identity x) x)

(define (curry f . args)
  (lambda (next . rest)
    (apply f (concat (append args next) rest))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (flip f)
  (lambda (x y)
    (f y x)))

(define not (curry eqv? #f))
(define null? (curry eqv? '()))

(define (foldr f end xs)
  (if (null? xs)
    end
    (f (car xs) (foldr f end (cdr xs)))))

(define (foldl f start xs)
  (if (null? xs)
    start
    (foldl f (f start (car xs)) (cdr xs))))

(define fold foldl)
(define reduce fold)

(define (unfold f start pred)
  (if (pred start)
    (cons init '())
    (cons init (unfold f (f init) pred))))

(define (map f xs) (foldr (lambda (x y)
                            (cons (f x) y))
                          '()
                          xs))

(define (filter pred xs)
  (foldr (lambda (x y)
           (if (pred x)
             (cons x y)
             y))
         '()
         xs))

(define (remove pred xs) (filter (compose not pred) xs))

(define (list . things) things)
(define length (curry fold (lambda (x y) (inc x)) 0))
(define reverse (curry foldl (flip cons) '()))

(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define nonzero? (compose not zero?))
(define nonpositive? (compose not positive?))
(define nonnegative? (compose not negative?))

(define inc (curry + 1))
(define dec (curry (flip -) 1))
(define (abs n) (if (positive? n) n (* -1 n)))
(define (! n) (fold * 1 (range 1 (inc n))))

(define (even? n)
  (= 0 (mod n 2)))

(define odd? (compose not even?))

(define (max first . rest)
  (fold (lambda (old new)
          (if (> old new) old new))
        first
        rest))

(define (min first . rest)
  (fold (lambda (old new)
          (if (< old new) old new))
        first
        rest))


"PROBLEMA 1 -- Suma nr. lista. "
(defun adder (lis)
  (if (null lis)
    0
    (let ((c (car lis)))
      (if (numberp c)
        (+ c (adder (cdr lis)))
        (adder (cdr lis))))))
      

(print (adder '(5 3 4 A 2 1)))


"PROBLEMA 2 -- Avg nr. lista. "


(defun count (lis)
  (if (null lis)
    0
    (let ((c (car lis)))
      (if (numberp c)
        (+ 1 (count (cdr lis)))
        (count (cdr lis))))))

(defun avg(l)
 (/ (adder l) (count l) )
)

(print (avg '(5 3 4 A 2 1)))


"PROBLEMA 3 -- Sterge prima aparitie a unui element. "

(defun remove-first(element list (a 0))
(cond 
((null list) 'nil)
((and (= a 0) (eq (car list) element)) (remove-first element (cdr list) (+ a 1)))
(t (cons (car list) (remove-first element (cdr list) a)))

)

)

(print (remove-first 7 '(5 6 7 4 3 2 1 5 3 2 1 6 7) 0))




"PROBLEMA 4 -- Lista cu elementele de pe pozitiile pare. "

"(defun anyoddp (x)
(cond 
((null x) 'nil)
((oddp (first x)) (cons(car x)(anyoddp(cdr x))))
(t (anyoddp (cdr x)))
)
)


(print (anyoddp  '(2 4 6 2 1 7 5 3)))"

(defun check-all-oddp (n (a 0))

   (cond ((null n) nil)
         ((oddp a )  (cons (car n)(check-all-oddp (cdr n) (+ a 1 ))))
         (t (check-all-oddp (cdr n) (+ a 1)))
    )
)

(print (check-all-oddp '(5 6 3 2 1 6 4 23 1) 0))

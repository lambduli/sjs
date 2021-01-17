(import `(fn fun str) "./module.sjs")
(export omega "some string")

;; (import `(foo bar baz) "./jsmodule.mjs");;
 
;; single line comment;;
;; also this ^^;;


(define sumlists (lista listb)
  ( if  (null lista)
        nil
        (cons (* (car lista) (car listb))
              (sumlists (cdr lista) (cdr listb))
        )
  )
)

(define fact (n)
  (if (or (= n 0) (= n 1))
    1
    (* n (fact (- n 1)))
  )
)

(define map (list fn)
  (if (null list)
    nil
    (cons (fn (car list)) (map (cdr list) fn))
  )
)


(console.log "(let ((foo (+ 1 2)) (bar (* 3 4))) (+ foo bar))")
(println (let ((foo (+ 1 2)) (bar (* 3 4))) (+ foo bar)))

;; (println (letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))) )))) (fact 3) ));;

(console.log "(sumlists `(1 2 3 4) `(3 2 1 5) )")
(println (sumlists `(1 2 3 4) `(3 2 1 5) ))

(console.log "(and true (or false true))")
(println (and true (or false true)))

(console.log "(map `(1 2 3 4 5) (lambda (n) (* n n)))")
(println (map `(1 2 3 4 5) (lambda (n) (* n n))))

(console.log "(fact 5)")
(println (fact 5))

(console.log "`(1 2 3 4 ,(* 6 (+ 2 3)))")
(println `(1 2 3 4 ,(* 6 (+ 2 3))))

(console.log "`(1 2 3 4 ,(* 6 (+ 2 `(23 24 25))))")
(println `(1 2 3 4 ,(* 6 (+ 2 `(23 24 25)))))

(console.log "`(,(fn 2 3) ,(fun 2 3)")
(println `(,(fn 2 3) ,(fun 2 3)))

(console.log "str")
(println str)

(console.log "END")
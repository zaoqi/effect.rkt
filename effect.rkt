;;  Copyright (C) 2017  Zaoqi

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Affero General Public License for more details.

;;  You should have received a copy of the GNU Affero General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#lang racket
(module s racket
  (provide (rename-out [structt struct]))
  (define-syntax-rule (structt x ...)
    (struct x ... #:transparent)))
(require 's)
(struct op (op v))
(struct handlev (ops ret bind))
(struct >>= (x f))
(struct return (v))

(define (run h s x)
  (cond
    [(op? x) (%run h s x return)]
    [(>>=? x) (%run h s (>>=-x x) (>>=-f x))]
    [(return? x) (return ((handlev-ret h) s (return-v x)))]))
(define (%run h s x f)
  (cond
    [(op? x) (if (set-member? (handlev-ops h) (op-op x))
                 ((handlev-bind h) s x (λ (s1 x1) (run h s1 (f x1))))
                 (>>= x (λ (x1) (run h s (f x1)))))]
    [(>>=? x) (%run h s (>>=-x x) (λ (x1) (>>= ((>>=-f x) x1) f)))]
    [(return? x) (run h s (f (return-v x)))]))
(define (run0 x)
  (if (>>=? x)
      (let ([x1 (run0 (>>=-x x))])
        (if (return? x1)
            (run0 ((>>=-f x) (return-v x1)))
            (>>= x1 (>>=-f x))))
      x))
(define (out x) (return-v (run0 x)))
(define-syntax do
  (syntax-rules (let <-)
    [(_ x) x]
    [(_ let x v s ...) (let ([x v]) (do s ...))]
    [(_ x <- v s ...) (>>= v (λ (x) (do s ...)))]
    [(_ v s ...) (>>= v (λ (x) (do s ...)))]))
(define (amb) (op 'amb '()))
(define ambh (handlev (set 'amb) (λ (s x) (list x))
                      (λ (s x cb)
                        (do
                            s1 <- (cb s #t)
                          s2 <- (cb s #f)
                          (return (append s1 s2))))))
(define (put x) (op 'put x))
(define (get) (op 'get '()))
(define state (handlev (set 'get 'put) (λ (s x) (cons s x))
                       (λ (s x cb)
                         (if (eq? (op-op x) 'get)
                             (cb s s)
                             (let ([ns (op-v x)])
                               (cb ns '()))))))
(define (update f)
  (do
      s <- (get)
    (put (f s))))
(run0 (run ambh '() (run state 5 (do
                                     x <- (amb)
                                   (update (λ (x) (+ x 1)))
                                   (if x
                                       (return 0)
                                       (return 1))))))

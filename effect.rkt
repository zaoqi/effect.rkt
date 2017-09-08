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
(require racket/hash)

(struct op (effect op args))
(struct handlev (id effects ret bind))
(struct >>= (x f))

(define (%run-handle h s x f)
  (cond
    [(>>=? x)
     (%run-handle h s (>>=-x x)
                  (λ (s2 x2 f2)
                    (f2 s2 (%run-handle h s2 ((>>=-f x) s2 x2) f))))]
    [(op? x)
     (if (set-member? (handlev-effects h) (op-effect x))
         ((handlev-bind h) s x f)
         (>>= x (λ (s2 x2)
                  (f (hash-union s s2) x2 (λ (x) x)))))]
    [else (f s x (λ (x) x))]))

(define es (hash 0 '()))
(define amb (handlev 0 (set 'amb) (λ (x) (list x)) (λ (s x f) (append (f s #t (λ (x) x)) (f s #f (λ (x) x))))))
(define tf (op 'amb 'tf '()))

(%run-handle amb es (>>= tf (λ (s x) (not x))) (handlev-ret amb))

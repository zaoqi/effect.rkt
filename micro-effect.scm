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

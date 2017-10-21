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

(effect
 (amb))
(define-handle runamb remuse
  [(return x) (list x)]
  [(amb) (do
             s1 <- (remuse #t)
           s2 <- (remuse #f)
           (return (append s1 s2)))])

(effect
 (put x)
 (get))
(define-handle state s remuse
  [(return x) (cons s x)]
  [(get) (remuse s s)]
  [(put x) (remuse x '())])
(define (update f)
  (do
      s <- (get)
    (put (f s))))

(effect
 (throw x))
(define (catch f v)
  ((handle remuse
          [(return x) x]
          [(throw x) (return (f x))]) v))

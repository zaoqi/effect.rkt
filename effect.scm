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
(define-record-type op
  (op v)
  op?
  (v op-v))
(define-record-type >>=
  (>>= x f)
  >>=?
  (x >>=-x)
  (f >>=-f))
(define-record-type return
  (return v)
  return?
  (v return-v))
(define-record-type handlev
  (handlev ops ret bind)
  handlev?
  (ops handlev-ops)
  (ret handlev-ret)
  (bind handlev-bind))
(define (set . xs) xs)
(define (set-member? xs x)
  (if (null? xs)
      #f
      (or (equal? (car xs) x)
          (set-member? (cdr xs) x))))
(load "micro-effect.scm")
(load "mini-effect.scm")
(load "effects.scm")

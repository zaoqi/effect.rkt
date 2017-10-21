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
(define-syntax do
  (syntax-rules (let <-)
    [(_ x) x]
    [(_ let x v s ...) (let ([x v]) (do s ...))]
    [(_ x <- v s ...) (>>= v (λ (x) (do s ...)))]
    [(_ v s ...) (>>= v (λ (x) (do s ...)))]))
(define-syntax effect
  (syntax-rules ()
    [(_) (void)]
    [(_ (o x ...) s ...)
     (begin
       (define (o x ...) (op (quote o) (list x ...)))
       (effect s ...))]))
(define-syntax handle
  (syntax-rules (return)
    [(_ state
        remuse
        [(return retx) rete]
        [(opn x ...) opv] ...)
     (let ([h (handlev (set (quote opn) ...)
                       (λ (state retx) rete)
                       (λ (state opx remuse)
                         (let ([o (op-op opx)])
                           (cond
                             [(eq? o (quote opn)) (apply (λ (x ...) opv) (op-v opx))] ...
                             [else (error 'handle)]))))])
       (λ (empty-state value)
         (run h empty-state value)))]
    [(_ remuse
        [(return retx) rete]
        [(opn x ...) opv] ...)
     (let ([h (handlev (set (quote opn) ...)
                       (λ (state retx) rete)
                       (λ (state opx remuse0)
                         (let ([o (op-op opx)] [remuse (λ (cbv) (remuse0 state cbv))])
                           (cond
                             [(eq? o (quote opn)) (apply (λ (x ...) opv) (op-v opx))] ...
                             [else (error 'handle)]))))])
       (λ (value)
         (run h '() value)))]))
(define-syntax define-handle
  (syntax-rules ()
    [(_ f x ...) (define f (handle x ...))]))

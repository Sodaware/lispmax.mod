;;; library.lisp --- Core lisp functionality for LispMax

;; Copyright (C) 2014         Phil Newton <phil@sodaware.net>

;; This file contains functionality required by LispMax.

;; Author: Phil Newton <phil@sodaware.net>
;; URL: http://www.sodaware.net/dev/blitz/lispmax/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this proglram.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; Defining things

(defmacro (defun name args docstring &rest body)
  `(define (,name ,@args) ,@body))


;; Core Logic Helpers

(defmacro (and a b)
  (list 'if a b nil))

(define (nand a b)
  (not (and a b)))

(define (or a b)
  (nand (nand a a) (nand b b)))

(define (not x)
  (if (eq? nil x) t nil))
 
(defmacro (when condition &rest body)
  `(if ,condition (progn ,@body) nil))

(defmacro (unless condition &rest body)
  `(if (not ,condition) (progn ,@body) nil))

;; Maths Helpers

(define (abs x)
  (if (< x 0) (- 0 x) x))

(define (zero? n)
  (= n 0))

(define (positive? n)
  (> n 0))

(define (negative? n)
  (< n 0))

(define (odd? n)
  (= (mod n 2) 1))

(define (even? n)
  (not (odd? n)))


;; List Helpers

(define (caar x)
  (car (car x)))

(define (cadr x)
  (car (cdr x)))

(define (length lst)
  (if (nil? lst)
      0
    (+ 1 (length (cdr lst)))))

(define (list . items)
  (foldr cons nil items))

(define (reverse list)
  (foldl (lambda (a x) (cons x a)) nil list))

(define (foldl proc init list)
  (if list
      (foldl proc
             (proc init (car list))
             (cdr list))
    init))

(define (foldr proc init list)
  (if list
      (proc (car list)
            (foldr proc init (cdr list)))
    init))

(define (reduce func init lst)
  (if lst
      (func (car lst)
            (reduce func init (cdr lst)))
    init))

(define (sum lst)
  (reduce + 0 lst))

(define (square x)
  (* x x))

(define (random-list-index list)
  (- (rand 1 (length list)) 1))

(define (random-list-item list)
  (nth (random-list-index list) list))

(define (assoc key records)
  (cond ((nil? records) false)
        ((eq? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (unary-map proc2 list)
  (foldr (lambda (x remaining) (cons (proc2 x) remaining))
         nil
         list))

(define (map proc . arg-list)
  (if (car arg-list)
      (cons (apply proc (unary-map car arg-list))
            (apply map (cons proc
                             (unary-map cdr arg-list))))
    nil))

(define (mapcar func arg-list)
  (if arg-list
    (cons (func (car arg-list)) 
          (mapcar func (cdr arg-list)))
    nil))

(define (append a b)
  (foldr cons b a))

(defmacro (funcall . form)
  form)

(defmacro (quasiquote x)
  (if (pair? x)
      (if (eq? (car x) 'unquote)
          (cadr x)
          (if (and (pair? (car x)) (eq? (caar x) 'unquote-splicing))
              (list 'append
                    (cadr (car x))
                    (list 'quasiquote (cdr x)))
              (list 'cons
                    (list 'quasiquote (car x))
                    (list 'quasiquote (cdr x)))))
      (list 'quote x)))


;; Loops

;(defmacro (something defs)
;  `(print ,(car defs)))

(defmacro (let defs &rest body)
  ;(print (map car defs)))
  (mapcar 'print defs))
  
  ;`((lambda ,(map car defs) ,@body)
  ;  ,@(map cadr defs)))

(defmacro (cond conds)
  (foldr
   (lambda (cnd acc)
     `(if ,(car cnd) ,(cadr cnd) ,acc))
   nil conds))

;(defmacro (cond &rest clauses)
 ; (when clauses
 ;   (let ((test1 (caar clauses))
 ;         (forms1 (cdar clauses)))
 ;     (if forms1
 ;         `(if ,test1
 ;              (progn ,@forms1)
 ;            (cond ,@(cdr clauses)))
 ;       (let ((tmp (gensym)))
 ;         `(let ((,tmp ,test1))
 ;            (if ,tmp
 ;                ,tmp
 ;              (cond ,@(cdr clauses)))))))))



;(define +
;  (let ((old+ +))
;    (lambda xs (foldl old+ 0 xs))))

;(defmacro (or_ a b) `(if ,a t (if ,b t nil)))
;(defmacro (or x y @args)
;  (foldr (\ (expr acc) `(or_ ,acc ,expr)) `(or_ ,x ,y) args))

;(defun < (x y) (. x < y))
;(defun = (x y) (. x == y))
;(defun /= (x y) (not (= x y)))
;(defun <= (x y) (or (< x y) (= x y)))
;(defun > (x y) (. x == y))
;(defun /= (x y) (not (= x y)))
;(defun <= (x y) (not (<= x y)))
;(defun >= (x y) (not (< x y)))
;(defun between? (x y z) (and (> x y) (< x z)))
;(defun nil? (x) (eq? x nil))
;(defun cons? (e) (. e is_a? (ruby Cons)))
;(defun atom? (e) (not (cons? e)))
;(defun eq? (x y) (. x equal? y))
;
;(defmacro until (cond body)
;  `(do (not ,cond) ,body))
;(defmacro cond (@conds)
;  (foldr
;   (lambda (cnd acc)
;     `(if ,(car cnd) ,(cadr cnd) ,acc))
;   nil conds))

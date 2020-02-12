;;; library.lisp --- Core lisp functionality for LispMax

;; Copyright (C) 2014 - 2019  Phil Newton <phil@sodaware.net>

;; This file contains functionality required by LispMax.

;; Author: Phil Newton <phil@sodaware.net>
;; URL: https://www.sodaware.net/lispmax/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


;; --------------------------------------------------
;; -- Boolean Logic

(defmacro (and a b)
  (list 'if a b nil))

(define (nand a b)
  (not (and a b)))

(define (not x)
  (if (eq? nil x) t nil))

;; This is required to make `when` and `unless` work.
(defmacro (-unused)
  (progn nil))


;; --------------------------------------------------
;; -- Math Functions

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

(define (square x)
  (* x x))

(define (<= x y)
  (not (> x y)))

(define (>= x y)
  (not (< x y)))

;; TODO: Move this to a builtin for better performance.
(define (! n)
  (if (= n 0)
      1
      (* n (! (- n 1)))))


;; --------------------------------------------------
;; -- List Functions

;; Create a new list containing ITEMS.
;; (define (list . items)
;;   (foldr cons nil items))

(define (acons x y a)
  (cons (cons x y) a))

(define (caar x)
  (car (car x)))

(define (cadr x)
  (car (cdr x)))

;; TODO: Move this to a built-in.
(define (length lst)
  (if (nil? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (reverse list)
  (foldl (lambda (a x) (cons x a)) nil list))

(define (foldl proc init list)
  (if list
      (foldl proc
             (proc init (car list))
             (cdr list))
      init))

(define (foldr func init lst)
  (if lst
      (func (car lst)
            (foldr func init (cdr lst)))
      init))

;; Apply FUNC to all items in COLLECTION and return the result
;; e.g. (map + (list 1 2 3) (list 1 2 3)) => (2 4 6)
(define (map func . collection)
  (if (car collection)
      (cons (apply func (unary-map car collection))
            (apply map (cons func
                             (unary-map cdr collection))))
      nil))

;; Applies FUNC to all items in COLLECTION and returns a list containing the results.
(define (mapcar func collection)
  (if collection
      (cons (func (car collection))
            (mapcar func (cdr collection)))
      nil))

;; Applies FUNC to all items in COLLECTION, and returns the original COLLECTION.
(define (mapc func collection)
  (if collection
      (cons (func (car collection))
            (mapc func (cdr collection)))
      nil)
  collection)

(define (reduce func init lst)
  (fast-foldr func init lst))

(define (sum lst)
  (reduce + 0 lst))

;; (define (remove-if condition list)
;;   (delq nil (mapcar (lambda (x)
;;                       (and (funcall condition x) x)) list)))

(define (random-list-index list)
  (- (rand 1 (length list)) 1))

(define (random-list-item list)
  (nth (random-list-index list) list))

(define (assoc key records)
  (if (nil? records)
      nil
      (if (eq? key (caar records))
          (car records)
          (assoc key (cdr records)))))

(define (cdr-assoc key records)
  (let ((property-pair (assoc key records)))
    (if property-pair
        (cdr property-pair)
        nil)))

;; ;; < this is nicer but much, MUCH slower >
;; (define (assoc-cond key records)
;;     (cond ((nil? records) nil)
;;           ((eq? key (caar records)) (car records))
;;           (T (assoc-cond key (cdr records)))))

;; Do we need this?
(define (unary-map proc2 list)
  (fast-foldr (lambda (x remaining)
                (cons (proc2 x) remaining))
              nil
              list))

;; Have to both be lists
(define (append a b)
  (fast-foldr cons b a))


;; --------------------------------------------------
;; -- Language Features

(defmacro (funcall . form)
  form)

;; TODO: Move this up to the compiler if possible...
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

(defmacro (let defs &rest body)
  `((lambda ,(mapcar car defs) ,@body)
    ,@(mapcar cadr defs)))

;; TODO: Would /really/ like to use fast-foldr here
(defmacro (cond &rest conds)
  (foldr
   (lambda (cnd acc)
     `(if ,(car cnd) ,(cadr cnd) ,acc))
   nil conds))


;;(define (+)
;;    (let ((old+ +))
;;      (lambda xs (foldl old+ 0 xs))))

;;(defun /= (x y) (not (= x y)))
;;(defun <= (x y) (or (< x y) (= x y)))
;; (defun /= (x y) (not (= x y)))

;;(defun between? (x y z) (and (> x y) (< x z)))
;;(defun cons? (e) (. e is_a? (ruby Cons)))
;;(defun atom? (e) (not (cons? e)))
;;(defun eq? (x y) (. x equal? y))

;;(defmacro until (cond body)
;;  `(do (not ,cond) ,body))


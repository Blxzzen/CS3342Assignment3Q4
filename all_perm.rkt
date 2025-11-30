; all_perm.rkt
#lang racket

; insert x into a sorted list, maintaining sorted order
(define (insert x lst)
  (cond
    ((null? lst) (list x))
    ((< x (car lst)) (cons x lst))
    (else (cons (car lst) (insert x (cdr lst))))))

; sort a list using insertion sort
(define (sort-list lst)
  (cond
    ((null? lst) '())
    (else (insert (car lst) (sort-list (cdr lst))))))

; remove the first occurrence of x from a list
(define (remove-one x lst)
  (cond
    ((null? lst) '()) ; x not found or empty list
    ((equal? x (car lst)) (cdr lst)) ; found x, return the rest
    (else (cons (car lst) (remove-one x (cdr lst))))))

; computes all permutations of a list in lexicographical order
(define (all_perm lst)
  (let ((sorted-lst (sort-list lst))) ; sort the input list
    (letrec ((permutations-helper ; recursive helper to generate permutations
               (lambda (current-list)
                 (cond
                   ((null? current-list) (list '())) ; permutation of an empty list is a list containing one empty list
                   (else
                    ; recursively find permutations of the list without that item
                    ; prepend the current item to each of those subpermutations
                    ; flatten all these results into a single list
                    (apply append
                           (map (lambda (item)
                                  (map (lambda (p) (cons item p))
                                       (permutations-helper (remove-one item current-list))))
                                current-list)))))))
      (permutations-helper sorted-lst))))

(all_perm '())
(all_perm '(2 3 1))
(all_perm '(1 2))

(ns kata6-anagrams.logic
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.string :as str]))

(defn anagram? [word1 word2]
  (run* [queryvar]
        (== queryvar word1)
        (== queryvar word2)))

;;; classical prolog permutation solution as from R.A. O'Keefe's "Craft of Prolog"
;;; permutation(Xs, Ys) :-
;;; 	permutation(Xs, Ys, Ys).
;;; permutation([],[],[]).
;;; permutation([X|Xs], Ys1, [_|Bound]) :-
;;; 	permutation(Xs, Ys, Bound),
;;; 	insert(Ys, X, Ys1).
;;; insert(L, X, [X|L]).
;;; insert([H|T], X, [H|L]) :-
;;; 	insert(T,X,L).

(defn insert [x l nl]
  (conde
   [(conso x l nl)]
   [(fresh [h t]
           (conso h t l)
           (conso h l nl)
           (insert x t l))]))


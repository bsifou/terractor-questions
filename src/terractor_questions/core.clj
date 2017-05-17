(ns terractor-questions.core
  (:require [clojure.test :refer :all])
  (:gen-class))

;; 1

(comment
  I think it was when I've implemented a web app
  https://github.com/bsifou/select-and-get-selected, last year as part
  of final course project. Officially I was only tasked to use
  the framework (Meteor at that time) that  we've learned during
  semester. Since I've already played with it before, I went ahead
  and learned an other library (React) that I liked it's approach and
  used that to implement the requirements, was fun. I'm glad all
  played well.)


;; 2

(comment
  Whenever I have time to spare, I prefer watching talks from
  great confereces that takes place in software world, like
  StrageLoop.  One talk really opend my eyes on how to approach
  software design is Rich Hickey talk "Design, Composition and
  Performance."  The basic premise is "Constraints fuel creativity"
  where he gives us examples from Music world and what we can learn
  from it. Musician uses his tool _instrucment_ to *make* music the
  same way software developper outght to use his tools _editor,
  languages, libraries_ to make what's important *software*, and not
  to be slowed or paralysed by them.)

;; 3

(comment
  So many great features, I think the ebay Store integration module is
  really cool. A lot of the users need t be target as much clients they
  can, and most of those clients their destination to look up products
  is Ebay. I like the idea of "leverage" and provide a steping stone for
  those users.)

;; 4

;; ----------------------------------
;; challenge #1
;; ----------------------------------


(defn my-flatten
  "Takes any nested combination of vectors and returns it as single flat vector"
  [v]
  (reduce (fn [result element]
            (if (sequential? element)
              ;; we use recursion to reduce inner vectors
              (into result (my-flatten element))
              (conj result element)))
          [] v))


;; example tests

(deftest test-my-flatten
  (testing "Testing my-flatten"
    (is (=  [1 2] (my-flatten [1 2])))
    (is (=  [1 2 3 4] (my-flatten [1 2 [3 4] []])))
    (is (=  [] (my-flatten [])))))

;; --------------------------------
;; challenge #2
;; --------------------------------


;; helper util functions

;; log2 of 8 is 3 (2^3 is 8)
(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

;; pow2 of 3 is 8 (2^3 is 8)
(defn pow2 [n]
  (Math/pow 2 n))


;; to get number x such that our (n <= 2^x)
(defn get-upper-power
  [n]
  (Math/floor (log2 n)))


;; from integer to character
;; 0 => \a
;; 3 => \c
(defn power->char
  [p]
  (char (+ p (int \a))))

;; from a character to number
;; \a => 0
;; \c => 3
(defn char->power
  [c]
  (- (int c) (int \a)))


;; our interface

(defprotocol  LocationNumbers
  (int->location-num [this])
  (location-num->int [this])
  (abbrev-location-number [this]))

;; our implementation

;; a private helper method
(defn- all-powers-of-two
  "Takes a number and returns sequence of powers of two that form it"
  [n]
  (let [p (get-upper-power n)]
    (when (pos? n)
      (lazy-seq
       (cons
        p
        (all-powers-of-two (- n (pow2 p))))))))


(defn int->location-num
  "Convert an integer to string rep of location number"
  [n]
  (->> n
       all-powers-of-two
       (map power->char)
       reverse
       (apply str)))

(defn location-num->int
  "Takes string rep of location number and convert it to integer"
 [s]
  (->> s
       (map char->power)
       (map pow2)
       (apply +)
       int))


(defn abbrev-location-number
  "Takes location number and abbreviates it"
 [s]
  (int->location-num (location-num->int s)))

;; example testing

(deftest test-location-numbers
  (testing "Testing conversion from integer to location number"
    (is (= "abcdefghijklmnopqrstuvwxyz" (int->location-num 67108863))
        "Doesn't handle up to 2^26 ")   ; helpfull error messages

    (is (= "ad" (int->location-num 9))))


  (testing "Testing conversion from location number to integer"
    (is (= 87 (location-num->int "abceg"))))


  (testing "Testing abbreviation"
    (is (= 8 (location-num->int (int->location-num 8))))
    (is (= "ad" (abbrev-location-number "abbc")))))

;; to run tests in our name-space

(run-tests)

(defn -main
  "Entry point"
  [& args]
  (run-tests))









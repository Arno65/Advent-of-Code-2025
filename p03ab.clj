;;;;    Advent of Code 2025 - Day 3 part One and Two
;;;;    https://adventofcode.com/2025/day/3
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    Part one: The total output joltage is: 17427
;;;;    Part two: The total output joltage is: 173161749617495
;;;;
;;;;    (cl) by Arno Jacobs, 2025-12-21
;;;;    

(ns p03ab
  (:require [clojure.string :as str]))

;;;; Read the data-set
(defn get-lines
  [file]
  (->> file
       slurp
       (str/split-lines)))

;;;; Convert a string with digits into a list of the numerical values of the digits 
(defn get-digits
  [line]
  (map #(- (int %) 48) line))

(def joltage-ratings (map get-digits (get-lines "./data/inputDay03_2025.txt")))

;;;; In this case the digits input need to be reversed
(defn reversed-digits-to-int
  [digits]
  (if (empty? digits)
    0
    (let [rds (* 10 (reversed-digits-to-int (rest digits)))]
      (+ (first digits) rds))))

;;;; The main function
(defn maxJoltage
  [dp mjs js]
  (if (= dp 0)
    (reversed-digits-to-int mjs)
    (let [take-length (- (+ (count js) 1) dp)         ;;; drop last little part of the digits
          jsp (take take-length js)
          mc  (reduce max jsp)                        ;;; get maximum digit value
          njs (cons mc mjs)                           ;;; create reversed  maximum digits list
          rjs (rest (drop-while #(not= mc %) js))]    ;;; get new list after maximum
      (maxJoltage (- dp 1) njs rjs))))

(defn work-day-3
  [joltages code-length]
  (reduce + (map #(maxJoltage  code-length '() %) joltages)))

;;; The 'main' program - - -
(defn program [] 
  (println "Advent of Code 2025 - day 3  (Clojure)") 
  (print   "Part one: The total output joltage is: ") 
  (println (work-day-3 joltage-ratings 2))
  (print   "Part two: The total output joltage is: ") 
  (println (work-day-3 joltage-ratings 12)) 
  (println "0K.\n"))

;; Run in terminal via: clojure -M p03ab.clj
(program)

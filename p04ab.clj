;;;;    Advent of Code 2025 - Day 4 part One and Two
;;;;    https://adventofcode.com/2025/day/4
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    Part one: The number of rolls of paper that can be removed by a forklift is: 1370
;;;;    Part two: The number of rolls of paper that can be removed by a forklift is: 8437
;;;;
;;;;    (cl) by Arno Jacobs, 2025-12-24
;;;;    

(ns p04ab
  (:require [clojure.string :as str]))

;;; Some helper defines
(def paper-roll \@)
(def clean-space \.)
(def part1 1)
(def part2 2)

;;;; Read the data-set
(defn get-lines
  [file]
  (->> file
       slurp
       (str/split-lines)))

(def grid (get-lines "./data/inputDay04_2025.txt"))

;;;; helper - pretty print grid 
(defn pretty-grid
  [grid]
  (doseq [line grid]
    (println line)))

;;;; Helper for counting neighbouring rolls
(defn add-roll
  [grid x y max-x max-y]
  (if (and (>= x 0) (>= y 0) (< x max-x) (< y max-y))
    (if (= paper-roll (nth (nth grid y) x))
      1
      0)
    0))

;;; take one paper roll if there are less than 4 neighbouring paper rolls
(defn take-paper-roll
  [grid x y max-x max-y]
  (if (= paper-roll (nth (nth grid y) x))
    (if (< (+ (add-roll grid (- x 1) (- y 1) max-x max-y) 
              (add-roll grid (- x 1)    y    max-x max-y) 
              (add-roll grid (- x 1) (+ y 1) max-x max-y) 
              (add-roll grid    x    (- y 1) max-x max-y) 
              (add-roll grid    x    (+ y 1) max-x max-y) 
              (add-roll grid (+ x 1) (- y 1) max-x max-y) 
              (add-roll grid (+ x 1)    y    max-x max-y) 
              (add-roll grid (+ x 1) (+ y 1) max-x max-y)) 4)
      clean-space
      paper-roll)
    clean-space))

;;; Work one row - column by column
(defn work-row
  [grid x y max-x max-y]
  (if (>= x max-x)
    nil
    (cons (take-paper-roll grid x y max-x max-y)
          (work-row grid (+ 1 x) y max-x max-y))))
  
;;;; Work row by row
(defn work-grid
  [grid row max-x max-y]
  (if (>= row max-y)
     nil
     (cons (work-row grid 0 row max-x max-y)
           (work-grid grid (+ 1 row) max-x max-y))))
  
(defn take-paper-rolls
  [grid]
  (let [max-x (count (first grid))
        max-y (count grid)] 
    (work-grid grid 0 max-x max-y)))

;;; Count the remaining number of paper rolls on the grid
(defn count-paper-rolls
  [grid]
  (count (filter #(= paper-roll %) (str/join grid))))

(defn count-taken-paper-rolls
  [part grid]
  (let [current-rolls         (count-paper-rolls grid)
        next-rolls            (take-paper-rolls grid)
        next-number-of-rolls  (count-paper-rolls next-rolls)
        rolls-taken           (- current-rolls next-number-of-rolls)] 
    (if (= part part1) 
      rolls-taken
      (if (= 0 rolls-taken) 
        0 
        (+ rolls-taken (count-taken-paper-rolls part next-rolls))))))

;;; The 'main' program - - -
(defn program []
  (println "Advent of Code 2025 - day 4  (Clojure)")
  (print   "Part one: The number of rolls of paper that can be removed by a forklift is: ")
  (println (count-taken-paper-rolls part1 grid))
  (print   "Part two: The number of rolls of paper that can be removed by a forklift is: ")
  (println (count-taken-paper-rolls part2 grid))
  (println "0K.\n"))

;; Run in terminal via: clojure -M p04ab.clj
(program)




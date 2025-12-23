;;;;    Advent of Code 2025 - Day 2 part One and Two
;;;;    https://adventofcode.com/2025/day/2
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    Part one: The sum of all invalid ID's is: 23534117921
;;;;    Part two: The sum of all invalid ID's is: 31755323497
;;;;
;;;;    (cl) by Arno Jacobs, 2025-12-23
;;;;    

(ns p02ab
  (:require [clojure.string :as str]))

(defn split-on-comma
  [line]
  (str/split line #","))

(defn split-on-dash
  [IDs]
  (str/split IDs #"-"))

(defn to-ints
  [sranges]
  (map #(parse-long (apply str %)) sranges))

;;;; Read the data-set and split into from-to-ranges
(defn get-ranges
  [file]
  (->> file
       slurp
       split-on-comma
       (map split-on-dash)
       (map to-ints)))

(def ID-ranges (get-ranges "./data/inputDay02_2025.txt"))

;;; Only check for equal halfs
;;; This is faster then using 'equal-parts'
(defn two-equal-halfs [testID]
  (let [sID (str testID)
        c (count sID)
        halfway (/ c 2)]
      (if (even? c)
        (if (= (take halfway sID) (drop halfway sID))
          testID
          0)
        0)))

;;;; Look if there is any repeated sequence of digits
(defn has-equal-parts
  [step max-step sID] 
  (if (> step max-step)
    false
    (if (apply = (partition-all step sID)) ;;; the actual splitting
      true
      (has-equal-parts (+ 1 step) max-step sID))))

;;; Check for all multiple parts
;;; split the 'long' in [1,2,.. half way it's length] parts
(defn test-equal-parts
  [testID]
  (let [sID (str testID)
        halfway (/ (count sID) 2)]
    (if (has-equal-parts 1 halfway sID)
      testID
      0)))

;;;; Here there is a split for part 1 and part 2
;;;  Using 'test-equal-parts' for part 1 does not work 0K.
(defn checkValidIds
  [part start-ID max-ID]
  (let [sum (atom 0)]
    (doseq [id (range start-ID (+ 1 max-ID))]
      (if (= part 1)
        (swap! sum + (two-equal-halfs id))
        (swap! sum + (test-equal-parts id))))
      @sum))

(defn work-day-2
  [part ID-ranges]
  (reduce + (map #(checkValidIds part (first %) (last %)) ID-ranges)))

;;; The 'main' program - - -
(defn program []
  (println "Advent of Code 2025 - day 2  (Clojure)")
  (print   "Part one: The sum of all invalid ID's is: ")
  (println (work-day-2 1 ID-ranges))
  (print   "Part two: The sum of all invalid ID's is: ")
  (println (work-day-2 2 ID-ranges))
  (println "0K.\n"))

;; Run in terminal via: clojure -M p02ab.clj
(program)

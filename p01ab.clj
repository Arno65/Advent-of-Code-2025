;;;;    Advent of Code 2025 - Day 1 part One and Two
;;;;    https://adventofcode.com/2025/day/1
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The password to open the door for part one: 1147
;;;;    The password to open the door for part two: 6789
;;;;
;;;;    (cl) by Arno Jacobs, 2025-12-21
;;;;    

(ns p01ab
  (:require [clojure.string :as str]))

(def start-dial 50)
(def dial-resolution 100)

;;;; Read the data-set
(defn get-lines
  [file]
  (->> file
       slurp
       (str/split-lines)))

;;;; Convert the rotation-stings to integer values (R to + and L to -)
(defn get-rotation
  [line]
  (let [direction (first line)
        steps (parse-long (apply str (rest line)))]
    (if (= direction \R)
      steps
      (- steps))))

(def rotations (map get-rotation (get-lines "./data/inputDay01_2025.txt")))

;;; Count the times the dial is at 0 after a R or L turn
(defn rotate-and-count-on-zero
  [rotations dial-position dial-resolution zero-counts]
  (if (empty? rotations)
    zero-counts
    (let [rotation (first rotations)
          new-dial-position (mod (+ dial-position rotation) dial-resolution)
          next-count (+ zero-counts (if (= 0 new-dial-position) 1 0))]
      (rotate-and-count-on-zero (rest rotations) new-dial-position dial-resolution next-count))))

;;;; Count the times the dial is passing 0 after a R or L turn
(defn rotate-and-count-zero-clicks-helper
  [position rotation]
  (let [mc (quot rotation dial-resolution)      ;;; possible multiple times a dial passes 0
        mr (mod rotation dial-resolution)]
    (+ mc (quot (+ position mr) dial-resolution))))

(defn rotate-and-count-zero-clicks
  [rotations dial-position dial-resolution zero-click-counts]
  (if (empty? rotations)
    zero-click-counts
    (let [rotation (first rotations)
          new-dial-position (mod (+ dial-position rotation) dial-resolution)
          position (if (< rotation 0) (- dial-resolution dial-position) dial-position)
          absolute-rotation (abs rotation)
          next-zero-clicks (rotate-and-count-zero-clicks-helper position absolute-rotation)
          position-p0 (quot absolute-rotation dial-resolution)]
      ;;; dial at 0 is NOT a pass
      (if (= 0 dial-position)
        (rotate-and-count-zero-clicks (rest rotations)
                                      new-dial-position
                                      dial-resolution
                                      (+ zero-click-counts position-p0))
        (rotate-and-count-zero-clicks (rest rotations)
                                      new-dial-position
                                      dial-resolution
                                      (+ zero-click-counts next-zero-clicks))))))

;;; The 'main' program - - -
(defn program []
  (println "Advent of Code 2025 - day 1  (Clojure)")
  (print   "The password to open the door for part one: ")
  (println (rotate-and-count-on-zero rotations start-dial dial-resolution 0))
  (print   "The password to open the door for part two: ")
  (println (rotate-and-count-zero-clicks rotations start-dial dial-resolution 0))
  (println "0K.\n"))

;; Run in terminal via: clojure -M p01ab.clj
(program)

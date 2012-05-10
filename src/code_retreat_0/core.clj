(ns code-retreat-0.core
  (:use midje.sweet))

;; [[0 1 0]
;;  [0 0 0]
;;  [0 0 0]]

(defn nextState
  [m c])

(defn gol "Game of life"
  [m]
  (let [l (count m)
        c (count (first m))
        coord (for [x (range l)
                    y (range c)] [y x])]
    (map #(update-in m %
                     (fn [o] (nextState m %))) coord)))

(future-fact
  (gol [[0 0 0]
        [0 0 0]
        [0 0 0]]) => [[0 0 0]
                      [0 0 0]
                      [0 0 0]])

;.;. [31mFAIL[0m at (NO_SOURCE_FILE:1)
;.;. You claimed the following was needed, but it was never used:
;.;.     (nextState m [0 0])
;.;. 
;.;. [31mFAIL[0m at (NO_SOURCE_FILE:1)
;.;. You claimed the following was needed, but it was never used:
;.;.     (nextState m [0 1])
;.;. 
;.;. [31mFAIL[0m at (NO_SOURCE_FILE:1)
;.;. You claimed the following was needed, but it was never used:
;.;.     (nextState m [0 2])
;.;. 
;.;. [31mFAIL[0m at (NO_SOURCE_FILE:1)
;.;. You claimed the following was needed, but it was never used:
;.;.     (nextState m [1 0])
;.;. 
;.;. [31mFAIL[0m at (NO_SOURCE_FILE:1)
;.;. You claimed the following was needed, but it was never used:
;.;.     (nextState m [1 1])
;.;. 
;.;. [31mFAIL[0m at (NO_SOURCE_FILE:1)
;.;. You claimed the following was needed, but it was never used:
;.;.     (nextState m [1 2])
;.;. 
;.;. [31mFAIL[0m at (NO_SOURCE_FILE:1)
;.;. You claimed the following was needed, but it was never used:
;.;.     (nextState m [2 0])
;.;. 
;.;. [31mFAIL[0m at (NO_SOURCE_FILE:1)
;.;. You claimed the following was needed, but it was never used:
;.;.     (nextState m [2 1])
;.;. 
;.;. [31mFAIL[0m at (NO_SOURCE_FILE:1)
;.;. You claimed the following was needed, but it was never used:
;.;.     (nextState m [2 2])
;.;. 
;.;. [31mFAIL[0m at (NO_SOURCE_FILE:1)
;.;.     Expected: [[1 1 1] [1 0 1] [1 1 1]]
;.;.       Actual: [[1 1 1] [1 1 1] [1 1 1]]
(fact
  (let [m [[1 1 1]
           [1 1 1]
           [1 1 1]]]
    (gol m) => [[1 1 1]
                [1 0 1]
                [1 1 1]]
    (provided
      (nextState m [0 0]) => 1
      (nextState m [0 1]) => 1
      (nextState m [0 2]) => 1
      (nextState m [1 0]) => 1
      (nextState m [1 1]) => 0
      (nextState m [1 2]) => 1
      (nextState m [2 0]) => 1
      (nextState m [2 1]) => 1
      (nextState m [2 2]) => 1)))  

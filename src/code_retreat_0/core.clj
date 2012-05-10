(ns code-retreat-0.core
  (:use midje.sweet))

(fact
  (+ 1 1) => 2)

;; [[0 1 0]
;;  [0 0 0]
;;  [0 0 0]]

(defn nb "Compute the state of the neigbours of the cell with coord [y x]"
  [u y x]
  (map #(get-in u %)
       (filter #(not= % [y x])
        (for [b [-1 0 1]
                     a [-1 0 1]]
                 [(+ b y) (+ a x)]))))

(fact
  (nb [[0 1 0]
       [0 0 0]
       [0 0 0]] 1 1) => [0 1 0 0 0 0 0 0])

(defn next-state
  [u y x]
  (let [state (get-in u [y x])
        nb-n (reduce + (nb u y x))]
    (if (= 1 state)
      (cond (< nb-n 2) 0
            (<= 2 nb-n 3) 1
            :else 0)
      (if (= 3 nb-n) 1 0))))

(fact "next time"
  (next-state [[0 1 0]
               [0 1 0]
               [0 0 0]] 1 1) => 0
  (next-state [[0 1 1]
               [0 1 0]
               [0 0 0]] 1 1) => 1
  (next-state [[1 1 1]
               [0 1 0]
               [1 0 0]] 1 1) => 0
  (next-state [[0 1 1]
               [0 0 0]
               [1 0 0]] 1 1) => 1
  (next-state [[0 0 1]
               [0 0 0]
               [1 0 0]] 1 1) => 0)
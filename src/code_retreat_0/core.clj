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
  (let [nb-n (reduce + (nb u y x))]
    (if (< nb-n 2) 0 1)))

;.;. Intellectual 'work' is misnamed; it is a pleasure, a dissipation, and is its own highest reward. -- Twain
(fact "next time"
  (next-state [[0 1 0]
               [0 1 0]
               [0 0 0]] 1 1) => 0
  (next-state [[0 1 1]
               [0 1 0]
               [0 0 0]] 1 1) => 1)
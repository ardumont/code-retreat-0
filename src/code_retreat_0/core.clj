(ns code-retreat-0.core
  (:use midje.sweet))

(defn neighbours-coord
  [y x]
  (for [b [-1 0 1]
        a [-1 0 1]
        :let [y+ (+ b y)
              x+ (+ a x)]
        :when (not= [y+ x+] [y x])]
    [y+ x+]))

(fact
  (neighbours-coord 1 1) => [[0 0] [0 1] [0 2] [1 0] [1 2] [2 0] [2 1] [2 2]])

(defn nb "Compute the state of the neigbours of the cell with coord [y x]"
  [u y x]
  (map #(get-in u %) (neighbours-coord y x)))

(fact
  (nb [[0 1 0]
       [0 0 0]
       [0 0 0]] 1 1) => [0 1 0 0 0 0 0 0])

(defn next-state
  [u y x]
  (let [state (get-in u [y x])
        nb-n (reduce + (filter #(not= nil %) (nb u y x)))]
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

(defn gol
  [u]
  (reduce
   (fn [r [y x :as c]]
     (assoc-in r c (next-state u y x)))
   u
   (for [y (range (count u))
         x (range (count u))] [y x])))

(fact
  (gol [[0 1 0 0]
        [0 0 0 0]
        [0 0 0 0]
        [0 0 0 0]]) => [[0 0 0 0]
                        [0 0 0 0]
                        [0 0 0 0]
                        [0 0 0 0]])
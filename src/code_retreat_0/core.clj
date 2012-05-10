(ns code-retreat-0.core)

;; no more than 4 lines of code

;; [[0 1 0]
;;  [0 0 0]
;;  [0 0 0]]
;; 0 dead
;; 1 alive

;; nb m [0 0]
;; next-state m [0 0] => 
;; get-in m [y x] => val

(defn nb "Given a universe u and a coordinate y and x that represents the coordinate of the cell in the universe" [u [y x :as cell]]
  (let [l (count u) c (count (first u))
        y- (dec y), x- (dec x), y+ (+ y 2), x+ (+ x 2)]
    (for [b (range y- y+) a (range x- x+)
          :when (and (<= 0 b) (<= 0 a) (< b l) (< a c))] [b a])))

(defn alive? [u cell]
  (= 1 (get-in u cell)))

(defn next-state [u cell]
  (let [nb-alive-neighbour (count (filter #(alive? u %) (nb u cell)))]
    (if (alive? cell)
      (cond (< nb-alive-neighbour 2)    0
            (<= 2 nb-alive-neighbour 3) 1
            :else 0)
      (if (= 3 nb-alive-neighbour) 1 0))))


(comment
  (nb m [0 0]) => [[0 1] [1 0] [1 1]])

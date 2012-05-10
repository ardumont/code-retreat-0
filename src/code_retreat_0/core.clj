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
  (neighbours-coord 0 0) => [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
  (neighbours-coord 1 1) => [[0 0] [0 1] [0 2] [1 0] [1 2] [2 0] [2 1] [2 2]])

(defn neighbours-state "Compute the state of the neigbours of the cell with coord [y x]"
  [u y x]
  (map #(get-in u %) (neighbours-coord y x)))

(fact
  (neighbours-state [[0 1 0]
                     [0 0 0]
                     [0 0 0]] 1 1) => [0 1 0 0 0 0 0 0])

(defn next-state-cell "Given a universe u and a cell with coordinate y x, compute the next state of the cell [y x] in the universe u"
  [u y x]
  (let [state (get-in u [y x])
        nb-n (count (filter #(= 1 %) (neighbours-state u y x)))]
    (if (= 1 state)
      (cond (< nb-n 2) 0
            (<= 2 nb-n 3) 1
            :else 0)
      (if (= 3 nb-n) 1 0))))

(fact "next state"
  (next-state-cell [[0 1 0]
               [0 1 0]
               [0 0 0]] 1 1) => 0
  (next-state-cell [[0 1 1]
               [0 1 0]
               [0 0 0]] 1 1) => 1
  (next-state-cell [[1 1 1]
               [0 1 0]
               [1 0 0]] 1 1) => 0
  (next-state-cell [[0 1 1]
               [0 0 0]
               [1 0 0]] 1 1) => 1
  (next-state-cell [[0 0 1]
               [0 0 0]
               [1 0 0]] 1 1) => 0)

(defn coordinates "Compute the coordinates of the matrix"
  [u]
  (let [set-rows (range (count u))]
    (for [y set-rows
          x set-rows] [y x])))

(fact
  (coordinates [[0 1]
                [0 0]]) => [[0 0] [0 1] [1 0] [1 1]])

(defn next-state "Given a universe, compute the next state of the universe"
  [u]
  (reduce
   (fn [r [y x :as c]]
     (assoc-in r c (next-state-cell u y x)))
   u
   (coordinates u)))

(fact
  (next-state [[0 1 0 0]
               [0 0 0 0]
               [0 0 0 0]
               [0 0 0 0]]) => [[0 0 0 0]
                               [0 0 0 0]
                               [0 0 0 0]
                               [0 0 0 0]])

;; ------------------------ Side effects -------------------------

(defn get-gfx "Given a width and a height, returns a frame with these dimension"
  [width height]
  (.getGraphics
   (doto (java.awt.Frame.)
     (.setSize width height)
     (.setVisible true))))

(defn clear
  [g width height]
  (.clearRect g 0 0 width height))

(defn random-universe "Random universe"
  [size]
  (vec (map vec (partition-all size
                               (for [x (range size)
                                     y (range size)] (rand-int 2))))))

(defn draw "Draw the game of life"
  [gfx w h u]
  (let [color {0 (java.awt.Color. 255 255 255)
               1 (java.awt.Color. 0 0 0)}
        offset 29]
    (clear gfx w h)
    (doseq [x (range (count u))
            y (range (count u))]
      (.setColor gfx (color (get-in u [y x])))
      (.fillRect gfx
                 (* 10 x)
                 (+ offset (* 10 y))
                 10 10))))

(defn game-of-life "Game of life"
  [n]
  (let [w 300
        h 300
        gfx (get-gfx w h)]
    (iterate (fn [u] (let [nxt-universe (next-state u)]
                      (do (draw gfx w h nxt-universe)
                          (Thread/sleep 200)
                          nxt-universe))) (random-universe n))))
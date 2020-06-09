(ns suduku.core)

(def numbers #{1 2 3 4 5 6 7 8 9})

(defn suduku-blank
  "Make an empty board"
  []
  (for [x (range 9)
        y (range 9)]
    {:x (+ 1 x) :y (+ 1 y) :val nil}))

(defn collect-x
  [x board]
  (set (reduce (fn [res n] (if (= x (:x n)) (cons (:val n) res) res))
               '()
               board)))

(defn collect-y
  [y board]
  (set (reduce (fn [res n] (if (= y (:y n)) (cons (:val n) res) res))
               '()
               board)))

(defn collect-xy
  [x y board]
  (set (reduce (fn [res n] (if (and (= y (:y n)) (= x (:x n))) (cons (:val n) res) res))
               '()
               board)))

(defn to-square
  "given a row or column, provide the capturing 3 rows or columns"
  [x]
  (let [ bx (quot x 3) ]
    (set (cond
           (= bx 0) (range 3)
           (= bx 1) (range 3 6)
           (= bx 2) (range 6 9)))))

(defn in-square?
  "Test to see if an item is within a given magic square range"
  [xrange yrange item]
  (and (contains? xrange (:x item)) (contains? yrange (:y item))))

(defn collect-square
  "given a position, return the 9 values in the magic square"
  [x y board]
  (let [sx (to-square x)
        sy (to-square y)]
    (set (reduce (fn [result e] (if (in-square? sx sy e) (conj result (:val e)) result))
                 '()
                 board))))
  
(defn valid-xy
  "Find a valid number for the position given current state"
  [x y board]
  (let [a (collect-square x y board)
        b (collect-y y board)
        c (collect-x x board)]
    (clojure.set/difference numbers (clojure.set/union a b c))))

(defn solve
  "Based on an initial board state, solve for a 9x9 suduku board"
  [board]
  (let [next-move (first (filter #(nil? (:val %)) board))
        x (:x next-move)
        y (:y next-move)]
    (if (nil? next-move)
      board
      (loop [options (shuffle (valid-xy x y board))]
        (let [choice (first options)]
          (if (nil? choice)
            nil
            (let [next-board (solve (replace {next-move {:x x :y y :val choice}} board))]
              (if (nil? next-board)
                (recur (rest options))
                next-board))))))))

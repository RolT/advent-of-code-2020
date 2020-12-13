(ns solution)


(defn parse-puzzle
  [f]
  (mapv vec (clojure.string/split-lines (slurp f))))

(defn next-cell-state
  [arr i j]
  (let [cur-state (get-in arr [i j])
        adjacents (for [k (range (dec i) (+ 2 i))
                        l (range (dec j) (+ 2 j))
                        :when (not= [i j] [k l])]
                    (get-in arr [k l] \.))]
    (case cur-state
      \. \.

      \L (if (zero? (count (filter #(= \# %) adjacents)))
           \#
           \L)

      \# (if (<= 4 (count (filter #(= \# %) adjacents)))
           \L
           \#))))

(defn next-state
  [arr]
  (vec (for [i (range (count arr))]
         (vec (for [j (range (count (first arr)))]
                (next-cell-state arr i j))))))

(defn part1
  []
  (let [puzzle (parse-puzzle "../11/puzzle.txt")]
    (loop [arr puzzle]
      (let [new-arr (next-state arr)]
        (if (= arr new-arr)
          (count (filter #(= \# %) (mapcat identity arr)))
          (recur new-arr))))))

(defn first-adjacent
  [arr i j k l]
  (let [new-i (+ k i)
        new-j (+ l j)
        seat (get-in arr [new-i new-j])]
    (case seat
      nil nil
      \.  (recur arr new-i new-j k l)
      seat)))

(defn next-cell-state2
  [arr i j]
  (let [cur-state (get-in arr [i j])
        adjacents (for [[k l] [[-1 -1]
                               [-1 0]
                               [-1 1]
                               [0 -1]
                               [0 1]
                               [1 -1]
                               [1 0]
                               [1 1]]]
                    (first-adjacent arr i j k l))]
    (case cur-state
      \. \.

      \L (if (zero? (count (filter #(= \# %) adjacents)))
           \#
           \L)

      \# (if (<= 5 (count (filter #(= \# %) adjacents)))
           \L
           \#))))

(defn next-state2
  [arr]
  (vec (for [i (range (count arr))]
         (vec (for [j (range (count (first arr)))]
                (next-cell-state2 arr i j))))))

(defn part2
  []
  (let [puzzle (parse-puzzle "../11/puzzle.txt")]
    (loop [arr puzzle]
      (let [new-arr (next-state2 arr)]
        (if (= arr new-arr)
          (count (filter #(= \# %) (mapcat identity arr)))
          (recur new-arr))))))

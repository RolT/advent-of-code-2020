(ns user)

(ns part1
  (:require [clojure.core.matrix :refer :all]))
(defn parse-puzzle [f]
  (let [puzzle (mapv vec (clojure.string/split-lines (slurp f)))
        rows (count puzzle)
        cols (count (first puzzle))
        array (new-array :ndarray [(+ 4 rows) (+ 4 cols) 5])]
    (doseq [i (range rows)
            j (range cols)]
      (mset! array (+ 2 i) (+ 2 j) 2 (= \# (get-in puzzle [i j]))))
    array))

(defn active-neighboors [arr i j k]
  (let [range-x (range (dec i) (+ 2 i))
        range-y (range (dec j) (+ 2 j))
        range-z (range (dec k) (+ 2 k))]
    (reduce (fn [c [x y z]] (if (and (not (and (= i x)
                                               (= j y)
                                               (= z k)))
                                     (mget arr x y z))
                              (inc c) c))
            0
            (for [x range-x
                  y range-y
                  z range-z]
              [x y z]))))

(defn count-active
  [arr]
  (let [dim0 (range (dimension-count arr 0))
        dim1 (range (dimension-count arr 1))
        dim2 (range (dimension-count arr 2))]
    (reduce
      (fn [c [x y z]] (if (mget arr x y z) (inc c) c))
      0
      (for [x dim0
            y dim1
            z dim2]
        [x y z]))))

(defn next-state [prev-arr]
  (let [dim0 (dimension-count prev-arr 0)
        dim1 (dimension-count prev-arr 1)
        dim2 (dimension-count prev-arr 2)
        new-arr (new-array :ndarray [(+ 2 dim0)
                                     (+ 2 dim1)
                                     (+ 2 dim2)])
        range0 (range 1 (dec dim0))
        range1 (range 1 (dec dim1))
        range2 (range 1 (dec dim2))]
    (doseq [i range0
            j range1
            k range2]
      (let [c (active-neighboors prev-arr i j k)]
        (if (mget prev-arr i j k)
          (mset! new-arr (inc i) (inc j) (inc k) (or (= 2 c) (= 3 c)))
          (mset! new-arr (inc i) (inc j) (inc k) (= 3 c)))))
    new-arr))

(comment
  (count-active (reduce (fn [acc _] (next-state acc)) (parse-puzzle "puzzle.txt") (range 6))))


(ns part2
  (:require [clojure.core.matrix :refer :all]))

(defn parse-puzzle [f]
  (let [puzzle (mapv vec (clojure.string/split-lines (slurp f)))
        rows (count puzzle)
        cols (count (first puzzle))
        array (new-array :ndarray [(+ 4 rows) (+ 4 cols) 5 5])]
    (doseq [i (range rows)
            j (range cols)]
      (mset! array (+ 2 i) (+ 2 j) 2 2 (= \# (get-in puzzle [i j]))))
    array))

(defn active-neighboors [arr i j k l]
  (let [range-x (range (dec i) (+ 2 i))
        range-y (range (dec j) (+ 2 j))
        range-z (range (dec k) (+ 2 k))
        range-a (range (dec l) (+ 2 l))]
    (reduce (fn [c [x y z a]]
              (if (and (not (and (= i x)
                                 (= j y)
                                 (= z k)
                                 (= a l)))
                       (mget arr x y z a))
                (inc c)
                c))
            0
            (for [x range-x
                  y range-y
                  z range-z
                  a range-a]
              [x y z a]))))

(defn count-active
  [arr]
  (let [dim0 (range (dimension-count arr 0))
        dim1 (range (dimension-count arr 1))
        dim2 (range (dimension-count arr 2))
        dim3 (range (dimension-count arr 3))]
    (reduce
      (fn [c [x y z a]] (if (mget arr x y z a) (inc c) c))
      0
      (for [x dim0
            y dim1
            z dim2
            a dim3]
        [x y z a]))))

(defn next-state [prev-arr]
  (let [dim0 (dimension-count prev-arr 0)
        dim1 (dimension-count prev-arr 1)
        dim2 (dimension-count prev-arr 2)
        dim3 (dimension-count prev-arr 3)
        new-arr (new-array :ndarray [(+ 2 dim0)
                                     (+ 2 dim1)
                                     (+ 2 dim2)
                                     (+ 2 dim3)])
        range0 (range 1 (dec dim0))
        range1 (range 1 (dec dim1))
        range2 (range 1 (dec dim2))
        range3 (range 1 (dec dim3))]
    (doseq [i range0
            j range1
            k range2
            l range3]
      (let [c (active-neighboors prev-arr i j k l)]
        (if (mget prev-arr i j k l)
          (mset! new-arr (inc i) (inc j) (inc k) (inc l) (or (= 2 c) (= 3 c)))
          (mset! new-arr (inc i) (inc j) (inc k) (inc l) (= 3 c)))))
    new-arr))

(comment
  ;; Slooooooooooooooooow
  (set! *warn-on-reflection* true)
  (defn parse-puzzle [f]
    (let [puzzle (mapv vec (clojure.string/split-lines (slurp f)))
          rows (count puzzle)
          cols (count (first puzzle))
          array (make-array Boolean/TYPE (+ 4 rows) (+ 4 cols) 5 5)]
      (doseq [i (range rows)
              j (range cols)]
        (aset-boolean array (+ 2 i) (+ 2 j) 2 2 (= \# (get-in puzzle [i j]))))
      array))

  (defn active-neighboors [ ^"[[[[Z" arr i j k l]
    (let [range-x (range (dec i) (+ 2 i))
          range-y (range (dec j) (+ 2 j))
          range-z (range (dec k) (+ 2 k))
          range-a (range (dec l) (+ 2 l))]
      (reduce (fn [c [x y z a]] (if (and (not (and (= i x)
                                                   (= j y)
                                                   (= z k)
                                                   (= a l)))
                                         (aget arr x y z l))
                                  (inc c) c))
              0
              (for [x range-x
                    y range-y
                    z range-z
                    a range-a]
                [x y z a]))))

  (defn count-active
    [^"[[[[Z" arr]
    (let [dim0 (range (alength ^"[[[[Z" arr))
          dim1 (range (alength ^"[[[Z" (aget arr 0)))
          dim2 (range (alength ^"[[Z" (aget arr 0 0)))
          dim3 (range (alength ^"[Z" (aget arr 0 0 0)))]
      (reduce
        (fn [c [x y z a]] (if (aget arr x y z a) (inc c) c))
        0
        (for [x dim0
              y dim1
              z dim2
              a dim3]
          [x y z a]))))

  (defn next-state [^"[[[[Z" prev-arr]
    (let [dim0 (alength ^"[[[[Z" prev-arr)
          dim1 (alength ^"[[[Z" (aget prev-arr 0))
          dim2 (alength ^"[[Z" (aget prev-arr 0 0))
          dim3 (alength ^booleans (aget prev-arr 0 0 0))
          new-arr (make-array Boolean/TYPE
                              (+ 2 dim0)
                              (+ 2 dim1)
                              (+ 2 dim2)
                              (+ 2 dim3))
          range0 (range 1 (dec dim0))
          range1 (range 1 (dec dim1))
          range2 (range 1 (dec dim2))
          range3 (range 1 (dec dim3))]
      (doseq [i range0
              j range1
              k range2
              l range3]
        (let [c (active-neighboors prev-arr i j k l)]
          (if (aget prev-arr i j k l)
            (aset new-arr (inc i) (inc j) (inc k) (inc l) (or (= 2 c) (= 3 c)))
            (aset new-arr (inc i) (inc j) (inc k) (inc l) (= 3 c)))))
      new-arr)))
  

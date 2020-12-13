(->> (slurp "../../adventofcode/2020/03/puzzle.txt")
     (clojure.string/split-lines)
     (map-indexed (fn [i s]
                    (if (= \# (get s (mod (* 3 i) (count s))))
                      1
                      0)))
     (reduce +))


;; ugly solution...
(let [pattern (->> (slurp "../../adventofcode/2020/03/puzzle.txt")
                   (clojure.string/split-lines))]
  (->> (for [[right down] [[1 1] [3 1] [5 1] [7 1] [1/2 2]]]
         (->> pattern
              (map-indexed (fn [i s]
                             (if (and (zero? (mod i down)) (= \# (get s (mod (* right i) (count s)))))
                               1
                               0)))
              (reduce +)))
       (reduce *)))

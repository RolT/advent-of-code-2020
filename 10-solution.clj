(ns solution)

(defn parse-puzzle [f]
  (->> (slurp f)
       (clojure.string/split-lines)
       (map #(Integer/parseInt %))))

(defn part1
  []
  (let [jolts (sort (parse-puzzle "puzzle.txt"))
        adapters (concat [0] jolts [(+ 3 (last jolts))])
        diffs (map - (next adapters) adapters)]
    (* (count (filter #(= 1 %) diffs)) (count (filter #(= 3 %) diffs)))))

(def combinaisons
  (memoize
    (fn [adapters n]
      (cond
        (zero? n) 1
        (adapters n) (+ (combinaisons adapters (- n 1))
                        (combinaisons adapters (- n 2))
                        (combinaisons adapters (- n 3)))
        :else 0))))

(defn part2
  []
  (let [jolts (sort (parse-puzzle "puzzle.txt"))
        adapters (concat [0] jolts [(+ 3 (last jolts))])]
    (run! (partial combinaisons (set adapters)) adapters)
    (combinaisons (set adapters) (last adapters))))

;; second take with a potentially parallelisable algorithm
(defn find-combinations
  [adapters]
  (let [[start next & tail] adapters]
    (cond
      (nil? next) 1
      (< 3 (- next start)) 0
      (empty? tail) 1
      :else
      (if (>= 3 (- next start))
        (+ (find-combinations (conj tail start))
           (find-combinations (conj tail next)))
        0))))

(defn part2
  []
  (let [jolts (sort (parse-puzzle "puzzle.txt"))
        adapters (concat [0] jolts [(+ 3 (last jolts))])]
    (loop [adapters (drop 1 adapters)
           current-val 0
           current-seq '(0)
           combinations 1]
      (let [[next & tail] adapters]
        (cond
          (empty? tail) (* combinations (find-combinations (reverse current-seq)))
          (< (- next current-val) 3) (recur tail next (conj current-seq next) combinations)
          (= (- next current-val) 3) (recur tail next (list next) (* combinations (find-combinations (reverse current-seq)))))))))

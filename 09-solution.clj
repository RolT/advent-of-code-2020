(let [puzzle (->> (slurp "../../adventofcode/2020/09/puzzle.txt")
                  (clojure.string/split-lines)
                  (map #(Long/parseLong %)))
      preamble-length 25
      [preamble message] (split-at preamble-length puzzle)
      preamble (vec preamble)
      all-sums (fn [preamble]
                 (->>
                   (for [i (range preamble-length)]
                     (for [j (range (inc i) preamble-length)]
                       (+ (preamble i) (preamble j))))
                   (mapcat identity)
                   (set)))]
  (:result (reduce (fn [acc v]
                     (let [sums (all-sums (:preamble acc))]
                       (if (sums v)
                         {:preamble (conj (subvec (:preamble acc) 1) v)}
                         (reduced {:result v})))) {:preamble preamble} message)))

(let [puzzle (->> (slurp "../../adventofcode/2020/09/puzzle.txt")
                  (clojure.string/split-lines)
                  (map #(Long/parseLong %))
                  (vec))
      preamble-length 25
      [preamble message] (split-at preamble-length puzzle)
      preamble (vec preamble)
      all-sums (fn [preamble]
                 (->>
                   (for [i (range preamble-length)]
                     (for [j (range (inc i) preamble-length)]
                       (+ (preamble i) (preamble j))))
                   (mapcat identity)
                   (set)))
      invalid-number (:result
                      (reduce (fn [acc v]
                                (let [sums (all-sums (:preamble acc))]
                                  (if (sums v)
                                    {:preamble (conj (subvec (:preamble acc) 1) v)}
                                    (reduced {:result v})))) {:preamble preamble} message))]
  (reduce (fn [_ n]
            (when-let [v (reduce (fn [_ i]
                                   (let [subset (subvec puzzle i (+ i n))]
                                     (when (= invalid-number (reduce + subset))
                                       (println subset i n invalid-number)
                                       (reduced (+ (apply min subset) (apply max subset))))))
                                 nil
                                 (range (- (count puzzle) n)))]
              (reduced v)))
          nil
          (range 2 (count puzzle))))

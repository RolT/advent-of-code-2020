;; part 1
(let [report (->> (slurp "expense_report.txt")
                  (clojure.string/split-lines)
                  (map #(Integer/parseInt ^String %))
                  (sort)
                  (vec))]
  (loop [i 0
         j (dec (count report))]
    (let [result (+ (report i) (report j))]
      (cond
        (< result 2020) (recur (inc i) j)
        (= result 2020) (* (report i) (report j))
        (> result 2020) (recur i (dec j))))))

;; part 2
(let [report (->> (slurp "../../adventofcode/2020/01/expense_report.txt")
                  (clojure.string/split-lines)
                  (map #(Integer/parseInt ^String %))
                  (sort)
                  (vec))]
  (->> (for [k (range (count report))]
         (let [objective (- 2020 (report k))]
           (loop [i 0
                  j (dec (count report))]
             (when (< i j)
               (let [result (+ (report i) (report j))]
                 (cond
                   (< result objective) (recur (inc i) j)
                   (= result objective) (* (report i) (report j) (report k))
                   (> result objective) (recur i (dec j))))))))
       (some identity)))

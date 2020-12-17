(ns solution)

(defn parse-ticket
  [t]
  (mapv #(Integer/parseInt %) (clojure.string/split t #",")))

(defn parse-rule
  [r]
  (let [[_ field r1 r2 r3 r4] (re-matches #"(.+): (\d+)-(\d+) or (\d+)-(\d+)" r)
        [r1 r2 r3 r4] (map #(Integer/parseInt %) [r1 r2 r3 r4])]
    [field [[r1 r2] [r3 r4]]]))

(defn parse-puzzle [f]
  (let [[rules _ my-ticket _ nearby-tickets] (->> (slurp f)
                                                  clojure.string/split-lines
                                                  (partition-by clojure.string/blank?))]
    {:rules (into {} (map parse-rule rules))
     :my-ticket (parse-ticket (second my-ticket))
     :nearby-tickets (map parse-ticket (drop 1 nearby-tickets))}))

(defn in-range?
  [[from to] n]
  (<= from n to))

(defn part1
  []
  (let [puzzle (parse-puzzle "puzzle.txt")
        ranges (mapcat identity (vals (:rules puzzle)))
        values (mapcat identity (:nearby-tickets puzzle))]
    (reduce + (remove (fn [n] (some #(in-range? % n) ranges)) values))))

(defn in-ranges?
  [ranges n]
  (some #(in-range? % n) ranges))

(defn valid-fields
  [fields col]
  (keep (fn [[field ranges]]
          (when (every? #(in-ranges? ranges %) col)
            field))
        fields))

(defn part2
  []
  (let [puzzle (parse-puzzle "puzzle.txt")
        ranges (mapcat identity (vals (:rules puzzle)))
        valid-tickets (filter (fn [ticket] (every? (fn [n] (some #(in-range? % n) ranges)) ticket)) (:nearby-tickets puzzle))
        cols (for [i (range (count (first valid-tickets)))]
               (mapv #(get % i) valid-tickets))
        ;; this solution is incomplete, but works for that particular puzzle
        fields (loop [valid-fields (map (comp set (partial valid-fields (:rules puzzle))) cols)]
                 (if-let [uniq-fields (seq (keep #(if (= 1 (count %)) (first %)) valid-fields))]
                   (let [new-valid-fields (map (fn [v]
                                                 (if (= 1 (count v))
                                                   v
                                                   (reduce disj v uniq-fields)))
                                               valid-fields)]
                     (if (= new-valid-fields valid-fields)
                       valid-fields
                       (recur new-valid-fields)))
                   valid-fields))]
    (->>
      (zipmap (map first fields)
              (:my-ticket puzzle))
      (filter (fn [[k v]] (re-matches #"departure.*" k)))
      (vals)
      (reduce *))))

(ns user)

(defn parse-puzzle [f]
  (->> f
       slurp
       clojure.string/split-lines
       (map (fn [x] (if-let [[_ mask] (re-matches #"mask = ([01X]+)" x)]
                      {:mask mask}
                      (let [[_ addr value] (re-matches #"mem\[(\d+)\] = (\d+)" x)]
                        {:addr (clojure.edn/read-string addr)
                         :value (clojure.edn/read-string value)}))))
       (partition-by :mask)
       (partition-all 2)
       (map (fn [[[mask] [& instructions]]]
              {:mask (:mask mask)
               :instructions instructions}))))

(defn execute
  [mem {:keys [mask instructions]}]
  (let [mask-fn (fn [value]
                  (->> mask
                       (reverse)
                       (map-indexed (fn [i x]
                                      (case x
                                        \0 #(bit-clear % i)
                                        \1 #(bit-set % i)
                                        nil)))
                       (remove nil?)
                       (reduce (fn [acc f] (f acc)) value)))]
    (reduce (fn [mem {:keys [addr value]}]
              (assoc mem addr (mask-fn value)))
            mem instructions)))

(defn part1 []
  (let [puzzle (parse-puzzle "puzzle.txt")]
    (reduce + (vals (reduce execute {} puzzle)))))

(defn memory-addresses
  [mask addr]
  (let [mask (reverse mask)
        base-addr (->> mask
                       (map-indexed (fn [i x] (when (= x \1) i)))
                       (filter identity)
                       (reduce bit-set addr))
        floating (->> mask
                      (map-indexed (fn [i x] (when (= x \X) i)))
                      (filter identity))]
    ((fn mutate [addr bits]
       (if (seq bits)
         (lazy-cat (mutate (bit-set addr (first bits)) (rest bits))
                   (mutate (bit-clear addr (first bits)) (rest bits)))
         [addr]))
     base-addr floating)))

(defn execute2
  [mem {:keys [mask instructions]}]
  (reduce (fn [mem {:keys [addr value]}]
            (let [addrs (memory-addresses mask addr)]
              (reduce (fn [mem addr] (assoc mem addr value)) mem addrs)))
          mem instructions))

(defn part2 []
  (let [puzzle (parse-puzzle "puzzle.txt")]
    (reduce + (vals (reduce execute2 {} puzzle)))))

(let [prog (->> (slurp "../../adventofcode/2020/08/puzzle.txt")
                (clojure.string/split-lines)
                (mapv (fn [s]
                        (let [[command value] (clojure.string/split s #" ")]
                          [(keyword command) (Integer/parseInt value)]))))]
  (loop [seen #{}
         n 0
         acc 0]
    (if (seen n)
      acc
      (let [[command value] (prog n)]
        (case command
          :nop (recur (conj seen n) (inc n) acc)
          :acc (recur (conj seen n) (inc n) (+ acc value))
          :jmp (recur (conj seen n) (+ n value) acc))))))

(let [prog (->> (slurp "../../adventofcode/2020/08/puzzle.txt")
                (clojure.string/split-lines)
                (mapv (fn [s]
                        (let [[command value] (clojure.string/split s #" ")]
                          [(keyword command) (Integer/parseInt value)]))))
      next-state (fn [prog {:keys [seen n acc]}]
                   (let [[command value] (prog n)]
                     (case command
                       :nop {:seen (conj seen n) :n (inc n) :acc acc}
                       :acc {:seen (conj seen n) :n (inc n) :acc (+ acc value)}
                       :jmp {:seen (conj seen n) :n (+ n value) :acc acc})))
      exec (fn [prog {:keys [seen n acc] :as state}]
             (cond
               (< (count prog) n) [:return acc]
               (seen n) [:error acc]
               :else (recur prog (next-state prog state))))]
  (exec prog {:seen #{} :acc 0 :n 0}))


(let [prog (->> (slurp "../../adventofcode/2020/08/puzzle.txt")
                (clojure.string/split-lines)
                (mapv (fn [s]
                        (let [[command value] (clojure.string/split s #" ")]
                          [(keyword command) (Integer/parseInt value)]))))
      next-state (fn [prog {:keys [seen n acc]}]
                   (let [[command value] (prog n)]
                     (case command
                       :nop {:seen (conj seen n) :n (inc n) :acc acc}
                       :acc {:seen (conj seen n) :n (inc n) :acc (+ acc value)}
                       :jmp {:seen (conj seen n) :n (+ n value) :acc acc})))
      exec (fn [prog {:keys [seen n acc] :as state}]
             (cond
               (= (count prog) n) [:return acc]
               (seen n) [:error acc]
               :else (recur prog (next-state prog state))))
      mutate (fn [prog n]
               (update prog n (fn [[command value]] [({:jmp :nop
                                                       :nop :jmp} command)
                                                     value])))]
  (println
    "part 1:" (second (exec prog {:seen #{} :acc 0 :n 0})))
  (println
    "part 2:"
    (loop [state {:seen #{} :n 0 :acc 0}]
      (let [[command _] (prog (:n state))]
        (cond
          (#{:nop :jmp} command)
          (let [[return-code return-value] (exec (mutate prog (:n state)) state)]
            (case return-code
              :return return-value
              :error  (recur (next-state prog state))))

          :else
          (recur (next-state prog state)))))))

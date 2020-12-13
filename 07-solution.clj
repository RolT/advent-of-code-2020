(defn rules []
  (let [rules (->> (slurp "../../adventofcode/2020/07/puzzle.txt")
                   (clojure.string/split-lines)
                   (map (fn [rule]
                          (let [[_ parent-bag child-bags-str] (re-matches #"(\w+ \w+) bags contain (.*)" rule)
                                child-bags (when-not (= "no other bags." child-bags-str)
                                             (->> (clojure.string/split child-bags-str #", ")
                                                  (map #(let [[_ n child-bag] (re-matches #"(\d+) (\w+ \w+) bags?\.?" %)]
                                                          [child-bag n]))
                                                  (into {})))]
                            [parent-bag child-bags])))
                   (into {}))
        reverse-rules (->> (mapcat (fn [[parent childs]]
                                     (for [[child _] childs]
                                       [child parent])) rules)
                           (reduce (fn [acc [k v]] (update acc k #(conj % v))) {}))
        parents (atom #{})]
    ((fn add-parents [bag]
       (doseq [parent (reverse-rules bag)]
         (when-not (contains? @parents parent)
           (swap! parents conj parent)
           (add-parents parent)))) "shiny gold")
    @parents))

;; without stack overflow issue
(defn rules []
  (let [rules (->> (slurp "../../adventofcode/2020/07/puzzle.txt")
                   (clojure.string/split-lines)
                   (map (fn [rule]
                          (let [[_ parent-bag child-bags-str] (re-matches #"(\w+ \w+) bags contain (.*)" rule)
                                child-bags (when-not (= "no other bags." child-bags-str)
                                             (->> (clojure.string/split child-bags-str #", ")
                                                  (map #(let [[_ n child-bag] (re-matches #"(\d+) (\w+ \w+) bags?\.?" %)]
                                                          [child-bag n]))
                                                  (into {})))]
                            [parent-bag child-bags])))
                   (into {}))
        reverse-rules (->> (mapcat (fn [[parent childs]]
                                     (for [[child _] childs]
                                       [child parent])) rules)
                           (reduce (fn [acc [k v]] (update acc k #(conj % v))) {}))]
    (loop [parents #{}
           previously-added #{"shiny gold"}]
      (let [previously-added-parents (set (reduce (fn [acc bag] (clojure.set/union acc (reverse-rules bag))) #{} previously-added))
            new-bags (clojure.set/difference previously-added-parents parents)]
        (if (seq new-bags)
          (recur (clojure.set/union parents new-bags) new-bags)
          parents)))))

(defn go []
  (let [rules2 (->> (slurp "../../adventofcode/2020/07/puzzle.txt")
                    (clojure.string/split-lines)
                    (map (fn [rule]
                           (let [[_ parent-bag child-bags-str] (re-matches #"(\w+ \w+) bags contain (.*)" rule)
                                 child-bags (when-not (= "no other bags." child-bags-str)
                                              (->> (clojure.string/split child-bags-str #", ")
                                                   (map #(let [[_ n child-bag] (re-matches #"(\d+) (\w+ \w+) bags?\.?" %)]
                                                           [child-bag (Integer/parseInt n)]))
                                                   (into {})))]
                             [parent-bag child-bags])))
                    (into {}))]
    ((memoize (fn count-bag [bag]
                (let [childs (rules2 bag)]
                  (if childs
                    (reduce (fn [acc [child n]] (+ acc (* n (count-bag child)))) 1 childs)
                    1)))) "shiny gold")))

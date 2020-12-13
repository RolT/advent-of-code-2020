(ns solution)

(def mapping
  {\N :north
   \S :south
   \E :east
   \W :west
   \R :right
   \L :left
   \F :forward})

(defn parse-input
  [f]
  (map (fn [val] {:action (mapping (first val))
                  :value  (Integer/parseInt (subs val 1))})
       (clojure.string/split-lines (slurp f))))

(def direction-values
  {:east  0
   :north 90
   :west  180
   :south 270})

(def ->direction
  (into {} (map (fn [[k v]] [v k]) direction-values)))

(def direction-op
  {:right -
   :left +})

(defn turn
  [init-dir left-or-right degrees]
  (if-let [res (->direction (mod ((direction-op left-or-right) (direction-values init-dir) degrees) 360))]
    res
    (throw (ex-info "Unable to find new direction" {:from init-dir
                                                    :dir left-or-right
                                                    :degrees degrees}))))

(defn move
  [pos direction value]
  (case direction
    :north (update pos :y + value)
    :south (update pos :y - value)
    :east  (update pos :x + value)
    :west  (update pos :x - value)))

(defn next-state
  [direction pos {:keys [action value]}]
  (cond
    (#{:left :right} action) [(turn direction action value) pos]
    (#{:forward} action) [direction (move pos direction value)]
    (#{:north :east :west :south} action) [direction (move pos action value)]
    :else (throw (ex-info "Unknown action" {:action action}))))

(defn abs [x] (if (neg? x) (- x) x))

(defn manhattan-dist
  [pos]
  (->> pos
       (vals)
       (map abs)
       (reduce +)))

(defn part1
  []
  (let [puzzle (parse-input "../12/puzzle.txt")]
    (-> (reduce (fn [[direction position] instruction]
                  (next-state direction position instruction))
                [:east
                 {:x 0 :y 0}]
                puzzle)
        (second)
        (manhattan-dist))))

#_(defn rotate-90°-right
    [waypoint]
    (assoc waypoint
           :x (:y waypoint)
           :y (- (:x waypoint))))

(defn rotate-90°-left
  [waypoint]
  (assoc waypoint
         :x (- (:y waypoint))
         :y (:x waypoint)))

(defn rotate
  [position direction degrees]
  (let [rotate-count (quot (mod (({:right - :left +} direction) degrees) 360) 90)]
    (reduce (fn [p _] (rotate-90°-left p)) position (range rotate-count))))

(defn move-to-waypoint
  [ship waypoint value]
  (-> ship
      (update :x + (* value (:x waypoint)))
      (update :y + (* value (:y waypoint)))))

(defn next-state2
  [ship waypoint {:keys [action value]}]
  (cond
    (#{:left :right} action)
    [ship (rotate waypoint action value)]

    (#{:north :east :west :south} action)
    [ship (move waypoint action value)]

    (#{:forward} action)
    [(move-to-waypoint ship waypoint value) waypoint]))

(defn part2
  []
  (let [puzzle (parse-input "../12/puzzle.txt")]
    (-> (reduce (fn [[ship waypoint] instruction]
                  (println ship waypoint)
                  (println instruction)
                  (next-state2 ship waypoint instruction))
                [{:x 0 :y 0}
                 {:x 10 :y 1}]
                puzzle)
        (first)
        (manhattan-dist))))

(ns assignment-1.task_3)

(def children ["Alice" "Bob" "Charlie" "David" "Eve" "Fred" "Ginny" "Harriet" "Ileana" "Joseph" "Kincaid" "Larry"])

(def garden [[\V \R \C \G \V \V \R \V \C \G \G \C \C \G \V \R \G \C \V \C \G \C \G \V]
             [\V \R \C \C \C \G \C \R \R \G \V \C \G \C \R \V \V \C \V \G \C \G \C \V]])

(defn lookup-garden [child]
  (lookup-plant (mapcat #(take 2 (drop (* (.indexOf children child) 2) %)) garden)))

(defn lookup-plant [plants]
  (let [plant-map {\G "Grass" \C "Clover" \R "Radish" \V "Violet"}]
    (mapv #(get plant-map %) plants)))

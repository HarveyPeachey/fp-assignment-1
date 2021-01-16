(ns assignment-1.task_3)

(defn lookup-plant-name [plants]
  "Used to lookup the full name of a plant given it's shorthand character"
  (let [plant-map {\G "Grass" \C "Clover" \R "Radish" \V "Violet"}]
    (mapv #(get plant-map %) plants)))

(defn find-childs-plants [child garden]
  "Retrieves corresponding plants from the garden owned by a given child"
  (let [children ["Alice" "Bob" "Charlie" "David" "Eve" "Fred" "Ginny" "Harriet" "Ileana" "Joseph" "Kincaid" "Larry"]
        no-of-plants 2]
    (if (some #(= child %) children)
      (lookup-plant-name (mapcat #(take no-of-plants (drop (* (.indexOf children child) no-of-plants) %)) garden))
      "Feed me, Seymour! That child doesn't like plants")))

(defn format-garden [garden]
  "Used to transform garden into a usable format from a single string"
  (let [row-size 24]
    (vec (mapv vec (partition row-size row-size [] garden)))))

(defn find-plants
  "Entry point function to find plants given a name and an optional garden of plants"
  ([child garden]
   (cond
     (string? garden) (recur child (format-garden garden))
     (and (vector? garden) (vector? (first garden))) (find-childs-plants child garden)
     :else "Uh oh, that's a not a garden..."))
  ([child]
   (find-plants child [[\V \R \C \G \V \V \R \V \C \G \G \C \C \G \V \R \G \C \V \C \G \C \G \V]
                       [\V \R \C \C \C \G \C \R \R \G \V \C \G \C \R \V \V \C \V \G \C \G \C \V]])))

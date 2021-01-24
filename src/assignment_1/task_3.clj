(ns assignment-1.task_3 "Task 3 - Kindergardeners")

;; As with the previous tasks I decided to split up the code into reasonble functions that made sense to me.
;; From this decision I split the program into 4 different functions.

;; Function find-plants ------------------------------------------------------------------------------------------------------------------------
;; The first main entry point function find-plants is used to start the process, and handles the logic to decide
;; what to do when called with different data structures.
;; I've also used arity for this function to provide the default garden layout on the task sheet if only a childs name is provided,
;; it then recursivesly calls the find-plants function again with the garden layout which will satisfy the first arity parameters.
;; Then there is a cond branch macro, where it will test if the garden has been entered as a string and if so it will call the
;; format-garden function, to convert it into a usable format, and then uses recursion to call find-plants again with the formatted garden.
;; The next check in cond checks if the format of garden is valid which I've defined in the letfn special form for readability,
;; it basically checks if the outside container is a vector and if the items are all vectors. I made use of complement which negates
;; the collection type test and used some to return the first logical true so it doesn't have to traverse the whole collection,
;; which is why I didn't use a transducer like map.

;; Function format-garden ----------------------------------------------------------------------------------------------------------------------
;; This function handles the transformation of a passed in string into a garden format of a vector of vectors.
;; partition handles the organisation by chunking out the string passed into it as row sizes of 24 whilst also allowing overflow,
;; for example if a string of 25 characters is passed to it.
;; Then mapvec and vec are used to convert each row into a vector and then storing them as a vector.

;; Function find-childs-plant ------------------------------------------------------------------------------------------------------------------
;; This handles the main logic to find plants that a child owns.
;; Firstly I've binded data structures of a vector which contains all the childrens names in alphabetical
;; and the number of plants each own on a row.
;; I then use some to check if there name exists and if not it will return a string.
;; In the main logic for finding the plants that a child owns, I made use of a combination of take and drop https://www.braveclojure.com/core-functions-in-depth/#take__drop__take_while__and_drop_while
;; The reason I chose this method is because I know the relationship where each childs plants will be as they're are ordered alpahbetically.
;; So what I did was grab the index of the childs name in the children vector and mutliplied it by the number of plants they own on each row
;; to get the offset position. Using this with drop returns a sequence which starts with that childs plants.
;; It then passes this to take which pulls off the elements based on the number of plants.
;; This anonymous function is then applied to each row in the garden using mapcat, this then concatenates the plants into one sequence
;; which in then passed to a lookup function to get the names of the corresponding shorthands for the plants.
;; This route also allows incomplete garden rows to be processed correctly.

;; Function lookup-plant-name ------------------------------------------------------------------------------------------------------------------
;; This function is used to lookup a plant's name when given a shorthand character/string using an anonymous with get on the plant-names map.
;; If it doesn't exist then it returns nil.

(defn lookup-plant-name [plants]
  "Used to lookup the full name of a plant given it's shorthand character"
  (let [plant-names {\G "Grass" \C "Clover" \R "Radish" \V "Violet"}]
    (vec (remove nil? (map #(get plant-names %) plants)))))

(defn find-childs-plants [child garden]
  "Retrieves corresponding plants from the garden owned by a given child"
  (let [children ["Alice" "Bob" "Charlie" "David" "Eve" "Fred" "Ginny" "Harriet" "Ileana" "Joseph" "Kincaid" "Larry"]
        no-plants-row 2]
    (if (some #(= child %) children)
      (lookup-plant-name (mapcat #(take no-plants-row (drop (* (.indexOf children child) no-plants-row) %)) garden))
      "Feed me, Seymour! That child doesn't like plants")))

(defn format-garden [garden]
  "Used to transform garden into a usable format from a single string"
  (let [row-size 24]
    (mapv vec (partition row-size row-size [] garden))))

(defn find-plants
  "Entry point function to find plants given a name and an optional garden of plants"
  ([child garden]
   (letfn [(garden? [g] (and (vector? g) (not (some (complement vector?) g))))]
    (cond
      (string? garden) (recur child (format-garden garden))
      (garden? garden) (find-childs-plants child garden)
      :else "Uh oh, that's a not a garden...")))
  ([child]
   (find-plants child [[\V \R \C \G \V \V \R \V \C \G \G \C \C \G \V \R \G \C \V \C \G \C \G \V]
                       [\V \R \C \C \C \G \C \R \R \G \V \C \G \C \R \V \V \C \V \G \C \G \C \V]])))

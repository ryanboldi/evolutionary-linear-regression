(ns main)

(def vertices
  (list {:x 1, :y 1}
        {:x 3, :y 2}
        {:x 4, :y 4}
        {:x 5, :y 6}))

(def parameters 2) ; y = ax + b

(defn random-solution
  "returns a random solution"
  ([] (random-solution 10 10))
  ([a-lim b-lim] (zipmap [:a :b] [(rand a-lim) (rand b-lim)])))

(defn get-y
  "gets the respective y value for a point for a given solution"
  [solution x]
  (+ (* x (:a solution)) (:b solution)))

(defn compare-vert
  "compares a solution to a given vertex -> squared"
  [solution vert]
  (reduce * (repeat 2 (- (:y vert) (get-y solution (:x vert))))))


(defn assess-solution
  "returns the fitness of a solution"
  [solution]
  ())

(map #(get-y) vertices)


(defn -main
  [& args]
  (println "Hello world!"))

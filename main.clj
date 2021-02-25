(ns main)
(require 'incanter.stats)
(alias 'inc 'incanter.stats)

(def vertices
  (list {:x 1, :y 1}
        {:x 3, :y 2}
        {:x 4, :y 4}
        {:x 5, :y 6}))

(def parameters 2) ; y = ax + b
(def mutation-rate 0.2)
(def mutation-size 0.2) ; sd of the normal sampling

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
  (reduce + (map compare-vert (repeat solution (count vertices)) vertices)))

(defn mutate-solution
  [solution]
  (zipmap [:a :b] [(inc/sample-normal 1 :mean (:a solution) :sd mutation-size) (inc/sample-normal 1 :mean (:b solution) :sd mutation-size)]))

(defn cross-solutions
  [s1 s2]
  (zipmap [:a :b] [(:a s1) (:b s2)]))



(defn -main
  [& args]
  (println "Hello world!"))

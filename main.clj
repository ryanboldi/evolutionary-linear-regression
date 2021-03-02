(ns main
  (:require [incanter.core :as i.core :refer [view to-dataset]])
  (:require [incanter.charts :as i.charts :refer [histogram xy-plot add-points add-function]])
  (:require [incanter.stats :as i.stats :refer [sample-normal]])
  (:require [incanter.datasets :as i.data]))

(def vertices
  (list {:x 1, :y 1}
        {:x 2, :y 2}
        {:x 3, :y 3}
        {:x 4, :y 4}
        {:x 5, :y 5}
        {:x 6, :y 6}))

(i.core/to-dataset vertices)
(def plain-image (add-points (xy-plot) :x :y :data (to-dataset vertices)))

(def mutation-rate 0.2)
(def mutation-size 0.2) ; sd of the normal sampling

(defn random-solution
  "returns a random solution"
  ([] (random-solution 10 10))
  ([a-lim b-lim] (zipmap [:a :b] [(- (rand a-lim) (quot a-lim 2)) (- (rand b-lim) (quot b-lim 2))])))

(defn visualize-solution
  [solution]
  (-> (xy-plot)
      (add-points :x :y :data (to-dataset vertices))
      (add-function (fn [x] (+ (:b solution) (* (:a solution) x))) 0 6)))

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
  (zipmap [:a :b] [(sample-normal 1 :mean (:a solution) :sd mutation-size) (sample-normal 1 :mean (:b solution) :sd mutation-size)]))

(defn cross-solutions
  [s1 s2]
  (zipmap [:a :b] [(:a s1) (:b s2)]))

(view (histogram (sample-normal 1000)))

(defn -main
  [& args]
  (println "Hello world!"))

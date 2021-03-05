(ns main
  (:require [incanter.core :as i.core :refer [view to-dataset $]])
  (:require [incanter.charts :as i.charts :refer [histogram xy-plot add-points add-function]])
  (:require [incanter.stats :as i.stats :refer [sample-normal]])
  (:require [incanter.datasets :as i.data])
  (:require [incanter.io :as i.io :refer [read-dataset]]))


(def vertices (read-dataset "./data/brain_body.csv"
                            :header true
                            :delim \space))

(def xy-vertices
  (zipmap [:x :y] [(first ($ :body-kg vertices)) (first ($ :brain-g vertices))]))

xy-vertices

($ :body-kg vertices)

(to-dataset vertices)
(view vertices)

(def plain-image (add-points (xy-plot) :body-kg :brain-g (to-dataset vertices)))

(def population-size 100)
(def survival-rate 0.5)
(def num-parents (* survival-rate population-size))
(def mutation-rate 0.2)
(def crossover-rate 0.1)
(def mutation-size 0.2) ; sd of the normal sampling

;--- SOLUTION FUNCTIONS

(defn random-solution
  "returns a random solution"
  ([] (random-solution 10 10))
  ([a-lim b-lim] (zipmap [:a :b] [(- (rand a-lim) (quot a-lim 2)) (- (rand b-lim) (quot b-lim 2))])))

(defn visualize-solution
  "returns a viewable incanter plot"
  [solution]
  (-> (xy-plot)
      (add-points :x :y :data (to-dataset vertices))
      (add-function (fn [x] (+ (:b solution) (* (:a solution) x)))
                    (dec (:x (apply min-key :x vertices)))
                    (inc (:x (apply max-key :x vertices))))))

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
  (reduce + (map compare-vert (repeat (count vertices) solution) vertices)))

(defn mutate-solution
  "returns a mutated solution via a normal distribution"
  [solution]
  (zipmap [:a :b] [(sample-normal 1 :mean (:a solution) :sd mutation-size) (sample-normal 1 :mean (:b solution) :sd mutation-size)]))

(defn cross-solutions
  "crosses two solutions together"
  [s1 s2]
  (if (> (rand) 1)
    (zipmap [:a :b] [(:a s1) (:b s2)])
    (zipmap [:a :b] [(:a s2) (:b s1)])))

;--- POPULATION FUNCTIONS

(defn init-evolution
  "returns the starting generation of evolution"
  []
  (repeatedly population-size #(random-solution)))

(defn assess-population
  "returns a list of the population's individual fitnesses"
  [population]
  (map assess-solution population))

(defn get-worst-solution
  "returns the solution with the highest fitness (worst)"
  [population]
  (:solution
   (apply max-key :score
          (map
           #(zipmap [:solution :score] [% (assess-solution %)]) population))))

(defn normalize
  "normalize numbers to a new range"
  [old-min old-max new-min new-max x]
  (* (- new-max new-min) (/ (- x old-min) (- old-max old-min))))

(defn average-fitness
  "returns the average-fitness of a population"
  [population]
  (/ (reduce + (assess-population population)) (count population)))

(defn sqr [x]
  (* x x))

(defn inverse-scores
  "returns an inversed assessed population where a lower score is worse for roulette wheel"
  [scores]
  (let [worst-score (apply max scores)]
    (map #(- 100 (normalize 0 worst-score 0 100 %)) scores)))

(defn cull-score-if-needed
  [solution average-fitness]
  (if (< average-fitness (assess-solution solution))
    average-fitness
    (assess-solution solution)))

(defn get-culled-scores
  "returns a list with all numbers above the average set to the average value."
  [population]
  (let [average-score (average-fitness population)]
    (for [sol population]
      (cull-score-if-needed sol average-score))))

(defn roulette-wheel-select
  "randomly selects a solution weighted towards higher fitness solutions"
  [population]
  (let [pop population scores (inverse-scores (get-culled-scores pop)) total-score (reduce + scores) rand-num (rand)]
    (loop [index 0 sum-so-far (/ (nth scores 0) total-score)]
      (if (< rand-num sum-so-far)
        (nth pop index)
        (recur (inc index) (+ sum-so-far (/ (nth scores index) total-score)))))))

(defn maybe-mutate
  "every solution will pass through this function.
  mutate the solutions with a given probability"
  [solution]
  (if (< (rand) mutation-rate)
    (mutate-solution solution)
    solution))

(defn cross-and-create
  "given the entire parent population, 
   select two parents to crossover, returning the child"
  [parents]
  (cross-solutions (rand-nth parents) (rand-nth parents)))

(defn duplicate-and-create
  "randomly select a parent to just be placed into the next pop"
  [parents]
  (rand-nth parents))

(defn child-creation-instruction-functions
  "returns a lazy-seq of functions to create the missing children with"
  []
  (repeatedly  (- population-size num-parents)
               #(if (< (rand) crossover-rate)
                  cross-and-create
                  duplicate-and-create)))

(defn create-new-pop
  "given an old population, create a new one based on evolutionary rules and probabilities"
  [old-pop]
  (let [parents (repeat num-parents (roulette-wheel-select old-pop))]
    (into parents (map #(% parents) (child-creation-instruction-functions)))))

(defn get-best-solution
  "gets the solution with the smallest score in the population"
  [population]
  (:solution
   (apply min-key :score
          (map
           #(zipmap [:solution :score] [% (assess-solution %)]) population))))

(defn print-pop-stats
  "prints out the population's stats"
  [population]
  (println (str "Best Fitness: " (assess-solution (get-best-solution population))))
  (println (str "Average Fitness: " (average-fitness population))))


(view plain-image)
(view (visualize-solution (get-best-solution (init-evolution))))

(defn -main
  [& args]
  (println "Hello world!"))
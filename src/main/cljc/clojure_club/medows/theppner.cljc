(ns clojure-club.medows.theppner)

(def sample
  ["################################"
   "#######################     ####"
   "######################       ###"
   "#####################        ###"
   "##################     ###  ####"
   "#################     ##########"
   "#################     ##########"
   "#################     ##########"
   "#####  ##########    ###########"
   "####    ##########  ############"
   "###      #######################"
   "###      #######################"
   "####    ########################"
   "################################"
   "################################"
   "################################"
   "################################"
   "################################"
   "########   #####################"
   "#######     ####################"
   "#######     ####################"
   "#######    #####################"
   "########  ######################"
   "################################"
   "################################"
   "##########   ###################"
   "#########     ##################"
   "#########     ##################"
   "##########    ##################"
   "###########    #################"
   "###########    #################"
   "############  ##################"])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;utils;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn find-coords
  "Looks through a grid and returns all coordinates that have the specified char."
  ([m] (find-coords m \space))
  ([m c]
   (->> (for [x (range 0 (count (first m)))
              y (range 0 (count m))]
          (if (#{c} (get-in m [y x]))
            {:x x :y y :content c}))
        (filter some?))))

;interesting point!!!
;(assoc '(1 2 3 4) 0 "a")
;=> error
;
;(assoc "1234" 0 \a)
;=> error
;
;(assoc [1 2 3 4] 0 "a")
;=> ["a" 2 3 4]
(defn vectorize-grid
  "Insures our grid is a vector of vectors."
  [g]
  (mapv (partial apply vector) g))

(defn stringify-grid
  "Insures our grid is a vector of strings."
  [g]
  (mapv (partial apply str) g))


(defn on-grid?
  "Checks to see if coordinate is on the grid."
  [{:keys [x y] :as coord} max-x max-y]
  (when (and (<= 0 x max-x) (<= 0 y max-y))
    coord))

(defn coords-around
  "For neighbors like:
   [[n n n]
    [n x n]
    [n n n]"
  [{:keys [x y] :as coord} max-x max-y]
  (filter
    some?
    (for [x (range (dec x) (inc x))
          y (range (dec y) (inc y))]
      (on-grid? {:x x :y y} max-x max-y))))

(defn coords-around
  "For neighbors like:
   [[  n  ]
    [n x n]
    [  n  ]"
  [{:keys [x y] :as coord} max-x max-y]
  (->>[(update coord :x inc)
       (update coord :x dec)
       (update coord :y inc)
       (update coord :y dec)]
      (filter #(on-grid? % max-x max-y))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;a gross attempt at things;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;this is gross
(defn fill [m]
  (let [max-x (count (first m))
        max-y (count m)]
    (loop [[{:keys [x y]} & r :as coords] (find-coords m)
           flood-char 1
           m (assoc-in m [y x] flood-char)]
      (let [_ m
            new-m
            (reduce (fn [m {:keys [x y] :as coord}]
                      (if (some (fn [{:keys [x y] :as surrounding-coords}]
                                  (#{flood-char} (get-in m [y x])))
                                (coords-around coord max-x max-y))
                        (assoc-in m [y x] flood-char)
                        m)) m coords)]
        (let [[{:keys [x y]} & r :as coords] (find-coords new-m)]
          (cond
            (and (= m new-m) (seq coords))
            (recur coords (inc flood-char) (assoc-in m [y x] (inc flood-char)))

            (= m new-m)
            new-m

            :default
            (recur coords flood-char new-m)))))))

;(stringify-grid (fill (vectorize-grid sample)))
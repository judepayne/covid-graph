(ns covid-graph.patch)

;; If Johns Hopkins has inaccurate data for the last day, allow for it to be patched.


(defn update-in-list [l i x]
  (if (nil? x) l
      (loop [new-data [] old-list l]
        (if (seq old-list)
          (if (= (count new-data) i)
            (recur (conj new-data x) (rest old-list))
            (recur (conj new-data (first old-list)) (rest old-list)))
          (apply list new-data)))))


(defn index-of [s v]
  (loop [idx 0 items s]
    (cond
      (empty? items) nil
      (= v (first items)) idx
      :else (recur (inc idx) (rest items)))))


(def data-patches
  {"Italy" [["3/12/20" {:cases 15113}]]
   "United Kingdom" [["3/12/20" {:cases 590}] ["3/15/20" {:cases 1391 :deaths 35}]]
   "Ireland" [["3/12/20" {:cases 70}]["3/15/20" {:cases 169}]]
   "Norway" [["3/12/20" {:cases 800}]]
   "Sweden" [["3/12/20" {:cases 687}]]
   "Finland" [["3/12/20" {:cases 109}]]
   "France" [["3/12/20" {:cases 2876}]["3/15/20" {:cases 5423 :deaths 127}]]
   "Germany" [["3/12/20" {:cases 2745}]]
   "Spain" [["3/12/20" {:cases 3146}]]
   "Iran" [["3/12/20" {:cases 10075}]]
   "Brazil" [["3/15/20" {:cases 234}]]})


(defn patch-time-series
  [dates me]
  (let [[k v] me]
    (if-let [ptch (get data-patches k)]
      [k (reduce
          (fn [acc [date data]]
            (let [i (index-of dates date)
                  p1 (if (:cases data)
                       (update-in acc [:cases] update-in-list i (:cases data))
                       acc)]
              (if (:deaths data)
                (update-in p1 [:deaths] update-in-list i (:deaths data))
                p1)))
          v
          ptch)]
      me)))


(def names-map
  {" South\""          "South Korea"
   "Korea, South"      "South Korea"
   "Taiwan*"           "Taiwan"})


(defn fix-name [n]
  (if-let [nfix (get names-map n)]
    nfix
    n))

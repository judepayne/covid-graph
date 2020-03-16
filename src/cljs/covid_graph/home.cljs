(ns covid-graph.home
  (:require
   [reagent.core             :refer [atom cursor]]
   [reagent.ratom            :refer [run!]]
   [json-utils.uri           :as uri]
   [json-utils.util          :as util]
   [covid-graph.oz           :as oz]
   [covid-graph.static       :as static]
   [covid-graph.patch        :as patch]
   [clojure.string           :as str])
  (:refer-clojure :exclude [max min]))


(defn dbg
  ([x]
   (println "dbg>> " x)
   x)
  ([n x]
   (when (= 1 (rand-int n))
     (println "dbg sample>> " x))
   x))


(defn dbg-type
  [x]
  (println (type x))
  x)


(def state (atom {:controls {:chart3 "Eastern Asia" :chart12 "United Kingdom"}}))

;; an atom to hold the processed
(def processed (atom nil))

(def hdr (cursor state [:hdr]))
(def data (cursor state [:data]))


;; processing raw data into state atom
(defn parse-int [s]
  (try
    (js/parseInt s)
    (catch js/Error e s)))


(defn flot? [s]
  (let [f (js/parseFloat s)]
    (if (js/Number.isNaN f)
      false true)))


(defn deep-merge
  "Merges maps recursively down."
  [& maps]
  (apply
   (fn m [& maps]
     (cond
       (every? map? maps)        (apply merge-with m maps)
       (every? sequential? maps) (reduce concat [] maps)
      :else  maps))
   maps))


(defn patch [x] (patch/patch-time-series @hdr x))


(defn process-csv [series data]
  (let [data (map (fn [l] (str/split l #",")) (str/split-lines data))
        hdr ((comp rest rest rest rest) (first data))]
    {:hdr hdr
     :data (->> (rest data)
                (map (fn [row]
                       (if (flot? (nth row 2))
                         (cons (second row) (rest (rest (rest (rest row)))))
                         (cons (nth row 2) (rest (rest (rest (rest (rest row)))))))))
                (group-by first)
                (map (fn [[k v]]
                       [k (map (fn [rows] (map parse-int (rest rows))) v)]))
                (map (fn [[k v]]
                       [k (apply map + v)]))
                ;; do patching
                (reduce (fn [acc [k v]] (assoc acc (patch/fix-name k) {series v})) {})
                ;(reduce (fn [acc [k v]] (assoc acc k (patch/patch-time-series hdr [k v]))) {})
                )}))


(defn post-process-data []
  (let [hdr @hdr
        f (fn [dat] (into {} (map #(patch/patch-time-series hdr %) dat)))]
    (reset! processed (f @data))))


(run! (let [ex-data (first (:data @state))]
        (when (= '(:cases :deaths) (keys (second ex-data)))
          (post-process-data))))


(uri/slurp
 "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
 (fn [x] (let [cases (->> (:body x)
                          (process-csv :cases))]
           (swap! state deep-merge cases))))


(uri/slurp
 "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
 (fn [x] (let [deaths (->> (:body x)
                           (process-csv :deaths))]
           (swap! state deep-merge {:data (:data deaths)}))))



;; helper functions for analytics
(defn zip-seqs
  "Creates a sequence of maps, each created by merging keys and the interleaved seqs."
  [base & kseqs]
  (let [kseqs (partition 2 kseqs)
        ks (map first kseqs)
        seqs (partition (count kseqs) (apply interleave (map second kseqs)))]
    (map (fn [n] (apply assoc base (interleave ks n))) seqs)))


(defn rate
  "Converts a cumulative sequence into a rate of change."
  [coll]
  (let [n (cons 0 (butlast coll))]
    (map (fn [a b] (cljs.core/max 0 (- a b))) coll n)))


(defn avg [coll]
  (/ (reduce + coll)
      (count coll)))

(defn mov-avg [coll period]
  (if (= 1 period)
    coll
    (let [buffer-coll (concat (take period (repeat (first coll))) coll)]
      (map avg (partition period 1 buffer-coll)))))


;; start of analytics
;; in each case, coll is a coll of maps of form {:country "Sweden" :cases [1 34 98 ...] :deaths [...] etc}
;; cases, deaths etc are collectively called series
(defn peak
  "Returns the highest number in the sequence s."
  [s]
  (apply cljs.core/max s))


(defn floor
  "Returns the lowest number in the sequence s."
  [s]
  (apply cljs.core/min s))


(defn max
  "Returns the data series (country) which has the maximum value of all the data series
   when seq-fn is applied to each series."
  [seq-fn series coll]
  (reduce
   (fn [acc [k v]]
     (if (> (seq-fn (series v)) (seq-fn (series (second (first acc)))))
       {k v}
       acc))
   {nil {series '(0)}}
   coll))


(defn min
  "Returns the data series (country) which has the minimum value of all the data series
   when seq-fn is applied to each series."
  [seq-fn series coll]
  (reduce
   (fn [acc [k v]]
     (if (< (seq-fn (series v)) (seq-fn (series (second (first acc)))))
       {k v}
       acc))
   {nil {series '(10000000)}}
   coll))


(defn exclude-country [country coll]
  (filter #(not= country (first %)) coll))


(defn exclude-countries [countries coll]
  (let [countries (into #{} countries)]
    (remove (fn [[k v]] (contains? countries k)) coll)))


(defn aggregate [title coll]
  [title
   (reduce
    (fn [acc [k v]]
      (-> acc
          (update-in [:cases] (fn [x] (map + x (:cases v))))
          (update-in [:deaths] (fn [x] (map + x (:deaths v))))))
    {:cases (repeat 0)
     :deaths (repeat 0)}
    coll)])


(defn rate-of-change
  "turns series of accumulating numbers into a rate of change.
   accomodates the situation for when you want the result to be assoc'd
   in to the val under a new key."
  ([series coll]
   (rate-of-change series nil coll))
  ([src-series tgt-series coll]
   (if (nil? tgt-series)
     (map (fn [[k v]] [k (update-in v [src-series] rate)]) coll)
     (map (fn [[k v]] [k (assoc-in v [tgt-series] (rate (src-series v)))]) coll))))


(defn moving-average [series period coll]
  (map (fn [[k v]] [k (update-in v [series] mov-avg period)]) coll))


(defn mortality-rate [coll]
  (map (fn [[k v]]
         [k
          (assoc-in v
                    [:mortality]
                    (map (fn [b a] (if (or (zero? a) (zero? b)) 0 (* 100 (/ a b))))
                         (:cases v) (:deaths v)))])
       coll))


;; TODO delete
(defn ->vega-old [header coll]
  (map (fn [[k v]]
         (zip-seqs {:country k}
                   :date header
                   :cases (:cases v)
                   :deaths (:deaths v)))
       coll))


(defn ->vega-format [header coll]
  (map (fn [[k v]]
         (apply zip-seqs
                {:country k}
                :date header
                (reduce concat
                        (for [x (keys v)]
                          [x (x v)]))))
       coll))


(defn filter-peak-below [series floor coll]
  (filter (fn [[k v]]
            (if (> (peak (series v)) floor)
              true
              false))
          coll))


;; Vega templates
(def default
  {:width 800})


(defn bar-template [yaxis-title yfield]
  {:mark "bar"
   :width 800
   :encoding {:x {:field "date"
                  :type "temporal"
                  :axis {:title "date"}}
              :y {:field yfield
                  :type "quantitative"
                  :axis {:title yaxis-title}}
              :color {:field "country"
                      :type "nominal"}}})


(defn line-template [yaxis-title yfield]
  {:layer [{:mark {:type "line"
                   :interpolate "monotone"}
            :encoding {:x {:field "date"
                           :type "temporal"
                           :axis {:title "date"}}
                       :y {:field yfield
                           :type "quantitative"
                           :axis {:title yaxis-title}}
                       :color {:field "country"
                               :type "nominal"}}}]})


(defn dash-template [yaxis-title yfield]
  {:layer [{:mark {:strokeDash [6 4]
                   :type "line"
                   :interpolate "monotone"}
            :encoding {:x {:field "date"
                           :type "temporal"
                           :axis {:title "date"}}
                       :y {:field yfield
                           :type "quantitative"
                           :axis {:title yaxis-title}}
                       :color {:field "country"
                               :type "nominal"}}}]})


(def mark
  {:layer [{:data static/measures
            :mark "rule"
            :encoding {:x {:field "date"
                           :type "temporal"}
                       :size {:value 2}
                       :color {:field "country" :type "nominal"}}}]})


(defn format [hdr coll]
  (->> coll
       (->vega-format hdr)
       (apply concat)))


;; Generate vega data ready for plotting charts
;; Just china
(defn highest-country [series]
  (let [data (->> @data
                  (max last series)
                  (rate-of-change series :rate-cases)
                  (mortality-rate)
                  (format @hdr))]
    data))


;; A country
;; 33ms
(defn countries [series & cs]
  (let [cs (into #{} cs)
        data (->> @processed
                  (filter (fn [[k v]] (contains? cs k)))
                  (rate-of-change series :rate-cases)
                  (mortality-rate)
                  (format @hdr))]
    data))


(defn sub-region [series exclusions & subs]
  (let [subs (into #{} subs)
        data (->> @processed
                  (filter (fn [[k v]] (contains? subs (->> k (get static/regions) :sub-region))))
                  (exclude-countries exclusions)
                  (rate-of-change series :rate-cases)
                  (format @hdr))]
    data))


;; the country with the highest peak vs rest of world
;; 4799 ms
(defn vs-rest [series]
  (let [data (if (nil? @processed) '()
                 (let [highest (max last series @processed)
                       others (aggregate "Rest of the world" (exclude-country "China" @processed))
                       dat (assoc highest (first others) (second others))]
                   (->> dat
                        (rate-of-change series :rate-cases)
                        (format @hdr))))]
    data))


;; countries whose peak is above a floor number
;; 771ms
(defn countries-peak-above [series floor]
  (let [data (->> @processed
                  (filter-peak-below series floor)
                  (rate-of-change series :rate-cases)
                  (format @hdr))]
    data))


;; Other countries whose peak is above a floor number
;; 2330ms
(defn exclude-country-highest-above [series floor]
  (let [data (->> @processed
                  (exclude-country "China")
                  (filter-peak-below series floor)
                  (rate-of-change series :rate-cases)
                  (format @hdr))]
    data))


(defn ->dat [data]
  {:data {:values data}})


;; a generic dropdown box whose state is in the state atom at path
(defn fixed-select [path options]
  [:select
   {:field :list :id (last path)
    :values options
    :value (get-in @state path)
    :on-change #(swap! state update-in path
                       (fn [e] (-> % .-target .-value)))}
   (for [x options] [:option {:key x} x])])


(defn dyn-sub-region-chart [title path template]
  [:div
   [:div.control-wrapper title [fixed-select path static/sub-regions]]
   [:div [oz/vega-lite (merge
                        (->dat (sub-region :cases ["China"] (get-in @state path)))
                        template)]]])


(defn dyn-country-chart [title path template]
  [:div
   [:div.control-wrapper title [fixed-select path static/the-countries]]
   [:div [oz/vega-lite (merge
                           (->dat (countries :cases (get-in @state path)))
                           template)]]])


;; pgae layout
(defn home-page []
  [:div.page
   [:div.main
    [:div.title [:h3 "Series of COVID-19 trend charts."]
     [:div.info "This data is updated every day at around midnight UTC. source: Johns Hopkins github site."]]

    
    [:div.chart "Daily new confirmed cases, China."
     [:div [oz/vega-lite (merge
                          (->dat (highest-country :cases))
                          default
                          {:resolve {:scale {:y "independent"}}}
                          (deep-merge
                           (line-template "new cases per day" "rate-cases")))]]
     [:div.comment "China has managed to contain the virus by introducing strict social distancing measures."]]


    [:div.chart "Total confirmed cases, China vs the rest of the world."
     [:div [oz/vega-lite (merge
                          default
                          (->dat (vs-rest :cases))
                          (line-template "total cases" "cases"))]]]

    [:div.chart "Daily new confirmed cases, China vs the rest of the world."
     [:div [oz/vega-lite (merge
                          default
                          (->dat (vs-rest :cases))
                          (line-template "new cases per day" "rate-cases"))]]]
      

    [:div.chart (str "Daily new confimred cases, " (get-in @state [:controls :chart3]))
     [dyn-sub-region-chart
      "change sub region"
      [:controls :chart3]
      (merge
       default
       (line-template "new cases per day" "rate-cases"))]]


    [:div.chart "Daily new confirmed cases of countries with total cases > 1000."
     [:div [oz/vega-lite (merge
                          default
                          (->dat (countries-peak-above :cases 1000))
                          (bar-template "new cases per day" "rate-cases"))]]
     [:div.comment "From Eastern Asia, there has been a much broader spread."]]


    [:div.chart "Daily new confirmed cases, European countries with major outbreaks."
     [:div [oz/vega-lite (merge
                          (->dat (countries :cases "Italy" "France" "Germany" "Spain"))
                          default
                          (deep-merge
                           (line-template "new cases per day" "rate-cases")
                           mark))]]
     [:div.comment (str "Italy implemented strict measures on March 9th. "
                        "The average incubation period of the virus is 5-7 days.")]]

             
    [:div.chart "Daily new confirmed cases, Scandinavian countries"
     [:div [oz/vega-lite (merge
                          default
                          (->dat (countries :cases "Sweden" "Denmark" "Norway" "Finland" "Iceland"))
                          (line-template "new cases per day" "rate-cases"))]]]


    [:div.chart "Daily new confirmed cases, United Kingdom and Ireland"
     [:div [oz/vega-lite (merge
                          default
                          (->dat (countries :cases "United Kingdom" "Ireland"))
                          (line-template "new cases per day" "rate-cases"))]]]


    [:div.chart "Daily new confirmed cases, Northern America"
     [:div [oz/vega-lite (merge
                          default
                          (->dat (sub-region :cases nil "Northern America"))
                          (line-template "new cases per day" "rate-cases"))]]]


    [:div.chart "Total confirmed cases vs mortality rate (dashed), China & South Korea"
     [:div [oz/vega-lite (merge
                          (->dat (countries :cases "China" "South Korea"))
                          default
                          {:resolve {:scale {:y "independent"}}}
                          (deep-merge
                           (dash-template "mortaltity rate %" "mortality")
                           (line-template "total cases" "cases")))]]
     [:div.comment (str "Mortality rates vary with the quality of a country's healthcare provision "
                        "and the degree to which the size of the provision is overwhelmed or not. "
                        " Although China's overall "
                        "mortality rate is near 4%, in Hubei province it was 4.5%, in the rest "
                        "of China (which saw 13 thousand cases to Hubei's 67 thousand, the mortality "
                        "rate was 0.9% (calculation as of 15th March).")]]


    [:div.chart "Total confirmed cases vs mortality rate (dashed), Italy, France, Germany"
     [:div [oz/vega-lite (merge
                          (->dat (countries :cases "Italy" "France" "Germany"))
                          default
                          {:resolve {:scale {:y "independent"}}}
                          (deep-merge
                           (dash-template "mortaltity rate %" "mortality")
                           (line-template "total cases" "cases")))]]
     [:div.comment (str "The mortality rate will reach it's final number only some time after "
                        "the crisis is a country is over, but trends are visible.")]]


    [:div.chart (str "Total confirmed cases vs mortality rate (dashed), "
                     (get-in @state [:controls :chart12]))
     [dyn-country-chart
      "change country"
      [:controls :chart12]
      (merge
       default
       {:resolve {:scale {:y "independent"}}}
       (deep-merge
        (dash-template "mortaltity rate %" "mortality")
        (line-template "total cases" "cases")))]
     [:div.comment (str "Different testing regimes play a significant role in the mortality rate."
                        "For example, if a country hasn't the capacity to test people other than "
                        " those who are hospitalized, the true number of cases will be much higher "
                        " and this will make the mortality rate seem higher than it really is.")]]]])

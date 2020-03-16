(ns covid-graph.core
  (:require
   [goog.dom :as gdom]
   [reagent.dom  :as dom]
   [reagent.core :as reagent]
   [covid-graph.home :refer [home-page]]
   [secretary.core :as secretary :include-macros true]
   [accountant.core :as accountant]))


;; -------------------------
;; Current page

(def selected-page (reagent/atom home-page))

(defn page []
  [@selected-page])


(defn current-page []
  [:div.core
   [:header
    [:p ]]
   [page]
   [:footer
    [:p
     "This page is by "
     [:a {:href "https://github.com/judepayne" :tabIndex "0"}
      "Jude Payne"] "."]]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (reset! selected-page home-page))

(defn get-app-element []
  (gdom/getElement "app"))


(defn mount [el]
  (dom/render [current-page] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(defn init! []
    (accountant/configure-navigation!
     {:nav-handler
      (fn [path]
        (secretary/dispatch! path))
      :path-exists?
      (fn [path]
        (secretary/locate-route path))})
    (accountant/dispatch-current!)
    (mount-app-element))


(init!)


;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(ns send.render
  (:require [clojure.math.combinatorics :refer [combinations]]
            [reagent.core :as r]

            [send.core :as core]
            [send.net :refer [load-edn]]))

(def RED "#f09090")
(def GREEN "#90f090")
(def TAN "#ffffd0")

(defn mode-bg-style [token]
  {:background-color (get {:complete GREEN
                           true      GREEN
                           :shrunk   RED
                           false     RED}
                          token
                          TAN)})

(defn report-summary-info [slug-log]
  (let [shrink (or (:shrunk slug-log) (:shrinking slug-log))]
    (when (:smallest shrink)
      [", Shrink: "
       [:a {:href (str (:failing-size slug-log) ".html.txt")
            :title (str (get-in slug-log [:fail 0]))}
        (count (get-in slug-log [:fail 0]))]
       " \u2192 " ;; &rarr;
       [:a {:href (str (:smallest-iter slug-log) ".html.txt")
            :title (str (get-in shrink [:smallest 0]))}
        (count (get-in shrink [:smallest 0]))]
       " bytes"])))

(defn report-summary-text [slug-log]
  (let [slug (:test-slug slug-log)
        iteration (-> slug-log :iter-log keys sort last)]
    (apply
      conj
      [:div {:class "summary"}
       [:input.toggle
        {:type "checkbox"
         :defaultChecked (get-in @core/state [:tabs slug :thumbs])
         :on-change #(swap! core/state update-in [:tabs slug :thumbs] not)}]
       "Show thumbnails | "
       "Iterations " (inc iteration)
       ", Mode: " (name (or (:type slug-log) :init))
       (when (:fail slug-log)
         (str ", First Failure: " (:failing-size slug-log)))]
      (report-summary-info slug-log))))

(defn report-summary-row [slug-log]
  (let [slug (:test-slug slug-log)
        iteration (-> slug-log :iter-log keys sort last)]
    [:tr
     [:td
      [:button
       {:onClick
        (fn [evt]
          (swap! core/state assoc-in [:tabs slug :visible] true)
          (when (not (get-in @core/state [:test-state :log slug :iter-log 0]))
            (load-edn
              (str "/gen/" slug "/log.edn")
              #(swap! core/state assoc-in [:test-state :log slug] %))))}
       slug]]
     [:td (inc iteration)]
     [:td {:style (mode-bg-style (:type slug-log))}
      (name (or (:type slug-log) :init))]
     (apply
       conj
       [:td
        (when (:fail slug-log)
          (str "First Failure: " (:failing-size slug-log)))]
       (report-summary-info slug-log))]))


(defn report-table-header [browsers]
  [:tr [:th "Iteration"] [:th "Result"] [:th "Html"]
   [:th "\u00a0"] ;; &nbsp;
   (for [browser browsers]
     ^{:key browser}
     [:th browser])
   [:th "\u00a0"] ;; &nbsp;
   [:th "Average"]
   [:th "\u00a0"] ;; &nbsp;
   (for [[ba bb] (combinations browsers 2)]
     ^{:key (str ba bb)}
     [:th (str ba "\u0394" bb)])]) ;; &Delta;

(defn report-table-row [idx slug browsers log thumbs?]
  (let [url-fn (fn [& suffix]
                 (apply str "/gen/" slug "/" idx suffix))
        thumb-display (if thumbs? "" "none")
        pass (:result log)
        diffs (:diffs log)
        violations (:violations log)
        html (:html log)]
    [:tr
     [:td idx]
     [:td
      {:style (mode-bg-style pass)}
      (if pass "PASS" "FAIL")]
     [:td
      [:a {:href (url-fn ".html")
           :title html}
       "html"]
      " / "
      [:a {:href (url-fn ".html.txt")
           :title html}
       "txt"]]
     [:td "\u00a0"] ;; &nbsp;
     (for [browser browsers]
       ^{:key browser}
       [:td
        (if (= (disj (set browsers) browser)
               (set (keys (get violations browser))))
          {:style {:vertical-align "top"
                   :text-align "center"
                   :background-color RED}}
          {:style {:vertical-align "top"
                   :text-align "center"}})
        [:a {:style {:padding-left "2px"
                     :padding-right "2px"}
             :href (url-fn "_" browser ".png")}
         (when (not thumbs?)
           [:span.tlink "png"])
         [:img.thumb {:style {:display thumb-display}
                      :src (url-fn "_" browser
                                   "_thumb.png")}]]])
     [:td "\u00a0"] ;; &nbsp;
     [:td {:style {:vertical-align "top"}}
      [:a {:href (url-fn "_avg.png")}
       (when (not thumbs?)
         [:span.tlink "png"])
       [:img.thumb {:style {:display thumb-display}
                    :src (url-fn "_avg_thumb.png")}]]]
     [:td "\u00a0"] ;; &nbsp;
     (for [[ba bb] (combinations browsers 2)
           :let [odiff (get-in diffs [ba bb])]]
       ^{:key (str ba bb)}
       [:td
        (if (or (get violations ba)
                (get violations bb))
          {:style {:vertical-align "top"
                   :text-align "center"
                   :background-color RED}}
          {:style {:vertical-align "top"
                   :text-align "center"}})
        [:a {:href (url-fn "_diff_" ba
                           "_" bb ".png")}
         [:img.thumb {:style {:display thumb-display}
                      :src (url-fn "_diff_" ba
                                   "_" bb "_thumb.png")}]
         [:br.thumb {:style {:display thumb-display}}]
         (.toFixed odiff 6)]])]))

(defn report-table [slug browsers iter-log thumbs?]
  [:table {:id "results" #_ #_ :border "1px" :style {:border-spacing "4px 0px"}}
   [:tbody
    [report-table-header browsers]
    (when (get iter-log 0)
      (for [[idx log] (sort-by first iter-log)]
        ^{:key idx}
        [report-table-row idx slug browsers log thumbs?]))]])

;; ---

(defn tab-input [idx]
  [:input {:id (str "tab" idx)
            :class "tabButton"
            :type "radio"
            :name "tabs"
            :defaultChecked (= idx 0)}])

(defn tab-label-main [idx connected?]
  [:label {:for (str "tab" idx)}
   (if connected?
     [:span.connected.fontawesome.fa-exchange]
     [:span.disconnected.fontawesome.fa-exchange])
   [:span.fontawesome
    "Main"]])

(defn tab-content-main [idx indexed-slugs log connected?]
  [:section {:id (str "content" idx)}
   [:span "Network state: " (if connected? "Connected" "Disconnected")]
   [:br][:br]
   [:table {:border "1px"}
    [:tbody
     [:tr
      [:th "Test"]
      [:th "Iterations"]
      [:th "Mode"]
      [:th "Info"]]
     (for [[idx slug] indexed-slugs
           :let [slug-log (-> log (get slug))]]
       ^{:key idx} [report-summary-row slug-log])]]])

(defn tab-label-slug [idx slug]
  [:label {:for (str "tab" idx)}
   slug
   [:span.fontawesome-red
    {:onClick (fn [evt]
                (swap! core/state assoc-in [:tabs slug :visible] false)
                (js/setTimeout #(let [input (js/document.getElementById "tab0")]
                                  (set! (.-checked input) true))
                               100))}
    "\uf057"]])

(defn tab-content-slug [idx slug browsers slug-log thumbs?]
  (let [iter-log (-> slug-log :iter-log)]
    [:section {:id (str "content" idx)}
     [report-summary-text slug-log]
     [report-table slug browsers iter-log thumbs?]]))


(defn main-element []
  (let [{:keys [test-state tabs connected]} @core/state
        {:keys [cfg log test-slugs]} test-state
        browsers (map name (-> cfg :browsers keys))
        indexed-slugs (map vector (drop 1 (range))
                           (sort test-slugs))
        indexed-tabs (map vector (drop 1 (range))
                          (sort-by key (filter (comp :visible val) tabs)))]
    [:main
     (list
       ^{:key :input} [tab-input 0]
       ^{:key :label} [tab-label-main 0 connected]
       (for [[idx [slug tab]] indexed-tabs]
         ^{:key idx}
         (list
           ^{:key :input} [tab-input idx]
           ^{:key :label} [tab-label-slug idx slug]))

       ^{:key :content-main} [tab-content-main 0 indexed-slugs log connected]
       (for [[idx [slug tab]] indexed-tabs
             :let [slug-log (-> log (get slug))
                   thumbs? (get-in tabs [slug :thumbs])]]
         ^{:key idx} [tab-content-slug idx slug browsers slug-log thumbs?]))]))


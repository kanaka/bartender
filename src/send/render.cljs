(ns send.render
  (:require [clojure.math.combinatorics :refer [combinations]]

            [wend.util :refer [strip-wrap-ahem]]
            [send.core :as core]
            [send.net :refer [load-edn]]))

(def RED "#f09090")
(def GREEN "#90f090")
(def TAN "#ffffd0")

(defn format-html [html-text]
  (-> html-text
      strip-wrap-ahem))

(defn mode-bg-style [token]
  {:background-color (get {:complete GREEN
                           true      GREEN
                           :shrunk   RED
                           false     RED}
                          token
                          TAN)})

(defn report-summary-info [slug-log]
  (let [slug (:test-slug slug-log)
        shrink (or (:shrunk slug-log) (:shrinking slug-log))]
    (when (:smallest shrink)
      (let [start-value (get-in @core/state [:test-state :test-start-value])
            base-size (count start-value)
            fail (format-html (get-in slug-log [:fail 0]))
            fail-size (count fail)
            shrunk (format-html (get-in shrink [:smallest 0]))
            shrunk-size (count shrunk)]
        ;;(prn :base-size base-size :base-value start-value)
        ;;(prn :fail-size fail-size :fail-value (get-in slug-log [:fail 0]))
        ;;(prn :shrunk-size shrunk-size :shrunk-value (get-in shrink [:smallest 0]))
        [", Shrink: "
         [:a {:href (str "/gen/" slug "/" (:failing-size slug-log) ".html.txt")
              :title (str fail)}
          (- fail-size base-size)]
         [:span.heydings " > "]
         [:a {:href (str "/gen/" slug "/" (:smallest-iter slug-log) ".html.txt")
              :title (str shrunk)}
          (- shrunk-size base-size)]
         " bytes"
         [:div
          "Elements: "
          (str (get-in @core/state [:test-state
                                    :log
                                    slug
                                    :TAP-summary]))]]))))

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
        iteration (-> slug-log :iter-log keys sort last)
        show-iter (if (:shrunk slug-log)
                    (:smallest-iter slug-log)
                    iteration)
        url-fn (fn [& suffix]
                 (apply str "/gen/" slug "/" show-iter suffix))]
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
     [:td {:style {:vertical-align "top"}}
      [:a {:href (url-fn "_davg.png")}
       [:img.thumb {:src (url-fn "_davg_thumb.png")}]]]
     (apply
       conj
       [:td
        (when (:fail slug-log)
          (str "First Failure: " (:failing-size slug-log)))]
       (report-summary-info slug-log))]))


(defn report-table-header [browsers]
  [:tr [:th "Iteration"] [:th "Result"] [:th "Html"]
   [:th "\u000a"] ;; &nbsp;
   (for [browser browsers]
     ^{:key browser}
     [:th browser])
   [:th "\u000a"] ;; &nbsp;
   (for [[ba bb] (combinations browsers 2)]
     ^{:key (str ba bb)}
     [:th (str ba "\u0394" bb)]) ;; &Delta;
   [:th "\u000a"] ;; &nbsp;
   [:th "\u0394 Avg"]])

(defn report-table-row [idx slug browsers log thumbs?]
  (let [url-fn (fn [& suffix]
                 (apply str "/gen/" slug "/" idx suffix))
        thumb-display (if thumbs? "" "none")
        pass (:result log)
        diffs (:diffs log)
        violations (:violations log)
        html (format-html (:html log))]
    [:tr
     [:td idx]
     [:td
      {:style (mode-bg-style pass)}
      (if pass "PASS" "FAIL")]
     [:td
      [:span.tooltip
       [:a {:href (url-fn ".html")}
        "html"]
       [:span.tooltiptext.tooltip-mid
        html]]
      " / "
      [:span.tooltip
       [:a {:href (url-fn ".html.txt")}
        "txt"]
       [:span.tooltiptext.tooltip-mid
        html]]]
     [:td "\u000a"] ;; &nbsp;
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
     [:td "\u000a"] ;; &nbsp;
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
         (.toFixed odiff 6)]])
     [:td "\u000a"] ;; &nbsp;
     [:td {:style {:vertical-align "top"}}
      [:a {:href (url-fn "_davg.png")}
       (when (not thumbs?)
         [:span.tlink "png"])
       [:img.thumb {:style {:display thumb-display}
                    :src (url-fn "_davg_thumb.png")}]]]
     ]))

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
     [:span.heydings-green "O "]
     [:span.heydings-red "O "])
   [:span
    "Bartender"]])

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
      [:th "\u0394 Avg"]
      [:th "Info"]]
     (for [[idx slug] indexed-slugs
           :let [slug-log (-> log (get slug))]]
       ^{:key idx} [report-summary-row slug-log])]]])

(defn tab-label-slug [idx slug]
  [:label {:for (str "tab" idx)}
   slug
   [:span.heydings-red
    {:onClick (fn [evt]
                (swap! core/state assoc-in [:tabs slug :visible] false)
                (js/setTimeout #(let [input (js/document.getElementById "tab0")]
                                  (set! (.-checked input) true))
                               100))}
    " X"]])

(defn tab-content-slug [idx slug browsers slug-log thumbs?]
  (let [iter-log (-> slug-log :iter-log)]
    [:section {:id (str "content" idx)}
     [report-summary-text slug-log]
     [report-table slug browsers iter-log thumbs?]]))


(defn monitor-element []
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


(defn intro-element []
  [:main
   "This dynamic entry point is unused. See static page in gh-pages"])

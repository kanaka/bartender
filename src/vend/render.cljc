(ns vend.render
  (:require [clojure.math.combinatorics :refer [combinations]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as S]
            [clojure.set :as set]
            [cemerick.url :refer [url]]

            #?(:cljs [reagent.core :as r])

            [send.core :as core]
            [wend.util :refer [strip-wrap-ahem]]
            [send.util :refer [get-TAP-tree]]))

(def MAX-FLAT-LENGTH 100)
;(def FAIL "#f09090")
(def FAIL "")

(def history #?(:cljs js/history :clj nil))

(defn ^:export print-log
  [slug]
  (let [{:keys [test-state]} @core/state
        {:keys [log]} test-state]
    (pprint (get log slug))))

(defn ^:export print-summaries
  []
  (let [{:keys [test-state]} @core/state
        {:keys [log test-slugs]} test-state
        TAP-summaries (into {} (for [slug test-slugs]
                                 [slug (get-in log [slug :TAP-summary])]))
        tags-attrs (get-TAP-tree test-state :tags :attrs "[body]" "[None]")
        tags-props (get-TAP-tree test-state :tags :props "[body]" "[None]")]
    (prn :TAP-summaries)
    (pprint TAP-summaries)
    (prn :tags-attrs)
    (pprint tags-attrs)
    (prn :tags-props)
    (pprint tags-props)))


(def modal-states (#?(:cljs r/atom :clj atom) {}))

;; general elements

(defn mangle-state
  [top-state mode]
  (let [{:keys [test-state config]} top-state
        {:keys [log]} test-state
        {:keys [start q] :or {start 0}} config
        start (int start)
        log (into {} (filter (fn [[s d]] (not (:result d))) log))
        browsers (map name (-> test-state :cfg :browsers keys))
        slugs (sort (keys log))
        ;; TODO: Workaround weird initial state
        slugs (if (= [nil] slugs) nil slugs)
        slug-idx (zipmap slugs (map str (range)))
        slugs (if (not (empty? q))
                (for [slug slugs
                      :let [tsum (get-in log [slug :TAP-summary])
                            sums (apply set/union (vals tsum))]
                      :when (contains? sums q)]
                  slug)
                slugs)
        slug-count (count slugs)
        start (if (>= start slug-count) 0 start)
        slugs (condp = mode
                :tapv slugs
                :flat (take MAX-FLAT-LENGTH (drop start slugs)))
        slug-smallest (into {} (for [[slug slug-log] log]
                                 [slug (strip-wrap-ahem
                                         (-> slug-log
                                             :shrunk
                                             :smallest
                                             first))]))]
    {:test-state (assoc test-state :log log)
     :config (merge config
                    {:mode mode
                     :browsers browsers
                     :start start
                     :slug-count slug-count
                     :slug-idx slug-idx
                     :slug-smallest slug-smallest})
     :slugs slugs}))

(defn update-start
  [config up-down]
  (let [{:keys [url start slug-count]} config
        {:keys [query]} url
        try-start (+ (* up-down MAX-FLAT-LENGTH) start)
        new-start (if (or (< try-start 0)
                          (> try-start slug-count))
                    start
                    try-start)
        new-query (assoc query "start" new-start)
        new-url (str (assoc url :query new-query))]
    (swap! core/state assoc-in [:config :start] new-start)
    (.replaceState history nil "" new-url)))

(defn update-search
  [config event]
  (let [{:keys [url]} config
        {:keys [query]} url
        input (.-currentTarget event)
        q (.-value input)
        new-url (str (assoc url :query (assoc query "q" q)))]
    (swap! core/state #(-> %
                           (assoc-in [:config :q] q)
                           (assoc-in [:config :start] 0)))
    (.replaceState history nil "" new-url)))


(defn info-modal
  [config title content]
  [:div {:style (when (-> @modal-states (get :info) not)
                  {:display "none"})
         :class "modal"}
   [:button {:class "close-button"
             :on-click #(swap! modal-states
                               dissoc :info)}
    "X"]
   [:div {:class "modal-guts"}
    [:h1 title]
    content]])

(defn result-row
  [test-state config slug]
  (let [{:keys [log]} test-state
        {:keys [links gen-dir browsers slug-idx slug-smallest]} config
        row-id (get slug-idx slug)
        slug-log (get log slug)
        iter (:smallest-iter slug-log)
        summary (:TAP-summary slug-log)
        violations (:smallest-violations slug-log)
        html (get slug-smallest slug)
        url-fn (fn [& suffix]
                 (apply str gen-dir slug "/" iter suffix))]
    ^{:key slug}
    [:tr
     [:td row-id]
     [:td
      [:span.tooltip
       [:a {:href (url-fn ".html")}
        "html"]
       [:span.tooltiptext.tooltip-mid
        html]]
      (when links
        " / ")
      (when links
        [:span.tooltip
         [:a {:href (url-fn ".html.txt")}
          "txt"]
         [:span.tooltiptext.tooltip-mid
          html]])]
     [:td {:class "summary-td"}
      [:ul.tapv-summary
       [:li [:b "Tags:"] " " (S/join ", " (:tags summary))]
       [:li [:b "Attrs:"] " " (S/join ", " (:attrs summary))]
       [:li [:b "Props:"] " " (S/join ", " (:props summary))]]]
     (for [browser browsers]
       ^{:key browser}
       [:td
        (if (= (disj (set browsers) browser)
               (set (keys (get violations browser))))
          {:style {:vertical-align "top"
                   :text-align "center"
                   :background-color FAIL}}
          {:style {:vertical-align "top"
                   :text-align "center"}})
        (if links
          [:a {:style {:padding-left "2px"
                       :padding-right "2px"}
               :href (url-fn "_" browser ".png")}
           [:img.thumb {:src (url-fn "_" browser "_thumb.png")}]]
          [:img.thumb {:src (url-fn "_" browser "_thumb.png")}])])
     (if links
       [:td {:style {:vertical-align "top"}}
        [:a {:href (url-fn "_davg.png")}
         [:img.thumb {:src (url-fn "_davg_thumb.png")}]]]
       [:td {:style {:vertical-align "top"}}
        [:img.thumb {:src (url-fn "_davg_thumb.png")}]])]))


(defn result-table
  [test-state config table-class slugs]
  (let [{:keys [log]} test-state
        browsers (map name (-> test-state :cfg :browsers keys)) ]
    [:table {:class table-class}
     [:tbody
      [:tr
       [:th "ID"]
       [:th "HTML"]
       [:th "Summary"]
       (for [browser browsers]
         ^{:key browser}
         [:th browser])
       [:th "\u0394 Avg"]]
      (doall (map #(result-row test-state config %)
                  (sort slugs)))]]))


;; tapv specific elements

(defn tapv-cell-click [event]
  (let [button (.-currentTarget event)
        id (.-id button)
        [tid row col] (S/split id #"@")]
    (swap! modal-states assoc :cell [tid row col])))

(defn make-tapv-table
  [test-state config tid upper-left row-col-slug-tree]
  (let [{:keys [log]} test-state
        browsers (map name (-> test-state :cfg :browsers keys))
        rows (sort (set (keys row-col-slug-tree)))
        cols (sort (set (for [r rows
                              c (keys (get row-col-slug-tree r))]
                          c)))
        cell-count (fn [row col slugs]
                     ^{:key col}
                     [:td
                      [:button {:id (str tid "@" row "@" col)
                                :class "atn-btn"
                                :on-click tapv-cell-click}
                       (str (count slugs))]])]
    [:div
     [:table.elem-table {:class "table-header-rotated"}
      [:tbody
       [:tr
        [:th
         [:div
          [:span upper-left]]]
        (for [col cols]
          ^{:key col}
          [:th {:class "rotate-45"}
           [:div
            [:span col]]])]
       (doall (for [row rows]
                ^{:key row}
                [:tr
                 [:th {:class "row-header" :nowrap 1} row]
                 (doall (for [col cols
                              :let [slugs (get-in row-col-slug-tree [row col])]]
                          (if (= (count slugs) 0)
                            ^{:key col} [:td " "]
                            (cell-count row col slugs))))]))]]]))

(defn tapv-info-content
  [config]
  (let [{:keys [url]} config
        {:keys [path query]} url
        flat-url (str (assoc url
                             :path (S/replace path #"tapv\.html" "flat.html")
                             :query (dissoc query "q" "start")))]
    (prn :url url :flat-url flat-url)
    [:div
     [:p "This data was generated by " [:a {:href "https://github.com/kanaka/bartender"} "Bartender"] ". A flat linear representation of the same data is available " [:a {:href flat-url} "here"] "."]
     [:p [:b "Notes:"]]
     [:ul {:style {:margin-inline-start "30px"}}
      [:li "The data is organized into two tables. The first table organizes test cases by tag names and tag attributes that appear in the test case. The second table organizes test cases by tag names and CSS property names that appear in the test case. "]
      [:li "Every test case occurs in both tables at least once and can appear more than once in each table. Each unique test case has an ID that can be used to correlate it with occurences of the test case either in the same table, in the other table, and on the " [:a {:href flat-url} "flat representation"] "."]
      [:li "Each cell with a matching test case will have a button with the number of test cases that match that cell. Clicking on a cell button will open a modal dialog showing details for all the test cases that match that cell."]
      [:li "The 'BODY' tag is for tag attributes or property names that occur in the body tag. The '[None]' attribute means the test cases in the row do not have attributes (apart from the 'style' attribute). The '[None]' CSS property name means the test case in the row do not have any CSS styles."]
      [:li "Test cases occur more frequently under the 'div' and 'span' tags because they are weighted more heavily in the grammar used to generate the test cases. "]]]))


;; flat specific elements

(defn flat-info-content
  [config]
  (let [{:keys [url]} config
        {:keys [path query]} url
        tapv-url (str (assoc url
                             :path (S/replace path #"flat\.html" "tapv.html")
                             :query (dissoc query "q" "start")))]
    (prn :url url :tapv-url tapv-url)
    [:div
     [:p "This data was generated by " [:a {:href "https://github.com/kanaka/bartender"} "Bartender"] ". A representation of the same data grouped by HTML tags, attributes and CSS properties is available " [:a {:href tapv-url} "here"] "."]
     [:p "The 'HTML' column links to the web page test case (hover over to show a the raw HTML text). The 'Summary' column lists the tag names, tag attributes, and CSS style property names that occur in the test case. If you type the full name of an HTML tag, attribute or CSS property name and press enter, the list will be filtered to matching test cases."]]))


;; top-level start elements

(defn tapv-tables-element []
  #_(print-summaries)
  (let [{:keys [test-state config slugs]} (mangle-state @core/state :tapv)
        attrs-tags (get-TAP-tree test-state :attrs :tags "[None]" "BODY")
        props-tags (get-TAP-tree test-state :props :tags "[None]" "BODY")]
    [:div
     [:nav.controls
      [:span.controls-left
       [:button {:class "info-button"
                 :on-click #(swap! modal-states assoc :info true)}
        "Page Info"]]]
     [:div#tab
      (if (not slugs)
        [:main
         "Loading..."]
        [:main
         (info-modal config "Page Info" (tapv-info-content config))
         [:h2 "Rendering Differences Arranged by Tags & Attributes"]
         (make-tapv-table test-state config :TnA "\u000a" attrs-tags)
         [:br]
         [:h2 "Rendering Differences Arranged by Tags & Properties"]
         (make-tapv-table test-state config :TnP "\u000a" props-tags)

         ;; The modal dialog that will contain the table for the cell
         ;; that was clicked.
         [:div {:style (when (empty? @modal-states)
                         {:display "none"})
                :class "modal-overlay"
                :on-click #(reset! modal-states {})}
          (when-let [[tid row col :as cell] (get @modal-states :cell)]
            (let [slugs (condp = tid
                          ":TnA" (get-in attrs-tags [row col])
                          ":TnP" (get-in props-tags [row col]))]
              [:div {:style (when (not cell)
                              {:display "none"})
                     :class "modal"
                     :on-click #(.stopPropagation %)}
               [:button {:class "close-button"
                         :on-click #(reset! modal-states {})}
                "X"]
               [:div {:class "modal-guts"}
                [:h1 (str col " + " row)]
                (result-table test-state config "result-table" slugs)]]))]])]]))

(defn flat-table-element []
  (let [{:keys [test-state config slugs]} (mangle-state @core/state :flat)
        {:keys [mode url start slug-count]} config]
    [:div
     [:nav.controls
      [:span.controls-left
       [:button {:class "info-button"
                 :on-click #(swap! modal-states assoc :info true)}
        "Page Info"]]
      [:span.controls-right
       [:input {:type :text
                :size 15
                :default-value (when (:q config)
                                 (:q config))
                :placeholder "tag/attr/prop filter"
                :on-blur #(update-search config %)
                :on-key-up #(when (= 13 (.-keyCode %))
                              (update-search config %))}]
       [:button {:on-click #(update-start config -1)} "<-"]
       [:span (str start "-" (dec (min slug-count
                                       (+ MAX-FLAT-LENGTH start)))
                   " of " (dec slug-count))]
       [:button {:on-click #(update-start config 1)} "->"]]]
     [:div#tab
      (if (not slugs)
        [:main
         "Loading..."]
        [:main
         (info-modal config "Page Info" (flat-info-content config))
         [:h2 "Flat List of Rendering Differences"]
         (result-table test-state config "result-table" slugs)
         [:div {:style (when (empty? @modal-states)
                         {:display "none"})
                :class "modal-overlay"
                :on-click #(reset! modal-states {})}]])]]))

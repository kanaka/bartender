(ns vend.render
  (:require [clojure.math.combinatorics :refer [combinations]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as S]

            #?(:cljs [reagent.core :as r])

            [send.core :as core]
            [wend.util :refer [strip-wrap-ahem]]
            [send.util :refer [get-TAP-tree]]))

(def MAX-FLAT-LENGTH 100)

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
    (pprint tags-props)
    ))


(def modal-states (#?(:cljs r/atom :clj atom) {}))

;; general elements

(defn mangle-state
  [top-state]
  (let [{:keys [test-state config]} top-state
        {:keys [log]} test-state
        log (into {} (filter (fn [[s d]] (not (:result d))) log))
        browsers (map name (-> test-state :cfg :browsers keys))
        slugs (sort (keys log))
        ;; TODO: Workaround weird initial state
        slugs (if (= [nil] slugs) nil slugs)
        slug-idx (zipmap slugs (map str (range)))
        slug-smallest (into {} (for [[slug slug-log] log]
                                 [slug (strip-wrap-ahem
                                         (-> slug-log
                                             :shrunk
                                             :smallest
                                             first))]))]
    {:test-state (assoc test-state :log log)
     :config (merge config
                    {:browsers browsers
                     :slug-idx slug-idx
                     :slug-smallest slug-smallest})
     :slugs slugs}))

(defn info-modal
  [title content]
  [:div
   [:button {:class "info-button"
             :on-click #(swap! modal-states
                               assoc :info true)}
    "Page Info"]
   [:div {:style (when (-> @modal-states (get :info) not)
                   {:display "none"})
          :class "modal"}
    [:button {:class "close-button"
              :on-click #(swap! modal-states
                                dissoc :info)}
     "X"]
    [:div {:class "modal-guts"}
     [:h1 title]
     content]]])

(defn result-row
  [test-state config slug]
  (let [{:keys [log]} test-state
        {:keys [links gen-dir browsers slug-idx slug-smallest]} config
        row-id (get slug-idx slug)
        slug-log (get log slug)
        iter (:smallest-iter slug-log)
        summary (:TAP-summary slug-log)
        html (get slug-smallest slug)
        url-fn (fn [& suffix]
                 (apply str gen-dir slug "/" iter suffix))]
    ^{:key slug}
    [:tr
     [:td row-id]
     [:td
      (if links
        [:span.tooltip
         [:a {:href (url-fn ".html")}
          "html"]
         [:span.tooltiptext.tooltip-mid
          html]]
        [:span.tooltip
         "html"
         [:span.tooltiptext.tooltip-mid
          html]])
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
        {:style {:vertical-align "top"
                 :text-align "center"}}
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
       [:th "Html"]
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
  [:div
   [:p "This data was generated by " [:a {:href "https://github.com/kanaka/bartender"} "Bartender"] ". A flat linear representation of the same data is available " [:a {:href (str "flat.html" (:search config))} "here"] "."]
   [:p [:b "Notes:"]]
   [:ul {:style {:margin-inline-start "30px"}}
    [:li "The data is organized into two tables. The first table organizes test cases by tag names and tag attributes that appear in the test case. The second table organizes test cases by tag names and CSS property names that appear in the test case. "]
    [:li "Every test case occurs in both tables at least once and can appear more than once in each table. Each unique test case has an ID that can be used to correlate it with occurences of the test case either in the same table, in the other table, and on the " [:a {:href (str "flat.html" (:search config))} "flat representation"] "."]
    [:li "Each cell with a matching test case will have a button with the number of test cases that match that cell. Clicking on a cell button will open a modal dialog showing details for all the test cases that match that cell."]
    [:li "The 'BODY' tag is for tag attributes or property names that occur in the body tag. The '[None]' attribute means the test cases in the row do not have attributes (apart from the 'style' attribute). The '[None]' CSS property name means the test case in the row do not have any CSS styles."]
    [:li "Test cases occur more frequently under the 'div' and 'span' tags because they are weighted more heavily in the grammar used to generate the test cases. "]]])


;; flat specific elements

(defn flat-info-content
  [config]
  [:div
   [:p "This data was generated by " [:a {:href "https://github.com/kanaka/bartender"} "Bartender"] ". A representation of the same data grouped by HTML tags, attributes and CSS properties is available " [:a {:href (str "tapv.html" (:search config))} "here"] "."]
   [:p "The Html column has a 'button' that will show the raw HTML text of the test case when the mouse rolls over it. The summary column lists the tag names, tag attributes, and CSS style property names that occur in the test case. "]])


;; top-level start elements

(defn tapv-tables-element []
  #_(print-summaries)
  (let [{:keys [test-state config slugs]} (mangle-state @core/state)
        attrs-tags (get-TAP-tree test-state :attrs :tags "[None]" "BODY")
        props-tags (get-TAP-tree test-state :props :tags "[None]" "BODY")]
    (if (not slugs)
      [:main
       "Loading..."]
      [:main
       (info-modal "Page Info" (tapv-info-content config))
       [:h2 "Rendering Differences Arranged Tags & Attributes"]
       (make-tapv-table test-state config :TnA "\u000a" attrs-tags)
       [:br]
       [:h2 "Rendering Differences Arranged Tags & Properties"]
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
              (result-table test-state config "result-table" slugs)]]))]])))

(defn flat-table-element []
  (let [{:keys [test-state config slugs]} (mangle-state @core/state)
        {:keys [start] :or {start 0}} config
        start (int start)
        slugs (take MAX-FLAT-LENGTH (drop start slugs))]
    (if (not slugs)
      [:main
       "Loading..."]
      [:main
       (info-modal "Page Info" (flat-info-content config))
       [:h2 "Flat List of Rendering Differences"]
       (result-table test-state config "result-table" slugs)
       [:div {:style (when (empty? @modal-states)
                       {:display "none"})
              :class "modal-overlay"
              :on-click #(reset! modal-states {})}]])))

(ns vend.render
  (:require [clojure.math.combinatorics :refer [combinations]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as S]

            [reagent.core :as r]
            [antizer.reagent :as ant]

            [send.core :as core]
            [send.render :refer [format-html]]
            [send.net :refer [load-edn]]
            [send.util :refer [get-TAP-tree]]))

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

(def IDX-CHARS
  (concat (map str (range 10))
          (map #(char (+ 65 %)) (range 26))
          (map #(char (+ 97 %)) (range 26))))

(defn IDX
  [n]
  (if (= n 0)
    "0"
    (loop [acc '() n n]
      (if (= n 0)
        (S/join "" acc)
        (let [remainder (mod n (count IDX-CHARS))]
          (recur (conj acc (nth IDX-CHARS remainder))
                 (int (/ n (count IDX-CHARS)))))))))


(def modal-states (r/atom {}))

(defn result-row
  [test-state config slug]
  (let [{:keys [log]} test-state
        links? (:links config)
        gen-dir (:gen-dir config)
        browsers (map name (-> test-state :cfg :browsers keys))
        slug-idx (zipmap (sort (keys log)) (range))
        row-id (IDX (get slug-idx slug))
        slug-log (get log slug)
        iter (:smallest-iter slug-log)
        summary (:TAP-summary slug-log)
        html (format-html
               (-> slug-log :shrunk :smallest first))
        url-fn (fn [& suffix]
                 (apply str gen-dir slug "/" iter suffix))]
    ^{:key slug}
    [:tr
     [:td row-id]
     [:td
      (if links?
        [ant/tooltip {:title html}
         [:a {:href (url-fn ".html")}
          "html"]]
        [ant/tooltip {:title html}
         [:span.show-html {:href (url-fn ".html")}
          "html"]])
      (when links?
        " / ")
      (when links?
        [ant/tooltip {:title html}
         [:a {:href (url-fn ".html.txt")}
          "txt"]])]
     [:td
      [:ul.tapv-summary
       [:li [:b "Tags:"] " " (S/join ", " (:tags summary))]
       [:li [:b "Attrs:"] " " (S/join ", " (:attrs summary))]
       [:li [:b "Props:"] " " (S/join ", " (:props summary))]]]
     (for [browser browsers]
       ^{:key browser}
       [:td
        {:style {:vertical-align "top"
                 :text-align "center"}}
        (if links?
          [:a {:style {:padding-left "2px"
                       :padding-right "2px"}
               :href (url-fn "_" browser ".png")}
           [:img.thumb {:src (url-fn "_" browser "_thumb.png")}]]
          [:img.thumb {:src (url-fn "_" browser "_thumb.png")}])])
     (if links?
       [:td {:style {:vertical-align "top"}}
        [:a {:href (url-fn "_davg.png")}
         [:img.thumb {:src (url-fn "_davg_thumb.png")}]]]
       [:td {:style {:vertical-align "top"}}
        [:img.thumb {:src (url-fn "_davg_thumb.png")}]])]))


(defn result-table
  [test-state config table-class slugs]
  (let [{:keys [log]} test-state
        browsers (map name (-> test-state :cfg :browsers keys)) ]
    [:table {:class table-class #_#_:border "1px"}
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

(defn make-tapv-table
  [test-state config tid upper-left row-col-slug-tree]
  (let [{:keys [log]} test-state
        browsers (map name (-> test-state :cfg :browsers keys))
        rows (sort (set (keys row-col-slug-tree)))
        cols (sort (set (for [r rows
                              c (keys (get row-col-slug-tree r))]
                          c)))
        cell (fn [row col]
               (let [slugs (get-in row-col-slug-tree [row col])]
                 (if (= (count slugs) 0)
                   " "
                   [:span
                    [ant/button {:style {:background-color "#ffb088"
                                         :padding "1px"
                                         :height "unset"}
                                 :on-click #(swap! modal-states
                                                   assoc [tid row col] true)}
                     (str (count slugs))]
                    [ant/modal {:visible (-> @modal-states (get [tid row col]))
                                :title (str col " + " row)
                                :on-cancel #(swap! modal-states
                                                   dissoc [tid row col])
                                :footer nil}
                     (r/as-element
                       (result-table test-state config "modal-table" slugs))]])))]
    [:table.elem-table {:class "table-header-rotated" #_#_:border "1px"}
     [:tbody
      [:tr
       [:th
        [:div
         [:span upper-left]]]
       (for [col cols]
         ^{:key col}
         [:th {:class "rotate-45"}
          [:div
           [:span col]]]
         )]
      (doall (for [row rows]
        ^{:key row}
        [:tr
         [:th {:class "row-header" :nowrap 1} row]
         (doall (for [col cols]
           ^{:key col}
           [:td (cell row col)]))]))]]))

;; top-level start elements

(defn tapv-tables-element []
  #_(print-summaries)
  (let [{:keys [test-state config]} @core/state
        attrs-tags (get-TAP-tree test-state :attrs :tags "[None]" "BODY")
        props-tags (get-TAP-tree test-state :props :tags "[None]" "BODY")]
    [:main
     [:div
      [ant/button {:class "info-button"
                   :on-click #(swap! modal-states assoc "info" true)}
       "Page Info"]
      [ant/modal {:visible (-> @modal-states (get "info"))
                  :title "Page Info"
                  :on-cancel #(swap! modal-states dissoc "info")
                  :footer nil}
       [:p
        "This data was generated by "
        [:a {:href "https://github.com/kanaka/bartender"} "Bartender"] ". "
        "A flat linear representation of the same data is available "
        [:a {:href (str "flat.html" (:search config))} "here"] "."]
       [:p
        "The data is organized into two tables. "
        "The first table organizes test cases by tag names and tag attributes that appear in the test case. "
        "The second table organizes test cases by tag names and CSS property names that appear in the test case. "
        "Each cell with a matching test case will have a button with the number of test cases that match that cell. "
        "Clicking on a cell button will open a modal dialog showing details for all the test cases that match that cell. "
        "Every test case occurs in both tables at least once and can appear more than once in each table. "
        "Each unique test case has an ID that can be used to correlate it with occurences of the test case either in the same table or in the other table. "
        "The 'BODY' tag is for tag attributes or property names that occur in the body tag. "
        "The '[None]' attribute means the test cases in the row do not have attributes (apart from the 'style' attribute). "
        "The '[None]' CSS property name means the test case in the row do not have any CSS styles. "]
       ]]
     [:h2 "Rendering Differences Arranged Tags & Attributes"]
     (make-tapv-table test-state config :TnA "\u000a" attrs-tags)
     [:br]
     [:h2 "Rendering Differences Arranged Tags & Properties"]
     (make-tapv-table test-state config :TnP "\u000a" props-tags)]))


(defn flat-table-element []
  (let [{:keys [test-state config]} @core/state
        {:keys [log]} test-state
        browsers (map name (-> test-state :cfg :browsers keys))
        slugs (keys log)
        ;; TODO: Workaround weird initial state
        slugs (if (= [nil] slugs) nil slugs)]
    [:main
     [:div
      [ant/button {:class "info-button"
                   :on-click #(swap! modal-states assoc "info" true)}
       "Page Info"]
      [ant/modal {:visible (-> @modal-states (get "info"))
                  :title "Page Info"
                  :on-cancel #(swap! modal-states dissoc "info")
                  :footer nil}
       [:p
        "This data was generated by "
        [:a {:href "https://github.com/kanaka/bartender"} "Bartender"] ". "
        "A representation of the same data grouped by HTML tags, attributes and CSS properties is available "
        [:a {:href (str "tapv.html" (:search config))} "here"] "."]
       [:p
        "The Html column has a 'button' that will show the raw HTML text of the test case when the mouse rolls over it. "
        "The summary column lists the tag names, tag attributes, and CSS style property names that occur in the test case. "]
       ]]
     [:h2 "Flat List of Rendering Differences"]
     (result-table test-state config "modal-table" slugs)]))

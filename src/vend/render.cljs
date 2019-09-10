(ns vend.render
  (:require [clojure.math.combinatorics :refer [combinations]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as S]

            [reagent.core :as r]
            [antizer.reagent :as ant]

            [send.core :as core]
            [send.render]
            [send.net :refer [load-edn]]))

(defn ^:export print-log
  [slug]
  (let [{:keys [test-state]} @core/state
        {:keys [log]} test-state]
    (pprint (get log slug))))

(defn get-TAP-tree
  [test-state k1 k2 k1-none k2-none]
  (let [{:keys [log test-slugs]} test-state
        TAP-summaries (into {} (for [slug test-slugs]
                                 [slug (get-in log [slug :TAP-summary])]))
        only-k1 (for [[slug tsum] TAP-summaries
                      v1 (get tsum k1)
                      :when (= 0 (count (get tsum k2)))]
                  [v1 k2-none slug])
        only-k2 (for [[slug tsum] TAP-summaries
                      v2 (get tsum k2)
                      :when (= 0 (count (get tsum k1)))]
                  [k1-none v2 slug])
        both (for [[slug tsum] TAP-summaries
                  v1 (get tsum k1)
                  v2 (get tsum k2)]
              [v1 v2 slug])
        tree (reduce
               (fn [t [v1 v2 slug]]
                 (update-in t [v1 v2] (fnil conj []) slug))
               {}
               #_both
               (concat only-k1 only-k2 both))]
    tree))

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

(defn modal-table-row [row-id iter slug browsers html]
  (let [url-fn (fn [& suffix]
                 (apply str "/gen/" slug "/" iter suffix))]
    ^{:key slug}
    [:tr
     [:td row-id]
     [:td
      [ant/tooltip {:title html}
       [:a {:href (url-fn ".html")}
        "html"]]
      " / "
      [ant/tooltip {:title html}
       [:a {:href (url-fn ".html.txt")}
        "txt"]]]
     (for [browser browsers]
       ^{:key browser}
       [:td
	{:style {:vertical-align "top"
		 :text-align "center"}}
        [:a {:style {:padding-left "2px"
                     :padding-right "2px"}
             :href (url-fn "_" browser ".png")}
         [:img.thumb {:src (url-fn "_" browser
                                   "_thumb.png")}]]])
     [:td {:style {:vertical-align "top"}}
      [:a {:href (url-fn "_davg.png")}
       [:img.thumb {:src (url-fn "_davg_thumb.png")}]]]]))


(defn make-elem-table
  [test-state tid upper-left row-col-slug-tree]
  (let [{:keys [log]} test-state
        browsers (map name (-> test-state :cfg :browsers keys))
        slug-idx (zipmap (sort (keys log)) (range))
        rows (sort (set (keys row-col-slug-tree)))
        cols (sort (set (for [r rows
                              c (keys (get row-col-slug-tree r))]
                          c)))
        slug->row (fn [slug]
                    (let [row-id (IDX (get slug-idx slug))
                          slug-log (get log slug)
                          iter (:smallest-iter slug-log)
                          html (-> slug-log :shrunk :smallest)]
                      (modal-table-row row-id iter slug browsers html)))
        cell (fn [row col]
               (let [slugs (get-in row-col-slug-tree [row col])]
                 (if (= (count slugs) 0)
                   " "
                   [:span
                    [ant/button {:style {:background-color "#ff9018"
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
                       [:table.modal-table #_{:border "1px"}
                        [:tbody
                         [:tr
                          [:th "ID"]
                          [:th "Html"]
                          (for [browser browsers]
                            ^{:key browser}
                            [:th browser])
                          [:th "Average \u0394"]]
                         (doall (map slug->row (sort slugs)))]])]])))]
    [:table.elem-table {:border "1px"}
     [:tbody
      [:tr
       [:td upper-left]
       (for [col cols]
         ^{:key col}
         [:th col]
         )]
      (doall (for [row rows]
        ^{:key row}
        [:tr
         [:th row]
         (doall (for [col cols]
           ^{:key col}
           [:td (cell row col)]))]))]]))

(defn main-element []
  #_(print-summaries)
  (let [{:keys [test-state]} @core/state
        attrs-tags (get-TAP-tree test-state :attrs :tags "[None]" "BODY")
        props-tags (get-TAP-tree test-state :props :tags "[None]" "BODY")]
    [:main
     [:h2 "Tags & Attributes"]
     (make-elem-table test-state :TnA "\u00a0" attrs-tags)
     [:br]
     [:h2 "Tags & Properties"]
     (make-elem-table test-state :TnP "\u00a0" props-tags)]))


(ns rend.html
  (:require [hiccup.core :as hiccup]
            [clojure.math.combinatorics :refer [combinations]]))

(def RED "#ff8080")
(def GREEN "#80ff80")


(defn render-report-row [browsers log idx]
  (let [pass (:result log)
        diffs (:diffs log)
        violations (:violations log)
        html (:html log)]
    (vec
      (concat
        [:tr
         [:td idx]
         [:td
          {:style (if pass
                    (str "background-color: " GREEN)
                    (str "background-color: " RED))}
          (if pass "PASS" "FAIL")]
         [:td
          [:a {:href (str idx ".html")
               :title (str (hiccup/html html))}
           "html"]
          " / "
          [:a {:href (str idx ".html.txt")
               :title (str (hiccup/html html))}
           "txt"]]
         [:td "&nbsp;"]]
        (for [browser browsers]
          [:td
           (if (= (disj (set browsers) browser)
                  (set (keys (get violations browser))))
             {:style (str "vertical-align: top; text-align: center; "
                          "background-color: " RED)}
             {:style "vertical-align: top; text-align: center"})
           [:a {:style "padding-left: 2px; padding-right: 2px"
                :href (str idx "_" (:id browser) ".png")}
            [:span.tlink "png"]
            [:img.thumb {:style "display: none"
                         :src (str idx "_" (:id browser)
                                   "_thumb.png")}]]])
        [[:td "&nbsp;"]
         [:td {:style "vertical-align: top"}
          [:a {:href (str idx "_avg.png")}
           [:span.tlink "png"]
           [:img.thumb {:style "display: none"
                        :src (str idx "_avg_thumb.png")}]]]
         [:td "&nbsp;"]]
        (for [[ba bb] (combinations browsers 2)
              :let [odiff (get-in diffs [ba bb])]]
          [:td
           (if (or (get violations ba)
                   (get violations bb))
             {:style (str "vertical-align: top; text-align: center; background-color: " RED)}
             {:style (str "vertical-align: top; text-align: center")})
           [:a {:href (str idx "_diff_" (:id ba)
                           "_" (:id bb) ".png")}
            [:img.thumb {:style "display: none"
                         :src (str idx "_diff_" (:id ba)
                                   "_" (:id bb) "_thumb.png")}]
            [:br.thumb {:style "display: none"}]
            (format "%.6f" odiff)]])))))

(defn render-summary [state]
  (let [report (:latest-report state)
        rtype (:type report)]
    (vec
      (concat
        [:span
         (str "Iteration " (:iteration state)
              ", Mode: " (name (or rtype :init))
              (when (:first-fail-number state)
                (str ", First Failure: " (:first-fail-number state))))]
        (when (:smallest state)
          [", Shrink: "
           [:a {:href (str (:first-fail-number state) ".html.txt")
                :title (str (hiccup/html (get-in state [:failing-args 0])))}
            (count (get-in state [:failing-args 0]))]
           " &rarr; "
           [:a {:href (str (:smallest-number state) ".html.txt")
                :title (str (hiccup/html (get-in state [:smallest :args 0])))}
            (count (get-in state [:smallest :args 0]))]
            " bytes"])))))

;; Generate an HTML index page for the current test results
(defn render-report [state]
  (let [cfg       (-> state :cfg)
        port      (-> cfg :web :port)
        threshold (-> cfg :compare :threshold)
        browsers  (-> cfg :browsers)
        run       (-> state :run)
        logs      (-> state :run-log (get run) :iter-log)]
    [:html
     [:style "a {text-decoration: none}"]
     [:body {:style "margin: 0px;"}
      [:div {:class "header"
             :style "position: fixed;
                     height: 45px;
                     width: 100%;
                     padding: 5px;
                     background-color: #efefef"}
       [:input#toggle {:type "button"
                       :value "Show Thumbnails"
                       :onclick "toggle_thumbs()"}]
       [:span {:style "padding: 4px;"}
        "Threshold value: " (format "%.6f" threshold)]
       [:div {:id "summary"
              :style "padding: 4px;"}
        (render-summary state)]]
      [:div {:class "content"
             :style "padding-top: 55px"}
       (vec
         (concat
           [:table {:id "results" :style "border-spacing: 4px 0px"}
            (vec
              (concat
                [:tr [:th "Iteration"] [:th "Result"] [:th "Html"]
                 [:th "&nbsp;"]]
                (for [browser browsers]
                  [:th (str (:id browser))])
                [[:th "&nbsp;"] [:th "Average"] [:th "&nbsp;"]]
                (for [[ba bb] (combinations browsers 2)]
                  [:th (str (:id ba) "&Delta;" (:id bb))])))]
           (for [[i log-entry] (sort-by first logs)]
             (render-report-row browsers log-entry i))))]
      [:script {:src "../static/report.js"}]
      [:script (str "connect('ws://' + location.host + '/ws')")]]]))

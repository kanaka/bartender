(ns rend.webdriver
  (:require [org.httpkit.client :as http]
            [clojure.data.json :as json]))

(def browser-state (atom {}))

(defn addr [cfg]
  (let [host (get cfg :host "localhost")
        port (:port cfg)]
    (str "http://" host ":" port)))

(defn status [browser]
  (let [url (str (addr browser) "/status")
        opts {:headers {"Host" (str "localhost:" (:port browser))}}
        {:keys [status error body] :as resp} @(http/get url opts)]
;    (prn :status status :error error :body body :resp resp)
    (if (or error (< status 200) (> status 299))
      (if error
        (throw (Exception. error))
        (throw (Exception. body)))
      (json/read-str body :key-fn keyword))))

(defn- init-session* [browser desired]
  (let [url (str (addr browser) "/session")
        body (json/write-str {:desiredCapabilities (or desired {})})
        opts {:headers {"Host" (str "localhost:" (:port browser))}
              :body body}
;        _ (prn :init-session :url url :opts opts)
        {:keys [status error body] :as resp} @(http/post url opts)]
;    (prn :status status :error error :body body :resp resp)
    (if (or error (< status 200) (> status 299))
      (if error
        (throw (Exception. error))
        (throw (Exception. body)))
      (let [session (json/read-str body :key-fn keyword)]
        (swap! browser-state assoc browser session)))))

(defn init-session [browser desired]
  (let [sessions @browser-state
        session (get sessions  browser)]
    (if session
      sessions
      (init-session* browser desired))))

(defn get-session [browser]
  (let [session (get @browser-state browser)]
    (if session
      session
      (init-session browser {}))))

(defn GET [browser path]
  (let [session (get-session browser)
        session-id (:sessionId session)
        url (str (addr browser) "/session/" session-id "/" path)
        opts {:headers {"Host" (str "localhost:" (:port browser))}}
        {:keys [status error body] :as resp} @(http/get url opts)]
;    (prn :get :status status :error error :body body :resp resp)
    (if (or error (< status 200) (> status 299))
      (if error
        (throw (Exception. error))
        (throw (Exception. body)))
      (json/read-str body :key-fn keyword))))

(defn POST [browser path data]
  (let [session (get-session browser)
        session-id (:sessionId session)
        url (str (addr browser) "/session/" session-id "/" path)
        body (json/write-str data)
        opts {:headers {"Host" (str "localhost:" (:port browser))}
              :body body}
;        _ (prn :post :url url :opts opts)
        {:keys [status error body] :as resp} @(http/post url opts)]
;    (prn :post :status status :error error :body body :resp resp)
    (if (or error (< status 200) (> status 299))
      (if error
        (throw (Exception. error))
        (throw (Exception. body)))
      (json/read-str body :key-fn keyword))))

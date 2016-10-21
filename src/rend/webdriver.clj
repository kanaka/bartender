(ns rend.webdriver
  (:require [org.httpkit.client :as http]
            [clojure.data.json :as json]))

(def browser-state (atom {}))

(defn status [browser]
  (let [host (get browser :host "localhost")
        port (:port browser)
        opts {}
        url (str "http://" host ":" port "/status")
        {:keys [status error body] :as resp} @(http/get url opts)]
;    (prn :status status :error error :body body :resp resp)
    (if (or (< status 200) (> status 299))
      (if error
        (throw (Exception. error))
        (throw (Exception. body)))
      (json/read-str body :key-fn keyword))))

(defn init-session [browser desired]
  (let [host (get browser :host "localhost")
        port (:port browser)
        body (json/write-str {:desiredCapabilities (or desired {})})
        opts {:body body}
        url (str "http://" host ":" port "/session")
        {:keys [status error body] :as resp} @(http/post url opts)]
;    (prn :status status :error error :body body :resp resp)
    (if (or (< status 200) (> status 299))
      (if error
        (throw (Exception. error))
        (throw (Exception. body)))
      (let [session (json/read-str body :key-fn keyword)]
        (swap! browser-state assoc browser session)
        session))))

(defn get-session [browser]
  (let [session (get @browser-state browser)]
    (if session
      session
      (init-session browser {}))))

(defn GET [browser path]
  (let [session (get-session browser)
        session-id (:sessionId session)
        host (get browser :host "localhost")
        port (:port browser)
        opts {}
        url (str "http://" host ":" port "/session/" session-id "/" path)
        {:keys [status error body] :as resp} @(http/get url opts)]
;    (prn :get :status status :error error :body body :resp resp)
    (if (or (< status 200) (> status 299))
      (if error
        (throw (Exception. error))
        (throw (Exception. body)))
      (json/read-str body :key-fn keyword))))

(defn POST [browser path data]
  (let [session (get-session browser)
        session-id (:sessionId session)
        host (get browser :host "localhost")
        port (:port browser)
        body (json/write-str data)
        opts {:body body}
        url (str "http://" host ":" port "/session/" session-id "/" path)
        {:keys [status error body] :as resp} @(http/post url opts)]
;    (prn :post :status status :error error :body body :resp resp)
    (if (or (< status 200) (> status 299))
      (if error
        (throw (Exception. error))
        (throw (Exception. body)))
      (json/read-str body :key-fn keyword))))

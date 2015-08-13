;; copyright (c) 2014 Sean Corfield
;;
;; small demo to show Om / Sente playing together
;;
;; no claim is made of best practices - feedback welcome

(ns om-sente.server
  (:require [clojure.core.async :as async
             :refer [<! <!! chan go go-loop thread]]
            [clojure.core.cache :as cache]
            [compojure.core :refer [defroutes GET POST routes]]
            [compojure.handler :as h]
            [compojure.route :as r]
            [org.httpkit.server :as kit]
            [taoensso.sente :as s]))

;; create the Sente web socket connection stuff when we are loaded:

(let [{:keys [ch-recv send-fn ajax-post-fn
              ajax-get-or-ws-handshake-fn connected-uids] :as sente-info}
      (s/make-channel-socket! {})]
  (def ring-ajax-post   ajax-post-fn)
  (def ring-ajax-get-ws ajax-get-or-ws-handshake-fn)
  (def ch-chsk          ch-recv)
  (def chsk-send!       send-fn)
  (def connected-uids   connected-uids))

;; session cache to maintain authentication - so we can rely
;; entirely on socket communication instead of needing to login
;; to the application first: 5 minutes of inactive will log you out

(def session-map (atom (cache/ttl-cache-factory {} :ttl (* 5 60 1000))))

(def uids (atom #{}))

(defn keep-alive [uid]
  (println "keep-alive" uid (java.util.Date.))
  (when-let [token (get @session-map uid)]
    (swap! session-map assoc uid token)))

(defn add-token [uid token]
  (println "add-token" uid token (java.util.Date.))
  (swap! session-map assoc uid token))

(defn get-token [uid]
  (let [token (get @session-map uid)]
    (println "get-token" uid token (java.util.Date.))
    token))

(defn root [path]
  (str (System/getProperty "user.dir") path))

(defn unique-id [] (rand-int 10000))

(defn session-uid [req] (get-in req [:session :uid]))

(defn start-broadcaster! [msg]
  (doseq [uid (:any @connected-uids)]
    (chsk-send! uid [:test/reply msg])))

(defn index [req]
  {:status 200
   :session (if (session-uid req)
              (:session req)
              (assoc (:session req) :uid (unique-id)))
   :body (slurp "index.html")})

;; minimal set of routes to handle:
;; - home page request
;; - web socket GET/POST
;; - general files (mainly JS)
;; - 404

(defroutes server
  (-> (routes
       (GET  "/"   req (#'index req))
       (GET  "/ws" req (#'ring-ajax-get-ws req))
       (POST "/ws" req (#'ring-ajax-post   req))
       (r/files "/" {:root (root "")})
       (r/not-found "<p>Page not found. I has a sad!</p>"))
      h/site))

(defn session-status [req]
  (when-let [uid (session-uid req)]
    (chsk-send! uid [:session/state (if (get-token uid) :secure :open)])))

;; Reply with the session state - either open or secure.

(defmulti handle-event
  "Handle events based on the event ID."
  (fn [[ev-id ev-arg] ring-req] ev-id))

(defmethod handle-event :session/status
  [_ req]
  (session-status req))

;; Reply with authentication failure or success.
;; For a successful authentication, remember the login.

(defmethod handle-event :session/auth
  [[_ [username password]] req]
  (when-let [uid (session-uid req)]
    (let [valid (and (= "admin" username)
                     (= "secret" password))]
      (when valid
        (add-token uid (unique-id)))
      (chsk-send! uid [(if valid :auth/success :auth/fail)]))))

;; Reply with the same message, followed by the reverse of the message a few seconds later.
;; Also record activity to keep session alive.

(defmethod handle-event :test/echo
  [[_ msg] req]
  (when-let [uid (session-uid req)]
    (keep-alive uid)
    (println "~~~~~~~")
    (println @connected-uids)
    (println ";;;;;;;")
    ;(chsk-send! uid [:test/reply msg])
    ;(Thread/sleep 3000)
    (start-broadcaster! (clojure.string/reverse msg))))
    ;(chsk-send! uid [:test/reply (clojure.string/reverse msg)])))


;; When the client pings us, send back the session state:

(defmethod handle-event :chsk/ws-ping
  [_ req]
  (session-status req))

;; Handle unknown events.
;; Note: this includes the Sente implementation events like:
;; - :chsk/uidport-open
;; - :chsk/uidport-close

(defmethod handle-event :default
  [event req]
  nil)

(defn event-loop []
  (go (loop [{:keys [client-uuid ring-req event] :as data} (<! ch-chsk)]
        (println "-" event)
        (thread (handle-event event ring-req))
        (recur (<! ch-chsk)))))

(defn -main [& args]
  (event-loop)
  (let [port (or (System/getenv "PORT") 8444)]
    (println "Starting Sente server on port" port "...")
    (kit/run-server #'server {:port port})))

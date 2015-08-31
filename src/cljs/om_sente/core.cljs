;; copyright (c) 2014 Sean Corfield
;;
;; small demo to show Om / Sente playing together
;;
;; no claim is made of best practices - feedback welcome

(ns om-sente.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [taoensso.sente :as s]
            [cljs.core.async :as async :refer [<! >! chan put!]]))

(enable-console-print!)

(let [{:keys [chsk ch-recv send-fn state]}
      (s/make-channel-socket! "/ws" {:type :auto})]
  (def chsk       chsk)
  (def ch-chsk    ch-recv)
  (def chsk-send! send-fn)
  (def chsk-state chsk-state))


(defn field-change [e owner field]
  (let [value (.. e -target -value)]
    (om/set-state! owner field value)))

(defn send-text-on-enter [e owner state]
  (let [kc (.-keyCode e)
        w (.-which e)]
    (when (or (== kc 13) (== w 13))
      (chsk-send! [:test/echo (:text state)])
      (om/set-state! owner :text ""))))

(def text-length 32)

(defn text-sender [app owner]
  (reify
    om/IInitState
    (init-state [this]
      {:text ""})
    om/IRenderState
    (render-state [this state]
      (html [:input {:type "text" :value (:text state) :size text-length :max-length text-length
                     :on-change #(field-change % owner :text)
                     :on-key-press #(send-text-on-enter % owner state)}]))))

(defn make-color [v]
  (let [r v
        g (Math/round (- 150 (/ v 3)))
        b (Math/round (/ v 2))
        hex (fn [n] (.substring (str (when (< n 16) "0") (.toString n 16)) 0 2))]
    (str "#" (hex r) (hex g) (hex b))))

(defn make-target [s]
  (take text-length (concat (map #(.charCodeAt %) s) (repeat 0))))

(defmulti handle-event
  (fn [[ev-id ev-arg] app owner] ev-id))

(defmethod handle-event :test/reply
  [[_ msg] app owner]
  (println "receive test/reply")
  (om/update! app :tttt msg))

(defmethod handle-event :session/state
  [[_ state] app owner]
  (om/set-state! owner :session/state state))

(defmethod handle-event :auth/fail
  [_ app owner]
  (om/update! app [:notify/error] "Invalid credentials"))

(defmethod handle-event :auth/success
  [_ app owner]
  (om/set-state! owner :session/state :secure))

(defmethod handle-event :default
  [event app owner]
  #_(println "UNKNOWN EVENT" event))

(defn test-session [owner]
  (chsk-send! [:session/status]))

(defn event-loop [app owner]
  (go (loop [[op arg] (:event (<! ch-chsk))]
        #_(println "-" op)
        (case op
          :chsk/recv (handle-event arg app owner)
          ;; we ignore other Sente events
          (test-session owner))
        (recur (:event (<! ch-chsk))))))

(defn attempt-login [e app owner]
  (let [username (-> (om/get-node owner "username") .-value)
        password (-> (om/get-node owner "password") .-value)]
    (om/update! app [:notify/error] nil)
    (chsk-send! [:session/auth [username password]]))
  false)

(defn login-form [app owner]
  (reify
    om/IInitState
    (init-state [this]
      {:username "" :password ""})
    om/IRenderState
    (render-state [this state]
      (html [:div {:style {:margin "auto" :width "175"
                           :border "solid blue 1px" :padding 20}}
             (when-let [error (:notify/error app)]
               [:div {:style #js {:color "red"}} error])
             [:h1 "Login"]
             [:form {:on-submit #(attempt-login % app owner)}
              [:div
               [:p "Username"]
               [:input {:ref "username" :type "text" :value (:username state)
                        :on-change #(field-change % owner :username)}]]
              [:div
               [:p "Password"]
               [:input {:ref "password" :type "password" :value (:password state)
                        :on-change #(field-change % owner :password)}]]
              [:div
               [:input {:type "submit" :value "Login"}]]]]))))

(defn graph-data-changing [old new]
  (not= old new))

(defn d3-test [app owner]
  (reify
    om/IDidMount
    (did-mount [this]
      (attendance-graph (:tttt app)))
    om/IDidUpdate
    (did-update [this prev-props prev-state]
     ;   (.remove (.-firstChild (om/get-node owner "d3-node")))
        (attendance-graph (:tttt app)))
    om/IRender
    (render [this]
      (dom/div #js {:style #js {:height 400 :float "right" :width "100%"}
                    :react-key "d3-node" ;; ensure React knows this is non-reusable
                    :ref "d3-node"       ;; label it so we can retrieve it via get-node
                    :id "d3-node"}
               (dom/svg #js {:class "svg" :id "svg-node"})
               ))))   ;; set id so D3 can find it!

(defn attendance-graph [raw-data]
  (let [h 380 w 480 m 30
        _data (clj->js raw-data)
        y-scale (.. js/d3 -scale linear
                    (domain #js [0 (apply max raw-data)])
                    (range #js [(- h m) (+ 0 m)]))
        height-scale (.. js/d3 -scale linear
                    (domain #js [0 (apply max raw-data)])
                    (range #js [(+ 0 m) (- h m)]))
        x-scale (.. js/d3 -scale linear
                    (domain #js [0 (count raw-data)])
                    (range #js [(+ 0 m) (- w m)]))
        svg (.. js/d3 (select "#svg-node")
                (attr #js {:width w :height h}))
        drag (-> js/d3 .-behavior .drag
                 (.on "drag" (fn [d]
                               (this-as this
                               (println d)

                               (.. js/d3 (select this)
                                   (attr "fill" "orange"))
                             ;  (.. js/d3 (select this)
                             ;      (attr "y" (.. js/d3 -event -y)))
                                        (swap! app-state assoc :tttt (assoc (:tttt @app-state) d (/ (.. js/d3 -event -y) 50)))
                                        (println app-state)
                                        (chsk-send! [:test/change-data (:tttt @app-state)])
                                        (println (.. js/d3 -event -y))))))]
    (.. svg (selectAll "rect")
        (data _data)
        (enter)
        (append "rect"))

    (.. svg (selectAll "rect")
        (data _data)
        (attr "x" (fn [d i] (+ (x-scale i) 25)))
        (attr "y" (y-scale d))
        (attr "width" (fn [d i] (/ w (count raw-data))))
        (attr "height" (fn [d i] (height-scale d)))
        (attr "fill" "blue")
        (call drag))))

(defn attempt-attend [e app owner]
  (println e))

(defn attend-form [app owner]
  (reify
    om/IInitState
    (init-state [this]
      {:start_at "" :end_at ""})
    om/IRenderState
    (render-state [this state]
      (html [:div {:style {:margin "auto"
                           :border "solid blue 1px" :padding 20}}
             (when-let [error (:notify/error app)]
               [:div {:style #js {:color "red"}} error])
             [:form {:on-submit #(attempt-attend % app owner)}
              [:div
               [:p "出勤"]
               [:input {:ref "start_at" :type "date" :value (:username state)
                        :on-change #(field-change % owner :username)}]]
              [:div
               [:p "退勤"]
               [:input {:ref "end_at" :type "date" :value (:password state)
                        :on-change #(field-change % owner :password)}]]
              [:div
               [:input {:type "submit" :value "打刻"}]]]]))))

(defn day-row [app owner]
  (reify
    om/IRender
    (render [this]
      (html
        [:tr
         [:td (:day app)]
         [:td (:start_at app)]
         [:td (:end_at app)]]))))

(defn table [app owner]
  (reify
    om/IRender
    (render [this]
      (html
        [:table
         [:tr
          [:th "出勤日"]
          [:th "出勤"]
          [:th "退勤"]]
         (om/build-all day-row (:attendance app))]))))

(defn secured-application [app owner]
  (reify
    om/IRender
    (render [this]
      (html [:div {:style {:margin "auto" :width "1000"
                           :border "solid blue 1px" :padding 20}}
             [:h1 "てすと"]
            ; (om/build attend-form app {})
            ; (om/build table app {})
             (om/build d3-test app {})
             [:div {:style {:clear "both"}}]]))))

(defn application [app owner]
  (reify
    om/IInitState
    (init-state [this]
      {:session/state :unknown})
    om/IWillMount
    (will-mount [this]
      (event-loop app owner))
    om/IRenderState
    (render-state [this state]
      (dom/div #js {:style #js {:width "100%"}}
                 (om/build secured-application app {})))))

;(def app-state (atom {:data/text "Enter a string and press RETURN!"}))
(def app-state
  (atom
    {:tttt [0 1 2 3 4 5 6 7 8 9 10]
     :attendance [
                  {:day "2015-08-01" :start_at "10:55" :end_at "21:15"}
                  {:day "2015-08-02" :start_at "09:45" :end_at "19:15"}
                  {:day "2015-08-03" :start_at "09:55" :end_at "20:40"}]}))

(om/root application
         app-state
         {:target (. js/document (getElementById "app"))})

(def input-chan
  (let [c (chan)]
    (.addEventListener js/document "keydown" #(put! c (.-keyCode %)))
    c))

(defn main []
  (go
    (while true
      (let [turn (<! input-chan)]
        (println "event")
        (om/update! app-state conj :tttt (last (:tttt app-state)))))))

(main)

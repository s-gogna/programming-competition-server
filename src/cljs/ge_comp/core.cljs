(ns ge-comp.core
  (:require-macros
   [cljs.core.async.macros :refer [go]])
  (:require
   [clojure.string :as string]
   [cljs.reader :refer [read-string]]
   [cljs-http.client :as http]
   [cljs.core.async :refer [<!]]
   [reagent.core :as r :refer [atom]]))

(def scoreboard-refresh-time 10000)

(def repository-url "https://github.com/wmedrano/programming-competition-server")

(def password-len 3)

(enable-console-print!)

(defn read-file
  [file callback]
  {:pre [(fn? callback)]}
  (let [reader (js/FileReader.)
        wrapped #(callback (-> %1 .-target .-result))]
    (aset reader "onload" wrapped)
    (.readAsText reader file)))

(defonce state (atom {:competition-name ""
                      :registration? false
                      :scoreboard {}
                      :problems '()
                      :username nil}))

(defn fetch-competition-name!
  []
  (go (let [res (<! (http/get "/competition.edn"
                              {:query-params {}}))
            body (:body res)
            name (:competition-name body)]
        (swap! state assoc :competition-name name))))
(fetch-competition-name!)

(defn fetch-scoreboard!
  []
  (go (let [res (<! (http/get "/scoreboard.edn"
                              {:query-params {}}))
            body (:body res)]
        (swap! state assoc :scoreboard body))))
(fetch-scoreboard!)

(defn fetch-problems!
  []
  (go (let [res (<! (http/get "/problems.edn"
                              {:query-params {}}))
            body (:body res)]
        (swap! state assoc :problems body))))
(fetch-problems!)

(defn login!
  [username password]
  {:pre [(string? username) (string? password)]}
  (go (let [res (<! (http/get "/login"
                              {:query-params {:username username
                                              :password password}}))
            {:keys [authorize-error admin]} (:body res)]
        (if authorize-error
          (js/alert authorize-error )
          (if admin
            (swap! state assoc :registration? true)
            (swap! state assoc :username username))))))

(defn register!
  [email username password]
  {:pre [(string? email) (string? username) (string? password)]}
  (go (let [res (<! (http/post "/register"
                               {:edn-params {:email email
                                             :username username
                                             :password password}}))
            {:keys [username password error]} (:body res)]
        (if (some? error)
          (js/alert (str "Registration Failed" \newline error))
          (js/alert (str "Registration Succeeded" \newline
                         "username: " username \newline
                         "password: " password))))))

(defn submit-problem!
  [username fname code]
  {:pre [(string? username) (string? fname) (string? code)]}
  (go (let [[problem code-type] (string/split fname #"\.")
            res (<! (http/post "/submit"
                               {:edn-params {:username username
                                             :problem problem
                                             :code code
                                             :code-type code-type}}))
            msg (str "Problem: " problem \newline
                     (-> res :body :message))]
        (js/alert msg)
        (fetch-scoreboard!))))

(defonce update-score-interval (atom nil))
(if (some? @update-score-interval) (js/clearInterval @update-score-interval))
(reset! update-score-interval
        (js/setInterval fetch-scoreboard!
                        scoreboard-refresh-time))

(defn calc-score
  "Returns a tuple that represents a persons score. Lower is better."
  [user-record]
  (let [solved (filter (fn [v] (-> v :accepted some?))
                       (vals user-record))
        problem-score (- (count solved))
        latest-time (apply max (map :accepted solved))
        penalty (apply + (map :penalty solved))
        time-score (+ latest-time penalty)]
    [problem-score time-score]))

(defn timestamp-to-time
  [timestamp]
  (if (nil? timestamp) "--:--:--"
      (-> timestamp
          js/Date.
          (string/split #" ")
          (get 4))))

(defn penalty-to-time
  [penalty]
  (-> penalty
      (/ 1000) ;; to seconds
      (/ 60))) ;; to minutes

(defn r-scoreboard []
  (let [me @(r/cursor state [:username])
        problems @(r/cursor state [:problems])
        scoreboard @(r/cursor state [:scoreboard])
        contestants (keys scoreboard)
        ordered (sort-by #(calc-score (get scoreboard %1))
                         contestants)]
    [:div.r-scoreboard.well.well-sm
     [:h3 "Leaderboard"]
     [:table.table.table-striped.table-hover
      [:thead
       [:tr
        [:th "#"]
        [:th "User"]
        [:th "Solved"]
        [:th "Penalty"]
        (map (fn [p] [:th {:key p} p])
             problems)]]
      [:tbody
       (map-indexed (fn [idx user]
                      (let [record (get scoreboard user)
                            score (calc-score record)
                            solved (filter (fn [v] (-> v :accepted some?))
                                           (vals record))]
                        [:tr {:key user
                              :class (if (= user me) "success")}
                         [:th (str (inc idx))]
                         [:th user]
                         [:th (- (first score))]
                         [:th (penalty-to-time (reduce + (map #(get %1 :penalty 0) solved)))]
                         (map (fn [p] [:th {:key p}
                                       (str (timestamp-to-time (get-in record
                                                                       [p :accepted]))
                                            \/
                                            (penalty-to-time (get-in record
                                                                     [p :penalty])))])
                              problems)]))
                    ordered)]]
     [:p
      "Score is primarily determined by how many problems are solved. "
      "In case of a tie, the person with the latest submission loses. "
      "Additionally, submitting a faulty solution incurs a penalty of 10 minutes. "]]))

(defn r-submit []
  (let [username @(r/cursor state [:username])]
    [:div.r-submit
     [:form.well.well-sm
      [:label {:for "problem-submit"}
       [:h3 "Submit Solution"]]
      [:input {:type "file"
               :id "problem-submit"
               :value nil
               :on-change (fn [e] (let [f (-> e .-target .-files (.item 0))
                                        fname (.-name f)
                                        cback #(submit-problem! username
                                                                fname
                                                                %1)]
                                    (read-file f cback)))}]]]))

(defn r-logged-in []
  (let [username @(r/cursor state [:username])]
    [:div.r-logged-in
     [:h2 "Welcome " username]
     [r-scoreboard]
     [r-submit]
     [:div {:style {:margin-top "1rem"}}
      [:button.btn.btn-danger
       {:on-click #(swap! state assoc :username nil)}
       "Log Out"]]]))

(defn r-login []
  (let []
    [:div.r-login
     [:form.form-horizontal
      {:on-submit #(do (login! (-> "username-input"
                                   js/document.getElementById
                                   .-value)
                               (-> "password-input"
                                   js/document.getElementById
                                   .-value))
                       (.preventDefault %1))}
      [:fieldset
       [:legend "Log In"]
       [:div.form-group
        [:label.col-lg-2 {:for "username-input"} "username"]
        [:div.col-lg-10
         [:input {:type "text" :id "username-input"}]]]
       [:div.form-group
        [:label.col-lg-2 {:for "password-input"} "password"]
        [:div.col-lg-10
         [:input {:type "password" :id "password-input"}]]]
       [:div.form-group
        [:div.col-lg-10.col-lg-offset-2
         [:button.btn.btn-primary {:type "submit"}
          "Log In"]]]]]
     [r-scoreboard]]))

(def valid-chars (mapv char (range 97 123)))
(defn gen-password [password-len]
  (string/join (map #(rand-nth valid-chars)
                    (range password-len))))

(defn r-registration []
  (let []
    [:div.r-registration
     [:form.form-horizontal
      {:on-submit #(let [password (gen-password password-len)]
                     (register! (-> "email-register"
                                    js/document.getElementById
                                    .-value)
                                (-> "username-register"
                                    js/document.getElementById
                                    .-value)
                                password)
                     (aset (.getElementById js/document "username-register")
                           "value"
                           "")
                     (aset (.getElementById js/document "email-register")
                           "value"
                           "")
                     (.preventDefault %1))}
      [:fieldset
       [:legend "Registration"]
       [:div.form-group
        [:label.col-lg-2 {:for "email-register"} "email"]
        [:div.col-lg-10
         [:input {:type "text" :id "email-register"}]]]
       [:div.form-group
        [:label.col-lg-2 {:for "username-register"} "username"]
        [:div.col-lg-10
         [:input {:type "text" :id "username-register"}]]]
       [:div.form-group
        [:div.col-lg-10.col-lg-offset-2
         [:button.btn.btn-primary {:type "submit"} "Register"]
         [:button.btn.btn-default {:on-click #(do (swap! state assoc :registration? false)
                                                  (.preventDefault %1))}
          "Exit"]]]]]]))

(defn r-footer []
  (let []
    [:div.r-footer.well.well-sm
     [:h4 "Issues or Suggestions?"]
     [:p
      "Report them on " [:a {:href repository-url
                                      :target "_blank"} "github"] "."]]))

(defn app []
  (let [registration? @(r/cursor state [:registration?])
        logged-in? (some? @(r/cursor state [:username]))
        competition-name @(r/cursor state [:competition-name])]
    [:div.app {:style {:margin "2rem"}}
     [:h1 competition-name]
     (cond
       registration? [r-registration]
       logged-in? [r-logged-in]
       :else [r-login])
     [r-footer]]))

(r/render [app] (js/document.getElementById "app"))

(ns ge-comp.server
  (:require
   [ge-comp.judge :as judge]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [clojure.java.io :as io]
   [compojure.core :refer [ANY GET PUT POST DELETE defroutes]]
   [compojure.route :refer [resources]]
   [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
   [ring.middleware.gzip :refer [wrap-gzip]]
   [ring.middleware.logger :refer [wrap-with-logger]]
   [environ.core :refer [env]]
   [ring.adapter.jetty :refer [run-jetty]])
  (:gen-class))

;; username -> {:password string}
(defonce users (atom {}))
(def admin-username "admin")
(def admin-password "clojurebot")
(def competition-info {:competition-name "Advanced"})
(def scoreboard judge/scoreboard)

(defn backup-str [fname s]
  "Creates a backup file in the backup directory."
  (let [fname (str "backup/" (.getTime (java.util.Date.)) fname)]
    (spit fname s)
    fname))

(defroutes routes
  (GET "/" _
       {:status 200
        :headers {"Content-Type" "text/html; charset=utf-8"}
        :body (io/input-stream (io/resource "public/index.html"))})
  (GET "/login" req
       (let [query (:query-params req)
             username (get query "username")
             password (get query "password")
             exists? (contains? @users username)
             authorized? (and exists? (= (get-in @users [username :password])
                                         password))
             error (cond (not exists?) "username not found."
                         (not authorized?) "invalid password"
                         :else nil)]
         {:status 200
          :headers {"Content-Type" "application/edn"}
          :body (pr-str {:authorized authorized? :error error})}))
  (GET "/admin" req
       (let [query (:query-params req)
             username (get query "username")
             password (get query "password")
             authorized? (and (= username admin-username)
                              (= password admin-password))
             error (if-not authorized? "Unauthorized")]
         {:status 200
          :headers {"Content-Type" "application/edn"}
          :body (pr-str {:authorized authorized?
                         :error error})}))
  (POST "/register" req
        (let [body (-> req :body slurp read-string)
              {:keys [email username password]} body
              username-available? (not (contains? @users username))
              error (if-not username-available? "Username is not available")]
          (if username-available?
            (swap! users assoc username {:password password :email email}))
          {:status 200
           :headers {"Content-Type" "application/edn"}
           :body (pr-str {:username username
                          :password password
                          :error error})}))
  (POST "/submit" req
        (let [body (-> req :body slurp read-string)
              {:keys [username problem code code-type]} body
              timestamp (.getTime (java.util.Date.))
              res (judge/score! username timestamp problem code code-type)]
          (log/info "code submission: "
                    {:username username
                     :timestamp timestamp
                     :problem problem
                     :code-file (backup-str (str problem \. code-type) code)
                     :res res})
          {:status 200
           :headers {"Content-Type" "application/edn"}
           :body (pr-str {:message res})}))
  (GET "/competition.edn" _
       {:status 200
        :headers {"Content-Type" "application/edn"}
        :body (pr-str competition-info)})
  (GET "/users.edn" _
       {:status 200
        :headers {"Content-Type" "application/edn"}
        :body (pr-str (keys @users))})
  (GET "/problems.edn" _
       {:status 200
        :headers {"Content-Type" "application/edn"}
        :body (pr-str (sort judge/problem-set))})
  (GET "/scoreboard.edn" _
       {:status 200
        :headers {"Content-Type" "application/edn"}
        :body (pr-str @judge/scoreboard)})
  (resources "/"))

(def http-handler
  (-> routes
      (wrap-defaults api-defaults)
      wrap-with-logger
      wrap-gzip))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 10555))]
    (run-jetty http-handler {:port port :join? false})))

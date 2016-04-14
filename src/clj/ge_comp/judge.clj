(ns ge-comp.judge
  (:require
   [ge-comp.runners :as runners]
   [clojure.string :as string])
  (:gen-class))

(defonce scoreboard (atom {}))

(def problem-set #{"a" "b" "c" "d" "e" "f" "g" "h"})

(def time-limit 2) ;; seconds

(def penalty (* 1000 60 10)) ;; msecs

(defn- format-whitespace
  "Trims lines, and removes empty lines."
  [text]
  (->> text
       string/split-lines
       (map string/trim)
       (remove string/blank?)
       (string/join \newline)))

(def code-types #{"cpp" "py"})
(defn- judge-helper
  "Returns true if the code produces the same output as out-file when run with
  in-file."
  [code code-type in-file out-file]
  {:pre [(string? code) (string? code-type) (string? in-file) (string? out-file)]}
  (let [run-fn (case code-type
                 "cpp" runners/run-cpp
                 "py" runners/run-py)
        out-str (run-fn code (slurp in-file) time-limit)]
    (= (format-whitespace out-str)
       (format-whitespace (slurp out-file)))))

;; only one file should be judged at a time
(defonce judge-lock (Object.))

(defn- judge-problem
  "Returns the string accepted if the problem is good. If not, then a string
  explaining the failure is returned."
  [problem code code-type]
  {:pre [(string? problem) (string? code)]}
  (locking judge-lock
    (try
      (if (judge-helper code code-type
                        (str "problems/" problem "/in")
                        (str "problems/" problem "/out"))
        "accepted"
        "wrong answer")
      (catch Exception e (.getMessage e)))))


(defn score!
  "Judges the problem and updates the scoreboard if necessary. The string
  `\"accepted\"` is returned on success, while other strings indicate the reason
  for failure."
  [username timestamp problem code code-type]
  {:pre [(string? username) (number? timestamp)
         (string? problem) (string? code) (string? code-type)]}
  (cond
    (-> @scoreboard
        (get username)
        (get problem)
        :accepted
        some?)
    "You have already solved this problem."

    (not (contains? code-types code-type))
    (str code-type " is not an accepted code type.")

    (not (contains? problem-set
                    problem))
    (str problem " is not a valid problem.")

    :else
    (let [res (judge-problem problem code code-type)
          successful? (= res "accepted")]
      (if successful?
        (swap! scoreboard assoc-in
               [username problem :accepted] timestamp)
        (swap! scoreboard update-in
               [username problem :penalty]
               #(+ (or %1 0) %2) penalty))
      res)))

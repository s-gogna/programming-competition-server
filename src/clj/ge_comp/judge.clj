(ns ge-comp.judge
  (:require
   [clojure.java.shell :refer [sh]]
   [clojure.string :as string]))

(defonce scoreboard (atom {}))

(def insecure #{"fstream"
                "popen"
                "system"
                "fopen"})

(def problem-set #{"a" "b" "c" "d" "e" "f" "g" "h"})

(def time-limit 2) ;; seconds

(def penalty (* 1000 60 10)) ;; msecs

(defn- ensure-secure
  "Throws an exception if the code is not secure. Returns true otherwise"
  [code]
  {:pre [(string? code)]}
  (let [bad-phrase (some #(string/includes? code %1) insecure)]
    (if (some? bad-phrase)
      (throw (Exception. (str bad-phrase " not allowed!")))
      true)))

(defn- g++-compile
  "Compiles code to the provided binary. An exception is thrown if compilation
  fails."
  [code-file binary]
  {:pre [(string? code-file) (string? binary)]}
  (let [{:keys [exit out err]} (sh "g++" "-O2" code-file (str "-o" binary))]
    (if-not (zero? exit)
      (throw (Exception. err)))))

(defn- format-whitespace
  "Trims lines, and removes empty lines."
  [text]
  (->> text
       string/split-lines
       (map string/trim)
       (remove string/blank?)
       (string/join \newline)))

(defn- timed-sh
  "The `command` has `time` seconds to complete. If execution is terminated
  because of timeout, then the exit code will not be 0."
  [time & command]
  (apply sh (concat ["timeout" (str time "s")] command)))

(defn- judge-helper
  "Returns true if the code produces the same output as out-file when run with
  in-file."
  [code in-file out-file]
  {:pre [(string? code) (string? in-file) (string? out-file)]}
  (let [code-fname "problems/code.cpp"
        bin "problems/solver"
        in-fname in-file
        in-str (slurp in-file)
        out-fname out-file]
    (ensure-secure code)
    (spit code-fname code)
    (g++-compile code-fname bin)
    (let [{:keys [exit out err]} (timed-sh time-limit bin :in in-str)]
      (cond
        (= exit 124) (throw (Exception. "Time exceeded"))
        (not (zero? exit)) (throw (Exception. err)))
      (= (format-whitespace out)
         (format-whitespace (slurp out-fname))))))

;; only one file should be judged at a time
(defonce judge-lock (Object.))

(defn- judge-problem
  "Returns the string accepted if the problem is good. If not, then a string
  explaining the failure is returned."
  [problem code]
  {:pre [(string? problem) (string? code)]}
  (locking judge-lock
    (try
      (if (judge-helper code
                        (str "problems/" problem "/in")
                        (str "problems/" problem "/out"))
        "accepted"
        "wrong answer")
      (catch Exception e (.getMessage e)))))

(defn score!
  "Judges the problem and updates the scoreboard if necessary. The string
  `\"accepted\"` is returned on success, while other strings indicate the reason
  for failure."
  [username timestamp problem code]
  {:pre [(string? username) (number? timestamp)
         (string? problem) (string? code)]}
  (cond
    (-> @scoreboard
        (get username)
        (get problem)
        :accepted
        some?)
    "You have already solved this problem."

    (not (contains? problem-set
                    problem))
    "Not a valid problem"

    :else
    (let [res (judge-problem problem code)
          successful? (= res "accepted")]
      (if successful?
        (swap! scoreboard assoc-in
               [username problem :accepted] timestamp)
        (swap! scoreboard update-in
               [username problem :penalty]
               #(+ (or %1 0) %2) penalty))
      res)))

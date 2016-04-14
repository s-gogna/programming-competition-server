(ns ge-comp.runners
  (:require
   [clojure.java.shell :refer [sh]]
   [clojure.string :as string]))

(defn- timed-sh
  "The `command` has `time` seconds to complete. If execution is terminated
  because of timeout, then the exit code will not be 0."
  [time & command]
  (apply sh (concat ["timeout" (str time "s")] command)))

(defn- ensure-secure-cpp
  "Throws an exception if the code is not secure. Returns true otherwise"
  [code]
  {:pre [(string? code)]}
  (let [insecure #{"fstream"
                   "popen"
                   "system"
                   "fopen"}
        bad-phrase (some #(string/includes? code %1) insecure)]
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

(defn run-cpp
  "Runs the cpp program `code` with `input` through standard in. Throws an
  exception if execution does not terminate as usual. The output from standard
  out is returned if successful."
  [code input time-limit]
  (ensure-secure-cpp code)
  (let [code-fname "problems/code.cpp"
        bin "problems/solver"]
    (spit code-fname code)
    (g++-compile code-fname bin)
    (let [{:keys [exit out err]} (timed-sh time-limit bin :in input)]
      (cond
        (= exit 124) (throw (Exception. "Time exceeded"))
        (not (zero? exit)) (throw (Exception. err))
        :else out))))

(defn- ensure-secure-py
  "Throws an exception if the code is not secure. Returns true otherwise"
  [code]
  {:pre [(string? code)]}
  (let [insecure #{"open" "import"}
        bad-phrase (some #(string/includes? code %1) insecure)]
    (if (some? bad-phrase)
      (throw (Exception. (str bad-phrase " not allowed!")))
      true)))

(defn run-py
  "Same as cpp-run, but with python code."
  [code input time-limit]
  (ensure-secure-py code)
  (let [code-fname "problems/code.py"]
    (spit code-fname code)
    (let [{:keys [exit out err]} (timed-sh time-limit "python" code-fname :in input)]
      (cond
        (= exit 124) (throw (Exception. "Time exceeded"))
        (not (zero? exit)) (throw (Exception. err))
        :else out))))

(ns leiningen.eris
  (:require [clojure.java.io :as io]
            [clojure.test :as t])
  (:import [java.io File PushbackReader]))

(defn get-ns
  [rdr]
  (try
    (loop [form nil]
      (if (= 'ns (first form))
        (second form)
        (recur (read rdr))))
    (catch RuntimeException _)))

(defn chaos
  [f]
  (with-open [in (PushbackReader. (io/reader f))]
    (when-let [ns-sym (get-ns in)]
      (let [prefix-sym (fn [sym]
                         (->> sym
                              (str ns-sym "/")
                              (symbol)))]
        (try
          (loop [distorted (atom []) next-form (read in)]
            ;; :-)
            (recur (read in)))
          (catch RuntimeException))))))

(defn ls
  [^File dir]
  (.listFiles dir))

(defn source-files
  [dir]
  (loop [acc [] [nxt & rst] (ls (io/file dir))]
    (if nxt
      (if (.isDirectory nxt)
        (recur acc (concat rst (ls nxt)))
        (if (re-find #"\.clj$" (.getPath nxt))
          (recur (conj acc nxt) rst)
          (recur acc rst)))
      acc)))


(defn eris
  [project & args]
  )

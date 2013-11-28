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

(defn sexpr-type
  [sexpr]
  (let [sexpr (macroexpand sexpr)]
    ;; Only when things are interesting
    (when (>= (count sexpr) 3)
      (let [third (fn [x] (first (rest (rest x))))
            fst (first sexpr)
            sec (second sexpr)
            thd (third sexpr)]
        (cond
         ;; We found a def or defn
         (= 'def fst)
         [:def sexpr]
         ;; We found a defmacro
         (and (= 'do fst) (= 'clojure.core/defn (first sec))
              (= 'setMacro (first (third thd))))
         [:defmacro sexpr]
         ;; We found a defmethod
         (and (= '. fst) (= 'clojure.core/addMethod thd))
         [:defmethod sexpr])))))

(declare distort)
(defn chaos
  [f]
  (with-open [in (PushbackReader. (io/reader f))]
    (when-let [ns-sym (get-ns in)]
      (let [distorted (atom '(do))
            ;; Load namespace first
            _ (swap! distorted conj `(require '~ns-sym))
            ;; Then switch to it
            _ (swap! distorted conj `(in-ns '~ns-sym))]
        (try
          (loop [next-form (read in)]
            (swap! distorted
                   (fn [form]
                     (if-let [d (distort next-form)]
                       (conj form d)
                       form)))
            (recur (read in)))
          (catch RuntimeException _
            (reverse @distorted)))))))

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

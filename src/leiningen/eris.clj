(ns leiningen.eris
  (:require [clojure.java.io :as io]
            [clojure.test :as t])
  (:import [java.io File PushbackReader]))

(defn third
  [sq]
  (-> sq rest rest first))

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
      (let [fst (first sexpr)
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

(defn rand-str
  [])

(defmulti mangle
  "Change a 'primitive'"
  (fn [x]
    (cond
     ;; Probably a function
     (seq? x)
     :seq
     (map? x)
     :map
     :else (type x))))

(defmethod mangle
  :seq
  [s]
  (if-not (#{'clojure.core/fn 'fn 'clojure.core/defn 'defn}
           (first s))
    (map mangle s)
    (let [[fn-head fn-tail] (if (symbol? (second s)) ; has a name
                              (if (vector? (third s))
                                ;; Make the tail consistent with multiple arity
                                [(take 2 s) (list (drop 2 s))]
                                [(take 2 s) (drop 2 s)])
                              (if (vector? (second s))
                                [(take 1 s) (list (drop 1 s))]
                                [(take 1 s) (drop 1 s)]))]
      (concat fn-head
              (map (fn [[arglist & body]]
                     (concat (list arglist)
                             (map mangle body)))
                   fn-tail)))))

(defmethod mangle
  String
  [_]
  (rand-str))

(defmethod mangle
  Long
  [n]
  (- (rand-int 2001)
     1001))

(defmethod mangle
  Double
  [n]
  (double (- (rand-int 2001)
             1001)))

(defmethod mangle
  clojure.lang.Keyword
  [k]
  (if (#{:keys :strs :as :or :else :default} k)
    k
    (keyword (rand-str))))

(defmethod mangle
  clojure.lang.Symbol
  [x]
  x)

(defmethod mangle
  clojure.lang.PersistentVector
  [v]
  (mapv mangle v))

(defmethod mangle
  clojure.lang.PersistentHashSet
  [s]
  (set (map mangle s)))

(defmethod mangle
  :map
  [m]
  (into {} (map (fn [k v] [(mangle k) (mangle v)])
                m)))

(defn distort
  [[type sexpr]]
  (case type
   :def
   :defmacro
   :defmethod))

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

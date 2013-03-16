(ns chlorine.util
  (:use [pathetic.core :only [normalize url-normalize]])
  (:require [clojure.pprint])
  (:import [java.util Calendar]
           [java.text SimpleDateFormat]))

(def ^:dynamic *cwd*)
(def ^:dynamic *cpd* (str (.getCanonicalFile (clojure.java.io/file ".")) "/"))

(defn unzip
  "Reverse of 'clojure.core/zipmap"
  [s]
  (let [parts (partition 2 s)]
    [(into (empty s) (map first parts))
     (into (empty s) (map second parts))]))

(defn format-code
  "Print Clojure's forms in pretty format"
  [form]
  (clojure.pprint/write form :pretty true :stream nil))

; copied from clojure.core because it's private there
(defmacro assert-args [fnname & pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  ~(str fnname " requires " (second pairs)))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args fnname more)))))

(defn re-compare
  "Compare regular expressions"
  ([x] (= (type x) java.util.regex.Pattern))
  ([x y]
     (and (= (type x) (type y) java.util.regex.Pattern)
          (= (str x) (str y))))
  ([x y & more]
     (if (re-compare x y)
       (if (next more)
         (recur y (first more) (next more))
         (re-compare y (first more)))
       false)))

(defn path-type
  "Detects type of a path."
  [path]
  (cond
   (or (.startsWith path "./")
       (.startsWith path "../"))
   :file-relative

   (.startsWith path "~/")
   :home-relative
   (or (.startsWith path "/")
       (.startsWith path "r://")
       (.startsWith path "http://")
       (.startsWith path "https://")
       (.startsWith path "file://"))
   :absolute

   :else
   :dir-relative))

(defn file-and-dir
  "Detects file and directory name from paths."
  [path]
  (let [bare-file (if (vector? path)
                    (second path)
                    path)
        normalized-bare-file
        (case (path-type bare-file)
          :file-relative
          (let [cwd (if (vector? *cwd*)
                      (second *cwd*)
                      *cwd*)]
            (if (or (.startsWith cwd "http://")
                    (.startsWith cwd "https://"))
              (url-normalize (str cwd bare-file))
              (normalize (str cwd bare-file))))

          :absolute
          (if (or (.startsWith bare-file "http://")
                  (.startsWith bare-file "https://"))
            (url-normalize bare-file)
            bare-file)

          :home-relative
          (clojure.string/replace bare-file
                                  #"^~" (System/getProperty "user.home"))

          :dir-relative
          (str *cpd* bare-file))

        bare-dir
        (if (or (.startsWith normalized-bare-file "http://")
                (.startsWith normalized-bare-file "https://"))
          (url-normalize (str normalized-bare-file "/../"))

          (normalize (str normalized-bare-file "/../")))
        bare-dir
        (if (.endsWith bare-dir "/")
          bare-dir
          (str bare-dir "/"))

        output->resource?
        (or (vector? path)
            (and (= :file-relative (path-type bare-file))
                 (vector? *cwd*)))]
    (if output->resource?
      [[:resource normalized-bare-file] [:resource bare-dir]]
      [normalized-bare-file bare-dir])))

(defn replace-map
  "Replaces match sub-strings with replacements found in a map.
Use array-map to reserve key orders."
  [s m]
  (loop [s s m m]
    (if-let [[k v] (first m)]
      (recur (clojure.string/replace s k v)
             (rest m))
      s)))

(defn re?
  "Checks if an object is a regular expression."
  [expr] (= (class expr) java.util.regex.Pattern))

(defmacro with-timeout
  "Executes a form within a time limit in miliseconds"
  [ms & body]
  `(let [f# (future ~@body)]
     (.get f# ~ms java.util.concurrent.TimeUnit/MILLISECONDS)))

(defn timestamp
  "Generates timestamp string in HH:mm:ss format."
  []
  (let [c (Calendar/getInstance)
        f (SimpleDateFormat. "HH:mm:ss")]
    (.format f (.getTime c))))

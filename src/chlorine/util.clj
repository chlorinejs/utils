(ns chlorine.util
  (:use [pathetic.core :only [normalize url-normalize]])
  (:require [clojure.pprint])
  (:import [java.util Calendar]
           [java.text SimpleDateFormat]))

(def ^:dynamic *cwd*)

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

(defn resource-path?
  "Check if a string is a resource url (starting with r:/)."
  [s]
  (.startsWith s "r:/"))

(defn to-resource
  "Makes resource object from r:/ path."
  [s]
  (clojure.java.io/resource
   (clojure.string/replace s #"^r:/" "")))

(defn url?
  "Check if a string is an url."
  [s]
  (or (.startsWith s "http://")
      (.startsWith s "https://")
      (.startsWith s "file://")))

(defn get-dir
  "Get directory path from absolute file path."
  [abs-file]
  (let [normalizer (if (url? abs-file)
                     url-normalize
                     normalize)]
    (normalizer (str abs-file "/../"))))

(defn to-abs-path
  "Converts a path to an absolute one.
  - If given path is home-relative, expands it
  - If given path is already absolute path (files, urls or resources)
  returns it.
  - Else (given path is relative), returns nil."
  [path]
  (cond
   (or (.startsWith path "./")
       (.startsWith path "../"))
   nil

   (.startsWith path "~/")
   (clojure.string/replace
    path
    #"^~" (System/getProperty "user.home"))

   (or (resource-path? path)
       (url? path)
       (.startsWith path "/"))
   path
   ;; else?
   ;; "something/some-path.cl2", "some-file.cl2"
   ;; -> relative -> nil
   ))

(defn to-full-path
  "Makes a full path from a directory path and a relative file name."
  [path-dir fname]
  (let [normalizer (if (url? path-dir)
                     url-normalize
                     normalize)]
    (normalizer (str (clojure.string/replace path-dir #"/$" "")
                     "/"
                     fname))))

(defn file-exists?
  "If file exists, returns file. Else returns nil.
  Please note URLs are not checked and there should be a
  exception handler in case URL doesn't exist."
  [abs-file]
  (cond
   (resource-path? abs-file)
   (when (to-resource abs-file)
     abs-file)

   (or (url? abs-file)
       (.isFile (clojure.java.io/file abs-file)))
   abs-file))

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

(ns chlorine.util-test
  (:use [chlorine.util]
        [clojure.test]))

(deftest unzip-test
  (is (= (unzip [:foo 1 :bar 2 :baz 3])
         [[:foo :bar :baz] [1 2 3]])))

(deftest replace-map-test
  (is (= "_ooFerfeZ"
         (replace-map "fooFarfaz" (array-map #"^f" "_"
                                             "a" "e"
                                             "z" "Z"))))
  ;; order matters!
  (is (= "fooFarfaZ"
         (replace-map "fooFarfaz" (array-map "z" "Z"
                                             #"z$" "_")))))

(deftest get-dir-tests
  (is (= "r:/foo/bar"
         (get-dir "r:/foo/bar/bazz.cl2")))
  (is (= "/foo/bar"
         (get-dir "/foo/bar/bazz.cl2")))
  (is (= "http://foo.com/bar"
         (get-dir "http://foo.com/bar/bazz.cl2"))))

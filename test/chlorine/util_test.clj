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

(deftest path-type-tests
  (is (= (path-type "./foo")
         :file-relative))
  (is (= (path-type "../foo")
         :file-relative))
  (is (= (path-type "~/foo")
         :home-relative))
  (is (= (path-type "foo/")
         :dir-relative))
  (is (= (path-type "/full/path")
         :absolute))
  (is (= (path-type "r://full/path")
         :absolute))
  (is (= (path-type "http://full/path")
         :absolute))
  (is (= (path-type "https://full/path")
         :absolute))
  (is (= (path-type "file:///full/path")
         :absolute)))

(deftest file-and-dir-tests
  (is (= (file-and-dir "/my/file")
         ["/my/file" "/my/"]))
  (is (= (file-and-dir "http://abc.de/file.cl2")
         ["http://abc.de/file.cl2" "http://abc.de/"]))
  (is (= (file-and-dir "http://abc.de/dir/file.cl2")
         ["http://abc.de/dir/file.cl2" "http://abc.de/dir/"]))
  (is (= (file-and-dir "~/my/file")
         [(str (System/getProperty "user.home") "/" "my/file")
          (str (System/getProperty "user.home") "/" "my/")]))
  (is (= (binding [*cpd* "/fun/ban/"]
           (file-and-dir "my/file"))
         ["/fun/ban/my/file" "/fun/ban/my/"]))
  (is (= (binding [*cwd* "/foo/bar/"]
           (file-and-dir "./my/file"))
         ["/foo/bar/my/file" "/foo/bar/my/"]))
  (is (= (binding [*cwd* "/foo/bar/"]
           (file-and-dir "../my/file"))
         ["/foo/my/file" "/foo/my/"])))

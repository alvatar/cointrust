(ns the-oracle.common-test
  #? (:cljs (:require-macros [cljs.test :refer (is deftest testing)]))
  (:require [the-oracle.common :as sut]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test])))

(deftest example-passing-test-cljc
  (is (= 1 1)))

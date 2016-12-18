(ns the-oracle.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [the-oracle.core-test]
   [the-oracle.common-test]))

(enable-console-print!)

(doo-tests 'the-oracle.core-test
           'the-oracle.common-test)

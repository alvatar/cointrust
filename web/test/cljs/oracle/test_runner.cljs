(ns oracle.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [oracle.core-test]
   [oracle.common-test]))

(enable-console-print!)

(doo-tests 'oracle.core-test
           'oracle.common-test)

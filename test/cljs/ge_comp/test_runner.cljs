(ns ge-comp.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [ge-comp.core-test]))

(enable-console-print!)

(doo-tests 'ge-comp.core-test)

(defsystem "aoc2023"
  :description "Solutions for the Advent of Code 2023 event."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :license "ISC"
  :pathname "src"
  :depends-on
  ("tungsten-core"
   "tungsten-http")
  :serial t
  :components
  ((:file "utils")
   (:file "day-01")))

#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("confluence/client"
    (exe:
     "confluence/confluence"
     "-ld-options"
     "-lyaml"
     )))

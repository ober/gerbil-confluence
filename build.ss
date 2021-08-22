#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("confluence/client"
    (static-exe:
     "confluence/confluence"
     "-ld-options"
     "-lpthread -lyaml -ldl -lssl -lz -L/usr/lib64 -L/usr/pkg/lib -I/usr/pkg/include")))

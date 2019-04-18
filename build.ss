#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)
(import :std/make)

;;(defbuild-script '("confluence.ss"))

(def build-spec
  '("confluence"
    (exe: "confluence" "-ld-options" "-lyaml -lz -L/usr/local/lib/" "-cc-options" "-I/usr/local/include")
    ))

(def build-spec-static
  '("confluence"
    (static-exe: "confluence"
                 "-ld-options" "-lyaml -lz -L/usr/local/lib -L/usr/local/opt/openssl/lib"
		 "-cc-options" "-I/usr/local/include -I/usr/local/opt/openssl/include"
                 "-prelude" "(declare (not safe))")))

(def srcdir
  (path-normalize (path-directory (this-source-file))))

(def (main . args)
  (match args
    (["deps"]
     (let (build-deps (make-depgraph/spec build-spec))
       (call-with-output-file "build-deps" (cut write build-deps <>))))
    (["static"]
     (let (depgraph (call-with-input-file "build-deps" read))
       (make srcdir: srcdir
             bindir: srcdir
             optimize: #t
             static: #t
             depgraph: depgraph
             prefix: "confluence"
             build-spec-static)))
    ([]
     (let (depgraph (call-with-input-file "build-deps" read))
       (make srcdir: srcdir
             bindir: srcdir
             optimize: #t
             debug: 'env
             static: #t
             depgraph: depgraph
             prefix: "confluence"
             build-spec)))))

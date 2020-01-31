;; -*- Gerbil -*-
;;; © ober

(import
  :gerbil/gambit
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/format
  :std/generic
  :std/generic/dispatch
  :std/iter
  :std/misc/list
  :std/misc/channel
  :std/misc/ports
  :std/net/address
  :std/net/request
  :std/pregexp
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  :std/text/base64
  :std/text/json
  :std/text/utf8
  :ober/oberlib
  :std/text/yaml)

(export #t)
(declare (not optimize-dead-definitions))
(def version "0.09")

(def config-file "~/.confluence.yaml")

(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))

(def program-name "confluence")

(def (ensure-config)
  "Ensure critical items are present in the config-file"
  (let ((config (load-config)))
    (let-hash config
      (unless .?space
        (begin
          (displayln "You must specify the default space=mygroup in your " .config-file)
          (exit 2))))
    config))

(def (update id content-file)
  "Update the Body of a document with the contents of content-file on document of id"
  (let-hash (ensure-config)
      (let* ((url (format "~a/rest/api/content/~a" .url id))
             (current (get id))
             (current-title (let-hash current .title))
             (new-title (pregexp-replace* "-" (pregexp-replace* ".cml$" content-file "") " "))
             (current-number (let-hash current (let-hash .version .number)))
             (data (hash
                    ("type" "page")
                    ("title" new-title)
                    ("space" (hash
                              ("key" .?space)))
                    ("body" (hash
                             ("storage" (hash
                                         ("value" (read-file-string content-file))
                                         ("representation" "storage")))))
                    ("version" (hash ("number" (1+ current-number)))))))
        (with ([status body] (rest-call 'put url (default-headers .basic-auth) (json-object->string data)))
          (unless status
            (error body))
          (present-item body)))))

(def (create content-file)
  "Create a new document in Confluence containing the content of content-file
   Use naming convention with hypens -> Spaces, and .cml ending for file ending"
  (let-hash (ensure-config)
    (let* ((url (format "~a/rest/api/content?expand=body" .url))
           (title (pregexp-replace* "-" (pregexp-replace* ".cml$" content-file "") " "))
           (data (hash
                  ("type" "page")
                  ("title" title)
                  ("space" (hash (key .?space)))
                  ("body" (hash
                           ("storage"
                            (hash
                             ("value" (read-file-string content-file))
                             ("representation" "storage")))))))
	   (results (rest-call 'post url (default-headers .basic-auth) (json-object->string data))))
      (displayln results))))

(def (convert markdown-file)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/contentbody/convert/storage" .url))
	   (data (hash
		  ("value" (read-file-string markdown-file))
		  ("representation" "wiki")))
           (cml (format "~a.cml" (pregexp-replace ".cmd" markdown-file ""))))
      (with ([status body] (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
        (unless status
          (error body))
        (displayln body)
        (when (table? body)
          (let-hash body
            (with-output-to-file cml (cut displayln .value))))))))

(def (md2c markdown-file)
  (let ((cml (format "~a.cmd" (pregexp-replace ".md" markdown-file ""))))
    (markdown-to-confluence markdown-file cml)
    (displayln "Output file " cml " created.")))

;; (def (md2c markdown-file)
;;   (let* ((data (read-file-string markdown-file))
;;          (rules [
;;                  [ "^# ([a-zA-Z0-9]+)$" "h1. \\1" ]
;;                  [ "^## ([a-zA-Z0-9]+)$" "h2. \\1" ]
;;                  [ "^### ([a-zA-Z0-9]+)$" "h3. \\1" ]
;;                  [ "^#### ([a-zA-Z0-9]+)$" "h4. \\1" ]
;;                  [ "^##### ([a-zA-Z0-9]+)$" "h5. \\1" ]
;;                  ])
;;          (results ""))
;;     (with-output-to-file cml
;;       (lambda ()
;;         (set! results data)
;;         (for (rule rules)
;;           (let ((pattern (nth 0 rule))
;;                 (substitution (nth 1 rule)))
;;             (displayln "pattern is " pattern " substitution is " substitution)
;;             (set! results (pregexp-replace* pattern data substitution))))
;;         (displayln data)))))

(def (converter in-file in-format out-format)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/contentbody/convert/~a" .url out-format))
	   (data (hash
		  ("value" (read-file-string in-file))
		  ("representation" in-format)))
	   (results (rest-call 'post url (default-headers .basic-auth) (json-object->string data)))
	   (myjson (from-json results))
           (cml (format "~a.cml" (pregexp-replace ".cmd" in-file ""))))
      (let-hash myjson
        (with-output-to-file cml (cut displayln .value))))))

(def (longtask last)
  (let-hash (load-config)
    (let* ((results (rest-call 'get (format "~a/longtask" .url) (default-headers .basic-auth))))
      ;;(myjson (with-input-from-string results read-json)))
      (displayln (hash->list results)))))

(def (remove-doc id)
  "Delete Confluence document with the id"
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/content?id=~a" .url id))
           (results (rest-call 'delete url (default-headers .basic-auth))))
      ;;           (myjson (from-json results)))
      (displayln results))))

(def (make-web-safe string)
  (let* ((output (pregexp-replace* " " string "%20")))
    output))

(def (search q)
  "Search confluence for query"
  (let-hash (load-config)
    (let* ((outs [])
           (df [ "id" "type" "status" "title" "space" "expandables" "tinyurl" ])
           (sf .?search-fields)
           (query (make-web-safe q))
	   (url (if (string-contains query "~")
                  (format "~a/rest/api/content/search?cql=~a" .url query)
                  (format "~a/rest/api/content/search?cql=text~~~a" .url query)))
	   (results (rest-call 'get url (default-headers .basic-auth)))
           (headers (if (and sf
                             (list? sf)
                             (length>n? sf 1))
                      sf
                      df)))

      (with ([ status body ] results)
        (unless status
          (error body))
        (when (table? body)
          (let-hash body
            (set! outs (cons headers outs))
            (for (doc .results)
              (dp (hash->list doc))
              (let ((row (hash)))
                (let-hash doc
                  (when (and .?_expandable
                             (table? .?_expandable))
                    (let-hash .?_expandable
                      (hash-put! row "expandables" (hash-keys .._expandable))
                      (hash-put! row "space" .?space)))
                  (hash-put! row "id" .?id)
                  (hash-put! row "title" .?title)
                  (hash-put! row "status" .?status)
                  (hash-put! row "type" .?type)
                  (when (and .?_links
                             (table? ._links))
                    (let-hash ._links
                      (hash-put! row "tinyurl" (if .?tinyui
                                                 (format "~a~a" ....?url .tinyui)
                                                 "N/A"))
                      (hash-put! row "url" (if .?webui
                                             (format "~a~a" ....?url .webui)
                                             "N/A")))))
                (set! outs (cons (filter-row-hash row headers) outs))))
            (style-output outs)))))))


(def (info id)
  "Interactive version"
  (displayln (get id)))

(def (get id)
  "Return json object of the document with id"
  (let-hash (load-config)
    (let (url (format "~a/rest/api/content/~a" .url id))
      (with ([status body] (rest-call 'get url (default-headers .basic-auth)))
        (unless status
          (error body))
        body))))

(def (load-config)
  (let ((config (hash))
        (config-data (yaml-load config-file)))
    (unless (and (list? config-data)
                 (length>n? config-data 0)
                 (table? (car config-data)))
      (displayln (format "Could not parse your config ~a" config-file))
      (exit 2))
    (hash-for-each
     (lambda (k v)
       (hash-put! config (string->symbol k) v))
     (car config-data))
    (let-hash config
      (hash-put! config 'style (or .?style "org-mode"))
      (when .?secrets
	(let-hash (u8vector->object (base64-decode .secrets))
	  (let ((password (get-password-from-config .key .iv .password)))
	    (hash-put! config 'basic-auth (make-basic-auth ..?user password))
	    config))))))

(def (body id)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/content/~a?expand=body.view&depth=all" .url id))
	   (results (rest-call 'get url (default-headers .basic-auth)))
	   (myjson (with-input-from-string results read-json)))
      (let-hash myjson
	(let-hash .body
	  (let-hash .view
	    (displayln .value)))))))

(def (config)
  (let-hash (load-config)
    (displayln "What is your password?: ")
    (let* ((password (read-password ##console-port))
	   (cipher (make-aes-256-ctr-cipher))
	   (iv (random-bytes (cipher-iv-length cipher)))
	   (key (random-bytes (cipher-key-length cipher)))
	   (encrypted-password (encrypt cipher key iv password))
	   (enc-pass-store (u8vector->base64-string encrypted-password))
	   (iv-store (u8vector->base64-string iv))
	   (key-store (u8vector->base64-string key))
	   (secrets (base64-encode (object->u8vector
				    (hash
				     (password enc-pass-store)
				     (iv iv-store)
				     (key key-store))))))

      (displayln "Add the following lines to your " config-file)
      (displayln "secrets: " secrets))))

(def (get-password-from-config key iv password)
  (bytes->string
   (decrypt
    (make-aes-256-ctr-cipher)
    (base64-string->u8vector key)
    (base64-string->u8vector iv)
    (base64-string->u8vector password))))


;; old utils

(def (markdown-to-confluence in out)
  (def from (call-with-input-file in read-all-as-lines))
  (def to (open-output-file [path: out append: #f]))

  (write-string (string-append "{toc}" (string #\newline)) to)

  (def (write-line in out)
    (let loop ((c (read-char in)))
      (unless (eq? c #!eof)
        (write-char c out)
        (loop (read-char in)))))

  (def (read-until c port)
    (let loop ((c* (read-char port)) (cl []))
      (cond
       ((eq? c c*)
        (reverse cl))
       ((eq? c #!eof)
        (reverse cl))
       (else
        (loop (read-char port) (cons c* cl))))))

  (def (read-n n c port)
    (let loop ((c* (read-char port)) (n* 0) (cl []))
      (if (< n* n)
        (if (eq? c c*)
          (loop (read-char port) (1+ n*) (cons c* cl))
          #f)
        (reverse cl))))

  ;; markdown functions

  ;; '### ' -> 'h3. '
  (def (hn c port)
    (let loop ((c c) (n 0))
      (cond
       ((eq? c #\#)
        (loop (read-char port) (1+ n)))
       ((eq? c #\space)
        (string->list (string-append "h" (number->string n) ". ")))
       (else
        '()))))

  ;; '> ' -> ' ' && inside code-block
  (def (quote-block c port)
    (let loop ((c c))
      (cond
       ((eq? c #\>)
        (loop (read-char port)))
       ((eq? c #\space)
        (string->list " "))
       (else
        '()))))

  ;; for each line
  (let ((in-quote-block? #f))
    (for (i from)
      (let* ((port (open-input-string i))
             (output (open-output-string)))
        (let loop ((p (read-char port)))
          (cond
           ((eq? p #\#)
            (if (= (input-port-char-position port) 1)
              (let (h (hn p port))
                (map (cut write-char <> output) h))
              (write-char p output))
            (loop (read-char port)))
           ((eq? p #\>)
            (when (= (input-port-char-position port) 1)
              (unless in-quote-block?
                (map (cut write-char <> output) (string->list "{quote}"))
                (set! in-quote-block? #t)))
            (let ((l (quote-block p port)))
              (map (cut write-char <> output) l))
            (loop (read-char port)))
           ((eq? p #!eof)
            (let ((pos (input-port-char-position port)))
              (when (and
                      in-quote-block?
                      (= 0 pos))
                (set! in-quote-block? #f)
                (map (cut write-char <> output) (string->list "{quote}"))))
            (close-output-port output)
            (write-string (string-append (get-output-string output) (string #\newline)) to))
           (else
            (write-char p output)
            (loop (read-char port)))))
           (close-input-port port)))

    (when in-quote-block?
      (write-string "{quote}" to)))
  (force-output to)
  (close-output-port to))

(def (default-headers basic)
  [
   ["Accept" :: "*/*"]
   ["Content-type" :: "application/json"]
   ["Authorization" :: basic ]
   ])

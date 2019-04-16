;; -*- Gerbil -*-
package: confluence
namespace: confluence
(export main)

(declare (not optimize-dead-definitions))
(import
  :gerbil/gambit
  :scheme/base
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/format
  :std/generic
  :std/generic/dispatch
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
  :std/text/yaml
  )

(def config-file "~/.confluence.yaml")

(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))

(def DEBUG (getenv "DEBUG" #f))

(def program-name "confluence")

(def (dp msg)
  (when DEBUG
    (displayln msg)))

(def interactives
  (hash
   ("body" (hash (description: "Get body of doc") (usage: "body <id of doc>") (count: 1)))
   ("config" (hash (description: "Setup your user and password in the config encrypted") (usage: "config") (count: 0)))
   ("convert" (hash (description: "Convert Confluence Markdown to Confluence html") (usage: "convert <markdown string>") (count: 1)))
   ("get" (hash (description: "Get content of doc") (usage: "get <id of doc>") (count: 1)))
   ("search" (hash (description: "Search for docs matching string") (usage: "search <query string>") (count: 1)))
   ))

(def (main . args)
  (if (null? args)
    (usage))
  (let* ((argc (length args))
	 (verb (car args))
	 (args2 (cdr args)))
    (unless (hash-key? interactives verb)
      (usage))
    (let* ((info (hash-get interactives verb))
	   (count (hash-get info count:)))
      (unless count
	(set! count 0))
      (unless (= (length args2) count)
	(usage-verb verb))
      (apply (eval (string->symbol (string-append "confluence#" verb))) args2))))

(def (success? status)
  (and (>= status 200) (<= status 299)))

(def (do-post uri headers data)
  (dp (print-curl "post" uri headers data))
  (try
   (let* ((reply (http-post uri
			    headers: headers
			    data: data))
	  (status (request-status reply))
	  (text (request-text reply)))

     (if (success? status)
       (displayln status text)
       (displayln (format "Failure on post. Status:~a Text:~a~%" status text))))
   (catch (e)
     (begin
       (display-exception e)))))

(def (do-put uri headers data)
  (dp (print-curl "put" uri headers data))
  (let* ((reply (http-put uri
			  headers: headers
			  data: data))
	 (status (request-status reply))
	 (text (request-text reply)))

    (if (success? status)
      (displayln text)
      (displayln (format "Failure on post. Status:~a Text:~a~%" status text)))))

(def (do-delete uri headers params)
  (dp (print-curl "delete" uri headers params))
  (let* ((reply (http-delete uri
			     headers: headers
			     params: params))
	 (status (request-status reply))
	 (text (request-text reply)))

    (if (success? status)
      (displayln text)
      (displayln (format "Failure on delete. Status:~a Text:~a~%" status text)))))

(def (stringify-hash h)
  (let ((results []))
    (if (table? h)
      (begin
	(hash-for-each
	 (lambda (k v)
	   (set! results (append results (list (format " ~a->" k) (format "~a   " v)))))
	 h)
	(append-strings results))
      ;;        (pregexp-replace "\n" (append-strings results) "\t"))
      "N/A")))

(def (print-curl type uri headers data)
  ;;(displayln headers)
  (let ((heads "Content-type: application/json")
	(do-curl (getenv "DEBUG" #f)))
    (when do-curl
      (cond
       ((string=? type "get")
	(if (string=? "" data)
	  (displayln (format "curl -X GET -H \'~a\' ~a" heads uri))
	  (displayln (format "curl -X GET -H \'~a\' -d \'~a\' ~a" heads data uri))))
       ((string=? type "put")
	(displayln (format "curl -X PUT -H \'~a\' -d \'~a\' ~a" heads data uri)))
       ((string=? type "post")
	(displayln (format "curl -X POST -H \'~a\' -d \'~a\' ~a" heads data uri)))
       ((string=? type "delete")
	(displayln (format "curl -X DELETE -H \'~a\' -d \'~a\' ~a" heads data uri)))
       (else
	(displayln "unknown format " type))))))

(def (do-get uri)
  (print-curl "get" uri "" "")
  (let* ((reply (http-get uri))
	 (status (request-status reply))
	 (text (request-text reply)))
    (if (success? status)
      text
      (displayln (format "Error: got ~a on request. text: ~a~%" status text)))))

(def (do-post-generic uri headers data)
  (let* ((reply (http-post uri
			   headers: headers
			   data: data))
	 (status (request-status reply))
	 (text (request-text reply)))
    (dp (print-curl "post" uri headers data))
    (if (success? status)
      text
      (displayln (format "Error: Failure on a post. got ~a text: ~a~%" status text)))))

(def (do-get-generic uri headers)
  (let* ((reply (http-get uri
			  headers: headers))
	 (status (request-status reply))
	 (text (request-text reply)))
    (print-curl "get" uri "" "")
    (if (success? status)
      text
      (displayln (format "Error: got ~a on request. text: ~a~%" status text)))))

(def (usage-verb verb)
  (let ((howto (hash-get interactives verb)))
    (displayln "Wrong number of arguments. Usage is:")
    (displayln program-name " " (hash-get howto usage:))
    (exit 2)))

(def (usage)
  (displayln "Usage: confluence <verb>")
  (displayln "Verbs:")
  (for-each
    (lambda (k)
      (displayln (format "~a: ~a" k (hash-get (hash-get interactives k) description:))))
    (sort! (hash-keys interactives) string<?))
  (exit 2))

(def (nth n l)
  (if (or (> n (length l)) (< n 0))
    (error "Index out of bounds.")
    (if (eq? n 0)
      (car l)
      (nth (- n 1) (cdr l)))))

(def (float->int num)
  (inexact->exact
   (round num)))

(def (print-date date)
  (date->string date "~c"))

(def (from-json json)
  (try
   (with-input-from-string json read-json)
   (catch (e)
     (displayln "error parsing json " e))))

(def (epoch->date epoch)
  (cond
   ((string? epoch)
    (time-utc->date (make-time time-utc 0 (string->number epoch))))
   ((flonum? epoch)
    (time-utc->date (make-time time-utc 0 (float->int epoch))))
   ((fixnum? epoch)
    (time-utc->date (make-time time-utc 0 epoch)))))

(def (date->epoch mydate)
  (string->number (date->string (string->date mydate "~Y-~m-~d ~H:~M:~S") "~s")))

(def (flatten x)
  (cond ((null? x) '())
	((pair? x) (append (flatten (car x)) (flatten (cdr x))))
	(else (list x))))

(def (strip-quotes str)
  (pregexp-replace*
   "\""
   str
   ""))

(def (convert markdown)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/contentbody/convert/storage" .url))
	   (data (hash
		  ("value" markdown)
		  ("representation" "wiki")))
	   (results (do-post-generic
		     url
		     (default-headers .basic-auth)
		     (json-object->string data)))
	   (myjson (with-input-from-string results read-json)))
      (let-hash myjson
	(displayln .value)))))

(def (make-basic-auth user password)
  (format "Basic ~a"
	  (base64-encode
	   (string->utf8 (format "~a:~a" user password)))))

(def (search query)
  (let-hash (load-config)
    (let* ((query (make-web-safe query))
	   (url (if (string-contains query "~")
		  (format "~a/rest/api/content/search?cql=~a" .url query)
		  (format "~a/rest/api/content/search?cql=text~~~a" .url query)))
	   (results (do-get-generic url (default-headers .basic-auth)))
	   (myjson (with-input-from-string results read-json)))
      (let-hash myjson
	(displayln "| Title | id | Type |url|tinyurl| ")
	(displayln "|-|")
	(for-each
	  (lambda (p)
	    (let-hash p
	      (let-hash ._links
		(displayln
			   "|" ..title
			   "|" ..id
			   "|" ..type
			   "|" ....url .?webui
			   "|" ....url .?tinyui
			   "|"))))
	  .results)))))

(def (get id)
  (let-hash (load-config)
    (let* ((url (format "~a/rest/api/content/~a" .url id))
	   (results (do-get-generic url (default-headers .basic-auth)))
	   (myjson (with-input-from-string results read-json)))
      (displayln results))))
;; (let-hash myjson
;;   (displayln "| Title | id | Type |url|tinyurl| ")
;;   (displayln "|-|")
;;   (for-each
;; 	(lambda (p)
;; 	  (let-hash p
;; 	    (let-hash ._links
;; 	      (displayln "|"
;; 			 ..title "|"
;; 			 ..id "|"
;; 			 ..type "|"
;; 			 server .?webui "|"
;; 			 server .?tinyui "|"
;; 			 "|"))))
;; 	.results))))


(def (default-headers basic)
  [
   ["Accept" :: "*/*"]
   ["Content-type" :: "application/json"]
   ["Authorization" :: basic ]
   ])

(def (load-config)
  (let ((config (hash)))
    (hash-for-each
     (lambda (k v)
       (hash-put! config (string->symbol k) v))
     (car (yaml-load config-file)))
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
	   (results (do-get-generic url (default-headers .basic-auth)))
	   (myjson (with-input-from-string results read-json)))
      (let-hash myjson
	(let-hash .body
	  (let-hash .view
	    (displayln .value)))))))

(def (make-web-safe string)
  (let* ((output (pregexp-replace* " " string "%20")))
    output))


(def (config)
  (let-hash (load-config)
    (displayln "What is your password?: ")
    (let* ((password (read-line (current-input-port)))
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
      (displayln "-----------------------------------------")
      (displayln "secrets: " secrets)
      (displayln "-----------------------------------------"))))

(def (get-password-from-config key iv password)
  (bytes->string
   (decrypt
    (make-aes-256-ctr-cipher)
    (base64-string->u8vector key)
    (base64-string->u8vector iv)
    (base64-string->u8vector password))))

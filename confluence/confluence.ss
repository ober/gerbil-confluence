;;; -*- Gerbil -*-
;;; © ober
;;; Confluence Client Binary

(import
  :gerbil/gambit
  :gerbil/gambit/ports
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/db/dbi
  :std/debug/heap
  :std/iter
  :std/error
  :std/format
  :std/generic
  :std/generic/dispatch
  :std/misc/channel
  :std/misc/list
  :std/misc/ports
  :std/net/address
  :std/net/request
  :std/net/uri
  :std/pregexp
  :std/srfi/1
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  :std/text/base64
  :std/text/json
  :std/text/utf8
  :std/text/yaml
  :std/text/zlib
  :std/xml/ssax
  :ober/oberlib
  :ober/confluence/client)

(export main)
(declare (not optimize-dead-definitions))
(def program-name "confluence")

(def interactives
  (hash
   ("body" (hash (description: "Get body of doc") (usage: "body <id of doc>") (count: 1)))
   ("config" (hash (description: "Setup your user and password in the config encrypted") (usage: "config") (count: 0)))
   ("convert" (hash (description: "Convert Confluence Markdown to Confluence html") (usage: "convert <markdown string>") (count: 1)))
   ("create" (hash (description: "Publish a new document")(usage: "create <file of content>") (count: 1)))
   ("info" (hash (description: "Get information of doc") (usage: "info <id of doc>") (count: 1)))
   ("longtask" (hash (description: "List longtasks") (usage: "longask <seconds>") (count: 1)))
   ("remove-doc" (hash (description: "Remove a document")(usage: "remove-doc <id of doc>") (count: 1)))
   ("search" (hash (description: "Search for docs matching string") (usage: "search <query string>") (count: 1)))
   ("update" (hash (description: "Publish an update to an existing document")(usage: "update <id> <file of content>") (count: 2)))
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
      (apply (eval (string->symbol (string-append "ober/confluence/client#" verb))) args2))))

(def (usage-verb verb)
  (let ((howto (hash-get interactives verb)))
    (displayln "Wrong number of arguments. Usage is:")
    (displayln program-name " " (hash-get howto usage:))
    (exit 2)))

(def (usage)
  (displayln (format "Confluence: version ~a" version))
  (displayln "Usage: confluence <verb>")
  (displayln "Verbs:")
  (for (k (sort! (hash-keys interactives) string<?))
       (displayln (format "~a: ~a" k (hash-get (hash-get interactives k) description:))))
  (exit 2))

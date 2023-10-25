;;; -*- Gerbil -*-
;;; © ober 2023
;;; Confluence Client Binary

(import
  :gerbil/gambit
  :ober/confluence/client
  :ober/oberlib
  :std/crypto/cipher
  :std/error
  :std/format
  :std/generic/dispatch
  :std/getopt
  :std/iter
  :std/logger
  :std/misc/list
  :std/misc/ports
  :std/pregexp
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  :std/text/base64
  :std/text/json
  :clan/text/yaml)

(export main)
(def program-name "confluence")

(def (main . args)
  (def body
    (command 'body help: "Get body of doc"
	     (argument 'id help: "id of doc")))

  (def config
    (command 'config help: "Setup your user and password in the config encrypted"))

  (def convert
    (command 'convert help: "Convert Confluence Markup to Confluence XML™"
	     (argument 'file help: "markup .cml file")))

  (def get
    (command 'get help: "Retrieve information by id"
	     (argument 'id help: "id of document")))

  (def md2c
    (command 'md2c help: "Convert file from markdown to Confluence Markup"
	     (argument 'file help: "markdown .md file")))

  (def converter
    (command 'converter help: "Convert Between different formats"
	     (argument 'input help: "input file")
	     (argument 'iformat help: "In format: wiki, storage, view")
             (argument 'oformat help:  "output format: storage, view, editor, exported_view, styled_view")))

  (def create
    (command 'create help: "Publish a new document"
	     (argument 'file help: "file of content")))

  (def info
    (command 'info help: "Get information of doc"
	     (argument 'id help: "id of doc")))

  (def longtask
    (command 'longtask help: "List longtasks"
	     (argument 'secs help: "seconds")))

  (def remove-doc
    (command 'remove-doc help: "Remove a document"
	     (argument 'id help: "id of doc")))

  (def search
    (command 'search help: "Search for docs matching string"
	     (argument 'query help: "query string")))

  (def update-batch
    (command 'update-batch help: "Publish an update to an existing document"
	     (argument 'version help: "current version")
	     (argument 'id help: "id of document")
	     (argument 'title help: "title")
	     (argument 'file help: "file of content>")))

  (def update
    (command 'update :help "Publish an update to an existing document"
	     (argument 'id help: "id of document")
	     (argument 'file help: "file of content")))

  (call-with-getopt process-args args
		    program: "confluence"
		    help: "Confluence cli in Gerbil"
		    config
		    body
		    convert
		    get
		    md2c
		    converter
		    create
		    info
		    longtask
		    remove-doc
		    search
		    update-batch
		    update
		    ))

(def (process-args cmd opt)
  (let-hash opt
    (case cmd
      ((body)
       (body .id))
      ((config)
       (config))
      ((convert)
       (convert .file))
      ((get)
       (get .id))
      ((md2c)
       (md2c .file))
      ((converter)
       (converter .input .iformat .oformat))
      ((create)
       (create .file))
      ((info)
       (info .id))
      ((longtask)
       (longtask .secs))
      ((remove-doc)
       (remove-doc .id))
      ((search)
       (search .query))
      ((update-batch)
       (update-batch .version .id .title .file))
      ((update)
       (update .id .file)))))

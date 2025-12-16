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
    (command 'update help: "Publish an update to an existing document"
	     (argument 'id help: "id of document")
	     (argument 'file help: "file of content")))

  ;; Space commands
  (def list-spaces
    (command 'list-spaces help: "List all spaces"))

  (def get-space
    (command 'get-space help: "Get space information"
             (argument 'key help: "space key")))

  (def create-space
    (command 'create-space help: "Create a new space"
             (argument 'key help: "space key")
             (argument 'name help: "space name")
             (argument 'description help: "space description")))

  (def delete-space
    (command 'delete-space help: "Delete a space"
             (argument 'key help: "space key")))

  ;; Label commands
  (def get-labels
    (command 'get-labels help: "Get labels for content"
             (argument 'id help: "content id")))

  (def add-labels
    (command 'add-labels help: "Add labels to content"
             (argument 'id help: "content id")
             (argument 'labels help: "comma-separated labels")))

  (def remove-label
    (command 'remove-label help: "Remove a label from content"
             (argument 'id help: "content id")
             (argument 'label help: "label name")))

  ;; Attachment commands
  (def get-attachments
    (command 'get-attachments help: "Get attachments for content"
             (argument 'id help: "content id")))

  ;; Comment commands
  (def get-comments
    (command 'get-comments help: "Get comments for content"
             (argument 'id help: "content id")))

  (def create-comment
    (command 'create-comment help: "Create a comment"
             (argument 'id help: "parent content id")
             (argument 'text help: "comment text")))

  ;; User commands
  (def get-current-user
    (command 'get-current-user help: "Get current user information"))

  (def get-user
    (command 'get-user help: "Get user information"
             (argument 'username help: "username")))

  ;; Group commands
  (def list-groups
    (command 'list-groups help: "List all groups"))

  (def get-group
    (command 'get-group help: "Get group information"
             (argument 'name help: "group name")))

  ;; Content children/descendants
  (def get-children
    (command 'get-children help: "Get children of content"
             (argument 'id help: "content id")))

  (def get-descendants
    (command 'get-descendants help: "Get descendants of content"
             (argument 'id help: "content id")))

  ;; Version/History commands
  (def get-history
    (command 'get-history help: "Get content history"
             (argument 'id help: "content id")))

  (def list-versions
    (command 'list-versions help: "List content versions"
             (argument 'id help: "content id")))

  ;; System commands
  (def system-info
    (command 'system-info help: "Get system information"))

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
		    ;; Space commands
		    list-spaces
		    get-space
		    create-space
		    delete-space
		    ;; Label commands
		    get-labels
		    add-labels
		    remove-label
		    ;; Attachment commands
		    get-attachments
		    ;; Comment commands
		    get-comments
		    create-comment
		    ;; User commands
		    get-current-user
		    get-user
		    ;; Group commands
		    list-groups
		    get-group
		    ;; Content hierarchy
		    get-children
		    get-descendants
		    ;; History/versions
		    get-history
		    list-versions
		    ;; System
		    system-info
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
       (update .id .file))
      ;; Space commands
      ((list-spaces)
       (present-item (list-spaces)))
      ((get-space)
       (present-item (get-space .key)))
      ((create-space)
       (present-item (create-space .key .name .description)))
      ((delete-space)
       (present-item (delete-space .key)))
      ;; Label commands
      ((get-labels)
       (present-item (get-labels .id)))
      ((add-labels)
       (let ((label-list (string-split .labels #\,)))
         (present-item (add-labels .id label-list))))
      ((remove-label)
       (present-item (remove-label .id .label)))
      ;; Attachment commands
      ((get-attachments)
       (present-item (get-attachments .id)))
      ;; Comment commands
      ((get-comments)
       (present-item (get-comments .id)))
      ((create-comment)
       (present-item (create-comment .id .text)))
      ;; User commands
      ((get-current-user)
       (present-item (get-current-user)))
      ((get-user)
       (present-item (get-user username: .username)))
      ;; Group commands
      ((list-groups)
       (present-item (list-groups)))
      ((get-group)
       (present-item (get-group .name)))
      ;; Content hierarchy
      ((get-children)
       (present-item (get-content-children .id)))
      ((get-descendants)
       (present-item (get-content-descendants .id)))
      ;; History/versions
      ((get-history)
       (present-item (get-content-history .id)))
      ((list-versions)
       (present-item (list-content-versions .id)))
      ;; System
      ((system-info)
       (present-item (get-system-info))))))

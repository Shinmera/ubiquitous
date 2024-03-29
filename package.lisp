(in-package #:cl-user)
(defpackage #:ubiquitous
  (:nicknames #:org.shirakumo.ubiquitous)
  (:use #:cl)
  ;; accessor.lisp
  (:export
   #:field
   #:remfield
   #:augment)
  ;; config.lisp
  (:export
   #:*commit*
   #:*changed*
   #:value
   #:remvalue
   #:defaulted-value
   #:call-with-transaction
   #:with-transaction)
  ;; metadata.lisp
  (:export
   #:*metadata-version*
   #:*metadata-prefix*
   #:metadata-condition
   #:unknown-version
   #:version
   #:bad-configuration-package
   #:bad-package-name
   #:bad-metadata-header
   #:header
   #:find-metadata-package
   #:process-metadata
   #:generate-metadata
   #:maybe-read-metadata
   #:print-metadata
   #:with-processed-metadata)
  ;; pathname.lisp
  (:export
   #:config-directory
   #:config-pathname
   #:package-directory
   #:designator-pathname)
  ;; storage.lisp
  (:export
   #:*storage*
   #:*storage-type*
   #:*storage-pathname*
   #:no-storage-file
   #:file
   #:read-storage
   #:use-new-storage
   #:write-storage
   #:with-storage
   #:lazy-loader
   #:with-local-storage
   #:restore
   #:offload
   #:destroy
   #:unknown-reader-type
   #:reader-type
   #:define-ubiquitous-writer
   #:define-ubiquitous-reader))

;; packages.lisp
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

(in-package #:user)

;; (um:in-project #:com.sd.okeanos)

;; ---------------------------------
#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (um:alias-package '#:com.sd.okeanos.int  '#:{5BADD0BA-5E62-11E0-9F3C-0017F2CCD25E}))
|#
(defpackage #:com.sd.okeanos.int
  (:use #:common-lisp)
  (:import-from #:useful-macros
   #:IF-LET
   #:WHEN-LET
   #:DCASE
   #:MATCH
   #:COMPOSE
   #:CURRY
   #:RCURRY
   #:FOLDL
   #:FOLDR
   #:MKSTR
   #:FLATTEN
   #:MKLIST
   #:NLET
   #:NLET-TAIL
   #:DEFMACRO!
   #:EQL-TREE
   #:BIND*
   #:OR-SETF
   #:DELETEF-IF
   #:ENSURE-ASSOC
   #:SPLIT-STRING
   #:CEILING-PWR2
   #:ALIGN-PWR2
   #:DEFERRED)

  (:import-from #:com.sd.butterfly.int
   #:!
   #:?
   #:RECV
   #:SELF
   ;; #:EXIT-MESSAGE
   #:MAKE-EXIT-MESSAGE
   #:EXIT
   #:KILL
   #:SPAWN
   #:SPAWN-LINK
   #:LINK
   #:UNLINK
   #:MONITOR-LINK
   #:REGISTERED?
   #:REGISTER-SERVICE
   #:RESOLVE-SERVER
   #:REMOTE-SPAWN-LINK
   #:PID-NAME
   #:GET-PID-PROPERTY
   #:INIT-SERVER
   #:BFLY-EXN-TIMEOUT
   #:timed-out)

  (:import-from #:com.sd.butterfly.bb
   #:!?
   #:!?-send
   #:!?-send-multi
   #:!?-recv-multi
   #:with-rpc-timeout
   #:call-sync
   #:call-async
   #:make-bfly-service-loop
   #:make-service)
  (:import-from #:com.sd.butterfly.lfm
   #:LOG-INFO
   #:LOG-WARNING
   #:LOG-ERROR)
  
  (:import-from #:sdle-store
   #:RAWBYTES
   #:RAWBYTES-BYTES
   #:MAKE-RAWBYTES
   #:AFTER-RETRIEVE)
  (:export

   #:ALIGN-2
   #:ALIGN-4
   #:ALIGN-8
   #:ALIGN-16
   #:ALIGN-PAGE
   
   #:CMPLX32
   #:CMPLX64

   #:FLT32
   #:FLT64

   #:INT8
   #:INT16
   #:INT32
   #:INT64

   #:OFF_T
   #:OID_T
   #:SIZE_T
   #:SSIZE_T
   #:TS_T
   #:UUID_T

   #:UINT8
   #:UINT16
   #:UINT32
   #:UINT64

   #:W128-UNION
   #:W16-UNION
   #:W32-UNION
   #:W64-UNION
   #:A
   #:D
   #:F
   #:I
   #:U
   #:ULH

   #:OID
   #:WHEN-CREATED
   #:GET-NEW-OID
   #:PERSIST
   #:REF
   #:DEREF
   #:ROLLBACK
   #:ABORT-TRANSACTION
   #:COMMIT
   #:ATOMIC
   #:ORELSE
   #:SAME-REF
   #:GET-PERSISTENT-CLASS
   #:GET-PERSISTENT-OBJECT
   #:IS-PERSISTENT
   #:IS-DELETED
   #:GET-NEW-TS
   #:DELETE-PERSISTENT-OBJECT
   #:MARK-DIRTY
   #:OID-FOR-OBJECT
   #:RAISE-ROLLBACK-EXCEPTION
   #:CONNECT-TO-DATABASE
   #:CONNECT-TO-DATABASES
   #:DISCONNECT-FROM-DATABASE
   #:DISCONNECT-FROM-DATABASES
   #:BREAK-LOCK
   #:START-OKEANOS-SERVER

   #:PERSISTENT-CLASS
   #:PERSISTENT-OBJECT
   #:DESCRIBE-PERSISTENCE

   #:COLUMN
   #:SCHEMA
   #:COLUMN-C-ACCESS-SPEC
   #:COLUMN-C-TYPE
   #:COLUMN-COMPARE-FUNCTION
   #:COLUMN-CONSTRAINT
   #:COLUMN-DEFAULT-VALUE
   #:COLUMN-EQUALITY-PREDICATE
   #:COLUMN-FETCH-FUNCTION
   #:COLUMN-INDEXED
   #:COLUMN-NAME
   #:COLUMN-OFFSET
   #:COLUMN-POSITION
   #:COLUMN-STORE-FUNCTION
   #:COLUMN-TYPE
   #:COLUMN-VALUE-NORMALIZER
   #:COMPARE
   #:MAKE-SCHEMA
   #:ENSURE-SCHEMA
   #:SCHEMA
   #:SCHEMA-C-TYPE
   #:SCHEMA-COLUMN-SPECS
   #:SCHEMA-INDICES
   #:SCHEMA-NAME
   #:SCHEMA-NEEDS-INDEX-COUNTER
   #:SCHEMA-ROW-SIZE
   #:SCHEMA-UUID
   #:STORE-TABLE-STRING
   #:FETCH-TABLE-STRING
   #:STORE-TABLE-VALUE
   #:FETCH-TABLE-VALUE
   #:NORMALIZE-FOR-COLUMN
   #:NORMALIZE-ROW
   #:NOT-NULLABLE
   #:FIXED-LENGTH-RECORDS-P
   #:GET-COLUMN
   #:GET-COLUMN-NAMES
   #:GET-KEY-COLUMN
   #:GET-KEY-COLUMN-NAME
   #:GET-KEY-COLUMN-TEST
   #:MAKE-COLUMN

   #:OK-SET
   #:ADD-TO-SET
   #:REMOVE-FROM-SET
   #:SET-COUNT
   #:SET-MEMBER-P
   #:OK-SET-NAME
   #:MAP-SET
   #:DOSET
   #:FIND-OK-SET

   #:OK-MAP
   #:GET-MAP
   #:UNMAP
   #:OK-MAP-NAME
   #:MAP-COUNT
   #:LAST-KEY
   #:FIRST-KEY
   #:MAP-MAP
   #:FIND-OK-MAP

   #:OK-TABLE
   #:OK-SCHEMA
   #:OK-SCHEMA-NAME
   #:OK-TABLE-NAME
   #:TABLE-COUNT
   #:TABLE-SCHEMA
   #:FIND-OK-TABLE
   #:FIND-OK-SCHEMA
   #:BULK-LOAD-TABLE
   #:FETCH-ROW
   #:FETCH-FIRST-ROW
   #:FETCH-LAST-ROW
   #:GET-COLUMN-NAMES
   #:GET-KEY-COLUMN-NAME
   #:GET-KEY-COLUMN-TEST
   #:INSERT-ROW
   #:DELETE-ROW
   #:MAP-ROWS
   #:fetch-all-rows
   #:fetch-rows-for-column
   #:find-rows-for-column

   #:VIEW-BTREE
   
   #:FIND-INSTANCES
   #:FIND-INSTANCES*
   #:FIND-INSTANCES-FOR-SLOT
   #:FETCH-INSTANCES-FOR-SLOT
   #:FIRST-INSTANCES-FOR-SLOT
   #:LAST-INSTANCES-FOR-SLOT
   #:MAP-INSTANCES
   #:MAP-INSTANCES*
   #:MAP-INSTANCES-FOR-SLOT
   #:DELETE-INSTANCES-FOR-SLOT
   #:QUERY
   #:COMPILE-QUERY
   
   #:RPC
   #:?RPC
   #:?APPLY-RPC
   #:DEFRPC

   #:OID-MAPPINGS
   #:GET-FILE-UUID
   #:OID=
   #:MAKE-OID

   #:LOCK-DAEMON

   #:OID-NOT-FOUND-ERROR
   #:show-persistent-class-definition
   #:get-persistent-classes
   #:map-deref
   #:make-transaction-viewer
   ))

;; ----------------------------------------------------
;; User level package...

(defpackage #:com.sd.okeanos.user
  (:use #:common-lisp #:com.sd.okeanos.int)
  (:nicknames #:okeanos #:okno #:okdb)
  (:export
   #:CONNECT-TO-DATABASE
   #:CONNECT-TO-DATABASES
   #:DISCONNECT-FROM-DATABASE
   #:DISCONNECT-FROM-DATABASES
   #:BREAK-LOCK
   #:ATOMIC
   #:ORELSE
   #:ROLLBACK
   #:ABORT-TRANSACTION
   #:QUERY
   #:COMMITd

   #:get-file-uuid
   #:get-new-oid
   #:when-created
   
   #:persist
   #:deref
   #:ref
   #:mark-dirty
   #:is-persistent
   #:same-ref
   #:make-persistent
   #:delete-persistent-object
   #:replace-persistent-object
   #:discard-persistent-object
   #:oid-for-object
   #:get-persistent-object
   #:make-transaction-viewer

   #:OK-SET
   #:FIND-OK-SET
   #:OK-SET-NAME
   #:SET-COUNT
   #:ADD-TO-SET
   #:REMOVE-FROM-SET
   #:SET-MEMBER-P
   #:MAP-SET
   #:DO-SET

   #:OK-MAP
   #:FIND-OK-MAP
   #:OK-MAP-NAME
   #:MAP-COUNT
   #:GET-MAP
   #:UNMAP
   #:MAP-MAP
   #:FIRST-KEY
   #:LAST-KEY

   #:ok-table
   #:find-ok-table
   #:ok-table-name
   #:table-count
   #:get-column-names
   #:get-key-column-name
   #:insert-row
   #:delete-row
   #:find-row
   #:find-first-row
   #:find-last-row
   #:map-rows
   #:create-cursor
   #:bulk-load-table
   #:make-column

   #:persistent-object
   #:GET-PERSISTENT-CLASS
   #:FIND-INSTANCES
   #:FIND-INSTANCES*
   #:MAP-INSTANCES
   #:MAP-INSTANCES*
   #:FIRST-INSTANCE-FOR-SLOT
   #:LAST-INSTANCE-FOR-SLOT
   #:FIND-INSTANCES-FOR-SLOT
   #:MAP-INSTANCES-FOR-SLOT
   #:MAKE-CLASS-CURSOR
   #:CURSOR-NEXT
   #:CURSOR-PREVIOUS
   #:BASIC-SLOT-VALUE
   ))



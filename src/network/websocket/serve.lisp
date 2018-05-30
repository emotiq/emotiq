(in-package :websocket/wallet)

(defun note (message-or-format &rest args)
  "Emit a note of progress to the appropiate logging system."
  (let ((formats '(simple-date-time:|yyyymmddThhmmssZ|
                   simple-date-time:|yyyy-mm-dd hh:mm:ss|)))
    (format *error-output* 
            "~&~a ~a~&"
            (apply (second formats)
                   (list (simple-date-time:now)))
            (apply 'format 
                 nil
                 message-or-format
                 (if args args nil)))))

(defclass wallet-server (hunchensocket:websocket-resource)
  ((path :initarg :path :reader path))
  (:default-initargs :client-class 'wallet-client))

(defclass wallet-client (hunchensocket:websocket-client)
  ())

(defmethod hunchensocket:client-connected ((resource wallet-server) client)
  (declare (ignore client))
  (note "~a has connected." resource))

(defmethod hunchensocket:client-disconnected ((resource wallet-server) client)
  (declare (ignore client))
  (note "~a has disconnected." resource))

(defun broadcast (resource json)
  (loop :for peer :in (hunchensocket:clients resource)
     :doing (hunchensocket:send-text-message peer json)))

(defun send-as-json (client cl-json-object)
  (let ((json (json:with-explicit-encoder
                (cl-json:encode-json-to-string cl-json-object))))
    (hunchensocket:send-text-message client json)))

         

(defmethod hunchensocket:text-message-received ((resource wallet-server) client message)
  (note "~a-->~a:~&~t~a~&" client resource message)
  (let ((request  (cl-json:decode-json-from-string message)))
    (let ((method (alexandria:assoc-value request :method)))
      (unless method
        (note "No method specified in message: ~a" message)
        (return-from hunchensocket:text-message-received nil))
      (cond
        ((string= method "subscribe")
         (let ((response
                (make-instance 'response
                               :result `(:true)
                               :id (alexandria:assoc-value request :id))))
           ;;; FIXME: locking under load
           ;;; FIXME: only subscribe to consensus events
           (pushnew client *consensus-clients*)
           (unless *consensus-thread*
             (note "Spawning thread to mock consensus replies.")
             (bt:make-thread (lambda () (consensus)))
             (setf *consensus-thread* t))
           (send-as-json client response)))
        ((string= method "unsubscribe")
         (let ((response
                (make-instance 'response
                               :id (alexandria:assoc-value request :id))))
           (if (not (find client *consensus-clients*))
               (setf (slot-value response 'error)
                     "No such subscription.")
               (progn 
           ;;; FIXME: locking under load
                 (setf *consensus-clients* (remove client *consensus-clients*))
                 (setf (slot-value response 'result)
                       '(:true))))
           (send-as-json client response)))
        ((string= method "ping")
         (let ((response 
                (make-instance 'response
                               :id (alexandria:assoc-value request :id)
                               :result "pong")))
           (broadcast resource (json:with-explicit-encoder
                                   (cl-json:encode-json-to-string response)))))
        ((string= method "wallet")
         (let ((response 
                (make-instance 'response
                               :id (alexandria:assoc-value request :id)
                               :result `(:object
                                         (:address . ,(emotiq/wallet:primary-address (emotiq/wallet::wallet-deserialize)))
                                         (:amount . 0)))))
           (broadcast resource (json:with-explicit-encoder
                                   (cl-json:encode-json-to-string response)))))
        ((string= method "recovery-phrase")
         (let ((response
                (make-instance 'response
                               :id (alexandria:assoc-value request :id)
                               :result `(:object
                                         (:address . ,(emotiq/wallet:primary-address (emotiq/wallet::wallet-deserialize)))
                                         (:keyphrase . (:list ,@(emotiq/wallet::key-phrase (emotiq/wallet::wallet-deserialize))))))))
           (broadcast resource (json:with-explicit-encoder
                                   (cl-json:encode-json-to-string response)))))
        ((string= method "enumerate-wallets")
         (let ((response
                (make-instance 'response
                               :id (alexandria:assoc-value request :id)
                               :result (emotiq/wallet:enumerate-wallets))))
           (broadcast resource (cl-json:encode-json-to-string response))))
        ((string= method "transactions")
         (let ((response
                (make-instance 'response
                               :id (alexandria:assoc-value request :id)
                               :result (transactions))))
           (broadcast resource (json:with-explicit-encoder
                                 (cl-json:encode-json-to-string response)))))))))

(defvar *handlers* nil)

(defun handler (request)
  (let ((path (hunchentoot:script-name request)))
    (cond 
      ((string= path "/wallet")
       (let ((instance (make-instance 'wallet-server :path path)))
         (push instance *handlers*)
         instance))
      (t 
       (note "Unhandled request for resource path '~a'." path)))))
        
(eval-when (:load-toplevel :execute)
  (pushnew 'handler hunchensocket:*websocket-dispatch-table*))

(defvar *acceptor* nil)

(defun start-server (&key (port 3145))
  (unless *acceptor*
    (setf *acceptor*
          (make-instance 'hunchensocket:websocket-acceptor :port port)))
  (note "Starting websocket server on <ws://localhost:~a>"
        (slot-value *acceptor* 'hunchentoot::port))
  (hunchentoot:start *acceptor*))

(defclass json-rpc ()
  ((jsonrpc
    :initform "2.0")
   (id :initarg :id
       :initform nil)))
   
(defclass request (json-rpc)
  ((method :initarg :method)
   (params :initarg :params)))

(defclass response (json-rpc)
  ((result :initarg :result)
   (error :initarg :error)))


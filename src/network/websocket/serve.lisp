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
  (emotiq:note "~a has connected." resource))

(defmethod hunchensocket:client-disconnected ((resource wallet-server) client)
  (declare (ignore client))
  (emotiq:note "~a has disconnected." resource))

(defun broadcast (resource json)
  (loop :for peer :in (hunchensocket:clients resource)
     :doing (hunchensocket:send-text-message peer json)))

(defun send-as-json (client cl-json-object)
  (let ((json (json:with-explicit-encoder
                (cl-json:encode-json-to-string cl-json-object))))
    (hunchensocket:send-text-message client json)))

(defmethod hunchensocket:text-message-received ((resource wallet-server) client message)
  (emotiq:note "~a connected to ~a:~&~t~a~&" client resource message)
  (let ((request  (cl-json:decode-json-from-string message)))
    (let ((method (alexandria:assoc-value request :method)))
      (unless method
        (emotiq:note "No method specified in message: ~a" message)
        (return-from hunchensocket:text-message-received nil))
      (let ((response
             (make-instance 'response
                            :result `(:true)
                            :id (alexandria:assoc-value request :id))))
        (with-slots (result error)
            response
          (cond
            ((string= method "subscribe")
           ;;; FIXME: locking under load
             (if (find "consensus" (alexandria:assoc-value request :params) :test 'string=)
                 ;; error
                 (setf result nil
                       error `(:object (:code . -32602)
                                       (:message . "No such subscription available.")
                                       (:data . nil)))))
            ((string= method "unsubscribe")
             (if (not (find client *consensus-clients*))
                 (setf result nil
                       error `(:object (:code . -32602)
                                       (:message . "No subscription has been registered.")
                                       (:data . nil)))
                 (progn
                   ;; FIXME: locking under load
                   (setf *consensus-clients* (remove client *consensus-clients*))
                   (setf result '(:true)))))
            ((string= method "ping")
               (setf result "pong"))
            ((string= method "wallet")
             (setf result (model/wallet:get-wallet "My Wallet")))
            ((string= method "recovery-phrase")
             (setf result (model/wallet:recovery-phrase)))
            ((string= method "enumerate-wallets")
             (setf result (model/wallet:enumerate-wallets)))
            ((string= method "transactions")
             (setf result (model/wallet:transactions)))
            (t
             (setf result nil
                   error `(:object (:code . -32601)
                                   (:message . "No such method.")
                                   (:data . nil)))))
          (send-as-json client response))))))

(defvar *handlers* nil)

(defun handler (request)
  (let ((path (hunchentoot:script-name request)))
    (cond 
      ((string= path "/wallet")
       (let ((instance (make-instance 'wallet-server :path path)))
         (push instance *handlers*)
         instance))
      (t 
       (emotiq:note "Unhandled request for resource path '~a'." path)))))
        
(eval-when (:load-toplevel :execute)
  (pushnew 'handler hunchensocket:*websocket-dispatch-table*))

(defvar *acceptor* nil)

(defun start-server (&key (port 3145))
  (unless *acceptor*
    (setf *acceptor*
          (make-instance 'hunchensocket:websocket-acceptor
                         ;; Nb. We set the timeout to an hour large
                         ;; value because it corresponds to two
                         ;; separate values for reads and writes,
                         ;; which means unidirectional subscription
                         ;; messages without corresponding replies
                         ;; will tear down the connection.
                         :websocket-timeout 3600  
                         :port port)))
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


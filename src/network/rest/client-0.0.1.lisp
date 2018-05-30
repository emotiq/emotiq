(restas:define-module :route.client/0/0/1
  (:nicknames :route.client/0/0
              :route.client/0)
  (:use :cl :emotiq-rest))

(in-package :route.client/0/0/1)

(restas:define-route
    %index
    ("/"
     :content-type "text/html")
  (as-html
    (:html
     (:head
      (:script :src (restas:genurl '%client.js)))
     (:body
      (:h1
       "Websocket client interface testing")
      (:p "This page has loaded Javascript stub functions to assist in
testing the WebSocket implementation.  Opening a Javascript console
gives the toplevel definitions of the following functions:")

      (:dl
       (:dt "connect()")
       (:dd "Open the websocket connection.  Call this to initialize the connection or if one gets errors about not being connected.")
       (:dt "subscribe()")
       (:dd "Subscribe to the node consensus message endpoint.")
       (:dt "unsubscribe()")
       (:dd "Unsubscribe to the node consensus message endpoint.")
       (:dt "wallet()")
       (:dd "Return information about the default wallet.")
       (:dt "keyphrase()")
       (:dd "Return the recovery phrase of the default wallet.")
       (:dt "ping()")
       (:dd "Ping service test.")
       (:dt "transactions()")
       (:dd "Return all transactions associated with the default wallet."))))))

(restas:define-route
    %client.js
    ("/client.js"
     :content-type "application/javascript")
  (ps:ps

    (defvar *ws* nil)

    (defvar *wallet-uri* "ws://localhost:3145/wallet")

    (defun connect ()
      (let ((ws (ps:new (|WebSocket| *wallet-uri*))))
        (unless ws
          (console.log "Failed to make connection to %s" *wallet-uri*))
        (setf (ps:getprop ws 'onmessage)
              (lambda (event)
                (console.log (ps:@ event data))))
        (setf *ws* ws)))

    (defun send (rpc-message)
      (unless (and *ws*
                   (equal (ps:@ *ws* |readyState|)
                          (ps:getprop *ws* '*open*)))
        (connect))
      (let ((json (ps:chain *json* (stringify rpc-message))))
        (ps:chain *ws* (send json))))

    (defvar *request-id-counter* 0)

    (defun form-rpc-request (method params)
      (ps:create
       :jsonrpc "2.0"
       :method method
       :params params
       :id (ps:incf *request-id-counter*)))

    (defun enumerate-wallets ()
      (let ((message (form-rpc-request "enumerate-wallets" nil)))
        (send message)))

    (defun subscribe ()
      (let ((message (form-rpc-request "subscribe" '("consensus"))))
        (send message)))

    (defun unsubscribe ()
      (let ((message (form-rpc-request "unsubscribe" '("consensus"))))
        (send message)))

    (defun wallet ()
      (let ((message (form-rpc-request "wallet" nil)))
        (send message)))

    (defun keyphrase ()
      (let ((message (form-rpc-request "recovery-phrase" nil)))
        (send message)))

    (defun ping ()
      (let ((message (form-rpc-request "ping" nil)))
        (send message)))

    (defun transactions ()
      (let ((message (form-rpc-request "transactions" nil)))
        (send message)))))














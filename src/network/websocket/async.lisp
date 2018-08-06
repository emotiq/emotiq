(in-package :websocket/wallet)

(defvar *consensus-thread* nil)
(defvar *consensus-clients*  nil)

(defun add-client (client)
  "Add a new websocket CLIENT as a subscriber to the 'consensus' service

Returns the alist for for the response to return to the requesting client."

  (pushnew client *consensus-clients*)
  (unless *consensus-thread*
    (emotiq:note "Spawning thread to mock consensus replies.")
    (bt:make-thread (lambda () (model/wallet:mock #'notify-clients)))
    (setf *consensus-thread* t))
  `(:true))
    

(defun remove-client (client)
  "Removes the websocket CLIENT from the 'consensus' service"
  (emotiq:note "Removing client ~a from client connections." client)
  (setf *consensus-clients* (remove client *consensus-clients*)))

(defun notify-clients (notification)
  "Our message delivery hook for NOTIFICATION events to all subscribed clients to the consensus channel"
  (dolist (client *consensus-clients*)
    (with-slots ((state hunchensocket::state)
                 (write-lock hunchensocket::write-lock))
        client
      (if (not (and (eq state :connected)
                    write-lock))
          (remove-client client)
          (let ((message `(:object .
                                   ((:jsonrpc . "2.0")
                                    (:method . "consensus")
                                    (:params . ,notification)))))
            #+(or)
            (emotiq:note "Notifying client ~a with ~a~&" client message)
            (send-as-json client message))))))






(in-package :websocket/wallet)

(defvar *consensus-thread* nil)
(defvar *consensus-clients*  nil)

(defun add-client (client response)
  "Add a new websocket CLIENT as a subscriber to the 'consensus' service

Mutates RESPONSE to be an affirmative response to subscribing."
  (pushnew client *consensus-clients*)
  (unless *consensus-thread*
    (emotiq:note "Spawning thread to mock consensus replies.")
    (bt:make-thread (lambda () (model/wallet:mock #'notify-clients)))
    (setf *consensus-thread* t))
  (setf response `(:true)))

(defun remove-client (client)
  "Removes the websocket CLIENT from the 'consensus' service"
  (emotiq:note "Removing client ~a from client connections." client)
  (setf *consensus-clients* (remove client *consensus-clients*)))

(defun notify-clients (notification)
  "Our message delivery hook for NOTIFICATION events to all subscribed clients"
  (dolist (client *consensus-clients*)
    (with-slots ((state hunchensocket::state)
                 (write-lock hunchensocket::write-lock))
        client
      (if (not (and (eq state :connected)
                    write-lock))
          (remove-client client)
          (progn
            #+(or)
            (note "~&Notifying client ~a with ~a~&"
                  client
                  notification)
            (send-as-json client notification))))))




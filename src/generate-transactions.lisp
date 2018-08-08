(in-package :emotiq/app)


;;;;;;;;;;;;;;;;;;
; random transaction generator
;;;;;;;;;;;;;;;;;;

;; for lack of better graphics, let's use pipe syntax to show what I am building
;;
;; ticker | timer (randomizer) | transaction-creator
;; 
;; the pipe syntax cannot show that "ticker" is sending itself a message, endlessly
;;
;; maybe (loop ticker | ticker |2) | timer | transaction-creator
;;
;; these are honest message sends between concurrent pieces of code
;; the loopback to ticker is not recursion, it is a message send
;; 


(defparameter *all-accounts* nil
  "list of accounts which will be re-used whenever one of the from/to lists dry up, monitored")

(defparameter *from* nil
  "list of accounts which will send tokens from to *to*, monitored")

(defparameter *to* nil
  "list of accounts which will accept tokens, monitored")

;; for debug - hooks to the actors
(defparameter *ticker-actor* nil
  "wakes up periodically and sends 'random' events to create-transaction actor")

(defparameter *randomizer-actor* nil
  "wakes up periodically and sends 'random' events to create-transaction actor")

(defparameter *transaction-creator-actor* nil
  "creates a transaction using *from* and *to* lists, whenever pinged")

(useful-macros:defmonitor
    ((ensure-lists ()
       ;; helper that checks and resets (if necessary) the from and to lists
       ;; should be done atomically, hence in a (Hoare) monitor
       (when (or (null *from*)
                 (null *to*))
         (setf *from* *all-accounts*
               *to* (reverse *all-accounts*))))
  
     (get-from ()
       ;; return an account from the *from* list and pop
       (ensure-lists)
       (pop *from*))

     (get-to ()
       ;; return an account from the *to* list and pop
       (ensure-lists)
       (pop *to*))

     ;; end monitor
     ))

(defun generate-pseudo-random-transactions ()
  "spawn an actor that wakes up and spends tokens from some account in *from* to another account in *to*"
  (let ((keypairs (emotiq/config:get-keypairs))
        (nameindex -1))
    (let ((account-list (mapcar #'(lambda (pair)
                                    (make-account-given-keypairs
                                     (format nil "account~A" (incf nameindex))
                                     (first pair)
                                     (second pair)))
                                keypairs)))
      ;; some (any) kind of randomization of accounts
      ;; for now, run *front* from the beginning of *all-accounts* 
      ;; and *to* backwards
      ;; when one of them peters out, reset the lists
      (setf *all-accounts* account-list)
      (ensure-lists)

      ;; in general, using actors here is over-kill
      ;; actors are just an efficient way to invoke the process paradigm
      ;; processes/threads would be suffiecient here, since transactions only need
      ;; to be created in "human time" ; I'm using actors only because they are easily
      ;; at hand, for me

      (let ((tick-period 1) ;; bald guess at something that resembles "human time" and will allow a test set of transactions to propagate
            (node cosi-simgen::*my-node*))
        (setf *transaction-creator-actor*
              (actors:make-actor
               #'(lambda (&rest msg)
                   (transaction-creator
                    (first msg)
                    node
                    msg))))
        (setf *randomizer-actor*
              (actors:make-actor
               #'(lambda (&rest msg)
                   (random-timer-actor
                    *transaction-creator-actor*
                    (first msg)
                    node
                    msg))))
        (setf *ticker-actor* (actors:make-actor #'forever-ticker))
        (actors:send *transaction-creator-actor* :create-transaction node)
        #+nil(actors:send *ticker-actor* :again *ticker-actor* *rnadomizer-actor* tick-period node)
        ))))

;; actor body
(defun forever-ticker (&rest msg)
  (destructuring-bind (msg-symbol self randomizer sleep-amount node)
      msg
    (emotiq:note "ticker ~s ~s ~s ~s ~s" msg-symbol self randomizer sleep-amount node)
    
    ;; timers and errors (catch/throw stuff) are nothing special, they are just "events"
    
    ;; this actor gets a periodic tick (using existing actor timer code)
    ;; then sends a message to the randomizer, forever ;; this is over-kill, but, I hope
    ;; it is very obvious
    
    ;; ignore the message, just send a tick to the randomizer
    
    (sleep sleep-amount)
    (emotiq:note "sending to randomizer")
    #+nil(actors:send randomizer :tick node)
    (emotiq:note "sending to self/forever-ticker")
    #+nil(actors:send self :again self randomizer sleep-amount node)))

(defun get-fire-p (n)
  "return T if RANDOM(1/n) is 25% of n or less"
  ;; repeatable random numbers are created with RANDOM, after calling emotiq/random:init-random
  (multiple-value-bind (num dem)
      (random n)
    (declare (ignore num))
    (truncate n 4)
    (zerop dem)))

;; actor body
(defun random-timer-actor (transaction-maker &rest msg)
  "after a pseudo-random number of ticks, wake the transaction-creator up"

  (let ((msg-symbol (first msg))
        (node (second msg)))
    
    ;; should not receive anything but :tick messages,
    ;; messages are always lists, and, by convention, the first token in a message is a keyword
    ;; dlambda is just a convenience
    (assert (eq :tick msg-symbol))
    
    (let ((fire-p t)) ; (get-fire-p 50)))
      (emotiq:note "timer fire-p ~s" fire-p)
      (when fire-p
        (actors:send transaction-maker :create-transaction node)))))
  
(defun create-an-amount-lower-or-equal-to (n)
  "apply some heuristic to generate an amount which we can use in a randomly-created transaction"
  n)

;; actor body
(defun transaction-creator (&rest msg)

  (let ((msg-symbol (first msg))
        (node (second msg)))
    
    (emotiq:note "transaction-creator ~A ~A" msg-symbol node)

    (assert (eq msg-symbol :create-transaction))

    (let ((to-account (get-to))
          (from-account (get-from))
          (from-bal 0)
          (max-froms 16) ;; a bald guess as to the number of from-accounts we will search before giving up
          (some-lower-limit 100) ;; bald guess at what kind of (random) amount we want to use
          (fee 10))

      ;; search for an account with sufficient balance to handle the amount + fees
      (dotimes (i max-froms)
        (let ((bal (get-balance from-account)))
          (cond ((>= bal (+ fee some-lower-limit))
                 (setf from-bal bal)
                 (return))
                ((< bal some-lower-limit)
                 (emotiq:note "account ~A balance is ~A" from-account bal)
                 (setf from-account (get-from))))))
      ;; loop ends with bal set to something >= (+ fee some-lower-limit), or,
      ;; it terminates with bal still = 0
      
      (if (< from-bal some-lower-limit)
          (emotiq:note "can't create a transaction, since ~A accounts do not have suffient funds (~A)"
                       max-froms (+ fee some-lower-limit))
        (let ((new-amount (create-an-amount-lower-or-equal-to some-lower-limit)))
          (emotiq:note "transaction creator making a transaction of ~A from ~A to ~A with fee ~A"
                       new-amount
                       from-account
                       to-account
                       fee)
          (spend from-account (account-pkey to-account) new-amount :fee fee))))))

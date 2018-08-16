(in-package :emotiq/app)


;;;;;;;;;;;;;;;;;;
; random transaction generator
;;;;;;;;;;;;;;;;;;

;; for lack of better graphics, let's use pipe syntax to show what I am building
;;
;; ticker | randomizer | transaction-creator
;;
;; the pipe syntax cannot show that "ticker" is sending itself a message, endlessly
;;
;; maybe (loop (ticker | ticker |2)) 2| randomizer | transaction-creator
;;
;; these are honest message sends between concurrent pieces of code
;; the loopback to ticker is not recursion, it is a message send (important point to understand)
;; smalltalk talks about using message-sends, but it actually uses call-return
;;

;; I've "unrolled" most of the code, in the hopes of making it simpler to understand.
;; It might be useful to delete the comments, before reading.


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

(defparameter *debug-counter-actor* nil
  "counts the number of time it has been sent a message (kind of a poor-mans oscilloscope)")

(defparameter *counter* 0
  "accessed only by *debug-counter-actor*")

(defun log-info (msg &key (subsystem :emotiq/app) (level :info) data)
  (emotiq:s-note msg :subsystem subsystem :level level :data data)
)

(defun log-debug (msg &key (subsystem :emotiq/app) (level :debug) data)
  (emotiq:s-note msg :subsystem subsystem :level level :data data)
)

(useful-macros:defmonitor

    ;; this is probably over-kill - it's a monitor for threads, not actors (i.e. much too coarse-grained)
    ;; TODO: implement monitors for actors, or just move these routines out of the monitor, or, think
    ;; about the semantics of the Actors lib and maybe discover that this monitor is not needed at all

    ;; get it working first, then optimize it away ...

    ((ensure-lists ()
       ;; helper that checks and resets (if necessary) the from and to lists
       ;; should be done atomically, hence, it's in a (Hoare) monitor
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

(defun generate-pseudo-random-transactions (most-tokens-account)
  "spawn an actor that wakes up and spends tokens from some account in *from* to another account in *to*"
  ;; in this testing code, one account has most of the tokens (*alice* in app,lisp)
  ;; spend some (a lot) of the tokens into the accounts used below
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

      ;; this line can be deleted when run in non-test environment
      ;; (i.e. when accounts in account-list already have tokens,
      ;;  in this test environment, all of the tokens have been spent
      ;;  to another account "most-tokens-account" in app.lisp)
      (setup-accounts most-tokens-account)

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
                   (randomizer-actor
                    *transaction-creator-actor*
                    (first msg)
                    node
                    msg))))

        (setf *ticker-actor* (actors:make-actor #'forever-ticker))

        (setf *debug-counter-actor* (actors:make-actor #'debug-counter))

        ;; testing - actors can be tested one at a time
        ;; in this case "test" means a "sanity check" before tying everything together
        ;; we might be able to roll some of these tests into lisp-unit for regression testing (with some thought)

        ;; (this is akin to what lispers do by testing functions in a REPL, but actors (threads) provide
        ;; encapsulation of control-flow which no call-return language (incl. lisp, oop, fp, etc.) can provide (because
        ;; of call-return semantics (incl. RPC))

        ;; test 1 - can the *transaction-creator-actor* create a single transaction?
        ; (actors:send *transaction-creator-actor* :create-transaction node)
        ;;  passed

        ;; test 2 - does the ticker create ticks ad infinitum?  It should create an emotiq:note every tick-amount seconds

        ; (actors:send *ticker-actor* :again *ticker-actor* *debug-counter-actor* tick-period node)

        ;; note that we are substituting *debug-counter-actor* where *randomizer-actor* would go - they are "pin compatible",
        ;; hence, we can use *counter-actor* like a probe, to see if *ticker-actor* is working
        ;;  passed - over 1700 ticks and nothing blew up

        ;; test 3 - the smoke test, attach the ticker to the randomizer attached to the transaction-creator
        ;; does the randomizer look to be sufficiently random???  Do we see the correct # of transactions in the blocks
        ;; (the # of txns should constantly grow)
        ;; ideally, the randomizer should have an output pin that we could simply monitor with a probe, but I'm itching to see
        ;; this work
        ;; N.B. we created the debug-counter, but aren't sending it any messages - it uses up a small amount of memory
        ;; and is just a no-op
        (actors:send *ticker-actor* :again *ticker-actor* *randomizer-actor* tick-period node)))))


(defun debug-counter (&rest msg)
  "on every message received, incs the private variable *counter* and creates an emotiq:note"
  (declare (ignore msg))
  (incf *counter*)
  #-lispworks(emotiq:s-note "Debug counter" :level :debug :subsystem :app :data '(:counter *counter*))
  #+lispworks(emotiq:s-note "Debug counter" :level :debug :subsystem :app :data `(:counter *counter* :room ,(system:room-values))))

;;;;;;;;;;;;;;
;; actor body
;;;;;;;;;;;;;;
(defun forever-ticker (&rest msg)
  (destructuring-bind (msg-symbol self randomizer sleep-amount node)
      msg

    ;; timers and errors (catch/throw stuff) are nothing special, they are just mere messages

    ;; this actor gets a periodic tick (using existing actor timer code)
    ;; then sends a message to the randomizer, forever ;; this is over-kill, but, I hope
    ;; it is very obvious

    (sleep sleep-amount)

    (assert (eq :again msg-symbol))

    (actors:send randomizer :tick node)

    ;; wake self up again by sending a message to self with the sleep-amount in the message
    (actors:send self msg-symbol self randomizer sleep-amount node)))


;;;;;;;;;;;;;;
;; actor body
;;;;;;;;;;;;;;

(defparameter *nticks* 0
  "private variable used by random-timer-actor")

(defun randomizer-actor (transaction-maker &rest msg)
  "after a pseudo-random number of ticks, wake the transaction-creator up"

  (let ((msg-symbol (first msg))
        (node (second msg)))

    ;; should not receive anything but :tick messages,
    ;; messages are always lists, and, by convention, the first token in a message is a keyword
    ;; dlambda is just a convenience
    (assert (eq :tick msg-symbol))

    (decf *nticks*)

    (when (<= *nticks* 0)
      (setf *nticks* (random 50)) ;; 50 is a just pulled out of thin air (might need tuning)
      (log-info "nticks reset to" :data `(:nticks ,*nticks*))
      (actors:send transaction-maker :create-transaction node))))


;;;;;;;;;;;;;;
;; actor body
;;;;;;;;;;;;;;

(defun create-an-amount-lower-or-equal-to (n)
  "apply some heuristic to generate an amount which we can use in a randomly-created transaction"
  n)

(defun transaction-creator (&rest msg)

  (let ((msg-symbol (first msg))
        (node (second msg)))

    (log-info "transaction-creator" :data `(:msg-symbol ,msg-symbol :node ,node))

    (assert (eq msg-symbol :create-transaction))

    (let ((to-account (get-to))
          (from-account (get-from))
          (from-bal 0)
          (max-froms 16) ;; a bald guess as to the number of from-accounts we will search before giving up
          (some-lower-limit 15) ;; bald guess at what kind of (random) amount we want to use
          (fee 10))

      ;; search for an account with sufficient balance to handle the amount + fees
      (dotimes (i max-froms)
        (let ((bal (get-balance from-account)))
          (cond ((>= bal (+ fee some-lower-limit))
                 (setf from-bal bal)
                 (return))
                ((< bal some-lower-limit)
                 (log-info "Account balance" :data `(:account ,from-account :balance ,bal))
                 (setf from-account (get-from))))))
      ;; loop ends with bal set to something >= (+ fee some-lower-limit), or,
      ;; it terminates with bal still = 0

      (if (< from-bal some-lower-limit)
          (log-info "can't create a transaction, since accounts do not have sufficient funds."
                       :data `(:accounts ,max-froms :funds ,(+ fee some-lower-limit)))
        (let ((new-amount (create-an-amount-lower-or-equal-to some-lower-limit)))
          (log-info "transaction creator making a transaction with fee"
                :data `(:amount ,new-amount
                        :from-account ,from-account
                        :to-account ,to-account
                        :fee fee))
          (spend from-account to-account new-amount :fee fee))))))


(defun setup-accounts (most-tokens-account)
  "side effect: spend some tokens into the test accounts from most-tokens-account"
  (let ((n (length *all-accounts*))
        (big-amount (get-balance most-tokens-account))
        (fee 10)
        (fudge-factor 20))
    (let ((per-account-amount (- (/ big-amount n)
                                 (* n (+ fee fudge-factor)))))
      (mapc #'(lambda (account-under-test)
                (spend most-tokens-account
                       account-under-test
                       per-account-amount
                       :fee fee))
            *all-accounts*)
      (sleep 20)))) ;; wait for transactions to propagate - is this necessary?

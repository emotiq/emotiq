;; packages.lisp - Actors packages
;;
;; DM/RAL  12/17
;; -------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#


(in-package :CL-USER)

#-lispworks
(defpackage :ansi-timer
  (:use :common-lisp)
  (:export
   :timer
   :make-timer
   :schedule-timer
   :schedule-timer-relative
   :unschedule-timer))

(defpackage #:actors
  (:use #:common-lisp)
  (:nicknames #:ac)
  (:import-from :mpcompat
   :make-lock
   :with-lock
   :CAS)
  (:import-from #:priq
   #:prio-mailbox
   #:make-prio-mailbox
   #:mailbox-read
   #:mailbox-send
   #:mailbox-empty-p
   #:make-unsafe-fifo
   #:make-priq
   #:make-fifo
   #:addq
   #:popq
   #:emptyq-p
   #:contents)
  (:import-from #:trivia
   #:match
   #:ematch
   #:guard
   #:lambda-match
   #:lambda-ematch)
  (:import-from #:useful-macros
   #:curry
   #:rcurry
   #:if-let
   #:when-let
   #:foreach
   #:nlet-tail
   #:dlambda
   #:dcase
   #:defmonitor
   #:critical-section
   #:capture-ans-or-exn
   #:recover-ans-or-exn
   #:<shared-plist>
   #:get-kv
   #:symb)

  #+:LISPWORKS
  (:import-from :mp
   :make-timer
   :schedule-timer-relative
   :unschedule-timer)
  #-:lispworks
  (:import-from :ansi-timer
   :make-timer
   :schedule-timer-relative
   :unschedule-timer)
  (:export
   #:install-actor-system
   #:*nbr-execs*
   #:actor
   #:make-actor
   #:send
   #:invalid-send-target
   #:ask
   #:aska
   #:current-actor
   #:spawn
   #:get-property
   #:set-property
   #:suspend
   #:become
   #:dispatch-message
   #:self-call
   #:register-actor
   #:unregister-actor
   #:pr
   #:find-actor
   #:get-actors
   #:recv
   #:=cont
   #:terminate-actor
   #:without-actor-status
   #:make-proxy

   #:=lambda
   #:=defun
   #:=bind
   #:=bind-callback
   #:=values
   #:=funcall
   #:=apply
   #:with-cont
   #:with-future
   #:smapcar
   #:smapc
   #:pmapcar
   #:pmapc
   #:par
   #:with-futures
   #:pfirst
   #:par-first
   #:with-first-future

   #:set-executive-pool
   #:with-borrowed-mailbox
   #:do-nothing

   #:install-actor-system

   ))

(defpackage #:linda
  (:use #:common-lisp #:ac)
  (:import-from #:um
   #:if-let
   #:when-let
   #:foreach
   #:nlet
   #:nlet-tail
   #:group
   #:dlambda
   #:curry
   #:rcurry
   #:defmacro!
   #:accum)
  (:export
   #:*linda*
   #:make-ts
   #:out
   #:rd
   #:in
   #:rdp
   #:inp
   #:outb
   #:rdb
   #:rdbp
   #:remove-bindings
   #:on-in
   #:on-inp
   #:on-rd
   #:on-rdp
   #:on-rdb
   #:on-rdbp
   #:remove-all-bindings
   #:remove-all-tuples
   #:reset
   #:srdp
   #:sinp
   #:srdbp
   #:remove-tuples
   #:remote-srdp
   #:remote-sinp
   #:remote-out
   #:remote-outb
   #:remote-srdbp
   #:remote-remove-bindings
   ))


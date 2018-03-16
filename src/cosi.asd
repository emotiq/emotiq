;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "cosi"
    :description "github.com/emotiq/research/research.asd"
    :version "0.2.0"
    :author "Copyright (c) 2018 Emotiq AG"
    :licence "<https://opensource.org/licenses/MIT>"
    :depends-on (cosi-bls
                 cosi-schnorr)
    :in-order-to ((test-op (test-op "cosi-bls-test"))))

(defsystem "mvp-tests"
  :version "0.0.1"
  :description "Emotiq"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (prove
               emotiq/startup
               emotiq/config)
  :defsystem-depends-on (prove-asdf) ;;; FIXME
  :perform (test-op (o s)
                    (symbol-call :prove :run s))
  :components ((:module tests :pathname "./"
                        :components ((:test-file "mvp")))))




#|
MIT License Terms:

Copyright (c) 2017, Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
|#

(asdf:defsystem "actors"
  :description "Actors: Green threads strewn across an OS thread pool"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2017 by Refined Audiometrics Laboratory, LLC. MIT License terms apply."
  :components  ((:file "packages")
                #-:lispworks
                (:file "ansi-timer")
                (:file "actors")
                ;; (:file "actors-mb")
                (:file "actors-machines")
                (:file "actors-startup")
                #+(AND :COM.RAL :LISPWORKS) (:file "futures")
                #+(AND :COM.RAL :LISPWORKS) (:file "linda-tuples")
                #|
                (:file "actors-macros")
                (:file "actors-globals")
                (:file "actors")
                (:file "actors-mbox-queue")
                (:file "actors-comms")
                (:file "actors-executive")
                ;; (:file "actors-user-pandoric")
                (:file "actors-user-clos")
                (:file "actors-startup")
                (:file "actors-data-structs")
                (:file "actors-components")
                (:file "actors-machines")
                (:file "futures")
                |#)
  :SERIAL T
  :depends-on   ("data-objects"
                 "trivia"
		 "mpcompat"
                 ))


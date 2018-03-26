# this has been test only on 32-bit LWM
# Cocoa adds complications (see Delivery manual).  This binary appears not to need Cocoa, so things are simple.
# basically, this script calls LWM with two arguments '-build' and 'deliver.lisp'
# Change the pathname appropriately for 64-bit LWM

'/Applications/LispWorks 7.0 (32-bit)/LispWorks (32-bit).app/Contents/MacOS/lispworks-7-0-0-x86-darwin' -build deliver.lisp


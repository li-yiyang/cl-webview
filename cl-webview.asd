(defsystem #:cl-webview
  :author ("凉凉")
  :version "0"
  :licence "MIT"
  :description
  "CL-WEBVIEW is a Common Lisp binding to Webview,
a tiny tiny cross-platform webview library to build
modern cross-platform GUIs. "
  :defsystem-depends-on (:cffi/c2ffi)
  :depends-on (:cffi :shasht)
  :serial t
  :components
  ((:file "package")
   (:file "preload")
   (:module lib
    :pathname "lib")
   (:module spec
    :pathname "spec"
    :components ((:cffi/c2ffi-file "webview.h"
                  :package #:cl-webview.lib
                  :ffi-name-transformer "cl-webview.preload::ffi-name-transformer"
                  :foreign-library-name "cl-webview.lib::webview"
                  :foreign-library-spec ((:darwin "libwebview.dylib")
                                         (:unix   "libwebview.so")
                                         (t (:default "libwebview"))))))
   (:file "wrapper")))

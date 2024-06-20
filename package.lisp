(uiop:define-package #:cl-webview.preload (:use :cl))

(uiop:define-package #:cl-webview.lib
  (:use)
  (:export
   #:webview-error-missing-dependency
   #:webview-error-error-canceled
   #:webview-error-invalid-state
   #:webview-error-invalid-argument
   #:webview-error-unspecified
   #:webview-error-ok
   #:webview-error-duplicate
   #:webview-error-not-found

   #:webview-version))

(defpackage #:cl-webview
  (:use :cl :cl-webview.lib)
  (:export
   ;; Globally Parameters
   #:*default-webview-title*
   #:*default-webview-width*
   #:*default-webview-height*
   
   ;; High level wrapper
   #:def-bind-callback
   #:def-dispatch-callback
   
   #:make-webview
   #:with-webview
   
   ;; Low-level wrapper
   #:ensure-main-webview-window
   #:quit-main-webview-window

   #:webview-create
   #:webview-set-size
   #:webview-bind-fn
   #:webview-bind
   #:webview-dispatch-fn
   #:webview-dispatch

   #:webview-set-title
   #:webview-navigate
   #:webview-set-html
   #:webview-init
   #:webview-eval
   #:webview-unbind
   
   #:webview-terminate
   #:webview-destroy

   ;; direct from cl-webview.lib
   #:webview-error-missing-dependency
   #:webview-error-error-canceled
   #:webview-error-invalid-state
   #:webview-error-invalid-argument
   #:webview-error-unspecified
   #:webview-error-ok
   #:webview-error-duplicate
   #:webview-error-not-found
   
   #:webview-set-title
   #:webview-navigate
   #:webview-set-html
   #:webview-init
   #:webview-eval
   #:webview-version
   )
  )


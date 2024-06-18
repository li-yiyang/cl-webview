(uiop:define-package #:cl-webview.preload (:use :cl))

(uiop:define-package #:cl-webview.lib
  (:use)
  (:export
   #:webview-terminate
   #:webview-set-title
   #:webview-navigate
   #:webview-set-html
   #:webview-init
   #:webview-eval
   #:webview-version))

(defpackage #:cl-webview
  (:use :cl :cl-webview.lib)
  (:export
   ;; Globally Parameters
   #:*default-webview-title*
   #:*default-webview-width*
   #:*default-webview-height*
   
   ;; High level wrapper
   #:with-webview
   
   ;; Low-level wrapper
   #:webview-create
   #:webview-set-size
   #:webview-bind-fn
   #:webview-bind
   
   ;; direct from cl-webview.lib
   #:webview-terminate
   #:webview-set-title
   #:webview-navigate
   #:webview-set-html
   #:webview-init
   #:webview-eval
   #:webview-version
   )
  )


(in-package :cl-webview)

(defparameter *default-window-width* 400
  "The default webview window width. ")

(defparameter *default-window-height* 400
  "The default webview window height. ")

(defparameter *default-window-title* "CL-WEBVIEW"
  "The default webview window title.")

(define-condition webview-terminate (condition) ())

(defun webview-create (&key debug
                         (native-window nil window?))
  "Creates a new webview instance.
Return `cl-webview::webview-t' of NULL for failure. 

Enable developer tools if `debug'.

`native-window' should be a optional native window handle pointer,
i.e. GtkWindow pointer, NSWindow pointer (Cocoa) or HWND (Win32).
By default `native-window' will be a NULL pointer, creating a
new window and both the window and application lifecycle are
managed by the webview instance, if non-NULL, the webview widget is
embedded into the given window, and the caller is expected to assume
responsibility for the window as well as application lifecycle.
"
  (let* ((debug   (if debug 1 0))
         (window  (if (and window? (cffi:pointerp native-window))
                      native-window
                      (cffi:null-pointer)))
         (webview (cl-webview.lib::webview-create debug window)))
    (when (cffi:null-pointer-p webview)
      (error
       "Creation fail (Runtime dependencies missing / window creation fails). "))    
    webview))

(defun webview-set-size (webview width height
                         &optional (hints :none))
  "Set webview `webview' window size with `width' and `height'.

The webview hints could be:
+ `:none'  width and height are default size (default)
+ `:min'   width and height are minimum bounds
+ `:max'   width and height are maximum bounds
+ `:fixed' width and height cannot changed by user
"
  (let ((hints (case hints
                 ((:none :webview-hint-none)
                  cl-webview.lib::webview-hint-none)
                 ((:min  :webview-hint-min)
                  cl-webview.lib::webview-hint-min)
                 ((:max  :webview-hint-max)
                  cl-webview.lib::webview-hint-max)
                 ((:fixed :webview-hint-fixed)
                  cl-webview.lib::webview-hint-fixed)
                 (otherwise
                  cl-webview.lib::webview-hint-none))))
    (cl-webview.lib::webview-set-size webview width height hints)))

(defmacro run-in-main-thread (&body body)
  "Run the body in main thread (if needed). "
  ;; In macOS, an app with GUI should have its UI thread as main thread.
  #+sbcl `(sb-thread:interrupt-thread
           (sb-thread:main-thread)
           (lambda ()
             (sb-int:with-float-traps-masked
                 (:underflow :overflow :inexact :invalid :divide-by-zero)
               ,@body)))
  #-sbcl `(progn ,@body))

(defmacro with-webview ((webview
                         &key (title *default-window-title*)
                           (width  *default-window-width*)
                           (height *default-window-height*)
                           (debug nil)
                           (size-hint :none))
                        &body body)
  "Create a webview instance and ensure it to be destroied after use.

Example:

    (with-webview (webview :title \"Hello\")
      (webview-set-html webview \"<h1>CL-WEBVIEW</h1>\"))
"
  `(run-in-main-thread
     (let ((,webview (webview-create :debug ,debug)))
       (webview-set-size  ,webview ,width ,height ,size-hint)
       (cl-webview.lib::webview-set-title ,webview ,title)
       (cl-webview.lib::webview-set-html  ,webview "")
       ,@body
       (block webview
         (handler-bind ((webview-terminate
                          (lambda (c)
                            (declare (ignore c))
                            (cl-webview.lib::webview-destroy ,webview)
                            (return-from webview))))
           (cl-webview.lib::webview-run       ,webview))
         (cl-webview.lib::webview-destroy   ,webview)))))

(defun webview-bind-fn (webview name fn)
  "Binds a function to a new global JavaScript function.

The `name' should be a string for toplevel JS function."
  (cffi:defcallback webview-bind-fn :void
      ((id :string) (req :string) (arg :pointer))
    (declare (ignore arg))
    (let ((status 1) result)
      (restart-case
          (let ((params (shasht:read-json req)))
            (setf result (apply fn (map 'list #'identity params))
                  status 0))
        (continue ()
          :report "Ignore error and return to main loop. "
          (setf status 1))
        (terminate ()
          :report "Terminate the Webview application. "
          (cl-webview.lib::webview-terminate webview)
          (signal 'webview-terminate)))
      (cl-webview.lib::webview-return
       webview id status (shasht:write-json result nil))))
  (cl-webview.lib::webview-bind
   webview name (cffi:callback webview-bind-fn) (cffi:null-pointer)))

(defmacro webview-bind (webview name lambda-list &body body)
  "Binds the function to webview, see `webview-bind-fn'. "
  `(webview-bind-fn ,webview ,name (lambda ,lambda-list ,@body)))

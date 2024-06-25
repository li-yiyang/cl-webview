(in-package :cl-webview)

(defparameter *default-window-width* 400
  "The default webview window width. ")

(defparameter *default-window-height* 400
  "The default webview window height. ")

(defparameter *default-window-title* "CL-WEBVIEW"
  "The default webview window title. ")

(defparameter *main-webview-window* nil
  "The main webview window. ")

(defparameter *main-webview-window-lock* (bt:make-lock)
  "The process lock of the main webview.
This is the lock on `*main-webview-window-live?*'. ")

(defparameter *main-webview-window-live?* nil
  "The status of main webview window. ")

;; ========== Webview Error Condition ==========

(define-condition webview-error (condition) ())

(define-condition webview-error-missing-dependency (webview-error) ())
(define-condition webview-error-error-canceled     (webview-error) ())
(define-condition webview-error-invalid-state      (webview-error) ())
(define-condition webview-error-invalid-argument   (webview-error) ())
(define-condition webview-error-unspecified        (webview-error) ())
(define-condition webview-error-ok                 (webview-error) ())
(define-condition webview-error-duplicate          (webview-error) ())
(define-condition webview-error-not-found          (webview-error) ())

;; ========== Low Level Wrapper ==========
;; These bindings are just simple wrap on cl-webview.lib functions.

;; ========== webview-create ==========

(defun webview-create (&key debug
                         (native-window nil window?))
  "Creates a new webview instance (use `make-webview' instead).
Return `cl-webview::webview-t' of NULL for failure. 

Enable developer tools if `debug', you could i.e. inspect element.

`native-window' should be a optional native window handle pointer,
i.e. GtkWindow pointer, NSWindow pointer (Cocoa) or HWND (Win32).
By default `native-window' will be a NULL pointer, creating a
new window and both the window and application lifecycle are
managed by the webview instance, if non-NULL, the webview widget is
embedded into the given window, and the caller is expected to assume
responsibility for the window as well as application lifecycle.

Example:

   (webview-create :debug t) ;; an webview window with develop tools
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

;; ========== webview-terminate ==========

(defun webview-terminate (webview)
  "Terminate the webview application. "
  (signal (cl-webview.lib::webview-terminate webview)))

;; ========== webview-destroy ==========
;; Not knowing if it is proper to expose this function. 

(defun webview-destroy (webview)
  "Destroy the webview application. "
  (signal (cl-webview.lib::webview-destroy webview)))

;; ========== webview-bind ==========

(defmacro def-bind-callback (name (webview &rest params) &body body)
  "A wrapper for cffi:callback funcion useful in `webview-bind-fn'.
The last evaulated form in `body' will be returned to JS call.

Example:

  ;; definition in CL side:
  (def-bind-callback name (webview params)
    (do-some-thing-with params))

  // js side:
  funcall_result = await name(possible_params);

Behind the scene:
+ JS send the possible_params as JSON
+ the JSON was parsed (`shasht:read-json') into list
+ The CL result will be decoded to JSON (`shasht:write-json') to JS
+ also, when calling the binding function, will pass the calling
  webview as the argument, see `webview-bind-fn' for details.
"
  `(cffi:defcallback ,name :void
       ((id :string) (req :string) (,webview :pointer))
     (let ((status 1)           
           result)
       (restart-case
           (progn
             ;; call lisp code from js
             (apply (lambda ,params
                      (setf result (progn ,@body)
                            status 0))
                    (map 'list #'identity (shasht:read-json req)))
             ;; return status and result
             (cl-webview.lib::webview-return
              ,webview id status (shasht:write-json result nil)))
         (continue ()
           :report "Ignore error and return to main loop. "
           (setf status 1))
         (terminate ()
           :report "Terminate this Webview application. "
           (warn "Terminated webview window. ")
           (webview-terminate ,webview))))))

(defun webview-bind-fn (webview name fn)
  "Binds a function to a new global JavaScript function.
Overwrite the old binding if exists.

The `name' should be a string for toplevel JS function.
The `fn' could be a function or a cffi callback symbol,
recommanded to use `def-bind-callback' to make the callback symbol.

Example:
1. `fn' be a function:

    (webview-bind-fn webview \"format\"
      (lambda (&optional message)
        (format t \"~a\" message)
        (force-output)))

2. `fn' be a cffi callback symbol:

    (def-bind-callback create-a-new-window-from (webview)
      (with-webview (w :title \"Window from Window\")
        (webview-set-html w (format nil \"This window from ~a\" webview))))

    (with-webview (webview :title \"Window of Window\")
      (webview-bind-fn  webview \"create_new_window\" 'create-a-new-window-from)
      (webview-set-html webview \"<h1 onclick='create_new_window()'>CLICK</h1>\"))

   This should be used if you are using a cffi callback function
   commonly. Behind first situation is a callback named
   `%webview-bind-fn' so not recommanded to make the same name.

"
  (declare (type (or function symbol) fn))
  (let ((fn* (cond ((functionp fn)
                    (def-bind-callback %webview-bind-fn (webview &rest params)
                      (apply fn (cons webview params)))
                    (cffi:callback %webview-bind-fn))
                   ((symbolp   fn)
                    (cffi:get-callback fn)))))
    (handler-case
        (signal (cl-webview.lib::webview-bind webview name fn* webview))
      (webview-error-duplicate (c)
        (declare (ignore c))
        (cl-webview.lib::webview-unbind webview name)
        (cl-webview.lib::webview-bind   webview name fn* webview)
        (warn "Rebind already existing with the specified name of ~s. " name)))))

(defmacro webview-bind ((webview &rest lambda-list) name &body body)
  "Binds the function to webview, see `webview-bind-fn' for details.

Example:

    (with-webview (webview :title \"User Input\")
      (webview-bind (webview message) \"format\"
        (format t \"Got message ~a\" message))
      (webview-set-html webview \"<h1>Input</h1>
<input id='message'></input>
<button onclick=\"format(document.getElementById('message').value)\">
  FORMAT!
</button>\"))

"
  `(webview-bind-fn ,webview ,name (lambda (,webview ,@lambda-list)
                                     (declare (ignorable ,webview))
                                     ,@body)))

;; ========== webview-dispatch ==========

(defmacro def-dispatch-callback (name (webview) &body body)
  "A wrapper for cffi:callback funcion useful in `webview-dispatch-fn'.
The `webview' is the webview instance calling the dispatch fn.

The form of `def-dispatch-callback' is much like `def-bind-callback'.
"
  `(cffi:defcallback ,name :void
       ((,webview cl-webview.lib::webview-t) (arg :pointer))
     (declare (ignore arg))
     (restart-case (progn ,@body)
       (continue ()
         :report "Ignore error and return to main loop. ")
       (terminate ()
         :report "Terminate this Webview application. "
         (warn "Terminated webview window. ")
         (webview-terminate ,webview)))))

(defun webview-dispatch-fn (webview fn)
  "Dispatch webview instance `webview' with function `fn'.

The function `fn' is called with an argument for `webview' instance."
  (declare (type (or function symbol)))
  (let* ((fn* (cond ((functionp fn)
                     (def-dispatch-callback %webview-dispatch-fn (webview)
                       (funcall fn webview))
                     (cffi:callback %webview-dispatch-fn))
                    ((symbolp fn)
                     (cffi:get-callback fn)))))
    (signal (cl-webview.lib::webview-dispatch webview fn* (cffi:null-pointer)))))

(defmacro webview-dispatch ((webview) &body body)
  "Schedules a function to be invoked on the thread with the run/event loop.
Use this function to interact with the library or native handles. "
  (declare (symbol webview))
  `(webview-dispatch-fn ,webview
                        (lambda (,webview)
                          (declare (ignorable ,webview))
                          ,@body)))

;; ========== webview-set-size ==========

(defun webview-set-size (webview width height
                         &optional (hints :none))
  "Set webview `webview' window size with `width' and `height'.

The webview hints could be:
+ `:none'  width and height are default size (default)
+ `:min'   width and height are minimum bounds
+ `:max'   width and height are maximum bounds
+ `:fixed' width and height cannot changed by user

Example:

    (webview-set-size webview
                      *default-webview-width*
                      *default-webview-height*)

See `*default-webview-width*' and `*default-webview-height*' for detail.
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
    (webview-dispatch (webview)
      (cl-webview.lib::webview-set-size webview width height hints))))

;; ========== webview-set-title ==========

(defun webview-set-title (webview title)
  "Updates the title of the native window. "
  (webview-dispatch (webview)
    (cl-webview.lib::webview-set-title webview title)))

;; ========== webview-navigate ==========

;; TODO: add custom URL support.
(defun %decode-url (url)
  "Decode custom `url'. "
  url)

(defun webview-navigate (webview url)
  "Navigates webview to the given `url'.
`url' may be a properly encoded data URI. "
  (let ((url (%decode-url url)))
    (webview-dispatch (webview)
      (cl-webview.lib::webview-navigate webview url))))

;; ========== webview-set-html ==========

;; TODO: add html generation support
(defun webview-set-html (webview html)
  "Load HTML content into the webview. "
  (webview-dispatch (webview)
    (cl-webview.lib::webview-set-html webview html)))

;; ========== webview-init ==========

(defun webview-init (webview js)
  "Injects JS code to be executed immediately upon loading a page.
The code will be executed before window.onload. "
  (webview-dispatch (webview)
    (cl-webview.lib::webview-init webview js)))

;; ========== webview-eval ==========

(defun webview-eval (webview js)
  "Evaluates arbitrary JS code.

Use bindings if you need to communicate the result of the evalutation. "
  (webview-dispatch (webview)
    (cl-webview.lib::webview-eval webview js)))

;; ========== webview-unbind ==========

(defun webview-unbind (webview name)
  "Removes a binding created with `webview-bind'. "
  (webview-dispatch (webview)
    (handler-case (signal (cl-webview.lib::webview-unbind webview name))
      (webview-error-not-found (c)
        (declare (ignore c))
        (warn (format nil "No binding exists with the name of ~s" name))))))

(defmacro without-float-traps (&body body)
  "Get rid of the float traps error (in SBCL). "
  #+sbcl `(sb-int:with-float-traps-masked
              (:underflow :overflow :inexact :invalid :divide-by-zero)
            ,@body)
  #-sbcl `(progn ,@body))

;; Why not use trivial-main-thread?
;; Well, maybe because just on my machine, the trivial-main-thread 
;; won't start the window. So I just patch the code from trivial-main-thread,
;; and write myself the plain version. 
(defmacro run-in-main-thread (&body body)
  "Run the body in main thread (if needed). "
  ;; In macOS, an app with GUI should have its UI thread as main thread.
  #+sbcl `(sb-thread:interrupt-thread
           (sb-thread:main-thread)
           (lambda ()
             (without-float-traps
               ,@body)))
  #-sbcl `(progn ,@body))

;; Need to support more platform
(defun in-main-thread? ()
  "Test if now in main thread. "
  #+sbcl (sb-thread:main-thread-p)
  #-sbcl (error "Not know if in main thread. "))

;; this should be run in main thread
(defun webview-run (main-webview)
  "Run the webview as the main webview window.
This should only be used for main webview window.
The `main-webview' will be destroyed when the main loop ends. "
  (without-float-traps
    (bt:with-lock-held (*main-webview-window-lock*)
      (unwind-protect
           (progn
             (setf *main-webview-window-live?* t)
             (cl-webview.lib::webview-run      main-webview) ; webview_run(w);
             (cl-webview.lib::webview-destroy  main-webview)) ; webview_destroy(w);
        (setf *main-webview-window-live?* nil)))))

(defun ensure-main-webview-window (&rest args &key debug native-window)
  "Make sure there is a main webview window running, create if not. "
  (declare (ignore debug native-window))
  (unless *main-webview-window-live?*
    (let ((channel (trivial-channels:make-channel)))
      (run-in-main-thread
        (let ((main (apply #'webview-create args)))
          (setf *main-webview-window* main)
          (trivial-channels:sendmsg channel t)
          (webview-run main)))
      (trivial-channels:recvmsg channel))))

(defun quit-main-webview-window ()
  "Make sure the main webview window is terminated. "
  (when *main-webview-window-live?*
    (cl-webview.lib::webview-terminate *main-webview-window*)
    ;; wait until the lock is released
    (bt:with-lock-held (*main-webview-window-lock*)
      (format t "main webview quited"))))

(defun make-webview (&key debug native-window
                       (width  *default-window-width*)
                       (height *default-window-height*)
                       (hints  :none)
                       (title  *default-window-title*)
                       (html   "")
                       (url    "" url-set?)
                     &allow-other-keys)
  "Make a webview by dispatching the main webview window.
If set with `url', will visit url regardless of the `html'. "
  (flet ((%make-webview ()
           (let ((webview (webview-create :debug         debug
                                          :native-window native-window)))
             (webview-set-size  webview width height hints)
             (webview-set-title webview title)
             (if url-set?
                 (webview-navigate  webview url)
                 (webview-set-html  webview html))
             webview)))
    (cond ((in-main-thread?)
           (if *main-webview-window-live?*
               ;; just create the webview window
               ;; by dispatching the main webview
               ;; see https://github.com/webview/webview/pull/1005
               (webview-dispatch (*main-webview-window*)
                 (%make-webview))
               ;; create the webview as the main webview
               ;; just in case you happened to run `make-webview'
               ;; in main thread
               (webview-run (%make-webview))))
          (t
           (ensure-main-webview-window)
           (let ((channel (trivial-channels:make-channel)))
             (webview-dispatch (*main-webview-window*)
               (trivial-channels:sendmsg channel (%make-webview)))
             (trivial-channels:recvmsg channel))))))

;; it seems not to be a appealing macro though...
(defmacro with-webview ((webview
                         &key (title *default-window-title*)
                           (width  *default-window-width*)
                           (height *default-window-height*)
                           (hints :none)
                           (html "") (url "" url-set?)
                           debug native-window                           
                         &allow-other-keys)
                        &body body)
  "Create a webview instance and ensure it to be destroied after use.

Example:

    (with-webview (webview :title \"Hello\")
      (webview-set-html webview \"<h1>CL-WEBVIEW</h1>\"))
"
  `(flet ((%make-webview ()
            (without-float-traps
              (let ((,webview (webview-create :debug         ,debug
                                              :native-window ,native-window)))
                (webview-set-size  ,webview ,width ,height ,hints)
                (webview-set-title ,webview ,title)
                ,(if url-set?
                     `(webview-navigate  ,webview ,url)
                     `(webview-set-html  ,webview ,html))
                ,@body
                ,webview))))
     (cond ((in-main-thread?)
            (if *main-webview-window-live?*
                ;; just create the webview window
                ;; by dispatching the main webview
                ;; see https://github.com/webview/webview/pull/1005
                (webview-dispatch (*main-webview-window*)
                  (%make-webview))
                ;; create the webview as the main webview
                ;; just in case you happened to run `make-webview'
                ;; in main thread
                (webview-run (%make-webview))))
           (t
            (ensure-main-webview-window)
            (let ((channel (trivial-channels:make-channel)))
              (webview-dispatch (*main-webview-window*)
                (trivial-channels:sendmsg channel (%make-webview)))
              (trivial-channels:recvmsg channel))))))

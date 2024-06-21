(in-package :cl-webview.preload)

(defun ffi-name-transformer (name kind &key &allow-other-keys)
  (flet ((trans-char (char)
           (cond ((char= char #\_) #\-)
                 (t char))))
    (let ((name (map 'string #'trans-char (string-upcase name))))
      (case kind
        (:constant (format nil "+~@:(~A~)+" name))
        (t         name)))))

;; Ensure the libwebview library
;; If does not exists, build
(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((path (&rest parts)
           (namestring
            (asdf:system-relative-pathname
             'cl-webview (format nil "~{~a~^/~}" parts)))))
    ;; lib should be the place where cffi search for libwebview
    (pushnew (path "lib/") cffi:*foreign-library-directories*)
    (unless (uiop:directory-exists-p (path "lib/"))
      (format t "~&Make lib directory. ")
      (ensure-directories-exist (path "lib/")))    
    (let ((lib (cond ((uiop:os-macosx-p) "libwebview.dylib")
                     ((uiop:os-unix-p)   "libwebview.so")
                     (t (warn "Unhandled platform... ") "libwebview"))))
      ;; check the webview submodule
      (unless (uiop:file-exists-p (path "webview" "webview.h"))
        (error "Incomplete webview git repo, please fetch it first. "))
      ;; link webview.h
      (unless (uiop:file-exists-p (path "spec" "webview.h"))
        (format t "~&Linking webview.h from webview. ~%")
        (uiop:run-program `("ln" "-s"
                                 ,(path "webview" "webview.h")
                                 ,(path "spec"    "webview.h"))
                          :output t))
      ;; build, link libwebview
      (unless (uiop:file-exists-p (path "lib" lib))
        (unless (uiop:file-exists-p (path "webview" "build" "library" lib))
          (format t "~&Building ~a in webview~%" lib)
          (uiop:run-program `("sh" ,(path "webview" "script"
                                          #+unix "build.sh"
                                          #-unix "build.bat"))
                            :output t))
        (format t "~&Linking ~a from webview~%" lib)
        (uiop:run-program `("ln" "-s" ,(path "webview" "build" "library" lib)
                                 ,(path "lib" lib))
                          :output t)))))

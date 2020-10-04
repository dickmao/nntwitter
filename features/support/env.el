;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'f)
(require 'ecukes)
(require 'espuds)
(add-to-list 'load-path (f-expand "lisp" (ecukes-project-path)))
(add-to-list 'load-path (f-expand "tests" (ecukes-project-path)))
(require 'nntwitter-test)

(defmacro if-demote (demote &rest forms)
  (declare (debug t) (indent 1))
  `(if ,demote
       (with-demoted-errors "demoted: %s"
         ,@forms)
     ,@forms))

(defun cleanup ()
  (let* ((newsrc-file gnus-current-startup-file)
         (quick-file (concat newsrc-file ".eld")))
    (when (file-exists-p quick-file)
      (message "Deleting %s" quick-file)
      (delete-file quick-file))))

(defun save-log (buffer-or-name file-name)
  "from tkf/emacs-ipython-notebook ein:testing-save-buffer."
  (when (and buffer-or-name (get-buffer buffer-or-name) file-name)
    (with-current-buffer buffer-or-name
      (let ((coding-system-for-write 'raw-text))
        (write-region (point-min) (point-max) file-name)))))

(defvar scenario-recording-alist '((touched nil)))
(defvar scenario-recording-p t)

(defmacro with-scenario (scenario &rest body)
  `(let* ((name (ecukes-scenario-name ,scenario))
          (filename (f-expand (replace-regexp-in-string "\\s-+" "-" name)
                              (f-expand "tests/recordings" (ecukes-project-path)))))
     ,@body))

(put 'with-scenario 'lisp-indent-function 1)

(Setup
 (add-function
  :before-until (symbol-function 'nntwitter-api-set-oauth-tokens)
  (lambda (&rest _args)
    (unless scenario-recording-p
      (setq nntwitter-api--access-token "access-token"
            nntwitter-api--access-token-secret "access-token-secret"))))

 (add-function
  :filter-args (symbol-function 'request)
  (lambda (args)
    (when scenario-recording-p
      (cl-destructuring-bind (url &rest plst)
          args
        (let* ((fun0 (plist-get plst :success))
               (fun1 (cl-function
                      (lambda (&rest args* &key data &allow-other-keys)
                        (gnus-score-set (intern url)
                                        (append (gnus-score-get
                                                 (intern url)
                                                 scenario-recording-alist)
                                                (list data))
                                        scenario-recording-alist)))))
          (setq args (cons url (plist-put
                                plst :success
                                (lambda (&rest args*)
                                  (prog1 (apply fun0 args*)
                                    (apply fun1 args*)))))))))
    args))

 (add-function
  :before-until (symbol-function 'request)
  (lambda (url &rest args)
    (unless scenario-recording-p
      (let* ((fun0 (plist-get args :success))
             (values (gnus-score-get (intern url) scenario-recording-alist))
             (plst (car values)))
        (if plst
            (progn
              (gnus-score-set (intern url) (cdr values) scenario-recording-alist)
              (funcall fun0 :data plst :response (make-request-response :url url)))
          (error "Could not playback %s" url)))))))

(Before
 (setq ecukes-reporter-before-scenario-hook
       (lambda (scenario)
         (with-scenario scenario
           (setq scenario-recording-p (not (file-exists-p filename)))
           (setq scenario-recording-alist
                 (if scenario-recording-p
                     '((touched nil))
                   (with-temp-buffer
                     (let ((coding-system-for-read score-mode-coding-system))
                       (insert-file-contents filename))
                     (goto-char (point-min))
                     (read (current-buffer))))))))
 (setq ecukes-reporter-after-scenario-hook
       (lambda (scenario)
         (with-scenario scenario
           (when scenario-recording-p
             (setq scenario-recording-alist
                   (assq-delete-all 'touched scenario-recording-alist))
             (gnus-make-directory (file-name-directory filename))
             (with-temp-buffer
               (gnus-prin1 scenario-recording-alist)
               (let ((coding-system-for-write score-mode-coding-system))
                 (gnus-write-buffer filename)))))
         (setq scenario-recording-alist '((touched nil)))
         (setq scenario-recording-p t))))
(After
 )

(Teardown
 (save-log request-log-buffer-name (f-expand "tests/request-log" (ecukes-project-path)))
 (cleanup)
)

(Fail
 (if noninteractive
     (with-demoted-errors "demote: %s"
       (Teardown))
   (backtrace)
   (keyboard-quit))) ;; useful to prevent emacs from quitting

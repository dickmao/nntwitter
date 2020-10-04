;;; nntwitter.el --- Gnus backend for twitter  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020 The Authors of nntwitter.el

;; Authors: dickmao <github id: dickmao>
;; Version: 0
;; Keywords: news
;; URL: https://github.com/dickmao/nntwitter

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with nntwitter.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A Gnus backend for Twitter.

;;; Code:

(require 'nnoo)
(require 'gnus)
(require 'gnus-start)
(require 'gnus-art)
(require 'gnus-sum)
(require 'gnus-msg)
(require 'gnus-cite)
(require 'gnus-srvr)
(require 'gnus-cache)
(require 'gnus-bcklg)
(require 'gnus-score)
(require 'subr-x)
(require 'mm-url)
(require 'cl-lib)
(require 'anaphora)
(require 'request)
(require 'url-http)
(require 'gnus-topic)
(require 'nntwitter-api)
(require 'seq)
(require 'json)

(nnoo-declare nntwitter)

(nnoo-define-basics nntwitter)

(defgroup nntwitter nil "A Gnus backend for Twitter."
  :group 'gnus)

(defcustom nntwitter-max-render-bytes 300e3
  "`quoted-printable-encode-region' bogs when the js spyware gets out of hand."
  :type 'integer
  :group 'nntwitter)

(defcustom nntwitter-render-submission t
  "If non-nil, follow link upon `gnus-summary-select-article'.

Otherwise, just display link."
  :type 'boolean
  :group 'nntwitter)

(defvar nntwitter-summary-voting-map
  (let ((map (make-sparse-keymap)))
    map)
  "Voting map.")

(defvar nntwitter-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "r" 'gnus-summary-followup)
    (define-prefix-command 'nntwitter-summary-voting-map)
    (define-key map "R" 'nntwitter-summary-voting-map)
    (define-key nntwitter-summary-voting-map "0" 'nntwitter-novote)
    (define-key nntwitter-summary-voting-map "-" 'nntwitter-downvote)
    (define-key nntwitter-summary-voting-map "=" 'nntwitter-upvote)
    (define-key nntwitter-summary-voting-map "+" 'nntwitter-upvote)
    map))

(defvar nntwitter-article-mode-map
  (copy-keymap nntwitter-summary-mode-map)) ;; how does Gnus do this?

(define-minor-mode nntwitter-article-mode
  "Minor mode for nntwitter articles.  Disallow `gnus-article-reply-with-original'.

\\{gnus-article-mode-map}
"
  :lighter " Twitter"
  :keymap nntwitter-article-mode-map)

(define-minor-mode nntwitter-summary-mode
  "Disallow \"reply\" commands in `gnus-summary-mode-map'.

\\{nntwitter-summary-mode-map}
"
  :lighter " Twitter"
  :keymap nntwitter-summary-mode-map)

(cl-defun nntwitter-novote ()
  "Retract vote."
  (interactive)
  (nntwitter-vote-current-article 0))

(cl-defun nntwitter-downvote ()
  "Downvote the article in current buffer."
  (interactive)
  (nntwitter-vote-current-article -1))

(cl-defun nntwitter-upvote ()
  "Upvote the article in current buffer."
  (interactive)
  (nntwitter-vote-current-article 1))

(defvar nntwitter--seq-map-indexed
  (if (fboundp 'seq-map-indexed)
      #'seq-map-indexed
    (lambda (function sequence)
      (let ((index 0))
        (seq-map (lambda (elt)
                   (prog1
                       (funcall function elt index)
                     (setq index (1+ index))))
                 sequence)))))

(defmacro nntwitter--normalize-server ()
  "Disallow \"server\" from being empty string, which is unsettling.
Normalize it to \"nntwitter-default\"."
  `(let ((canonical "nntwitter-default"))
    (when (equal server "")
      (setq server nil))
    (unless server
      (setq server canonical))
    (unless (equal server canonical)
      (error "`nntwitter--normalize-server': multiple servers unsupported!"))))

(defvar nntwitter-headers-hashtb (make-hash-table)
  "Group string -> interleaved submissions and comments sorted by created time.")

(defvar nntwitter-lookup-hashtb (make-hash-table)
  "Tweet-id -> (group . article-number).")

(defvar nntwitter-oob-hashtb (make-hash-table)
  "Out-of-band tweet-id -> tweet-text.")

(defsubst nntwitter-get-headers (group)
  "List headers from GROUP."
  (gethash (if (stringp group) (intern group) group) nntwitter-headers-hashtb))

(defun nntwitter-refs-for (id &optional depth)
  "Get message ancestry for ID up to DEPTH."
  (unless depth
    (setq depth most-positive-fixnum))
  (when (> depth 0)
    (let* ((tweet-in-question (nntwitter-get-header-for-id id))
           (refs (nreverse (cl-loop for level = 0 then level
                                    until (>= level depth)
                                    for tweet = tweet-in-question then
                                    (nntwitter-get-header-for-id parent-id)
                                    until (null tweet)
                                    for parent-id = (assoc-default 'in_reply_to_id tweet)
                                    until (null parent-id)
                                    collect parent-id
                                    do (cl-incf level))))
           (conversation-id (assoc-default 'conversation_id tweet-in-question)))
      (unless (equal (car refs) conversation-id)
        (push conversation-id refs))
      refs)))

(defsubst nntwitter--current-article-number ()
  "`gnus-article-current' is a global variable that gets clobbered."
  (or (cdr gnus-message-group-art)
      (and (gnus-buffer-live-p gnus-summary-buffer)
           (with-current-buffer gnus-summary-buffer
             (cdr gnus-article-current)))))

(defsubst nntwitter--current-group ()
  "`gnus-article-current' is a global variable that gets clobbered."
  (or (car gnus-message-group-art)
      (with-current-buffer gnus-summary-buffer
        (car gnus-article-current))))

(defsubst nntwitter-rpc-call (server generator_kwargs method &rest args)
  "Stub SERVER GENERATOR_KWARGS METHOD ARGS."
  (nntwitter--normalize-server)
  (condition-case-unless-debug err
      (apply #'ignore generator_kwargs method args)
    (error (gnus-message 3 "nntwitter-rpc-call: %s" (error-message-string err))
           nil)))

(defun nntwitter-vote-current-article (vote)
  "VOTE is +1, -1, 0."
  (unless gnus-newsgroup-name
    (error "No current newgroup"))
  (if-let ((article-number (or (nntwitter--current-article-number)
                               (with-current-buffer gnus-summary-buffer
                                 (gnus-summary-article-number)))))
      (let* ((header (nntwitter--get-header
                      article-number
                      (gnus-group-real-name (or (nntwitter--current-group)
                                                gnus-newsgroup-name))))
             (orig-score (format "%s" (assoc-default 'score header)))
             (new-score (if (zerop vote) orig-score
                          (concat orig-score " "
                                  (if (> vote 0) "+" "")
                                  (format "%s" vote))))
             (article-name (assoc-default 'id header)))
        (save-excursion
          (save-window-excursion
            (with-current-buffer gnus-summary-buffer
              (if (eq (gnus-summary-article-number) (cdr gnus-article-current))
                  (progn (with-current-buffer gnus-article-buffer
                           (let ((inhibit-read-only t))
                             (nnheader-replace-header "Score" new-score)))
                         (nntwitter-rpc-call nil nil "vote" article-name vote))
                (message "Open the article before voting"))))))
    (error "No current article")))

(defsubst nntwitter--gate (&optional group)
  "Apply our minor modes only when the following conditions hold for GROUP."
  (unless group
    (setq group gnus-newsgroup-name))
  (and (stringp group)
       (listp (gnus-group-method group))
       (eq 'nntwitter (car (gnus-group-method group)))))

(defun nntwitter-update-subscription (group level oldlevel &optional _previous)
  "Nntwitter `gnus-group-change-level' callback of GROUP to LEVEL from OLDLEVEL."
  (when (nntwitter--gate group)
    (let ((old-subbed-p (<= oldlevel gnus-level-subscribed))
          (new-subbed-p (<= level gnus-level-subscribed)))
      (unless (eq old-subbed-p new-subbed-p)
        ;; afaict, praw post() doesn't return status
        (if new-subbed-p
            (nntwitter-rpc-call nil nil "subscribe" (gnus-group-real-name group))
          (nntwitter-rpc-call nil nil "unsubscribe" (gnus-group-real-name group)))))))

(deffoo nntwitter-request-close ()
  (nntwitter-close-server)
  t)

(deffoo nntwitter-request-type (_group &optional _article)
  'news)

(deffoo nntwitter-server-opened (&optional server)
  (nntwitter--normalize-server)
  t)

(deffoo nntwitter-status-message (&optional server)
  (nntwitter--normalize-server)
  "")

(deffoo nntwitter-open-server (_server &optional _defs)
  t)

(deffoo nntwitter-close-group (_group &optional server)
  (nntwitter--normalize-server)
  t)

(defmacro nntwitter--with-group (group &rest body)
  "Disambiguate GROUP if it's empty and execute BODY."
  (declare (debug (form &rest form))
           (indent 1))
  `(let* ((group (or ,group (gnus-group-real-name gnus-newsgroup-name)))
          (gnus-newsgroup-name (gnus-group-full-name group "nntwitter:")))
     ,@body))

(defun nntwitter--get-header (article-number &optional group)
  "Get header for ARTICLE-NUMBER and GROUP."
  (nntwitter--with-group group
    (let ((headers (nntwitter-get-headers group)))
      (elt headers (1- article-number)))))

(defun nntwitter-get-header-for-id (id)
  "Get header for ID, if we even have it."
  (when-let ((found (gethash (intern id) nntwitter-lookup-hashtb)))
    (cl-destructuring-bind (group . article-number)
        found
      (nntwitter--get-header article-number group))))

(defun nntwitter-get-text (id)
  "Get full text of ID."
  (aif (gethash (intern id) nntwitter-oob-hashtb)
      (cdr it)
    (awhen (nntwitter-get-header-for-id id)
      (or (assoc-default 'retweeted_status it)
          (assoc-default 'text it)))))

(defsubst nntwitter--br-tagify (body)
  "Twitter-html BODY shies away from <BR>.  Should it?"
  (replace-regexp-in-string "\n" "<br>" body))

(defsubst nntwitter--citation-wrap (author body)
  "Cite AUTHOR using `gnus-message-cite-prefix-regexp' before displaying BODY.

Originally written by Paul Issartel."
  (with-temp-buffer
    (insert body)
    (mm-url-remove-markup)
    (mm-url-decode-entities)
    (fill-region (point-min) (point-max))
    (let* ((trimmed-1 (replace-regexp-in-string "\\(\\s-\\|\n\\)+$" "" (buffer-string)))
           (trimmed (replace-regexp-in-string "^\\(\\s-\\|\n\\)+" "" trimmed-1)))
      (concat author " wrote:<br>\n"
              "<pre>\n"
              (cl-subseq (replace-regexp-in-string "\n" "\n> " (concat "\n" trimmed)) 1)
              "\n</pre>\n\n"))))

(defun nntwitter--filter-after (after-this vop)
  "Get elements created AFTER-THIS in VOP (vector of plists)."
  (cl-loop for elt-idx in (funcall nntwitter--seq-map-indexed
                                   (lambda (elt idx) (cons elt idx)) vop)
           until (>= (assoc-default 'created_utc (car elt-idx)) after-this)
           finally return (seq-drop vop (or (cdr elt-idx) 0))))

(deffoo nntwitter-request-group-scan (group &optional server _info)
  "M-g from *Group* calls this.
Set flag for the ensuing `nntwitter-request-group' to avoid going out to PRAW
yet again."
  (nntwitter--normalize-server)
  (nntwitter--with-group group
    (gnus-message 5 "nntwitter-request-group-scan: scanning %s..." group)
    (gnus-activate-group gnus-newsgroup-name t)
    (gnus-message 5 "nntwitter-request-group-scan: scanning %s...done" group)
    t))

(defsubst nntwitter--shift-ranges (delta ranges)
  "Shift back by DELTA the elements of RANGES, removing any negative entries."
  (cl-remove-if-not (lambda (e)
                      (cond ((numberp e) (> e 0))
                            (t (> (cdr e) 0))))
                    (mapcar (lambda (e)
                              (cond ((numberp e) (- e delta))
                                    (t `(,(max 1 (- (car e) delta)) .
                                         ,(- (cdr e) delta)))))
                            ranges)))

;; gnus-group-select-group
;;   gnus-group-read-group
;;     gnus-summary-read-group
;;       gnus-summary-read-group-1
;;         gnus-summary-setup-buffer
;;           sets gnus-newsgroup-name
;;         gnus-select-newsgroup
;;           gnus-request-group
;;             nntwitter-request-group
(deffoo nntwitter-request-group (group &optional server _fast _info)
  (nntwitter--normalize-server)
  (when (nntwitter-api--works-p)
    (let* ((headers (nntwitter-get-headers group))
           (num-headers (length headers))
           (status (format "211 %d %d %d %s" num-headers 1 num-headers group)))
      (gnus-message 7 "nntwitter-request-group: %s" status)
      (nnheader-insert "%s\n" status)
      t)))

(deffoo nntwitter-request-scan (&optional group server)
  (nntwitter--normalize-server)
  (when (and group (nntwitter-api--works-p))
    (nntwitter--with-group group
      (cl-destructuring-bind (seconds num-gc seconds-gc)
          (benchmark-run (nntwitter-incoming group))
        (gnus-message 5 (concat "nntwitter-request-scan: %s took %s seconds,"
                                " with %s gc runs taking %s seconds")
                      group seconds num-gc seconds-gc)))))

(defsubst nntwitter--make-message-id (tweet-id)
  "Construct a valid Gnus message id from TWEET-ID."
  (format "<%s@twitter.com>" tweet-id))

(defun nntwitter--make-references (tweet-id)
  "Construct a space delimited string of message ancestors of TWEET-ID."
  (mapconcat (lambda (ref) (nntwitter--make-message-id ref))
             (nntwitter-refs-for tweet-id) " "))

(cl-defstruct (nntwitter-time (:type list))
  (second)
  (minute)
  (hour)
  (day)
  (month)
  (year)
  (weekday)
  (dst)
  (zone))

(defun nntwitter-backport-iso8601 (string)
  "The module iso8601 is only emacs-27; copy the logic here.
Convert STRING into a time structure."
  (let* ((concat-regexps
          (lambda (regexps)
            (mapconcat (lambda (regexp)
                         (concat "\\(?:"
                                 (replace-regexp-in-string "(" "(?:" regexp)
                                 "\\)"))
                       regexps "\\|")))
         (date-match "\\([+-]?[0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)")
         (time-match "\\([0-9][0-9]\\):?\\([0-9][0-9]\\)?:?\\([0-9][0-9]\\)?[.,]?\\([0-9]*\\)")
         (zone-match "\\(Z\\|\\([+-]\\)\\([0-9][0-9]\\):?\\([0-9][0-9]\\)?\\)")
         (regexp (concat "\\(" (funcall concat-regexps (list date-match)) "\\)"
                         "\\(?:T\\("
                         (replace-regexp-in-string "(" "(?:" time-match)
                         "\\)"
                         "\\(" zone-match "\\)?\\)?")))
    (if (not (string-match (concat "\\`" regexp "\\'") string))
        (signal 'wrong-type-argument string)
      (let ((date-string (match-string 1 string))
            (time-string (match-string 2 string))
            (result (make-nntwitter-time)))
        (string-match (concat "\\`" date-match "\\'") date-string)
        (let ((day (string-to-number (match-string 3 date-string)))
              (month (string-to-number (match-string 2 date-string)))
              (year (string-to-number (match-string 1 date-string))))
          (setf (nntwitter-time-year result) year)
          (setf (nntwitter-time-month result) month)
          (setf (nntwitter-time-day result) day))
        (string-match (concat "\\`" time-match "\\'") time-string)
        (let ((hour (string-to-number (match-string 1 time-string)))
              (minute (string-to-number (match-string 2 time-string)))
              (second (string-to-number (match-string 3 time-string))))
          (setf (nntwitter-time-hour result) hour)
          (setf (nntwitter-time-minute result) minute)
          (setf (nntwitter-time-second result) second))
        (setf (nntwitter-time-zone result) 0)
        result))))

(defsubst nntwitter--make-header (article-number &optional group)
  "Construct full headers of articled indexed ARTICLE-NUMBER in GROUP."
  (nntwitter--with-group group
    (when-let ((header (nntwitter--get-header article-number group)))
      (let* ((public-metrics (assoc-default 'public_metrics header))
             (score (assoc-default 'like_count public-metrics))
             (num-comments (assoc-default 'num_comments public-metrics))
             (conversation-id (assoc-default 'conversation_id header))
             (id (assoc-default 'id header))
             (title (car (split-string (nntwitter-get-text conversation-id) "[\n\r\v]+"))))
        (make-full-mail-header
         article-number
         (if (equal id conversation-id)
             title
           (concat "Re: " title))
         (assoc-default 'author_user_name header)
         (format-time-string
          "%a, %d %h %Y %T %z (%Z)"
          (let ((time-struct (nntwitter-backport-iso8601 (assoc-default 'created_at header))))
            (apply #'encode-time time-struct)))
         (nntwitter--make-message-id (assoc-default 'id header))
         (nntwitter--make-references (assoc-default 'id header))
         0 0 nil
         (append (and (integerp score)
                      `((X-Twitter-Likes . ,(number-to-string score))))
                 (and (integerp num-comments)
                      `((X-Twitter-Comments . ,(number-to-string num-comments))))))))))

(cl-defun nntwitter--request-error (caller
                                   &key response symbol-status error-thrown
                                   &allow-other-keys
                                   &aux (response-status
                                         (request-response-status-code response)))
  "Refer to CALLER when reporting a submit error.
Also report http code of RESPONSE, which is distinct from SYMBOL-STATUS, and
ERROR-THROWN.  The http code is stored in RESPONSE-STATUS."
  (gnus-message 3 "%s %s: http status %s, %s"
                caller symbol-status response-status
                (error-message-string error-thrown)))

(cl-defun nntwitter--request (caller
                             url
                             &rest attributes &key parser (backend 'url-retrieve)
                             &allow-other-keys)
  "Prefix errors with CALLER when executing synchronous request to URL.

Request shall contain ATTRIBUTES, one of which is PARSER of the response, if
provided (shall default to verbatim dump of response, if not).  BACKEND can
be curl (defaults to `url-retrieve')."
  (unless parser
    (setq attributes (nconc attributes (list :parser #'buffer-string))))
  (setq attributes (cl-loop for (k v) on attributes by (function cddr)
                            unless (eq k :backend)
                            collect k and collect v))
  (let ((request-backend backend))
    (apply #'request url
           :sync t
           :error (apply-partially #'nntwitter--request-error caller)
           attributes)))

(cl-defun nntwitter--content-handler
    (&key data response &allow-other-keys
     &aux (header (request-response--raw-header response)))
  "Wrap DATA in uri if RESPONSE has HEADER that is image."
  (let* ((_ (string-match "Content-Type:\\s-*\\([[:graph:]]+\\)" header))
         (content-type (match-string 1 header)))
    (cl-destructuring-bind (type _subtype) (split-string content-type "/")
      (cond ((equal type "image")
             (format "<p><img src=\"data:%s;base64,%s\" />"
                     content-type
                     (base64-encode-string (encode-coding-string data 'binary) t)))
            ((equal type "text") (format "<div>%s<br></div>" data))
            (t (error "`nntwitter--content-handler': passing on %s" content-type))))))

(deffoo nntwitter-request-article (article-number &optional group server buffer)
  (nntwitter--normalize-server)
  (nntwitter--with-group group
    (with-current-buffer (or buffer nntp-server-buffer)
      (erase-buffer)
      (let* ((header (nntwitter--get-header article-number group))
             (mail-header (nntwitter--make-header article-number))
             (score (cdr (assq 'X-Twitter-Likes (mail-header-extra mail-header))))
             (body (nntwitter-get-text (or (assoc-default 'retweet_id header)
                                           (assoc-default 'id header)))))
        (when body
          (insert
           "Newsgroups: " group "\n"
           "Subject: " (mail-header-subject mail-header)  "\n"
           "From: " (or (mail-header-from mail-header) "nobody") "\n"
           "Date: " (mail-header-date mail-header) "\n"
           "Message-ID: " (mail-header-id mail-header) "\n"
           "References: " (mail-header-references mail-header) "\n"
           (format "Archived-at: <https://www.twitter.com/%s/status/%s>\n"
                   (assoc-default 'author_user_name header)
                   (assoc-default 'id header))
           "Score: " (or score "") "\n"
           "\n")
          (mml-insert-multipart "alternative")
          (mml-insert-tag 'part 'type "text/html"
                          'disposition "inline"
                          'charset "utf-8")
          (save-excursion (mml-insert-tag '/part))
          (-when-let*
              ((parent-id (assoc-default 'in_reply_to_id header))
               (parent-author (assoc-default 'in_reply_to_user_name header))
               (parent-body (nntwitter-get-text parent-id)))
            (insert (nntwitter--citation-wrap parent-author parent-body)))
          (insert "<p>\n")
          (insert (nntwitter--br-tagify body))
          (awhen (assoc-default 'quoted_id header)
            (insert "<p>\n" (nntwitter--br-tagify (nntwitter-get-text it))))
          (awhen (and nntwitter-render-submission
                      (or (assoc-default 'url header)
                          (assoc-default 'preview_image_url header)))
            (condition-case err
                (nntwitter--request
                 "nntwitter-request-article" it
                 :success
                 (lambda (&rest args)
                   (let ((data (apply #'nntwitter--content-handler args)))
                     (when (< (length data) nntwitter-max-render-bytes)
                       (insert data)))))
              (error (gnus-message 5 "nntwitter-request-article: %s %s"
                                   it (error-message-string err)))))
          (insert "\n")
          (if (mml-validate)
              (message-encode-message-body)
            (gnus-message 2 "nntwitter-request-article: Invalid mml:\n%s"
                          (buffer-string)))
          (cons group article-number))))))

(deffoo nntwitter-retrieve-headers (article-numbers &optional group server _fetch-old)
  (nntwitter--normalize-server)
  (nntwitter--with-group group
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (when (nntwitter-get-headers group)
        (dolist (i article-numbers)
          (nnheader-insert-nov (nntwitter--make-header i group))))))
  'nov)

(deffoo nntwitter-close-server (&optional server _defs)
  (nntwitter--normalize-server)
  t)

(deffoo nntwitter-request-list (&optional server)
  (nntwitter--normalize-server)
  (let ((newsrc (cl-mapcan (lambda (info)
                             (when (and (equal "nntwitter:" (gnus-info-method info))
                                        (<= (gnus-info-level info)
                                            gnus-level-subscribed))
                               (list (gnus-info-group info))))
                           gnus-newsrc-alist)))
    (when (nntwitter-api--works-p)
      (nntwitter-api-route-friends-list
       (cl-function
        (lambda (&key data &allow-other-keys)
          (-let* (((&alist 'users) data)
                  (friends (append users nil)))
            (with-current-buffer nntp-server-buffer
              (mapc (lambda (realname)
                      (let ((group (gnus-group-full-name
                                    realname
                                    `(nntwitter ,(or server "")))))
                        (erase-buffer)
                        (gnus-message 5 "nntwitter-request-list: scanning %s..." realname)
                        (gnus-activate-group group t)
                        (gnus-message 5 "nntwitter-request-list: scanning %s...done" realname)
                        (when (> (gnus-group-level group) gnus-level-subscribed)
                          (gnus-group-unsubscribe-group group gnus-level-default-subscribed t))
                        (setq newsrc (cl-remove group newsrc :test #'equal))))
                    friends)
              (mapc (lambda (group)
                      (gnus-message 4 "nntwitter-request-list: missing subscription %s" group)
                      ;; (nntwitter-rpc-call nil nil "subscribe" (gnus-group-real-name group))
                      ;; (gnus-activate-group group t)
                      )
                    newsrc)
              (erase-buffer)
              (mapc (lambda (realname)
                      (insert (format "%s %d 1 y\n" realname
                                      (length (nntwitter-get-headers realname)))))
                    friends))))))))
  t)

(defun nntwitter-incoming (group)
  "Populate by-group data for GROUP."
  (nntwitter-api-set-oauth-tokens)
  (nntwitter--with-group group
    (let* ((info
            (or (ignore-errors (gnus-get-info gnus-newsgroup-name))
                (list gnus-newsgroup-name
                      gnus-level-default-subscribed
                      nil nil
                      (gnus-method-simplify (gnus-group-method gnus-newsgroup-name)))))
           (params (gnus-info-params info))
           (newsrc-read-ranges (gnus-info-read info))
           (newsrc-mark-ranges (gnus-info-marks info))
           (newsrc-unread-cons (gnus-group-parameter-value params 'last-unread t))
           (newsrc-unread-index (car newsrc-unread-cons))
           (newsrc-unread-id (cdr newsrc-unread-cons))
           (list-last-seen (last (nntwitter-get-headers group))))
      (nntwitter-api-route-recent-search
       group
       (-when-let* ((header (car list-last-seen))
                    (time-struct (nntwitter-backport-iso8601 (assoc-default 'created_at header)))
                    (last-seen-time (apply #'encode-time time-struct))
                    (seven-days-time (time-add (current-time) (+ (- (* 86400 7)) 300)))
                    (recent-enough-p (time-less-p seven-days-time last-seen-time)))
         (when recent-enough-p (assoc-default 'id header)))
       (cl-function
        (lambda (&key data &allow-other-keys)
          (-let* ((payload data)
                  ((&alist 'data
                           'includes (&alist 'users
                                             'tweets
                                             'media))
                   payload)
                  (new-conversation-ids (make-hash-table)))
            (mapc (lambda (datum)
                    (awhen (assoc-default 'conversation_id datum)
                      (when (eq 'missing (gethash (intern it)
                                                  nntwitter-api--conversation-ids 'missing))
                        (puthash (intern it) t new-conversation-ids))))
                  data)
            (when-let ((new-ids (mapcar #'symbol-name
                                        (hash-table-keys new-conversation-ids))))
              (nntwitter-api-route-tweets-lookup
               new-ids
               (cl-function
                (lambda (&key data &allow-other-keys)
                  (-let* ((payload data)
                          ((&alist 'data
                                   'includes (&alist 'users))
                           payload)
                          (author-id (assoc-default
                                      'id
                                      (seq-find
                                       (lambda (x) (equal group
                                                            (assoc-default 'username x)))
                                       users))))
                    (mapc (lambda (datum)
                            (let ((id (intern (assoc-default 'id datum))))
                              (puthash id
                                       (equal author-id (assoc-default 'author_id datum))
                                       nntwitter-api--conversation-ids)
                              (unless (or (gethash id nntwitter-oob-hashtb)
                                          (gethash id nntwitter-lookup-hashtb))
                                (puthash id (cons "somebody" (assoc-default 'text datum))
                                         nntwitter-oob-hashtb))))
                          data)))))
              (dolist (new-id new-ids)
                (when (eq 'missing (gethash (intern new-id)
                                            nntwitter-api--conversation-ids 'missing))
                  (puthash (intern new-id) nil nntwitter-api--conversation-ids)
                  (unless (or (gethash (intern new-id) nntwitter-oob-hashtb)
                              (gethash (intern new-id) nntwitter-lookup-hashtb))
                    (puthash (intern new-id) (cons "somebody" "something")
                             nntwitter-oob-hashtb)))))
            (let ((care-about
                   (nreverse
                    (append (cl-case (assoc-default group nntwitter-api-pleb-inclusivity)
                              ((somewhat nil)
                               (seq-filter
                                (lambda (datum)
                                  (gethash
                                   (intern (assoc-default 'conversation_id datum))
                                   nntwitter-api--conversation-ids))
                                data))
                              (otherwise data))
                            nil)))
                  (start (1+ (length (nntwitter-get-headers group)))))
              (dotimes (idx (length care-about))
                (-let* ((tweet (nth idx care-about))
                        ((&alist 'referenced_tweets referenced-tweets) tweet)
                        (parent-tweet (seq-find
                                       (lambda (x) (equal "replied_to"
                                                            (assoc-default 'type x)))
                                       referenced-tweets))
                        (retweeted-tweet (seq-find
                                          (lambda (x) (equal "retweeted"
                                                               (assoc-default 'type x)))
                                          referenced-tweets))
                        (quoted-tweet (seq-find
                                       (lambda (x) (equal "quoted"
                                                            (assoc-default 'type x)))
                                       referenced-tweets)))
                  (puthash (intern (assoc-default 'id tweet)) (cons group (+ start idx))
                           nntwitter-lookup-hashtb)
                  (-when-let* ((author-id (assoc-default 'author_id tweet))
                               (author-expansion
                                (seq-find
                                 (lambda (x)
                                   (equal author-id (assoc-default 'id x)))
                                 users)))
                    (setf (nth idx care-about)
                          (json-add-to-object (nth idx care-about) "author_user_name"
                                              (assoc-default 'username author-expansion))))
                  (-when-let* ((media-keys (append (assoc-default
                                                    'media_keys
                                                    (assoc-default 'attachments tweet))
                                                   nil))
                               (media-expansion
                                (seq-filter
                                 (lambda (x)
                                   (member (assoc-default 'media_key x)
                                           media-keys))
                                 media)))
                    (mapc
                     (lambda (obj)
                       (mapc (lambda (field)
                               (awhen (assoc-default field obj)
                                 (setf (nth idx care-about)
                                       (json-add-to-object (nth idx care-about)
                                                           (symbol-name field) it))))
                             '(type url preview_image_url)))
                     media-expansion))
                  (-when-let*
                      ((ref-tweet-id (or (assoc-default 'id parent-tweet)
                                         (assoc-default 'id retweeted-tweet)
                                         (assoc-default 'id quoted-tweet)))
                       (ref-tweet-expansion
                        (seq-find
                         (lambda (x)
                           (equal ref-tweet-id (assoc-default 'id x)))
                         tweets))
                       (ref-text (assoc-default 'text ref-tweet-expansion))
                       (ref-user-id (assoc-default 'author_id ref-tweet-expansion))
                       (ref-user-expansion
                        (seq-find
                         (lambda (x)
                           (equal ref-user-id (assoc-default 'id x)))
                         users))
                       (ref-username (assoc-default 'username ref-user-expansion)))
                    (unless (or (gethash (intern ref-tweet-id) nntwitter-oob-hashtb)
                                (gethash (intern ref-tweet-id) nntwitter-lookup-hashtb))
                      (puthash (intern ref-tweet-id) (cons ref-username ref-text)
                               nntwitter-oob-hashtb))
                    (cond (parent-tweet
                           (setf (nth idx care-about)
                                 (json-add-to-object (nth idx care-about)
                                                     "in_reply_to_user_name" ref-username))
                           (setf (nth idx care-about)
                                 (json-add-to-object (nth idx care-about)
                                                     "in_reply_to_id" ref-tweet-id)))
                          (retweeted-tweet
                           (setf (nth idx care-about)
                                 (json-add-to-object (nth idx care-about)
                                                     "retweet_id" ref-tweet-id)))
                          (quoted-tweet
                           (setf (nth idx care-about)
                                 (json-add-to-object (nth idx care-about)
                                                     "quoted_id" ref-tweet-id)))))))
              (puthash (intern group)
                       (nconc (nntwitter-get-headers group) care-about)
                       nntwitter-headers-hashtb))))))
      ;; remind myself how this works:
      ;; old-praw (1 - 20=emkdjrx)
      ;; read-ranges (1 - 10)                   (15 - 20)
      ;; unread-ranges       (11, 12, 13, 14)
      ;; new-praw    (12 13 14 15 16 17 18 19 20 - 100)
      ;; 20=emkdjrx in old-praw is 9=emkdjrx in new-praw.  index shift is 20-9=+11
      ;; new-unread-ranges   (0,  1,   2,  3)
      ;; new-read-ranges                        (4 - 9)
      (when (gnus-group-entry gnus-newsgroup-name)
        (let* ((headers (nntwitter-get-headers group))
               (num-headers (length headers))
               (newsrc-unread-index-now
                (if (not (stringp newsrc-unread-id))
                    ;; unread-indices are one-indexed !
                    1
                  (cl-loop with cand
                           for header in headers
                           for i = 1 then (1+ i)
			   ;; on 20211020 I saw evilhag ricarlo
			   ;; 1448368200656162816 duped, which
			   ;; resulted in the second one appearing
			   ;; unread
                           if (equal (assoc-default 'id header) newsrc-unread-id)
                           do (gnus-message 7 "nntwitter-incoming: exact=%s" i)
                           ;; skip outer finally
                           and return (cl-loop
                                       for j from (length headers) downto i
                                       for header = (nth (1- j) headers)
                                       until (equal (assoc-default 'id header)
                                                      newsrc-unread-id)
                                       finally return
                                       (prog1 j
                                         (when (/= i j)
                                           (gnus-message
                                            7 "nntwitter-incoming: duped-id=%s duped-index=%s -> %s"
                                            newsrc-unread-id i j))))
                           end
                           if (and (null cand)
                                   (string> (assoc-default 'id header) newsrc-unread-id))
                           do (gnus-message 7 "nntwitter-incoming: cand=%s" (setq cand i))
                           end
                           finally return (or cand 1))))
               (delta (if newsrc-unread-index
                          (max 0 (- newsrc-unread-index newsrc-unread-index-now))
                        0))
               (newsrc-read-ranges-shifted
                (nntwitter--shift-ranges delta newsrc-read-ranges))
               (newsrc-mark-ranges-shifted
                (mapcar (lambda (what-ranges)
                          (cl-case (car what-ranges)
                            (seen `(seen (1 . ,num-headers)))
                            (t (cons (car what-ranges)
                                     (nntwitter--shift-ranges delta (cdr what-ranges))))))
                        newsrc-mark-ranges)))
          (gnus-message 7 "nntwitter-incoming: unread-id=%s unread-index=%s -> %s"
                        newsrc-unread-id newsrc-unread-index newsrc-unread-index-now)
          (gnus-message 7 "nntwitter-incoming: read-ranges=%s shifted-read-ranges=%s"
                        newsrc-read-ranges newsrc-read-ranges-shifted)
          (gnus-message 7 "nntwitter-incoming: mark-ranges=%s shifted-mark-ranges=%s"
                        newsrc-mark-ranges newsrc-mark-ranges-shifted)
          (setf (gnus-info-read info) newsrc-read-ranges-shifted)
          (gnus-info-set-marks info newsrc-mark-ranges-shifted)
          (while (assq 'last-unread params)
            (gnus-alist-pull 'last-unread params))
          (gnus-info-set-params
           info
           (cons `(last-unread ,newsrc-unread-index-now . ,newsrc-unread-id) params)
           t)
          (unless (listp (gnus-info-method info))
            (gnus-info-set-method info (gnus-group-method gnus-newsgroup-name) t))
          (gnus-set-info (gnus-info-group info) info)
          (gnus-message 7 "nntwitter-incoming: new info=%s" info))))))

(defun nntwitter-dump-diagnostics (&optional server)
  "Makefile recipe test-run.  SERVER is usually nntwitter-default."
  (nntwitter--normalize-server)
  (dolist (b `(,byte-compile-log-buffer
               ,gnus-group-buffer
               "*Messages*"
               ,(format " *%s*" server)
               ,(format " *%s-stderr*" server)))
    (when (buffer-live-p (get-buffer b))
      (princ (format "\nBuffer: %s\n%s\n\n" b (with-current-buffer b (buffer-string)))
             #'external-debugging-output))))

(defsubst nntwitter--extract-name (from)
  "String match on something looking like t1_es076hd in FROM."
  (and (stringp from) (string-match "\\(t[0-9]+_[a-z0-9]+\\)" from) (match-string 1 from)))

;; C-c C-c from followup buffer
;; message-send-and-exit
;; message-send
;; message-send-method-alist=message-send-news-function=message-send-news
;; gnus-request-post
;; nntwitter-request-post
(deffoo nntwitter-request-post (&optional server)
  (nntwitter--normalize-server)
  (let* ((ret t)
         (kwargs (make-hash-table))
         (title (or (message-fetch-field "Subject")
                    (error "`nntwitter-request-post': no subject field")))
         (link (message-fetch-field "Link"))
         (reply-p (not (null message-reply-headers)))
         (edit-name (nntwitter--extract-name (message-fetch-field "Supersedes")))
         (cancel-name (nntwitter--extract-name (message-fetch-field "Control")))
         (root-p (message-fetch-field "Reply-Root"))
         (article-number (nntwitter--current-article-number))
         (group (if (numberp article-number)
                    (gnus-group-real-name (nntwitter--current-group))
                  (or (message-fetch-field "Newsgroups")
                      (error "`nntwitter-request-post': no newsgroups field"))))
         (header (when (numberp article-number)
                   (nntwitter--get-header article-number group)))
         (body
          (save-excursion
            (save-restriction
              (message-goto-body)
              (narrow-to-region (point) (point-max))
              (buffer-string)))))
    (cond (cancel-name (nntwitter-rpc-call server nil "delete" cancel-name))
          (edit-name (nntwitter-rpc-call server nil "edit" edit-name body))
          (reply-p (if (and header (assoc-default 'id header))
                       (nntwitter-rpc-call server nil "reply"
                                          (assoc-default 'id header)
                                          body (stringp root-p))
                     (backtrace)
                     (error "`nntwitter-request-post': no current article, header=%s id=%s"
                            header
                            (when header (assoc-default 'id header)))))
          (link (let* ((parsed-url (url-generic-parse-url link))
                       (host (url-host parsed-url)))
                  (if (and (stringp host) (not (zerop (length host))))
                      (progn
                        (puthash 'url link kwargs)
                        (nntwitter-rpc-call server kwargs "submit" group title))
                    ;; gnus-error might be better here
                    (error "`nntwitter-request-post': invalid url \"%s\"" link)
                    (setq ret nil))))
          (t (puthash 'selftext body kwargs)
             (nntwitter-rpc-call server kwargs "submit" group title)))
    ret))

(defun nntwitter--browse-root (&rest _args)
  "What happens when I click on Subject."
  (-when-let* ((article-number (nntwitter--current-article-number))
               (group (gnus-group-real-name (nntwitter--current-group)))
               (header (nntwitter--get-header article-number group))
               (conversation-id (assoc-default 'conversation_id header))
               (conversation-header (nntwitter-get-header-for-id conversation-id)))
    (browse-url (format "https://www.twitter.com/%s/status/%s"
                        (assoc-default 'author_user_name conversation-header)
                        (assoc-default 'id conversation-header)))))

(defun nntwitter--header-button-alist ()
  "Construct a buffer-local `gnus-header-button-alist' for nntwitter."
  (let* ((result (copy-alist gnus-header-button-alist))
         (references-value (assoc-default "References" result
                                          (lambda (x y) (string-match-p y x))))
         (references-key (car (rassq references-value result))))
    (setq result (cl-delete "^Subject:" result :test (lambda (x y) (cl-search x (car y)))))
    (setq result (cl-delete references-key result :test (lambda (x y) (cl-search x (car y)))))
    (push (append '("^\\(Message-I[Dd]\\|^In-Reply-To\\):") references-value) result)
    (push '("^Subject:" ".+" 0 (>= gnus-button-browse-level 0)
            nntwitter--browse-root 0)
          result)
    result))

(defun nntwitter-sort-by-number-of-articles-in-thread (t1 t2)
  "Whichever of the T1 or T2 has the most articles."
  (> (gnus-summary-number-of-articles-in-thread t1)
     (gnus-summary-number-of-articles-in-thread t2)))

(defun nntwitter-gather-threads-by-references (threads)
  "Gather THREADS by root reference, and don't be incomprehensible or buggy.
The built-in `gnus-gather-threads-by-references' is both."
  (cl-flet ((special-case
	     (thread)
	     (let ((header (cl-first thread)))
	       (if (stringp header)
		   thread
		 (list (mail-header-subject header) thread))))
	    (has-refs
	     (thread)
	     (let ((header (cl-first thread)))
	       (gnus-split-references (mail-header-references header)))))
    (let ((threads-by-ref (make-hash-table))
	  (separated (-separate #'has-refs threads))
	  result)
      (dolist (thread (cl-second separated))
	(let* ((header (cl-first thread))
	       (id (mail-header-id header))
	       (thread-special (special-case thread)))
	  (push thread-special result)
	  (puthash id thread-special threads-by-ref)))
      (dolist (thread (cl-first separated))
	(let* ((header (cl-first thread))
	       (refs (gnus-split-references (mail-header-references header)))
	       (ref-thread (cl-some (lambda (ref)
				      (gethash ref threads-by-ref))
				    refs)))
	  (if ref-thread
	      (setcdr ref-thread (nconc (cdr ref-thread) (list thread)))
	    (setq ref-thread (special-case thread))
	    (push ref-thread result)
	    (puthash (car refs) ref-thread threads-by-ref))))
      (nreverse result))))

(defsubst nntwitter--fallback-link ()
  "Cannot render submission."
  (let* ((group (gnus-group-real-name (nntwitter--current-group)))
         (header (nntwitter--get-header (nntwitter--current-article-number) group))
         (body (nntwitter-get-text (assoc-default 'id header))))
    (with-current-buffer gnus-original-article-buffer
      (article-goto-body)
      (delete-region (point) (point-max))
      (when body
        (insert (nntwitter--br-tagify body))))))

(defalias 'nntwitter--display-article
  (lambda (article &optional all-headers _header)
    (condition-case err
        (gnus-article-prepare article all-headers)
      (error
       (if nntwitter-render-submission
           (progn
             (gnus-message 7 "nntwitter--display-article: '%s' (falling back...)"
                           (error-message-string err))
             (nntwitter--fallback-link)
             (gnus-article-prepare article all-headers))
         (error (error-message-string err))))))
  "In case of shr failures, dump original link.")

(defsubst nntwitter--dense-time (time)
  "Convert TIME to a floating point number.

Written by John Wiegley (https://github.com/jwiegley/dot-emacs)."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defalias 'nntwitter--format-time-elapsed
  (lambda (header)
    (condition-case nil
        (let ((date (mail-header-date header)))
          (if (> (length date) 0)
              (let*
                  ((then (nntwitter--dense-time
                          (apply #'encode-time (parse-time-string date))))
                   (now (nntwitter--dense-time (current-time)))
                   (diff (- now then))
                   (str
                    (cond
                     ((>= diff (* 86400.0 7.0 52.0))
                      (if (>= diff (* 86400.0 7.0 52.0 10.0))
                          (format "%3dY" (floor (/ diff (* 86400.0 7.0 52.0))))
                        (format "%3.1fY" (/ diff (* 86400.0 7.0 52.0)))))
                     ((>= diff (* 86400.0 30.0))
                      (if (>= diff (* 86400.0 30.0 10.0))
                          (format "%3dM" (floor (/ diff (* 86400.0 30.0))))
                        (format "%3.1fM" (/ diff (* 86400.0 30.0)))))
                     ((>= diff (* 86400.0 7.0))
                      (if (>= diff (* 86400.0 7.0 10.0))
                          (format "%3dw" (floor (/ diff (* 86400.0 7.0))))
                        (format "%3.1fw" (/ diff (* 86400.0 7.0)))))
                     ((>= diff 86400.0)
                      (if (>= diff (* 86400.0 10.0))
                          (format "%3dd" (floor (/ diff 86400.0)))
                        (format "%3.1fd" (/ diff 86400.0))))
                     ((>= diff 3600.0)
                      (if (>= diff (* 3600.0 10.0))
                          (format "%3dh" (floor (/ diff 3600.0)))
                        (format "%3.1fh" (/ diff 3600.0))))
                     ((>= diff 60.0)
                      (if (>= diff (* 60.0 10.0))
                          (format "%3dm" (floor (/ diff 60.0)))
                        (format "%3.1fm" (/ diff 60.0))))
                     (t
                      (format "%3ds" (floor diff)))))
                   (stripped
                    (replace-regexp-in-string "\\.0" "" str)))
                (concat (cond
                         ((= 2 (length stripped)) "  ")
                         ((= 3 (length stripped)) " ")
                         (t ""))
                        stripped))))
      ;; print some spaces and pretend nothing happened.
      (error "    ")))
  "Return time elapsed since HEADER was sent.

Written by John Wiegley (https://github.com/jwiegley/dot-emacs).")

;; Evade melpazoid!
(funcall #'fset 'gnus-user-format-function-S
         (symbol-function 'nntwitter--format-time-elapsed))

(add-to-list
 'gnus-parameters
 `("^nntwitter"
   (gnus-summary-make-false-root 'adopt)
   (gnus-cite-hide-absolute 5)
   (gnus-cite-hide-percentage 0)
   (gnus-cited-lines-visible '(2 . 2))
   (gnus-auto-extend-newsgroup nil)
   (gnus-add-timestamp-to-message t)
   (gnus-summary-line-format "%3t%U%R%uS %I%(%*%-10,10f  %s%)\n")
   (gnus-thread-sort-functions (quote (gnus-thread-sort-by-number)))
   ;; (gnus-thread-sort-functions (quote (nntwitter-sort-by-number-of-articles-in-thread)))
   ;; (gnus-subthread-sort-functions (quote (gnus-thread-sort-by-number)))
   (gnus-summary-display-article-function
    (quote ,(symbol-function 'nntwitter--display-article)))
   (gnus-header-button-alist
    (quote ,(nntwitter--header-button-alist)))
   (gnus-visible-headers ,(concat gnus-visible-headers "\\|^Score:"))))

(nnoo-define-skeleton nntwitter)

(defun nntwitter-article-mode-activate ()
  "Augment the `gnus-article-mode-map' conditionally."
  (when (nntwitter--gate)
    (nntwitter-article-mode)))

(defun nntwitter-summary-mode-activate ()
  "Shadow some bindings in `gnus-summary-mode-map' conditionally."
  (when (nntwitter--gate)
    (nntwitter-summary-mode)))

(defun nntwitter-group-mode-activate ()
  "Augment the `gnus-group-mode-map' unconditionally."
  (if gnus-group-change-level-function
      (add-function :after gnus-group-change-level-function
                    #'nntwitter-update-subscription)
    (setq gnus-group-change-level-function #'nntwitter-update-subscription)))

(defmacro nntwitter--maphash (func table)
  "Map FUNC taking key and value over TABLE, return nil.
Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal
hashtables."
  (declare (indent nil))
  (let ((workaround 'gnus-gethash-safe))
    `(,(if (fboundp 'gnus-gethash-safe)
           'mapatoms
         'maphash)
      ,(if (fboundp 'gnus-gethash-safe)
           `(lambda (k) (funcall
                         (apply-partially
                          ,func
                          (symbol-name k) (,workaround k ,table))))
         func)
      ,table)))

(defsubst nntwitter-hash-values (table-or-obarray)
  "Return right hand sides in TABLE-OR-OBARRAY."
  (let (result)
    (nntwitter--maphash (lambda (_key value) (push value result)) table-or-obarray)
    result))

(defsubst nntwitter-hash-keys (table-or-obarray)
  "Return left hand sides in TABLE-OR-OBARRAY."
  (let (result)
    (nntwitter--maphash (lambda (key _value) (push key result)) table-or-obarray)
    result))

(defun nntwitter-update-unread-params ()
  "Save last-unread tweet id in `gnus-save-newsrc-hook'."
  (mapc
   (lambda (group)
     (let* ((method (gnus-group-method group))
            (backend (car-safe method)))
       (when (eq 'nntwitter (if (consp backend) (car backend) backend))
         (let* ((info (gnus-get-info group))
                (gnus-newsgroup-name (gnus-info-group info))
                (params (gnus-info-params info))
                (newsrc-read-ranges (gnus-info-read info)))
           (nntwitter--with-group nil
             (while (assq 'last-unread params)
               (gnus-alist-pull 'last-unread params))
             (-when-let* ((headers (nntwitter-get-headers group))
                          (num-headers (length headers))
                          (complement (gnus-uncompress-range (list `(1 . ,num-headers))))
                          (updated-unread-index
                           (or (car (gnus-list-range-difference complement
                                                                newsrc-read-ranges))
                               num-headers))
                          (updated-unread-id (awhen (nth (1- updated-unread-index) headers)
                                               (assoc-default 'id it))))
               (gnus-info-set-params
                info
                (cons `(last-unread ,updated-unread-index . ,updated-unread-id) params)
                t)
               (gnus-set-info gnus-newsgroup-name info)))))))
   (nntwitter-hash-keys gnus-newsrc-hashtb)))

;; I believe I did try buffer-localizing hooks, and it wasn't sufficient
(add-hook 'gnus-article-mode-hook #'nntwitter-article-mode-activate)
(add-hook 'gnus-group-mode-hook #'nntwitter-group-mode-activate)
(add-hook 'gnus-summary-mode-hook #'nntwitter-summary-mode-activate)
(add-hook 'gnus-save-newsrc-hook #'nntwitter-update-unread-params)

;; `gnus-newsgroup-p' requires valid method post-mail to return t
(add-to-list 'gnus-valid-select-methods '("nntwitter" post-mail) t)

(add-function
 :around (symbol-function 'message-followup)
 (lambda (f &rest args)
   (let ((twitter-from (and (nntwitter--gate) (message-make-from))))
     (prog1 (apply f args)
       (when twitter-from
         (save-excursion
           (message-replace-header "From" twitter-from)))))))

(add-function
 :around (symbol-function 'message-supersede)
 (lambda (f &rest args)
   (cond ((nntwitter--gate)
          (add-function :override
                        (symbol-function 'mml-insert-mml-markup)
                        'ignore)
          (unwind-protect
              (prog1 (apply f args)
                (remove-function (symbol-function 'mml-insert-mml-markup) 'ignore)
                (save-excursion
                  (save-restriction
                    (message-replace-header "From" (message-make-from))
                    (message-goto-body)
                    (narrow-to-region (point) (point-max))
                    (goto-char (point-max))
                    (mm-inline-text-html nil)
                    (delete-region (point-min) (point)))))
            (remove-function (symbol-function 'mml-insert-mml-markup) 'ignore)))
         (t (apply f args)))))

(add-function
 :around (symbol-function 'message-send-news)
 (lambda (f &rest args)
   (cond ((nntwitter--gate)
          (let* ((dont-ask (lambda (prompt)
                             (when (cl-search "mpty article" prompt) t)))
                 (link-p (not (null (message-fetch-field "Link"))))
                 (message-shoot-gnksa-feet (if link-p t message-shoot-gnksa-feet)))
            (unwind-protect
                (progn
                  (when link-p
                    (add-function :before-until (symbol-function 'y-or-n-p) dont-ask))
                  (apply f args))
              (remove-function (symbol-function 'y-or-n-p) dont-ask))))
         (t (apply f args)))))

(add-function
 :around (symbol-function 'gnus-summary-post-news)
 (lambda (f &rest args)
   (cond ((nntwitter--gate)
          (let* ((nntwitter-post-type (read-char-choice "[l]ink / [t]ext: " '(?l ?t)))
                 (link-header (apply-partially #'message-add-header "Link: https://"))
                 (add-link-header (apply-partially #'add-hook
                                                   'message-header-setup-hook
                                                   link-header))
                 (remove-link-header (apply-partially #'remove-hook
                                                      'message-header-setup-hook
                                                      link-header))
                 (twitter-from (message-make-from)))
            (cl-case nntwitter-post-type
              (?l (funcall add-link-header)))
            (unwind-protect
                (prog1 (apply f args)
                  (when twitter-from
                    (save-excursion
                      (message-replace-header "From" twitter-from))))
              (funcall remove-link-header))))
         (t (apply f args)))))

(add-function
 :filter-return (symbol-function 'message-make-fqdn)
 (lambda (val)
   (if (and (nntwitter--gate)
            (cl-search "--so-tickle-me" val))
       "twitter.com" val)))

(add-function
 :before-until (symbol-function 'message-make-from)
 (lambda (&rest _args)
   (when (nntwitter--gate)
     (concat (nntwitter-rpc-call nil nil "user_attr" "name") "@twitter.com"))))

(add-function
 :around (symbol-function 'message-is-yours-p)
 (lambda (f &rest args)
   (let ((concat-func (lambda (f &rest args)
                       (let ((fetched (apply f args)))
                         (if (equal (car args) "from")
                             (concat fetched "@twitter.com")
                           fetched)))))
     (when (nntwitter--gate)
       (add-function :around
                     (symbol-function 'message-fetch-field)
                     concat-func))
     (unwind-protect
         (apply f args)
       (remove-function (symbol-function 'message-fetch-field) concat-func)))))

(add-function
 :around (symbol-function 'url-http-generic-filter)
 (lambda (f &rest args)
   (cond ((nntwitter--gate)
          (condition-case err
              (apply f args)
            (error (gnus-message 7 "url-http-generic-filter: %s"
                                 (error-message-string err)))))
         (t (apply f args)))))

;; the let'ing to nil of `gnus-summary-display-article-function'
;; in `gnus-summary-select-article' dates back to antiquity.
(add-function
 :around (symbol-function 'gnus-summary-display-article)
 (lambda (f &rest args)
   (cond ((nntwitter--gate)
          (let ((gnus-summary-display-article-function
                 (symbol-function 'nntwitter--display-article)))
            (apply f args)))
         (t (apply f args)))))

;; Lars rejected my change for vectorizing `gnus-group-change-level-functions'
(add-function
 :after (symbol-function 'gnus-topic-change-level)
 (lambda (&rest args)
   ;; nntwitter-update-subscription calls nntwitter--gate
   (apply #'nntwitter-update-subscription args)))

;; disallow caching as the article numbering is wont to change
;; after PRAW restarts!
(setq gnus-uncacheable-groups
      (aif gnus-uncacheable-groups
          (format "\\(%s\\)\\|\\(^nntwitter\\)" it)
        "^nntwitter"))

(provide 'nntwitter)

;;; nntwitter.el ends here

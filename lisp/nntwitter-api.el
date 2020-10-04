;;; nntwitter-api.el --- API-related  -*- lexical-binding: t; coding: utf-8 -*-

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

(require 'gnus)
(require 'gnus-start)
(require 'dash)
(require 'warnings)
(require 'request)
(require 'anaphora)

(defcustom nntwitter-api-pleb-inclusivity nil
  "Do I care about the replies of non-friends?  Defaults to No."
  :group 'nntwitter
  :type '(alist	:key-type (string :tag "Friend")
		:value-type (choice (const :tag "Yes" t)
                                    (const :tag "Exclude replies to retweets" somewhat)
                                    (const :tag "No" nil))))

(defconst nntwitter-api-endpoint "https://41owt9pwx7.execute-api.us-east-2.amazonaws.com/dev")

(cl-defun nntwitter-api-cb-error-route (whence &key data error-thrown &allow-other-keys)
  "Errback for function WHENCE surfacing DATA and ERROR-THROWN."
  (gnus-message 3 "`%s': %s: %S" whence (error-message-string error-thrown) data))

(defun nntwitter-api-squirrel (access-token access-token-secret)
  "Persist ACCESS-TOKEN and ACCESS-TOKEN-SECRET to a sensible location."
  (let* ((data-home (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share")))
         (token-dir (concat (file-name-as-directory data-home) "nntwitter")))
    (unless (file-directory-p token-dir)
      (make-directory token-dir t))
    (unless (equal "700" (format "%o" (file-modes token-dir)))
      (set-file-modes token-dir #o700))
    (with-temp-file (concat (file-name-as-directory token-dir) "access_token")
      (insert access-token "\n")
      (insert access-token-secret "\n"))))

(defsubst nntwitter-api--works-p ()
  "Bug#43834 stymies `emacs -f gnus'.  Return nil if workaround necessary."
  (or noninteractive (>= emacs-major-version 28) (equal "dumb" (getenv "TERM"))))

(defun nntwitter-api-route-auth ()
  "Have user login via browser.
Then have user enter PIN from resulting webpage into Emacs,
then call `nntwitter-api-route-grant' for next steps."
  (interactive)
  (request (concat (file-name-as-directory nntwitter-api-endpoint) "route_auth")
    :type "POST"
    :sync t
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (aif (assoc-default 'url data)
                    (let* ((parsed-url (url-generic-parse-url it))
                           (query (cdr (url-path-and-query parsed-url)))
                           (token (seq-some
                                   (lambda (x)
                                     (cl-destructuring-bind (key value)
                                         (split-string x "=")
                                       (when (equal key "oauth_token")
                                         value)))
                                   (split-string query "&")))
                           success-p
                           (callback (cl-function
                                      (lambda (&key data &allow-other-keys)
                                        (-let (((&alist 'access_token access-token
                                                        'access_token_secret access-token-secret)
                                                data))
                                          (if (and access-token access-token-secret)
                                              (progn
                                                (nntwitter-api-squirrel access-token access-token-secret)
                                                (setq success-p t))
                                            (message "`nntwitter-api-route-auth': no token in %s" data)))))))
                      (browse-url it)
                      (let* ((preface "nntwitter: ")
                             (warning-levels `((:warning ,(format "%s%%s" preface))))
                             (warning-type-format "%s"))
                        (display-warning
                         'nntwitter
                         (format "%s\n%s%s"
                                 "Please check your browser and follow the instructions."
                                 (make-string (length preface) ? )
                                 "Then enter the OAuth PIN below.")
                         :warning))
                      (with-local-quit
                        (cl-loop for i from 1 to 2
                                 for pin = (read-no-blanks-input (if (= i 1) "PIN: " "Retry: "))
                                 do (nntwitter-api-route-grant token pin callback)
                                 until success-p))
                      (unless success-p
                        (message "`nntwitter-api-route-auth': grant failed")))
                  (message "`nntwitter-api-route-auth': no url in %s" data))))
    :error (apply-partially #'nntwitter-api-cb-error-route "nntwitter-api-route-auth")))

(defun nntwitter-api-route-grant (token pin callback)
  "Got intermediary TOKEN and PIN, now acquire access token for CALLBACK."
  (request (concat (file-name-as-directory nntwitter-api-endpoint) "route_grant")
    :type "POST"
    :sync t
    :parser 'json-read
    :data `(("oauth_verifier" . ,pin)
            ("oauth_token" . ,token))
    :success callback
    :error (apply-partially #'nntwitter-api-cb-error-route "nntwitter-api-route-grant")))

(defvar nntwitter-api--access-token nil)
(defvar nntwitter-api--access-token-secret nil)

(defvar nntwitter-api--conversation-ids (make-hash-table))

(cl-defun nntwitter-api-set-oauth-tokens (&key (force nil) (acquire-p t))
  "FORCE if non-nil reqacuires auth (no matter what).
ACQUIRE-P if nil refuses reacquisition (no matter what)."
  (let* ((data-home (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share")))
         (token-dir (concat (file-name-as-directory data-home) "nntwitter")))
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents (concat (file-name-as-directory token-dir) "access_token"))
        (cl-destructuring-bind (access-token* access-token-secret*)
            (split-string (buffer-string) "\n" t)
          (setq nntwitter-api--access-token access-token*
                nntwitter-api--access-token-secret access-token-secret*))))
    (when (or force
              (and acquire-p (or (not nntwitter-api--access-token)
                                 (not nntwitter-api--access-token-secret))))
      (nntwitter-api-route-auth)
      (nntwitter-api-set-oauth-tokens :force nil :acquire-p nil))
    (unless (and nntwitter-api--access-token nntwitter-api--access-token-secret)
      (error "`nntwitter-api-set-oauth-tokens': authentication failure"))))

(defun nntwitter-api-route-recent-search (group since-id callback)
  "Get tweets for screen-name GROUP after SINCE-ID for CALLBACK.

https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-recent."
  (let ((since-id-binding (when since-id `(("since_id" . ,since-id)))))
    (request (concat (file-name-as-directory nntwitter-api-endpoint) "route_recent_search")
      :type "POST"
      :sync t
      :parser 'json-read
      :data `(("access_token" . ,nntwitter-api--access-token)
              ("access_token_secret" . ,nntwitter-api--access-token-secret)
              ("screen_name" . ,group)
              ("include_to" . ,(cl-case (assoc-default group nntwitter-api-pleb-inclusivity)
                                 ((somewhat t) "yes")
                                 (otherwise "no")))
              ,@since-id-binding)
      :success callback
      :error (apply-partially #'nntwitter-api-cb-error-route "nntwitter-api-route-recent-search"))))

(defun nntwitter-api-route-tweets-lookup (ids callback)
  "Get texts for IDS for CALLBACK.

https://developer.twitter.com/en/docs/twitter-api/tweets/lookup/api-reference."
  (request (concat (file-name-as-directory nntwitter-api-endpoint) "route_tweets_lookup")
    :type "POST"
    :sync t
    :parser 'json-read
    :data `(("access_token" . ,nntwitter-api--access-token)
            ("access_token_secret" . ,nntwitter-api--access-token-secret)
            ("ids" . ,(mapconcat #'identity ids ",")))
    :success callback
    :error (apply-partially #'nntwitter-api-cb-error-route "nntwitter-api-route-tweets-lookup")))

(defun nntwitter-api-route-user-timeline (group callback)
  "Get API v1 timeline for screen-name GROUP for CALLBACK.

https://developer.twitter.com/en/docs/twitter-api/v1/tweets/timelines/api-reference/get-statuses-user_timeline."
  (request (concat (file-name-as-directory nntwitter-api-endpoint) "route_user_timeline")
    :type "POST"
    :sync t
    :parser 'json-read
    :data `(("access_token" . ,nntwitter-api--access-token)
            ("access_token_secret" . ,nntwitter-api--access-token-secret)
            ("screen_name" . ,group))
    :success callback
    :error (apply-partially #'nntwitter-api-cb-error-route "nntwitter-api-route-user-timeline")))

(defun nntwitter-api-route-friends-list (callback)
  "Get the people you're following for CALLBACK.

https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/follow-search-get-users/api-reference/get-friends-list."
  (nntwitter-api-set-oauth-tokens)
  (request (concat (file-name-as-directory nntwitter-api-endpoint) "route_friends_list")
           :type "POST"
           :sync t
           :parser 'json-read
           :data `(("access_token" . ,nntwitter-api--access-token)
                   ("access_token_secret" . ,nntwitter-api--access-token-secret))
           :success callback
           :error (apply-partially #'nntwitter-api-cb-error-route "nntwitter-api-route-friends-list")))

(provide 'nntwitter-api)

;;; nntwitter-api.el ends here

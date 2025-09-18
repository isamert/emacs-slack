;;; slack-scheduled-messages-buffer.el --- List and manage scheduled drafts -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:  <andrea-dev@hotmail.com>
;;; Commentary:
;;
;; This buffer allows you to list, schedule, and delete scheduled messages (drafts) for Slack.
;; Invoke with =slack-scheduled-messages-show=.
;; Schedule a new message with =slack-schedule-message=.
;; This version uses the internal drafts API.

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-team)
(require 'slack-room)
(require 'dash)
(require 's)

;; Internal API Endpoints
(defvar slack-drafts-create-url "https://slack.com/api/drafts.create")
(defvar slack-drafts-list-url "https://slack.com/api/drafts.list")
(defvar slack-drafts-delete-url "https://slack.com/api/drafts.delete")

;; A fixed boundary string for multipart requests, similar to slack-activity-feed-buffer.el
(defvar slack-scheduled-messages--boundary "----WebKitFormBoundaryEmacsSlackScheduled"
  "Boundary string for multipart/form-data requests.")

;;; API Request Functions

(defun slack--build-multipart-part (name value)
  "Helper to build one part of a multipart/form-data body."
  (format "Content-Disposition: form-data; name=\"%s\"\r\n\r\n%s" name value))

(defun slack--build-multipart-body (parts)
  "Build the full multipart/form-data body from an alist of PARTS."
  (let* ((boundary slack-scheduled-messages--boundary)
         (body-parts (--map (slack--build-multipart-part (car it) (cdr it)) parts)))
    (concat "--" boundary
            (format "\r\n%s\r\n" (s-join (format "\r\n--%s\r\n" boundary) body-parts))
            "--" boundary "--\r\n")))

(defun slack-schedule-message-request (team channel-id text post-at after-success)
  "Create a scheduled draft with TEXT for CHANNEL-ID in TEAM at POST-AT time."
  (let ((data-parts
         `(("token" . ,(oref team :token))
           ("blocks" . ,(format "[{\"type\":\"rich_text\",\"elements\":[{\"type\":\"rich_text_section\",\"elements\":[{\"type\":\"text\",\"text\":\"%s\"}]}]}]" text))
           ("destinations" . ,(format "[{\"channel_id\": \"%s\"}]" channel-id))
           ("date_scheduled" . ,post-at)
           ("is_from_composer" . "true")
           ("_x_reason" . "schedule-draft")
           ("_x_mode" . "online")
           ("_x_sonic" . "true")
           ("client_msg_id" . ,(with-temp-buffer (uuidgen t) (buffer-string)))
           ("file_ids" . "[]"))))
    (slack-request
     (slack-request-create
      slack-drafts-create-url team
      :type "POST"
      :headers `(("content-type" . ,(format "multipart/form-data; boundary=%s" slack-scheduled-messages--boundary)))
      :data (slack--build-multipart-body data-parts)
      :success after-success
      :error (lambda (&rest data)
               (slack-log (format "Error scheduling message: %s" data) team :level 'error))
      ))))

(defun slack-list-scheduled-messages-request (team after-success)
  "Request a list of scheduled drafts for TEAM."
  (let ((data-parts
         `(("token" . ,(oref team :token))
           ("is_active" . "true")
           ("limit" . "100")
           ("_x_reason" . "client-v2-boot-team")
           ("_x_mode" . "online")
           ("_x_sonic" . "true"))))
    (slack-request
     (slack-request-create
      slack-drafts-list-url team
      :type "POST"
      :headers `(("content-type" . ,(format "multipart/form-data; boundary=%s" slack-scheduled-messages--boundary)))
      :data (slack--build-multipart-body data-parts)
      :success after-success))))

(defun slack-delete-scheduled-message-request (team draft-id last-updated-ts after-success)
  "Delete a scheduled draft with DRAFT-ID in TEAM."
  (let ((data-parts
         `(("token" . ,(oref team :token))
           ("draft_id" . ,draft-id)
           ("_x_reason" . "DeleteDraftModal")
           ("_x_mode" . "online")
           ("_x_sonic" . "true")
           ("_x_app_name" . "client")
           ("client_last_updated_ts" . ,(let* ((str (s-join "" (s-split "\\." last-updated-ts))) ;; for some reason the . for these ts has always to leave 3 final numbers
                                               (pos (- (length str) 3)))
                                          (concat (s-left pos str) "." (s-right (- (length str) pos) str)))))))
    (slack-request
     (slack-request-create
      slack-drafts-delete-url team
      :type "POST"
      :headers `(("content-type" . ,(format "multipart/form-data; boundary=%s" slack-scheduled-messages--boundary)))
      :data (slack--build-multipart-body data-parts)
      :success after-success))))

;;; Data and Buffer Classes

(defclass slack-scheduled-message ()
  ((draft-id :initarg :draft-id :type string)
   (channel-id :initarg :channel-id :type string)
   (post-at :initarg :post-at :type integer)
   (last-updated-ts :initarg :last-updated-ts :type string)
   (text :initarg :text :type string)))

(defclass slack-scheduled-messages-buffer (slack-buffer)
  ((messages :initarg :messages :type list)))

(define-derived-mode slack-scheduled-messages-buffer-mode slack-buffer-mode "Slack Scheduled"
  "Major mode for listing scheduled Slack messages (drafts).")

;;; Class Methods

(cl-defmethod slack-buffer-name ((_class slack-scheduled-messages-buffer) team)
  (format "*slack %s Scheduled Msgs*" (oref team name)))

(cl-defmethod slack-buffer-name ((this slack-scheduled-messages-buffer))
  (format "*slack %s Scheduled Msgs*" (slack-team-name (slack-buffer-team this))))

(cl-defmethod slack-buffer-key ((_class slack-scheduled-messages-buffer))
  "scheduled-messages")

(cl-defmethod slack-buffer-key ((_class slack-scheduled-messages-buffer) &rest x)
  "scheduled-messages")

(cl-defmethod slack-team-buffer-key ((_class slack-scheduled-messages-buffer))
  'slack-scheduled-messages-buffer)

(cl-defmethod slack-scheduled-message-to-string ((msg slack-scheduled-message) team)
  "Format a scheduled message for display."
  (with-slots (draft-id channel-id post-at last-updated-ts text) msg
    (let ((room (slack-room-find channel-id team)))
      (propertize
       (format "#%s at %s: %s"
               (or (and room (slack-room-name room team)) channel-id "unknown-channel")
               (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time post-at))
               text)
       'team-id (oref team id)
       'channel-id channel-id
       'draft-id draft-id
       'last-updated-ts last-updated-ts))))

(cl-defmethod slack-buffer-insert ((this slack-scheduled-messages-buffer) msg)
  "Insert a single scheduled message MSG into the buffer THIS."
  (let ((team (slack-buffer-team this)))
    (lui-insert (slack-scheduled-message-to-string msg team))
    (lui-insert "\n\n")))

(cl-defmethod slack-buffer-init-buffer ((this slack-scheduled-messages-buffer))
  "Initialize the scheduled messages buffer."
  (let ((buffer (cl-call-next-method)))
    (with-current-buffer buffer
      (slack-scheduled-messages-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (with-slots (messages) this
        (if messages
            (cl-loop for msg in messages do (slack-buffer-insert this msg))
          (lui-insert "(No scheduled messages.)\n")))
      (setq-local mode-line-format
                  '(" "
                    mode-line-buffer-identification "   "
                    "(=d=elete, =g= refresh, =q= quit)")))
    buffer))



(cl-defmethod slack-buffer-has-next-page-p ((_this slack-scheduled-messages-buffer)))

(cl-defmethod slack-buffer-insert-history ((_this slack-scheduled-messages-buffer)))

(cl-defmethod slack-buffer-request-history ((_this slack-scheduled-messages-buffer) _after-success))

(cl-defmethod slack-buffer-loading-message-end-point ((_this slack-scheduled-messages-buffer)))

(cl-defmethod slack-buffer-delete-load-more-string ((_this slack-scheduled-messages-buffer)))

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-scheduled-messages-buffer)))

(cl-defmethod slack-buffer-insert--history ((_this slack-activity-feed-buffer)))


;;; Interactive Functions

(defun slack-scheduled-messages-show ()
  "Show scheduled messages (drafts) in a dedicated buffer."
  (interactive)
  (let ((team slack-current-team))
    (slack-list-scheduled-messages-request
     team
     (lambda (&rest data)
       (let* ((all-drafts (plist-get (plist-get data :data) :drafts))
              (messages (--keep
                         (let* ((draft-id (plist-get it :id))
                                (date-scheduled (plist-get it :date_scheduled))
                                (last-updated-ts (plist-get it :last_updated_ts))
                                (text (or (--> (plist-get it :blocks) car (plist-get it :elements) car (plist-get it :elements) car (plist-get it :text))
                                          "??"))
                                (channel-id (or (--> (plist-get it :destinations) car (plist-get it :channel_id))
                                                "??")))
                           ;; Only process drafts that are actually scheduled for the future.
                           (when (> date-scheduled 0)
                             (make-instance 'slack-scheduled-message
                                            :draft-id draft-id
                                            :channel-id channel-id
                                            :post-at date-scheduled
                                            :last-updated-ts last-updated-ts
                                            :text text)))
                         all-drafts))
              (buffer-obj (make-instance 'slack-scheduled-messages-buffer
                                         :team-id (oref slack-current-team id) ;; TODO fix reference
                                         :messages (sort messages (lambda (a b) (< (oref a post-at) (oref b post-at)))))))
         ;; (when-let ((old-buf (slack-buffer-find 'slack-scheduled-messages-buffer slack-current-team))) ;; TODO fix this
         ;;   (kill-buffer (oref old-buf buf)))
         (slack-buffer-display buffer-obj))))))


(defun slack-schedule-message (channel-id text minutes-from-now)
  "Schedule TEXT to CHANNEL-ID in MINUTES-FROM-NOW as a draft."
  (interactive
   (list
    (oref (let ((team slack-current-team)) ;; TODO don't use slack-current-team in this buffer!
            (slack-room-select
             (cl-loop for team in (list team)
                      append (append (slack-team-ims team)
                                     (slack-team-groups team)
                                     (slack-team-channels team)))
             team))
          id)
    (read-string "Message: ")
    (read-number "Minutes from now: " 30)))
  (let* ((team slack-current-team)
         (post-at (format "%d" (floor (+ (float-time) (* minutes-from-now 60))))))
    (slack-schedule-message-request
     team
     channel-id
     text
     post-at
     (lambda (&rest data)
       (if (not (plist-get (plist-get data :data) :error))
           (progn
             (message "Message scheduled for %s" (format-time-string "%H:%M:%S" (seconds-to-time (string-to-number post-at))))
             (when (and (derived-mode-p 'slack-scheduled-messages-buffer-mode)
                        (y-or-n-p "Refresh scheduled messages list? "))
               (slack-scheduled-messages-show)))
         (message "Failed to schedule message: %S" data))))))

(defun slack-scheduled-messages-delete-at-point ()
  "Delete the scheduled message (draft) at point."
  (interactive)
  (if-let* ((team-id (get-text-property (point) 'team-id))
            (draft-id (get-text-property (point) 'draft-id))
            (team (slack-team-find team-id))
            (last-updated-ts (get-text-property (point) 'last-updated-ts)))
      (when (y-or-n-p (format "Delete scheduled message: %s?" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (slack-delete-scheduled-message-request
         team draft-id last-updated-ts
         (lambda (&rest data)
           (if (not (plist-get (plist-get data :data) :error))
               (progn
                 (message "Scheduled message deleted.")
                 (slack-scheduled-messages-show))
             (message "Failed to delete message: %S" data)))))
    (message "No scheduled message at point.")))

(define-key slack-scheduled-messages-buffer-mode-map (kbd "d") #'slack-scheduled-messages-delete-at-point)
(define-key slack-scheduled-messages-buffer-mode-map (kbd "g") #'slack-scheduled-messages-show)

(provide 'slack-scheduled-messages-buffer)
;;; slack-scheduled-messages-buffer.el ends here

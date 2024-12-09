;;; slack-activity-feed-buffer.el ---                -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  <andrea-dev@hotmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This buffer allow you to see the latest activity in slack. You can invoke it with `slack-activity-feed-show'.

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-search)
(require 'slack-room)
(require 'slack-message-buffer)
(require 'slack-team)
(require 'dash)
(require 's)

(defvar slack-activity-feed-url "https://slack.com/api/activity.feed")

(defun slack-activity-feed-request (team &optional after-success)
  "Request activity feed for CHANNEL-ID of TEAM.
Run an action on the data returned with AFTER-SUCCESS."
  (cl-labels
      ((on-success (&key data &allow-other-keys)
         (slack-request-handle-error
          (data "slack-activity-feed-request")
          (if after-success
              (funcall after-success data)))))
    (slack-request
     (slack-request-create
      slack-activity-feed-url
      team
      :type "POST"
      :success #'on-success
      :data (concat "------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"token\"\r\n\r\n" (oref team :token) "\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"limit\"\r\n\r\n20\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"types\"\r\n\r\nthread_reply,message_reaction,internal_channel_invite,list_record_edited,bot_dm_bundle,at_user,at_user_group,at_channel,at_everyone,keyword,list_record_assigned,list_user_mentioned,external_channel_invite,shared_workspace_invite,external_dm_invite\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"mode\"\r\n\r\nchrono_reads_and_unreads\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"_x_reason\"\r\n\r\nfetchActivityFeed\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"_x_mode\"\r\n\r\nonline\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"_x_sonic\"\r\n\r\ntrue\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"_x_app_name\"\r\n\r\nclient\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie--\r\n")
      :headers (list
                (cons "content-type"
                      "multipart/form-data; boundary=----WebKitFormBoundaryh7x3DqJqAIvkEcie"))))))

(defclass slack-activity-feed ()
  ((activities :initarg :activities :initform nil :type (or null list))
   (pagination :initarg :pagination :type (or null string))))

(define-derived-mode slack-activity-feed-buffer-mode slack-buffer-mode "Slack Activity Feed"
  (remove-hook 'lui-post-output-hook 'slack-display-image t))

(defclass slack-activity-feed-buffer (slack-buffer)
  ((activity-feed :initarg :activity-feed :type slack-activity-feed)))

(cl-defmethod slack-buffer-name ((_class (subclass slack-activity-feed-buffer)) team)
  (format "*slack: %s Activity Feed*"
          (oref team name)
          ))

(cl-defmethod slack-buffer-name ((this slack-activity-feed-buffer))
  (format "*slack: %s Activity Feed %s*"
          (slack-team-name (slack-buffer-team this))
          (format-time-string "%s") ;; unix seconds to make unique so that it is never reused
          ))

(cl-defmethod slack-buffer-key ((_class (subclass slack-activity-feed-buffer)))
  "activity feed")

(cl-defmethod slack-buffer-key ((this slack-activity-feed-buffer))
  (slack-buffer-key 'slack-activity-feed-buffer))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-activity-feed-buffer)))
  'slack-activity-feed-buffer)

(defun slack-create-activity-feed-buffer (activity-feed team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-activity-feed-buffer team)))
      buffer
    (make-instance 'slack-activity-feed-buffer
                   :team-id (oref team id)
                   :activity-feed activity-feed)))

(defclass activity-message ()
  ((ts :initarg :ts :type string)
   (channel :initarg :channel :type string)
   (is-broadcast :initarg :is-broadcast :type boolean)
   (thread-ts :initarg :thread-ts :type (or null string))
   (author-id :initarg :author-id :type (or null string))))

(cl-defmethod slack-activity-message-to-string ((this activity-message) team)
  "Format THIS activity-message of TEAM as a string for presentation."
  (with-slots (channel ts is-broadcast thread-ts author-id) this
    (let* ((room (slack-room-find channel team))
           (header (propertize (format "%s%s"
                                       (if (slack-channel-p room)
                                           "#" "@")
                                       (or (ignore-errors (slack-room-name room team)) "name not available - try to update channel list")
                                       )
                               'face 'slack-search-result-message-header-face)))
      (propertize (concat header
                          (when-let ((author (slack-user-name author-id team))) (format " from %s" author))
                          "\n"
                          (or
                           (condition-case err
                               (when (or ts thread-ts)
                                 (let* ((message (condition-case err
                                                     (slack-room-find-message room ts)
                                                   (error
                                                    (message "error in: %s" (error-message-string err))
                                                    nil)))
                                        (on-success-history (lambda (messages _next-cursor)
                                                              (setq message (car messages))))
                                        (on-success-replies (lambda (messages _next-cursor _more-messages)
                                                              (setq message (car messages))))
                                        (thread-ts-in-halves (s-split "\\." thread-ts))
                                        (thread-ts-second-half (nth 1 thread-ts-in-halves)))
                                   ;; TODO this block is time consuming! We could retrieve these messages in parallel using the same waiting mechanism (accept-process-output,) but waiting on the list of messages. Needs to be done in caller, possibly passing the messages as an optional context parameter.
                                   (unless message
                                     (if (and
                                          thread-ts-second-half
                                          (not (string-equal ts thread-ts)))
                                         (slack-conversations-replies room thread-ts team
                                                                      ;; :latest thread-ts
                                                                      :inclusive "true"
                                                                      :limit "1"
                                                                      :after-success on-success-replies)
                                       (slack-conversations-history room team
                                                                    :latest ts
                                                                    :inclusive "true"
                                                                    :limit "1"
                                                                    :after-success on-success-history)))
                                   (while (null message)
                                     (accept-process-output nil 0.1))
                                   (slack-message-body message team)
                                   ))
                             (error
                              (message "slack-activity-message-to-string: Loading messages failed with: %S" (error-message-string err))
                              nil))
                           "TODO")
                          )
                  'ts ts
                  'team-id (oref team id)
                  'room-id (oref room id)
                  'thread-ts thread-ts))))

(defclass activity-reaction ()
  ((user :initarg :user :type string)
   (name :initarg :name :type string)))

(cl-defmethod slack-activity-reaction-to-string ((this activity-reaction) team)
  (with-slots (user name) this
    (format "  %s reacted with :%s:"
            (slack-user-name user team)
            name
            )))

(defclass activity-item ()
  ((type :initarg :type :type string)
   (message :initarg :message :type activity-message)
   (reaction :initarg :reaction :type (or null activity-reaction))))

(cl-defmethod slack-activity-item-to-string ((this activity-item) team)
  (with-slots (type message reaction) this
    (concat
     (slack-activity-message-to-string message team)
     (when reaction (concat "\n" (slack-activity-reaction-to-string reaction team)))
     )))

(defclass slack-activity ()
  ((is-unread :initarg :is-unread :type boolean)
   (feed-ts :initarg :feed-ts :type string)
   (item :initarg :item :type activity-item)))

(cl-defmethod slack-activity-to-string ((this slack-activity) team)
  (with-slots (is-unread item) this
    (format "%s %s" (if is-unread "*" " ") (slack-activity-item-to-string item team))))

(cl-defmethod slack-buffer-insert ((this slack-activity-feed-buffer) activity)
  (let* ((team (slack-buffer-team this))
         (time (slack-ts-to-time (oref activity feed-ts)))
         (lui-time-stamp-time time)
         (lui-time-stamp-format "[%Y-%m-%d %H:%M] "))
    (lui-insert (slack-activity-to-string activity team) t)
    (lui-insert "" t)))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-activity-feed-buffer))
  "Tell if there is another page of results for THIS SLACK-ACTIVITY-FEED-BUFFER."
  (with-slots (activity-feed) this
    (oref activity-feed pagination)))

(cl-defmethod slack-buffer-insert-history ((this slack-activity-feed-buffer))
  (message "TODO handle cursor pagination")
  ;; (let* ((search-result (oref this search-result))
  ;;        (pagination (oref search-result pagination))
  ;;        (first (oref pagination first))
  ;;        (last (oref pagination last))
  ;;        (matches (last (oref search-result matches) (1+ (- last first))))
  ;;        (cur-point (point)))
  ;;   (cl-loop for match in matches
  ;;            do (slack-buffer-insert this match))
  ;;   (goto-char cur-point))
  )

(cl-defmethod slack-buffer-request-history ((this slack-activity-feed-buffer) after-success)
  (message "TODO handle cursor pagination request")
  ;; (with-slots (search-result) this
  ;;   (slack-search-request search-result after-success (slack-buffer-team this)
  ;;                         (slack-search-paging-next-page
  ;;                          (oref search-result pagination))))
  )

(cl-defmethod slack-buffer-init-buffer ((this slack-activity-feed-buffer))
  (let ((buffer (cl-call-next-method)))
    (with-current-buffer buffer
      (slack-activity-feed-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (with-slots (activity-feed) this
        (let* ((activities (oref activity-feed activities)))
          (cl-loop for m in activities
                   do (slack-buffer-insert this m)))
        (let ((lui-time-stamp-position nil))
          (if (slack-buffer-has-next-page-p this)
              (slack-buffer-insert-load-more this)))))
    buffer))

(cl-defmethod slack-buffer-loading-message-end-point ((_this slack-activity-feed-buffer))
  (previous-single-property-change (point-max)
                                   'loading-message))

(cl-defmethod slack-buffer-delete-load-more-string ((this slack-activity-feed-buffer))
  (let* ((inhibit-read-only t)
         (loading-message-end
          (slack-buffer-loading-message-end-point this))
         (loading-message-start
          (previous-single-property-change loading-message-end
                                           'loading-message)))
    (delete-region loading-message-start
                   loading-message-end)))

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-activity-feed-buffer)))

(cl-defmethod slack-buffer-insert--history ((this slack-activity-feed-buffer))
  (slack-buffer-insert-history this)
  (if (slack-buffer-has-next-page-p this)
      (slack-buffer-insert-load-more this)
    (let ((lui-time-stamp-position nil))
      (lui-insert "(no more messages)\n" t))))

(defun slack-activity-feed-show ()
  "Show Slack activity feed."
  (interactive)
  (let ((team slack-current-team))
    (cl-labels
        ((jbool (jf) (not (eq jf :json-false)))
         (after-success (data)
           (let* ((activity-feed (make-instance
                                  'slack-activity-feed
                                  :activities
                                  (--map
                                   (make-instance
                                    'slack-activity
                                    :is-unread (jbool (plist-get it :is_unread))
                                    :feed-ts (format "%s" (plist-get it :feed_ts))
                                    :item (let* ((i (plist-get it :item))
                                                 (m (plist-get i :message))
                                                 (r (plist-get i :reaction)))
                                            (make-instance
                                             'activity-item
                                             :type (plist-get i :type)
                                             :message (make-instance
                                                       'activity-message
                                                       :ts (format "%s" (plist-get m :ts))
                                                       :channel (format "%s" (plist-get m :channel))
                                                       :is-broadcast (jbool (plist-get m :is_broadcast))
                                                       :thread-ts (format "%s" (plist-get m :thread_ts))
                                                       :author-id (format "%s" (plist-get m :author_user_id)))
                                             :reaction (when r (make-instance
                                                                'activity-reaction
                                                                :user (format "%s" (plist-get r :user))
                                                                :name (format "%s" (plist-get r :name)))))))
                                   (plist-get data :items))
                                  :pagination (plist-get (plist-get data :response_metadata) :next_cursor)))
                  (buffer (slack-create-activity-feed-buffer
                           activity-feed
                           team)))
             (slack-buffer-display buffer))))
      (slack-activity-feed-request team #'after-success))))

(defun slack-activity-feed-open-message ()
  "Open message at point of activity-feed."
  (interactive)
  (if-let* ((ts (get-text-property (point) 'ts))
            (team-id (get-text-property (point) 'team-id))
            (room-id (get-text-property (point) 'room-id))
            (thread-ts (get-text-property (point) 'thread-ts))
            (team (slack-team-find team-id)))
      (slack-open-message
       team
       (slack-room-find room-id team)
       (--find (s-matches-p "[0-9]" it) (list ts))
       ;; found out that when a ts is nil, it comes "nil"
       (--find (s-matches-p "[0-9]" it) (list thread-ts)))
    (error "Not possible to jump to message")))
(define-key slack-activity-feed-buffer-mode-map (kbd "RET") 'slack-activity-feed-open-message)

(provide 'slack-activity-feed-buffer)
;;; slack-activity-feed-buffer.el ends here

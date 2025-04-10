;;; slack-stars.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'eieio)
(require 'slack-util)
(require 'slack-request)
(require 'slack-team)
(require 'slack-file)
(require 'slack-buffer)

;; right this moved to saved for later https://api.slack.com/changelog/2023-07-its-later-already-for-stars-and-reminders
;; so the star naming is just Slack legacy (and I kept to keep things working while transitioning)
(defconst slack-stars-list-url "https://slack.com/api/saved.list")

(defclass slack-star ()
  ((cursor :initarg :cursor :type (or null string) :initform nil)
   (items :initarg :items :type (or null list) :initform nil)))

(defclass slack-star-item ()
  ((item-id :initarg :item-id :type string)
   (item-type :initarg :item-type :type string)
   (date-created :initarg :date-created :type integer)
   (date-due :initarg :date-due :type integer)
   (date-completed :initarg :date-completed :type integer)
   (date-updated :initarg :date-updated :type integer)
   (is-archived :initarg :is-archived :type boolean)
   (date-snoozed-until :initarg :date-snoozed-until :type integer)
   (ts :initarg :ts :type string)
   (state :initarg :state :type string)))


(cl-defmethod slack-ts ((this slack-star-item))
  "Get THIS star item timestamp."
  (oref this ts))

(cl-defmethod slack-star-has-next-page-p ((this slack-star))
  "Are there more saved for later for THIS?"
  (not (null (oref this cursor))))

(cl-defmethod slack-star-items ((this slack-star))
  "GET THIS star items."
  (oref this items))

(cl-defmethod slack-merge ((old slack-star) new)
  "Add OLD star items to NEW ones. We keep the newer cursor."
  (with-slots (cursor items) old
    (setq cursor (oref new cursor))
    (setq items (append (oref new items) items))))

(defun slack-create-star-items (payload)
  (mapcar #'(lambda (e) (slack-create-star-item e))
          payload))

(defun slack-create-star-item (payload)
  "Make `slack-star-item' from PAYLOAD."
  (make-instance 'slack-star-item
                 :item-id (plist-get payload :item_id)
                 :item-type (plist-get payload :item_type)
                 :date-created (plist-get payload :date_created)
                 :date-due (plist-get payload :date_due)
                 :date-completed (plist-get payload :date_completed)
                 :date-updated (plist-get payload :date_updated)
                 :is-archived (when-let ((e (plist-get payload :is_archived))) (not (equal e :json-false)))
                 :date-snoozed-until (plist-get payload :date_snoozed_until)
                 :ts (plist-get payload :ts)
                 :state (plist-get payload :state)))

(defun slack-create-star (payload)
  "Make star items from PAYLOAD."
  (let ((items (slack-create-star-items (plist-get payload :saved_items)))
        (cursor (plist-get (plist-get payload :response_metadata) :next_cursor)))
    (make-instance 'slack-star
                   :items (reverse items)
                   :cursor cursor)))

(defun slack-stars-list-request (team &optional cursor after-success)
  (cl-labels
      ((callback ()
         (when (functionp after-success)
           (funcall after-success)))
       (on-success (&key data &allow-other-keys)
         (slack-request-handle-error
          (data "slack-stars-list-request")
          (let* ((star (slack-create-star data))
                 (user-ids (slack-team-missing-user-ids
                            team nil)))
            (if (oref team star)
                (if cursor
                    (slack-merge (oref team star) star)
                  (oset team star star))
              (oset team star star))
            (if (< 0 (length user-ids))
                (slack-users-info-request
                 user-ids team
                 :after-success #'(lambda () (callback)))
              (callback))))))
    (slack-request
     (slack-request-create
      slack-stars-list-url
      team
      :type "POST"
      :data (list (when cursor (cons "cursor" cursor)))
      :success #'on-success))))

(defun slack-star-api-request (url params team)
  (cl-labels
      ((on-success (&key data &allow-other-keys)
         (slack-request-handle-error
          (data url))))
    (slack-request
     (slack-request-create
      url
      team
      :params params
      :success #'on-success))))

(cl-defmethod slack-star-remove-star ((this slack-star) ts team)
  "Remove from THIS stars the star at TS for TEAM."
  (slack-if-let* ((item
                   (--find
                    (string= (oref it ts) ts)
                    (oref this items))))
      (slack-star-api-request slack-message-stars-remove-url
                              (list (cons "ts" ts)
                                    (cons "item_id" (oref item item-id))
                                    (cons "item_type" (oref item item-type)))
                              team)
    (error "Could not find star to remove for ts")))


(provide 'slack-star)
;;; slack-star.el ends here

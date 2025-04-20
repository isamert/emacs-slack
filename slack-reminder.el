;;; slack-reminder.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  南優也

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
(require 'slack-room)
(require 'slack-team)
(require 'slack-request)
(require 'slack-message-formatter)
(require 'slack-message-faces)

(defvar slack-completing-read-function)
(declare-function slack-buffer-add-star "slack-thread-message-buffer.el")

(cl-defun slack-reminder-add-from-message (_room message _team)
  ""
  (let* ((message-ts (slack-ts message))
         (time (funcall slack-completing-read-function
                        "When: "
                        '("In 20 minutes"
                          "In 1 hour"
                          "In 3 hour"
                          "Tomorrow"
                          "Next week")
                        nil t))
         (due-in-ms (cond
                     ((string= time "In 20 minutes") (* 20 60 1000))
                     ((string= time "In 1 hour") (* 60 60 1000))
                     ((string= time "In 3 hours") (* 3 60 60 1000))
                     ((string= time "Tomorrow") (* 24 60 60 1000))
                     (t (* 7 24 60 60 1000)))))
    (cl-labels
        ((success (&key data &allow-other-keys)
           (slack-request-handle-error
            (data "slack-remind-add-from-message")
            (message "DATA: %S" data))))
      (slack-buffer-add-star message message-ts due-in-ms))))

(provide 'slack-reminder)
;;; slack-reminder.el ends here

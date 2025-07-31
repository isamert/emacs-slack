;;; slack-defcustoms.el --- slack client              -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ag91

;; URL: https://github.com/emacs-slack/emacs-slack
;; Author: andrea-dev@hotmail.com
;; Keywords: tools
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

;; Container of shared variables for emacs-slack

;; A common place to store shared variables.
;; Possibly the container of all defcustom but no time to refactor things at the moment.

;;; Code:

(defcustom slack-extra-subscribed-channels nil
  "A list of channel names you want to be subscribed.
These are added to the check on team's :subscribed-channels"
  :type 'list
  :group 'slack)

(defcustom slack-update-quick nil
  "Set to t if you hit rate limiting on connection.

Slack has strict rate limiting for user tokens. Updating public
channels can hit these limits pretty quickly if the Slack team is
large enough. Setting this to t, will make sure only channels you
are part of are updated via `slack-api-client-userboot-url'.

The cons of this approach is that you will need to explicitly
call `slack-conversations-list' if you want a sync of public
channels. You may need that if you don't find a channel you
expect to exist."
  :type 'boolean
  :group 'slack)

(defcustom slack-open-message-with-browser t
  "Open message permalink in browser if it cannot be opened in a message buffer.

So far (2024-10-29), emacs-slack doesn't support jumping on an old message for a channel:
it just loads the latest messages in a channel.
Ideally we want to have that, but for now we let the user jump to Slack app or web app to check the contents."
  :type 'boolean
  :group 'slack)

(defcustom slack-render-profile-images-p t
  "Show user profile avatars in message buffer.
When nil, they are not shown anymore for all teams."
  :type 'boolean
  :group 'slack)

(defcustom slack-test-out-load-older-messages-p nil
  "This is a feature toggle for testing out loading old messages in buffer when searching an older message via search and feed.
The UX is broken for now, it is really about having something somewhat functional when searching."
  :type 'boolean
  :group 'slack)

(defcustom slack-request-curl-options nil
  "List of curl command options.

List of strings that will be passed to every curl invocation.
You can pass extra options here, like setting the proxy."
  :type '(repeat string))

(provide 'slack-defcustoms)
;;; slack-defcustoms.el ends here

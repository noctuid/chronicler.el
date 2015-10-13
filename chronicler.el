;;; chronicler.el --- Statistics for writing in org mode.

;; Author: Lit Wakefield <noct@openmailbox.org>
;; URL: https://github.com/noctuid/chronicler
;; Created: September 25, 2015
;; Keywords: org, writing, prose, words
;; Package-Requires: ((names "0.5") (emacs "24"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; For more information see the README in the github repo.

;;; Code:
(require 'org)
;; autoloaded
;; (require 'time-date)
;; (require 'rx)

;; `define-namespace' is autoloaded, so there's no need to require
;; `names'. However, requiring it here means it will also work for
;; people who don't install through package.el.
(eval-when-compile (require 'names))

;;;###autoload
(define-namespace chronicler- :package chronicler :group convenience

(defcustom ignore-headings-by-default nil
  "The default behavior for whether to ignore the contents of a heading."
  :type 'boolean)

(defcustom store-dir "./.chronicler/"
  "Directory to store heading information in.
The path can either be absolute or relative to the current directory."
  :type 'string)

(defcustom day-start-time 4
  "The hour (0-23) to reset the daily counts to 0.
To have the daily counts accurate for the day that they were written on, set to
0. The default is 4am since it seems to be a likely time for one to be asleep."
  :type 'integer)

(defcustom daily-wc-goal 500
  "The daily word count goal."
  :type 'integer)

(defun -external-command (&optional extra-options)
  "Return a chronicler shell command string with user-set options.
EXTRA-OPTIONS will be added just before the positional file argument.
EXTRA-OPTIONS should start with a space."
  (concat "chronicler"
          " -d " store-dir
          " -t " (number-to-string day-start-time)
          (if ignore-headings-by-default
              " -i"
            "")
          (or extra-options "")
          " "
          (buffer-file-name)))

:autoload
(defun update-word-counts ()
  "Add or update the word count properties for all non-ignored headings."
  (interactive)
  (shell-command (-external-command " -u")))

:autoload
(defun message-day-progress ()
  "Message current day's word count and percentage of goal completed."
  (interactive)
  (let ((day-wc
         (string-to-number
          (shell-command-to-string (-external-command " -r")))))
    (message "%s" (concat "Day wc: " (number-to-string day-wc)
                          ", Day goal:" (number-to-string daily-wc-goal)
                          ", %Done: "
                          (number-to-string
                           (round (* 100 (/ day-wc
                                            (float daily-wc-goal)))))
                          "%"))))

;; end of namespace
)

(provide 'chronicler)
;;; chronicler.el ends here

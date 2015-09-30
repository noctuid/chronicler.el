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

(defcustom count-headings-by-default t
  "The default behavior for whether to count the contents of a heading."
  :type 'boolean)

(defcustom no-count-heading-property "no-count"
  "Property name to prevent counting anything under a heading."
  :type 'string)

(defcustom count-heading-property "count"
  "Property name to ensure everything under a heading is counted.
When a heading has both the no-count and count properties, the count property
takes precedence, and the heading will be counted."
  :type 'string)

(defcustom inherit-count-property t
  "Whether to recursively count or ignore headings based on
`no-count-heading-property' and `count-heading-property'."
  :type 'boolean)

(defcustom ignore-noexport-headings t
  "Whether to ignore headings with a :noexport: tag.
Note that org tags apply to all sub-headings by default."
  :type 'boolean)

(defcustom keep-past-days t
  "Whether to keep information on the daily counts for previous days."
  :type 'boolean)

(defcustom word-separator (rx space)
  "Regex string that detemines what separates words."
  :type 'regexp)

(defcustom day-start-time 4
  "The hour (1-24) to reset the daily counts to 0.
To have the daily counts accurate for the day that they were written on, set to
0. The default is 4am since it seems to be a likely time for one to be asleep."
  :type 'integer)

(defcustom daily-wc-goal 500
  "The daily word count goal."
  :type 'integer)

(defconst -org-heading-regexp (rx (and bol (1+ "*") " ")))

(defun -heading-ignored-p ()
  "Helper function to determine if the heading is ignored."
  ;; do count property takes precedence
  (cond ((org-entry-get (point) count-heading-property inherit-count-property)
         nil)
        ((or (org-entry-get (point) no-count-heading-property inherit-count-property)
             ;; tags are inherited by default
             (and ignore-noexport-headings
                  (member "noexport" (org-get-tags-at))))
         t)
        (t
         (if count-headings-by-default
             nil
           t))))

(defun -forward-heading-same-level (arg &optional invisible-ok)
  "Wrapper for `org-forward-heading-same-level'.
Return t when point has moved oherwise nil."
  (let ((last-point (point)))
    (org-forward-heading-same-level arg invisible-ok)
    (not (= (point) last-point))))

(defun -forward-text-start-or-max-point ()
  "Go to next non-empty line or the max point."
  (forward-line)
  (when (looking-at (rx (and bol eol)))
    (unless (re-search-forward (rx (and bol char)) nil t)
      (goto-char (point-max)))))

(defun count-words-for-heading (&optional recount)
  "Count all words under the current heading and sub-headings when not ignored.
RECOUNT determines whether to recount sub-headings or just take values
previously calculated when present. Ignores words in headings, comments, blocks,
drawers, and planning sections (e.g. SCHEDULED:...)."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (let ((wc 0)
          (start-bound (point-min))
          (end-bound (point-max))
          (first t))
      (save-excursion
        (goto-char start-bound)
        (while (< (point) end-bound)
          (cond
           ((org-at-heading-p)
            (cond ((-heading-ignored-p)
                   (if inherit-count-property
                       ;; all sub-headings will be ignored so skip
                       (unless (-forward-heading-same-level 1 t)
                         (goto-char end-bound))
                     ;; will go to end if not another heading
                     (outline-next-heading)))
                  (t (if (and (not first)
                              (not recount))
                         (let ((heading-wc (org-entry-get (point) "total-wc")))
                           (if heading-wc
                               (progn
                                 (setq wc
                                       (+ wc (string-to-number heading-wc)))
                                 (unless (-forward-heading-same-level 1 t)
                                   (goto-char end-bound)))
                             (-forward-text-start-or-max-point)))
                       (-forward-text-start-or-max-point)))))
           ((or (org-at-comment-p)
                (org-at-block-p)
                (org-at-drawer-p)
                (org-at-planning-p))
            (goto-char (org-element-property :end (org-element-at-point))))
           (t
            (if (re-search-forward word-separator end-bound t)
                (setq wc (1+ wc))
              (goto-char end-bound))))
          (setq first nil)))
      ;; (1+ wc)
      wc)))

(defun -decrement-month-or-day (month-or-day)
  "Return a string containing a number one less than that of MONTH-OR-DAY.
The string will be zero padded if the number is less than 10."
  (setq month-or-day (1- (string-to-number month-or-day)))
  (if (>= month-or-day 10)
      (setq month-or-day (number-to-string month-or-day))
    (setq month-or-day (concat "0" (number-to-string month-or-day))))
  month-or-day)

(defun -last-day-of-month (month year)
  "Return the last day for a MONTH during YEAR as a string."
  (setq month (string-to-number month))
  (setq year (string-to-number year))
  (cond ((member month '(1 3 5 7 8 10 12))
         "31")
        ((member month '(4 6 9 11))
         "30")                          ;
        ((= month 2)
         (if (date-leap-year-p year)
             "29"
           "28"))))

(defun -yesterday-date-stamp (&optional days)
  "Return a date stamp DAYS days ago as a string."
  (let ((days (or days 1))
        (day (format-time-string "%d"))
        (month (format-time-string "%m"))
        (year (format-time-string "%Y")))
    (dotimes (_ days)
      (cond ((and (equal day "01")
                  (equal month "01"))
             (setq day "31")
             (setq month "12")
             (setq year (number-to-string
                         (1- (string-to-number year)))))
            ((equal day "01")
             (setq month (-decrement-month-or-day month))
             (setq day (-last-day-of-month month year)))
            (t
             (setq day (-decrement-month-or-day day)))))
    (concat year "-" month "-" day)))

(defun -current-date-stamp ()
  "Return the current date stamp based on `chronicler-day-start-time'."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (if (>= hour day-start-time)
        (format-time-string "%F")
      (-yesterday-date-stamp))))

(defun set-word-count-for-heading (&optional recount)
  "Create properties in drawer for the word counts of the current heading.
RECOUNT determines whether to recount sub-headings or just take the values
previously calculated when present."
  (interactive)
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (when (not (-heading-ignored-p))
        (let* ((previous-total-wc (org-entry-get (point) "previous-total-wc"))
               (total-wc (count-words-for-heading recount))
               day-wc
               (hour (string-to-number (format-time-string "%H")))
               (current-stamp (concat (-current-date-stamp) "-wc"))
               (last-stamp (if (>= hour day-start-time)
                               (-yesterday-date-stamp)
                             (-yesterday-date-stamp 2)))
               (current-stamp-match (org-entry-get (point) current-stamp))
               (last-stamp-value (org-entry-get (point) last-stamp)))
          (org-set-property "total-wc" (number-to-string total-wc))
          (if (and current-stamp-match previous-total-wc)
              (setq day-wc (- total-wc
                              (string-to-number previous-total-wc)))
            ;; new day or first time run (or property deleted)
            (org-set-property "previous-total-wc" (number-to-string total-wc))
            (when (and last-stamp-value
                       (or (= (string-to-number last-stamp-value) 0)
                           (not keep-past-days)))
              (org-delete-property last-stamp))
            (setq day-wc 0))
          ;; so won't get negative day count'
          (unless (< day-wc 0)
            (org-set-property current-stamp (number-to-string day-wc))))))))

:autoload
(defun update-word-counts ()
  "Add or update the word count properties for all non-ignored headings."
  (interactive)
  (save-excursion
    ;; fix this
    (outline-show-all)
    (goto-char (point-max))
    ;; since going backwards, will hit inner headings first
    ;; prevents need for re-calculation
    (while (outline-previous-heading)
      (set-word-count-for-heading))))

:autoload
(defun message-day-progress ()
  "Message current day's word count and percentage of goal completed."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward -org-heading-regexp nil t)
    (let ((total-wc 0)
          (day-wc 0))
      (while (progn
               (let ((heading-total-wc (org-entry-get (point) "total-wc"))
                     (heading-day-wc
                      (org-entry-get (point)
                                     (concat (-current-date-stamp) "-wc"))))
                 (when (and heading-total-wc heading-day-wc)
                   (setq total-wc (+ total-wc
                                     (string-to-number heading-total-wc)))
                   (setq day-wc (+ day-wc (string-to-number heading-day-wc)))))
               (-forward-heading-same-level 1 t)))
      (message "%s" (concat "Day wc: " (number-to-string day-wc)
                            ", Day goal: " (number-to-string daily-wc-goal)
                            ", %Done: "
                            (number-to-string
                             (round (* 100 (/ day-wc
                                              (float daily-wc-goal))))) "%")))))

;; end of namespace
)

(provide 'chronicler)
;;; chronicler.el ends here

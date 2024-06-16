;;; org-hop.el --- Hop to Org headings and lines -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/org-hop
;; Version: 0.3
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

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

;; An interface to hop to Org headings and lines.


;;; Code:

(require 'org)
(require 'org-ql)


;;;; Custom variables

(defgroup org-hop nil
  "Group for `org-hop' customizations."
  :group 'org)

(defcustom org-hop-files 'buffers
  "Which Org files should be returned by function `org-hop-files'.

Default values are:

  \\='agenda      list of Org agenda files
  \\='buffers     list of open Org buffers
  \\='files       list of Org agenda files + open Org buffers

Alternatively, this variable can be a custom list of Org files."
  :group 'org-hop
  :type '(choice symbol (repeat file)))

(defcustom org-hop-files-extra nil
  "List of Org files that must always be scanned for headings."
  :group 'org-hop
  :type '(choice file (repeat file)))

(defcustom org-hop-files-ignore nil
  "List of ignored Org files."
  :group 'org-hop
  :type '(choice file (repeat file)))

(defcustom org-hop-headings-show-todo-prefix nil
  "If non-nil, prefix headings with their current Org TODO keyword."
  :group 'org-hop
  :type 'boolean)

(defcustom org-hop-headings-show-filename t
  "If non-nil, display Org file name before headings."
  :group 'org-hop
  :type 'boolean)

(defcustom org-hop-headings-show-tags t
  "If non-nil, display Org headings tags."
  :group 'org-hop
  :type 'boolean)

(defcustom org-hop-headings-width 256
  "Max Org heading entry length."
  :group 'org-hop
  :type 'integer)

(defcustom org-hop-recent-idle-interval 15
  "Idle interval in seconds to add current Org heading to recent list.
See `org-hop-recent-mode'."
  :group 'org-hop
  :type 'integer)

(defcustom org-hop-push-to-mark-ring t
  "If non-nil, push current position into the mark ring before hop."
  :group 'org-hop
  :type 'boolean)

(defcustom org-hop-to-entry-other-window nil
  "If non-nil, switch to entry in other window."
  :group 'org-hop
  :type 'boolean)

(defcustom org-hop-recenter nil
  "If nil, center point in selected window and maybe redisplay frame.
With a numeric value, recenter putting point on screen line
relative to the selected window.  See `recenter'."
  :group 'org-hop
  :type 'integer)

(defcustom org-hop-pre-hop-hook
  '(org-hop-mark-ring-push)
  "Hook run before hopping to heading or line."
  :group 'org-hop
  :type '(repeat (choice symbol)))

(defcustom org-hop-post-hop-hook
  '(org-hop-focus-entry org-hop-add-entry-at-point)
  "Hook run after hopping to heading or line."
  :group 'org-hop
  :type '(repeat (choice symbol)))


;;;; Org files

(defun org-hop-files ()
  "Return a list of all relevant Org files.
This function is controlled by the variable `org-hop-files'."
  (let ((files (cond ((eq org-hop-files 'agenda)
                      (org-agenda-files))
                     ((eq org-hop-files 'files)
                      (org-files-list))
                     ((or (eq org-hop-files 'buffers) (not org-hop-files))
                      (delete nil (mapcar #'buffer-file-name
                                          (org-buffer-list 'files t))))
                     (t org-hop-files))))
    (seq-difference
     (delete-dups (mapcar #'file-truename
                          (if org-hop-files-extra
                              (append org-hop-files-extra files)
                            files)))
     (mapcar #'file-truename org-hop-files-ignore))))


;;;; Heading & line data

(defun org-hop-format-heading (buffer path)
  "Format heading title info from BUFFER and Org heading PATH."
  (let ((todo (and org-hop-headings-show-todo-prefix (org-get-todo-state)))
        (heading (org-format-outline-path path org-hop-headings-width))
        (tags (and org-hop-headings-show-tags (org-get-tags))))
    (concat
     (and todo (format "#%s " todo))
     (and org-hop-headings-show-filename (format "%s:/" buffer))
     heading
     (and tags (propertize (format " %s" (org-make-tag-string tags))
                           'face 'org-tag)))))

(defun org-hop-format-line (buffer line-number)
  "Format line title info for BUFFER and LINE-NUMBER."
  (string-remove-suffix
   "\n" (format "%s:%s %s" buffer line-number (thing-at-point 'line))))

(cl-defun org-hop-get-entry (&optional (type 'heading))
  "Get data from Org heading at point.
Default TYPE is \\='heading; otherwise, get data from line at point."
  (let* ((marker (point-marker))
         (title  (if (eq type 'heading)
                     (org-hop-format-heading (marker-buffer marker)
                                             (org-get-outline-path t t))
                   (org-hop-format-line (marker-buffer marker)
                                        (line-number-at-pos)))))
    (if (eq type 'heading)
        (put-text-property 0 1 'org-marker marker title)
      (put-text-property 0 1 'consult-location
                         (cons marker (line-number-at-pos)) title))
    title))


;;;; Scan Org files

(defvar org-hop-headings-list nil
  "List of all known Org headings.")

(cl-defun org-hop-headings (&key (buffers-files (org-hop-files))
                                 (pred 'heading) (narrow t) pattern sort)
  "Return a list of Org headings from BUFFERS-FILES matching PATTERN.
PRED is a predicate from `org-ql-predicates', defaults to \\='heading.
NARROW and SORT are arguments for `org-ql-select', which see."
  (let* ((inhibit-message t)
         (message-log-max nil)
         (org-ql-default-predicate pred)
         (query (or (org-ql--query-string-to-sexp pattern) t)))
    (when (and buffers-files query)
      (ignore-errors
        ;; Ignore errors that might be caused by partially typed queries
        (org-ql-select buffers-files query
          :narrow narrow
          :action #'org-hop-get-entry
          :sort sort)))))

(defun org-hop-buffers-modified-tick-p ()
  "Return non-nil if Org buffers `buffer-modified-tick' sum changed."
  (let ((sum (seq-reduce #'+ (mapcar #'buffer-modified-tick
                                     (org-buffer-list 'files t))
                         0))
        (last-sum (or (get-register 'org-hop-buffers-tick-sum) 0)))
    (when (/= sum last-sum)
      (set-register 'org-hop-buffers-tick-sum sum))))

(defun org-hop-headings-list-update ()
  "Update `org-hop-headings-list' if necessary."
  (when (or (not org-hop-headings-list)
            (org-hop-buffers-modified-tick-p))
    (setq org-hop-headings-list (org-hop-headings))))


;;;;; Actions

(defun org-hop--entry-marker (entry)
  "Return ENTRY marker."
  (or (get-text-property 0 'org-marker entry)
      (car (get-text-property 0 'consult-location entry))))

(defun org-hop-to-entry (entry &optional other-window)
  "Hop to ENTRY.
Optional argument OTHER-WINDOW selects the buffer in other window."
  (let* ((marker (org-hop--entry-marker entry))
         (buffer (marker-buffer marker))
         (pos   (marker-position marker)))
    (if (not (buffer-live-p buffer))
        (org-hop-remove-missing entry)
      (run-hooks 'org-hop-pre-hop-hook)
      (if (or org-hop-to-entry-other-window other-window)
          (switch-to-buffer-other-window buffer)
        (switch-to-buffer buffer))
      (goto-char pos)
      (run-hooks 'org-hop-post-hop-hook))))

(defun org-hop-mark-ring-push ()
  "Push current position into the mark ring before to hop."
  (when org-hop-push-to-mark-ring
    (org-mark-ring-push)))

(defun org-hop-focus-entry ()
  "Proper focus selected entry."
  (when (and (derived-mode-p 'org-mode) (org-at-heading-p))
    (goto-char (pos-bol))
    (org-fold-show-entry)
    (org-fold-show-children))
  (recenter org-hop-recenter))

(defmacro org-hop-with-entry-buffer (entry &rest body)
  "Execute the forms in BODY with ENTRY location temporarily current."
  (declare (indent defun))
  `(when-let ((marker (org-hop--entry-marker entry)))
     (save-excursion
       (with-current-buffer (marker-buffer marker)
         (goto-char (marker-position marker))
         ,@body))))


;;;; Add entries to recently visited lists

(defvar org-hop-recent-headings-list nil
  "List of recently visited Org headings.")

(defvar org-hop-recent-lines-list nil
  "List of saved buffer lines.")

(defmacro org-hop-add (entry recent-list)
  "Put ENTRY in the beginning of RECENT-LIST and remove dups."
  `(setq ,recent-list (cl-remove-duplicates
                       (cons ,entry (delete ,entry ,recent-list))
                       :test #'equal :from-end t)))

(defun org-hop-add-entry (type verbose)
  "Add ENTRY to its recently visited list TYPE.
If VERBOSE is non-nil, show messages in echo area."
  (let ((entry (org-hop-get-entry type)))
    (when verbose
      (message "Saved: %s" entry))
    (if (eq type 'heading)
        (org-hop-add entry org-hop-recent-headings-list)
      (org-hop-add entry org-hop-recent-lines-list))))

(defun org-hop-add-heading-to-list (&optional verbose)
  "Add current Org heading to recent list.
If VERBOSE is non-nil, show messages in echo area."
  (interactive)
  (save-excursion
    (and (derived-mode-p 'org-mode)
         (buffer-file-name) ;; NOTE: ignore indirect/capture buffers
         (org-back-to-heading t)
         (org-hop-add-entry 'heading verbose))))

(defun org-hop-add-line-to-list (&optional verbose)
  "Add current line to recent list.
If VERBOSE is non-nil, show messages in echo area."
  (interactive)
  (org-hop-add-entry 'line verbose))

(defun org-hop-add-entry-at-point (&optional verbose)
  "Add entry at point, an Org heading or buffer line, to recent list.
If VERBOSE is non-nil, show messages in echo area."
  (interactive)
  (when (not (org-hop-add-heading-to-list verbose))
    (org-hop-add-line-to-list verbose)))


;;;; Remove entries from recent lists

(defmacro org-hop-remove (entry recent-list &optional verbose)
  "Remove an ENTRY from RECENT-LIST.
If VERBOSE is non-nil, show messages in echo area."
  `(prog1
       (setq ,recent-list (remove entry ,recent-list))
     (when (or ,verbose (called-interactively-p 'any))
       (message "Removed from recent list: %s" ,entry))))

(defun org-hop-remove-heading (entry &optional verbose)
  "Remove ENTRY from recent headings list.
If VERBOSE is non-nil, show messages in echo area."
  (org-hop-remove entry org-hop-recent-headings-list verbose))

(defun org-hop-remove-line (entry &optional verbose)
  "Remove ENTRY from recent lines list.
If VERBOSE is non-nil, show messages in echo area."
  (org-hop-remove entry org-hop-recent-lines-list verbose))

(defun org-hop-remove-missing (entry)
  "Remove ENTRY when missing buffer or marker."
  (ignore-errors
    (org-hop-remove entry org-hop-recent-headings-list)
    (org-hop-remove entry org-hop-recent-lines-list))
  (message "Buffer or heading vanished."))


;;;;; Interactively remove entries

(defmacro org-hop-remove-from-list (recent-list &optional arg)
  "Remove an item from a RECENT-LIST using `completing-read'.
With optional argument ARG, set RECENT-LIST to nil."
  `(if ,arg
       (setq ,recent-list nil)
     (let* ((entry (completing-read "Remove from list: " ,recent-list))
            (item (cdr (assoc entry ,recent-list))))
       (org-hop-remove item ,recent-list))))

(defun org-hop-remove-heading-from-list (&optional arg)
  "Remove an item from a recent list using `completing-read'.
With optional argument ARG, set list to nil."
  (interactive "P")
  (org-hop-remove-from-list org-hop-recent-headings-list arg))

(defun org-hop-remove-line-from-list (&optional arg)
  "Remove an item from a recent list using `completing-read'.
With optional argument ARG, set list to nil."
  (interactive "P")
  (org-hop-remove-from-list org-hop-recent-lines-list arg))



;;;; Commands

(defun org-hop-reset-recent-lists ()
  "Reset recent entries lists."
  (interactive)
  (setq org-hop-recent-headings-list nil
        org-hop-recent-lines-list    nil))

(defun org-hop-reset-caches ()
  "Reset Org caches and rebuild `org-hop-headings-list'."
  (interactive)
  (message "[org-hop] Resetting caches...")
  (setq org-ql-cache (make-hash-table :weakness 'key))
  (org-hop-reset-recent-lists)
  (setq org-hop-headings-list nil)
  (org-hop-headings-list-update)
  (org-persist-gc)
  (message "[org-hop] Resetting caches... done"))

(defun org-hop-reset (&optional arg)
  "Reset Org-Hop lists.
With a prefix argument ARG, run `org-hop-reset-recent-lists'.
With a double prefix argument, run `org-hop-reset-caches'."
  (interactive "P")
  (cond ((eq (prefix-numeric-value arg) 4) (org-hop-reset-recent-lists))
        ((eq (prefix-numeric-value arg) 16) (org-hop-reset-caches))
        (t (org-hop-headings-list-update))))

;;;###autoload
(defun org-hop (&optional arg)
  "Hop to a Org heading.
With optional argument ARG, run `org-hop-reset', which see."
  (interactive "P")
  (org-hop-reset arg)
  (let* ((headings (append org-hop-recent-headings-list
                           org-hop-recent-lines-list
                           org-hop-headings-list))
         (entry (completing-read "Hop to: " headings)))
    (org-hop-to-entry entry)))

;;;###autoload
(defun org-hop-current-buffer ()
  "Hop to a Org heading in current buffer."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (let* ((headings (org-hop-headings :buffers-files (current-buffer)))
             (entry (completing-read "Hop to: " headings)))
        (org-hop-to-entry entry))
    (user-error "Not an Org file")))

;;;###autoload
(defun org-hop-add-heading-or-line (&optional arg)
  "Add current Org heading or line to recent list.
With optional argument ARG, add current position as line."
  (interactive "P")
  (if arg
      (org-hop-add-line-to-list t)
    (org-hop-add-entry-at-point t)))

(define-minor-mode org-hop-recent-mode
  "Toggle `org-hop-recent-mode'.
When idle, add current Org heading to `org-hop-recent-headings-list'."
  :init-value nil
  :lighter nil
  :group 'org-hop
  (if org-hop-recent-mode
      (run-with-idle-timer org-hop-recent-idle-interval t
                           #'org-hop-add-heading-to-list)
    (cancel-function-timers #'org-hop-add-heading-to-list)))


(provide 'org-hop)

;;; org-hop.el ends here

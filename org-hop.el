;;; org-hop.el --- Hop to Org headings and lines -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/org-hop
;; Version: 0.3.2
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

(defcustom org-hop-files-tiers-regexp-alist nil
  "Alist of regexps matching file names and their tier values.

For example, setting this variable to

  `((\"notes\" . 0)
    (,(rx (or \"config\" \"refs\")) . 1))

will result in all \"notes\" files' headings appearing at the top of the
`org-hop-headings-list', followed by the headings from files whose paths
match either \"config\" or \"refs\". Any heading from other files will
be assigned to the tier defined by `org-hop-files-tier-default'.

When this variable is set to nil, no sorting is done and no tiers are
applied."
  :group 'org-hop
  :type 'sexp)

(defcustom org-hop-files-tier-default 2
  "Default tier value.
This is relevant only when `org-hop-files-tiers-regexp-alist' is non-nil."
  :group 'org-hop
  :type 'integer)

(defcustom org-hop-headings-show-todo-prefix nil
  "If non-nil, prefix headings with their current Org TODO keyword."
  :group 'org-hop
  :type 'boolean)

(defcustom org-hop-headings-show-filename t
  "If non-nil, display Org file name before headings."
  :group 'org-hop
  :type 'boolean)

(defcustom org-hop-headings-filename-prefix ""
  "Prefix for file names when `org-hop-headings-show-filename' is non-nil."
  :group 'org-hop
  :type 'string)

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

(defface org-hop-file-face
  '((t (:inherit (shadow))))
  "Face for the file name part of the candidate."
  :group 'org-hop)


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


;;;;; Files tiers

(defun org-hop-files-get-tier (file-name)
  "Return the tier value for FILE-NAME.
File tiers values must be defined in `org-hop-files-tiers-regexp-alist'."
  (or (and (stringp file-name)
           (assoc-default file-name
                          org-hop-files-tiers-regexp-alist
                          #'string-match))
      org-hop-files-tier-default))

(defun org-hop-files-tiers-sort (a b)
  "Sort function for file tiers."
  (< (get-text-property 0 'tier a)
     (get-text-property 0 'tier b)))


;;;; Format Org heading & line

(defun org-hop--format-heading (&optional current-heading)
  "Return CURRENT-HEADING as an outline string with text properties."
  (let* ((heading (or current-heading (org-element-at-point-no-context)))
         (marker (point-marker))
         (todo (and org-hop-headings-show-todo-prefix
                    (org-element-property :todo-keyword heading)))
         (tags (and org-hop-headings-show-tags
                    (org-element-property :tags heading)))
         (candidate
          (concat
           (and todo
                (concat (propertize "#" 'face 'shadow)
                        (propertize todo 'face (org-get-todo-face todo))
                        " "))
           (org-format-outline-path (org-get-outline-path t t)
                                    org-hop-headings-width
                                    (and org-hop-headings-show-filename
                                         (propertize
                                          (concat
                                           org-hop-headings-filename-prefix
                                           (buffer-name) ":")
                                          'face 'org-hop-file-face)))
           (and tags (concat " " (propertize (org-make-tag-string tags)
                                             'face 'org-tag))))))
    (put-text-property 0 1 'org-marker marker candidate)
    (and org-hop-files-tiers-regexp-alist
         (put-text-property 0 1 'tier
                            (org-hop-files-get-tier (buffer-file-name))
                            candidate))
    (cons candidate marker)))

(defun org-hop--format-line ()
  "Return current line as a string with text properties."
  (let* ((marker (point-marker))
         (line-number (line-number-at-pos))
         (candidate (format "%s:%s   %s"
                            (marker-buffer marker) line-number
                            (string-trim (thing-at-point 'line))))
         (prop (cons marker line-number)))
    (put-text-property 0 1 'consult-location prop candidate)
    (cons candidate marker)))


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
        (org-ql-select buffers-files query
          :narrow narrow
          :action #'org-hop--format-heading
          :sort sort)))))

(defun org-hop-buffers-modified-tick-p ()
  "Return non-nil if Org buffers `buffer-modified-tick' sum changed."
  (let ((sum (seq-reduce #'+ (mapcar #'buffer-modified-tick
                                     (org-buffer-list 'files t))
                         0))
        (last-sum (or (get 'org-hop-headings-list 'tick-sum) 0)))
    (when (/= sum last-sum)
      (put 'org-hop-headings-list 'tick-sum sum))))

(defun org-hop-headings-list-update (&optional force-update)
  "Update `org-hop-headings-list' only if necessary.
Or when optional argument FORCE-UPDATE is non-nil."
  (when (or (not org-hop-headings-list)
            (org-hop-buffers-modified-tick-p)
            force-update)
    (setq org-hop-headings-list
          (if org-hop-files-tiers-regexp-alist
              (cl-sort (org-hop-headings)
                       #'org-hop-files-tiers-sort
                       :key #'car)
            (org-hop-headings)))))


;;;; Actions

(defun org-hop--entry-marker (entry)
  "Return ENTRY marker."
  (if (markerp entry)
      entry
    (or (get-text-property 0 'org-marker entry)
        (car (get-text-property 0 'consult-location entry))
        (get-text-property 0 'consult--candidate entry))))

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
  `(when-let ((marker (org-hop--entry-marker ,entry)))
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

(defun org-hop-add-heading-to-list (&optional verbose)
  "Add current Org heading to `org-hop-recent-headings-list'.
If VERBOSE is non-nil, show message in echo area."
  (interactive)
  (save-excursion
    (and (derived-mode-p 'org-mode)
         (buffer-file-name) ;; NOTE: ignore indirect/capture buffers
         (ignore-errors (org-back-to-heading t))
         (let ((heading (org-hop--format-heading)))
           (org-hop-add heading org-hop-recent-headings-list)
           (and verbose (message "Saved: %s" (car heading)))
           t))))

(defun org-hop-add-line-to-list (&optional verbose)
  "Add current line to `org-hop-recent-lines-list'.
If VERBOSE is non-nil, show message in echo area."
  (interactive)
  (let ((line (org-hop--format-line)))
    (org-hop-add line org-hop-recent-lines-list)
    (and verbose (message "Saved: %s" (car line)))))

(defun org-hop-add-entry-at-point (&optional verbose)
  "Add entry at point, an Org heading or buffer line, to recent list.
If VERBOSE is non-nil, show messages in echo area."
  (interactive)
  (unless (org-hop-add-heading-to-list verbose)
    (org-hop-add-line-to-list verbose)))

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


;;;; Remove entries from recent lists

(defmacro org-hop-remove (entry recent-list &optional verbose)
  "Remove an ENTRY from RECENT-LIST.
If VERBOSE is non-nil, show messages in echo area."
  `(progn
     (setq ,recent-list (assoc-delete-all ,entry ,recent-list #'equal))
     (when (or ,verbose (called-interactively-p 'any))
       (message "Entry removed recent list."))))

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

(defmacro org-hop-remove-from-list (recent-list &optional clear)
  "Remove an item from a RECENT-LIST using `completing-read'.
With optional argument CLEAR, set RECENT-LIST to nil."
  `(if (not ,recent-list)
       (message "List is empty.")
     (if ,clear
         (setq ,recent-list nil)
       (let* ((entry (completing-read "Remove from list: " ,recent-list)))
         (org-hop-remove (assoc entry ,recent-list) ,recent-list)))))

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


;;;; Reset caches

(defun org-hop-reset-recent-lists ()
  "Reset recent entries lists."
  (interactive)
  (setq org-hop-recent-headings-list nil)
  (setq org-hop-recent-lines-list    nil))

(defun org-hop-reset-caches ()
  "Reset Org caches and rebuild `org-hop-headings-list'."
  (message "[org-hop] Resetting caches...")
  (setq org-ql-cache (make-hash-table :weakness 'key))
  (setq org-ql-tags-cache (make-hash-table :weakness 'key))
  (setq org-ql-node-value-cache (make-hash-table :weakness 'key))
  (org-persist-gc)
  (org-hop-reset-recent-lists)
  (setq org-hop-headings-list nil)
  (org-hop-headings-list-update)
  (message "[org-hop] Resetting caches... done"))

(defun org-hop-reset (&optional arg)
  "Reset Org-Hop lists.
With a prefix argument ARG, run `org-hop-reset-recent-lists'.
With a double prefix argument, run `org-hop-reset-caches'."
  (interactive "P")
  (cond ((eq (prefix-numeric-value arg) 4) (org-hop-reset-recent-lists))
        ((eq (prefix-numeric-value arg) 16) (org-hop-reset-caches))
        (arg (org-hop-headings-list-update :force-update))))


;;;; Commands

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
    (org-hop-to-entry (alist-get entry headings nil nil #'equal))))

;;;###autoload
(defun org-hop-current-buffer (&optional buffers-files)
  "Hop to a Org heading in current buffer.
When BUFFERS-FILES is a list of Org buffers or files, use it instead."
  (interactive)
  (if (or buffers-files (derived-mode-p 'org-mode))
      (let* ((headings (org-hop-headings
                        :buffers-files (or buffers-files (current-buffer))))
             (entry (completing-read "Hop to: " headings)))
        (org-hop-to-entry (alist-get entry headings nil nil #'equal)))
    (user-error "Not an Org buffer")))

;;;###autoload
(defun org-hop-add-heading-or-line (&optional arg)
  "Add current Org heading or line to recent list.
With optional argument ARG, add current position as line."
  (interactive "P")
  (if arg
      (org-hop-add-line-to-list t)
    (org-hop-add-entry-at-point t)))


(provide 'org-hop)

;;; org-hop.el ends here

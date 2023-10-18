;;; org-hop.el --- Hop to Org headings and lines -*- lexical-binding: t -*-

;; Copyright (C) 2022 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/org-hop
;; Version: 0.2
;; Package-Requires: ((emacs "27.2"))

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


;;;; Custom variables

(defgroup org-hop nil
  "Group for `org-hop' customizations."
  :group 'org)

(defcustom org-hop-files 'buffers
  "Which Org files should be scanned by `org-hop-update-cache'.
This variable controls the function `org-hop-files'.  Default values are:

  'agenda          list of Org agenda files
  'buffers         list of open Org buffers
  'org-files-list  list of Org agenda files + open Org buffers

Alternatively, this variable can be a custom list of Org files."
  :group 'org-hop
  :type 'sexp)

(defcustom org-hop-files-extra nil
  "List of Org files that must always be scanned for headings.
This list precedes the files returned by function `org-hop-files'."
  :group 'org-hop
  :type 'sexp)

(defcustom org-hop-files-ignore nil
  "List of ignored Org files."
  :group 'org-hop
  :type 'sexp)

(defcustom org-hop-headings-with-todo-prefix nil
  "If non-nil, prefix headings with their current Org TODO keyword."
  :group 'org-hop
  :type 'boolean)

(defcustom org-hop-headings-with-filename t
  "If non-nil, display Org file name before headings."
  :group 'org-hop
  :type 'boolean)

(defcustom org-hop-headings-with-tags t
  "If non-nil, display Org headings tags."
  :group 'org-hop
  :type 'boolean)

(defcustom org-hop-headings-width 256
  "Max Org heading entry length."
  :group 'org-hop
  :type 'integer)

(defcustom org-hop-recent-idle-interval 15
  "Idle interval in seconds to add current Org heading to `org-hop-headings-list'.
See `org-hop-recent-mode'."
  :group 'org-hop
  :type 'integer)

(defcustom org-hop-push-to-mark-ring t
  "If non-nil, push current position into the mark ring before to hop."
  :group 'org-hop
  :type 'boolean)

(defcustom org-hop-switch-to-buffer-other-window t
  "If non-nil, switch to buffer in another window."
  :group 'org-hop
  :type 'boolean)

(defcustom org-hop-recenter nil
  "If nil, center point in selected window and maybe redisplay frame.
With a numeric value, recenter putting point on screen line
relative to the selected window.  See `recenter'."
  :group 'org-hop
  :type 'integer)

(defcustom org-hop-update-cache-if-buffer-modified nil
  "If nil, update file cache only if it was modified (saved) since last scan.
If non-nil, update file cache if its *buffer* was modified since last scan."
  :group 'org-hop
  :type 'boolean)

(defcustom org-hop-update-cache-verbose nil
  "When non-nil, show messages in echo area when cache is being updated."
  :group 'org-hop
  :type 'boolean)


;;;; Variables

(defvar org-hop-cache nil
  "Org headings cache.  See `org-hop-update-cache'.")

(defvar org-hop-last-update 0
  "Time of the last `org-hop-cache' update.  See `org-hop-update-cache'.")

(defvar org-hop-headings-list nil
  "List of Org headings recently visited.")

(defvar org-hop-lines-list nil
  "List of saved buffer lines.")

(defvar org-hop-pre-hop-hook
  '(org-hop-mark-ring-push)
  "Hook run before hopping to heading or line.")

(defvar org-hop-post-hop-hook
  '(org-hop-focus-entry org-hop-add-entry-at-point)
  "Hook run after hopping to heading or line.")


;;;; Org files

(defun org-hop-reset ()
  "Reset lists."
  (setq org-hop-cache         nil
        org-hop-headings-list nil
        org-hop-lines-list    nil))

(defun org-hop-reset-recent ()
  "Reset recent lists."
  (interactive)
  (setq org-hop-headings-list nil
        org-hop-lines-list    nil))

(defun org-hop-file-attr-modified (file &optional buffer-modified)
  "Return FILE modification time attribute in seconds.
When BUFFER-MODIFIED is non-nil and buffer visiting FILE was modified
since its file was last read or saved, always update its cache."
  (let* ((fmodtime (file-attribute-modification-time (file-attributes file)))
         (modtime (string-to-number (format-time-string "%s" fmodtime))))
    (if (and buffer-modified (buffer-modified-p (find-buffer-visiting file)))
        (1+ org-hop-last-update)
      modtime)))

(defun org-hop-files-truename (files)
  "Return the true file names of FILES."
  (interactive)
  (let ((file-list))
    (dolist (file files)
      (cl-pushnew (file-truename file) file-list))
    (cl-set-difference
     (remove nil (delete-dups (reverse file-list)))
     (mapcar 'file-truename org-hop-files-ignore) :test #'equal)))

(defun org-hop-files ()
  "Return a list of Org files using their truenames.
This function is controlled by the variable `org-hop-files'."
  (let ((org-files
         (cond ((eq org-hop-files 'agenda)
                (org-agenda-files))
               ((eq org-hop-files 'org-files-list)
                (org-files-list))
               ((or (eq org-hop-files 'buffers) (eq org-hop-files nil))
                (let ((org-buffers))
                  (dolist (buffer (org-buffer-list 'files))
                    (cl-pushnew (buffer-file-name buffer) org-buffers))
                  (reverse (remove nil org-buffers))))
               (t
                org-hop-files))))
    (org-hop-files-truename (if org-hop-files-extra
                                (append org-hop-files-extra org-files)
                              org-files))))



;;;; Heading & line data

(defun org-hop-format-heading (buffer path)
  "Format heading title info from BUFFER and Org heading PATH."
  (let ((keyword (if org-hop-headings-with-todo-prefix
                     (org-get-todo-state)))
        (heading (org-format-outline-path path org-hop-headings-width))
        (tags    (if org-hop-headings-with-tags
                     (org-make-tag-string (org-get-tags)))))
    (concat (if keyword (format "#%s " keyword))
            (if org-hop-headings-with-filename (format "%s:/" buffer))
            heading
            (if tags (format " %s" tags)))))

(defun org-hop-get-heading ()
  "Get data from Org heading at point."
  (let* ((path        (org-get-outline-path t t))
         (file        (buffer-file-name))
         (buffer      (buffer-name))
         (line-number (line-number-at-pos))
         (char        (point))
         (title       (org-hop-format-heading buffer path)))
    `(,title (:path   ,path
              :file   ,file
              :buffer ,buffer
              :line   ,line-number
              :char   ,char))))

(defun org-hop-format-line (buffer line-number)
  "Format line title info from BUFFER and LINE-NUMBER."
  (let ((line         (thing-at-point 'line)))
    (replace-regexp-in-string
     "\n" "" (format "%s:%s %s" buffer line-number line))))

(defun org-hop-get-line ()
  "Get data from line at point."
  (let* ((file        (buffer-file-name))
         (buffer      (buffer-name))
         (line-number (line-number-at-pos))
         (char        (point))
         (title       (org-hop-format-line buffer line-number)))
    `(,title (:file   ,file
              :buffer ,buffer
              :line   ,line-number
              :char   ,char))))



;;;; Scan Org files

(defun org-hop-get-file-headings (org-file)
  "Return a list of Org headings from ORG-FILE."
  (let ((org-file-headings nil)
        (buffer-point nil))
    (with-current-buffer (find-file-noselect org-file)
      (setq buffer-point (point))
      (widen)
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (cl-pushnew (org-hop-get-heading) org-file-headings :test #'equal))
      (goto-char buffer-point))
    (reverse org-file-headings)))

(defun org-hop-update-cache (&optional force)
  "Update cache of Org headings stored in `org-hop-cache'.

Scan the files returned by function `org-hop-files' if their
modification times are greater than `org-hop-last-update';
or when files are missing from `org-hop-cache'.

See `org-hop-get-file-headings'.

When FORCE is non-nil, force the scan of all files."
  (let ((new-cache nil)
        (file-modified-time nil))
    (dolist (file (org-hop-files))
      (setq file-modified-time (org-hop-file-attr-modified
                                file
                                org-hop-update-cache-if-buffer-modified))
      (cond ((or force
                 (not org-hop-cache)
                 (> file-modified-time org-hop-last-update)
                 (not (assoc file org-hop-cache))
                 (not (find-buffer-visiting file)))
             (if org-hop-update-cache-verbose
                 (message (format "[org-hop] Updating cache for %s..."
                                  (file-name-nondirectory file))))
             (cl-pushnew `(,file . ,(list (org-hop-get-file-headings file)))
                         new-cache :test #'equal))
            (t
             (cl-pushnew (assoc file org-hop-cache)
                         new-cache :test #'equal))))
    (if org-hop-update-cache-verbose
        (message "[org-hop] Cache updated."))
    (setq org-hop-cache new-cache)
    (setq org-hop-last-update (string-to-number (format-time-string "%s")))))

(defun org-hop-all-headings (&optional force)
  "Return the list of all possible Org headings.
With optional argument FORCE, rescan all files."
  (let ((all-headings-list nil))
    (org-hop-update-cache force)
    (dotimes (n (length org-hop-cache))
      (setq all-headings-list
            (append (cadr (nth n org-hop-cache)) all-headings-list)))
    all-headings-list))


;;;;; Actions

(defun org-hop-get-coordinates (entry)
  "Return ENTRY coordinates."
  (let* ((case-fold-search nil)
         (file   (plist-get (car entry) :file))
         (buffer (or (plist-get (car entry) :buffer)
                     (find-buffer-visiting file)))
         (line   (plist-get (car entry) :line))
         (char   (plist-get (car entry) :char))
         (path   (plist-get (car entry) :path))
         (olp))
    (if (not (buffer-live-p buffer))
        (setq buffer (find-file-noselect file)))
    (setq olp (if path
                  (save-excursion (switch-to-buffer buffer)
                                  (ignore-errors (org-find-olp path t)))))
    (when olp
      (setq buffer (marker-buffer olp))
      (setq char   (marker-position olp)))
    `(:buffer ,buffer :line ,line :char ,char)))

(defun org-hop-to-buffer (buffer)
  "Hop to BUFFER."
  (if org-hop-switch-to-buffer-other-window
      (switch-to-buffer-other-window buffer)
    (switch-to-buffer buffer)))

(defun org-hop-to-char-or-line (char line)
  "If CHAR is non-nil, go to char.  Else, go to LINE."
  (if char (goto-char char)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun org-hop-mark-ring-push ()
  "Push current position into the mark ring before to hop."
  (if org-hop-push-to-mark-ring
      (org-mark-ring-push)))

(defun org-hop-focus-entry ()
  "Proper focus selected entry."
  (when (and (eq major-mode 'org-mode) (org-at-heading-p))
    (goto-char (point-at-bol))
    (org-fold-show-entry)
    (org-fold-show-children))
  (recenter org-hop-recenter))

(defun org-hop-to-entry (entry)
  "Hop to ENTRY."
  (let* ((coordinates (org-hop-get-coordinates entry))
         (buffer      (plist-get coordinates :buffer))
         (line        (plist-get coordinates :line))
         (char        (plist-get coordinates :char)))
    (if (not (buffer-live-p buffer))
        (org-hop-remove-missing entry)
      (run-hooks 'org-hop-pre-hop-hook)
      (org-hop-to-buffer buffer)
      (org-hop-to-char-or-line char line)
      (run-hooks 'org-hop-post-hop-hook))))



;;;; Add entries to recently visited lists

(defmacro org-hop-add-entry (entry recent-list)
  "Put ENTRY in the beginning of RECENT-LIST and remove dups."
  `(setq ,recent-list (cl-remove-duplicates
                       (cons ,entry (delete ,entry ,recent-list))
                       :test #'equal :key #'car :from-end t)))

(defun org-hop-add (type verbose)
  "Add ENTRY to its recently visited list TYPE.
If VERBOSE is non-nil, show messages in echo area."
  (if (eq type 'heading)
      (org-hop-add-entry (org-hop-get-heading) org-hop-headings-list)
    (org-hop-add-entry (org-hop-get-line) org-hop-lines-list))
  (when verbose
    (message (format "Saved %s" (caar (if (eq type 'heading)
                                          org-hop-headings-list
                                        org-hop-lines-list))))))

(defun org-hop-add-heading-to-list (&optional verbose)
  "Add current Org heading to recent list.
If VERBOSE is non-nil, show messages in echo area."
  (interactive)
  (let ((pos (point))
        (org-heading-p nil))
    (if (and (eq major-mode 'org-mode)
             (buffer-file-name) ; NOTE: ignores indirect/capture buffers
             (or (org-at-heading-p)
                 (and (re-search-backward org-heading-regexp nil t)
                      (org-at-heading-p))))
        (progn
          ;; NOTE: make sure we get the right heading
          (goto-char (point-at-eol))
          (org-hop-add 'heading verbose)
          (setq org-heading-p t)))
    (goto-char pos)
    org-heading-p))

(defun org-hop-add-line-to-list (&optional verbose)
  "Add current line to recent list.
If VERBOSE is non-nil, show messages in echo area."
  (interactive)
  (org-hop-add 'line verbose))

(defun org-hop-add-entry-at-point (&optional verbose)
  "Add entry at point, an Org heading or buffer line, to recent list.
If VERBOSE is non-nil, show messages in echo area."
  (interactive)
  (if (not (org-hop-add-heading-to-list verbose))
      (org-hop-add 'line verbose)))



;;;; Remove entries from recent lists

(defmacro org-hop-remove (entry-data recent-list &optional verbose)
  "Remove an ENTRY-DATA from RECENT-LIST.
If VERBOSE is non-nil, show messages in echo area."
  `(let ((entry (rassoc ,entry-data ,recent-list)))
     (setq ,recent-list (remove entry ,recent-list))
     (when (or ,verbose (called-interactively-p 'any))
       (message (format "Removed from recent list: %s" (car entry))))))

(defun org-hop-remove-heading (entry-data &optional verbose)
  "Remove ENTRY-DATA from recent headings list.
If VERBOSE is non-nil, show messages in echo area."
  (org-hop-remove entry-data org-hop-headings-list verbose))

(defun org-hop-remove-line (entry-data &optional verbose)
  "Remove ENTRY-DATA from recent lines list.
If VERBOSE is non-nil, show messages in echo area."
  (org-hop-remove entry-data org-hop-lines-list verbose))

(defun org-hop-remove-missing (entry)
  "Remove ENTRY when missing buffer or marker."
  (ignore-errors
    (org-hop-remove entry org-hop-headings-list)
    (org-hop-remove entry org-hop-lines-list))
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
  (org-hop-remove-from-list org-hop-headings-list arg))

(defun org-hop-remove-line-from-list (&optional arg)
  "Remove an item from a recent list using `completing-read'.
With optional argument ARG, set list to nil."
  (interactive "P")
  (org-hop-remove-from-list org-hop-lines-list arg))



;;;; Autoloads

;;;###autoload
(defun org-hop (&optional arg)
  "Hop to a Org heading.
With optional argument ARG, force update all lists."
  (interactive "P")
  (when arg (org-hop-reset))
  (let* ((org-headings (append org-hop-headings-list
                               org-hop-lines-list
                               (org-hop-all-headings arg)))
         (item (completing-read "Hop to: " org-headings))
         (entry (cdr (assoc item org-headings))))
    (org-hop-to-entry entry)))

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
When idle, add current Org heading to `org-hop-headings-list'."
  :init-value nil
  :lighter nil
  :group 'org-hop
  (if org-hop-recent-mode
      (run-with-idle-timer org-hop-recent-idle-interval t
                           #'org-hop-add-heading-to-list)
    (cancel-function-timers #'org-hop-add-heading-to-list)))


(provide 'org-hop)

;;; org-hop.el ends here

;;; org-hop.el --- Hop to Org headings -*- lexical-binding: t -*-

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

;; An interface to hop to Org headings and markers.

;;; Code:

(require 'org)


;;;; Custom variables

(defgroup org-hop nil
  "Group for `org-hop' customizations."
  :group 'org)

(defcustom org-hop-files 'buffers
  "Which Org files should be scanned by `org-hop-scan-headings'.
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

(defcustom org-hop-mark-ring-push t
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

(defcustom org-hop-cache-update-if-buffer-modified nil
  "If nil, update cache for file if it was modified (saved) since last scan.
If non-nil, update cache for file if its buffer was modified since last scan."
  :group 'org-hop
  :type 'boolean)


;;;; Variables

(defvar org-hop-cache nil
  "Org headings cache.  See `org-hop-scan-headings'.")

(defvar org-hop-last-scan 0
  "Time of the last `org-hop-cache'.  See `org-hop-scan-headings'.")

(defvar org-hop-headings-list nil
  "List of Org headings recently visited.  See `org-hop-add-heading'.")

(defvar org-hop-markers-list nil
  "List of saved point-markers.  See `org-hop-add-marker'.")


;;;; Org files

(defun org-hop-reset ()
  "Reset lists."
  (setq org-hop-cache         nil
        org-hop-headings-list nil
        org-hop-markers-list  nil))

(defun org-hop-file-attr-modified (file &optional buffer-modified)
  "Return FILE modification time attribute in seconds.
When BUFFER-MODIFIED is non-nil, return current time if the buffer
visiting FILE was modified since its file was last read or saved."
  (string-to-number
   (if (and buffer-modified (buffer-modified-p (find-buffer-visiting file)))
       (format-time-string "%s")
     (format-time-string
      "%s"
      (file-attribute-modification-time (file-attributes file))))))

(defun org-hop-files-truename (files)
  "Return the truenames of a list of FILES."
  (interactive)
  (let ((files-truename-list nil))
    (dolist (file files)
      (cl-pushnew (file-truename file) files-truename-list))
    (cl-set-difference
     (remove nil (delete-dups (reverse files-truename-list)))
     (mapcar 'file-truename org-hop-files-ignore)
     :test #'equal)))

(defun org-hop-files ()
  "Return a list of Org files using their truenames.
This function is controlled by the variable `org-hop-files'."
  (let ((org-files
         (cond
          ;; 'agenda - list of Org agenda files
          ((eq org-hop-files 'agenda)
           (org-agenda-files))

          ;; 'buffers - list of open Org buffers
          ((or (eq org-hop-files 'buffers) (eq org-hop-files nil))
           (let ((org-buffers))
             (dolist (buffer (org-buffer-list 'files))
               (cl-pushnew (buffer-file-name buffer) org-buffers))
             (reverse (remove nil org-buffers))))

          ;; 'org-files-list - list of Org agenda files + open Org buffers
          ((eq org-hop-files 'org-files-list)
           (org-files-list))

          ;; custom user list
          (t
           org-hop-files))))
    (org-hop-files-truename (if org-hop-files-extra
                                (append org-hop-files-extra org-files)
                              org-files))))


;;;; Heading & marker info

(defun org-hop-get-heading (&optional org-file)
  "Get Org heading at point.
Optional argument ORG-FILE replaces current buffer file name attribute."
  (interactive)
  (let ((org-filename (or org-file (buffer-file-name)))
        (tags (org-get-tags)))
    `(,(concat (org-format-outline-path
                (org-get-outline-path t t)
                org-hop-headings-width
                (if org-hop-headings-with-filename
                    (format "%s:" (file-name-nondirectory org-filename)))
                "/")
               (if (and org-hop-headings-with-tags tags)
                   (format " %s" (org-make-tag-string tags))))
      (:type      "heading"
       :marker    ,(point-marker)
       :file      ,org-filename
       :line      ,(line-number-at-pos)))))

(defun org-hop-get-marker ()
  "Get current point marker."
  (interactive)
  (let* ((file (buffer-file-name))
         (buffer (if file (file-name-nondirectory file)
                   (buffer-name)))
         (line-number (line-number-at-pos))
         (line (thing-at-point 'line)))
    `(,(replace-regexp-in-string
        "\n" ""
        (format "%s:%s %s" buffer line-number line))
      (:type   "marker"
       :marker ,(point-marker)
       :file   ,file
       :line   ,line-number))))

(defun org-hop-file-headings (org-file)
  "Return a list of Org headings from ORG-FILE."
  (let ((org-file-headings nil)
        (buffer-point nil))
    (with-current-buffer (find-file-noselect org-file)
      (setq buffer-point (point))
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (cl-pushnew (org-hop-get-heading org-file)
                    org-file-headings :test #'equal))
      (goto-char buffer-point))
    (reverse org-file-headings)))

(defun org-hop-scan-headings (&optional force)
  "Scan the files returned by function `org-hop-files' for Org headings.

The Org headings are stored in `org-hop-cache'.
Scan only happens when files' modification times are greater
than `org-hop-last-scan'; or when files are missing from `org-hop-cache'.
See `org-hop-file-headings'.

When FORCE is non-nil, force the scan of all files."
  (let ((org-hop-cache-new nil)
        (org-file-cache-modified-time nil))
    (dolist (org-file (org-hop-files))
      (setq org-file-cache-modified-time (org-hop-file-attr-modified
                                    org-file
                                    org-hop-cache-update-if-buffer-modified))
      (cond
       ;; re-scan org-file headings
       ((or force
            (not org-hop-cache)
            (> org-file-cache-modified-time org-hop-last-scan)
            (not (assoc org-file org-hop-cache))
            (not (find-buffer-visiting org-file)))
        (message (format "Updating cache for %s..."
                         (file-name-nondirectory org-file)))
        (cl-pushnew `(,org-file . ,(list (org-hop-file-headings org-file)))
                    org-hop-cache-new  :test #'equal))
       ;; or just copy them from cache
       (t
        (cl-pushnew (assoc org-file org-hop-cache)
                    org-hop-cache-new :test #'equal))))
    (setq org-hop-cache org-hop-cache-new)
    (setq org-hop-last-scan (string-to-number (format-time-string "%s")))))

(defun org-hop-all-headings (&optional force)
  "Build the list of all possible Org headings.
With optional argument FORCE, rescan all files."
  (let ((all-org-headings-list nil))
    (org-hop-scan-headings force)
    (dotimes (n (length org-hop-cache))
      (setq all-org-headings-list
            (append (cadr (nth n org-hop-cache)) all-org-headings-list)))
    all-org-headings-list))


;;;; Recent Org headings

(defmacro org-hop-remove-dups-and-move-to-front (elt seq)
  "Remove duplicate items from SEQ and put ELT as CAR of SEQ."
  `(setq ,seq (cl-remove-duplicates ,seq :test #'equal :key #'car :from-end t))
  `(setq ,seq (cons ,elt (delete ,elt ,seq))))

(defun org-hop-add-heading (heading)
  "Add Org HEADING to `org-hop-headings-list'."
  (org-hop-remove-dups-and-move-to-front heading org-hop-headings-list))

(defun org-hop-add-marker (marker)
  "Add MARKER to `org-hop-markers-list'."
  (org-hop-remove-dups-and-move-to-front marker org-hop-markers-list))

(defun org-hop-add-marker-to-list (&optional verbose)
  "Save current `point-marker' to `org-hop-markers-list'.
When VERBOSE is non-nil, shows a notification in echo area."
  (interactive)
  (let ((item (org-hop-get-marker)))
    (org-hop-add-marker item)
    (when (or verbose (called-interactively-p 'any))
      (message (format "Saved marker %s:%s"
                       (buffer-name)
                       (plist-get (cadr item) :line))))))

(defun org-hop-add-heading-to-list (&optional verbose)
  "When under an Org heading, add it to `org-hop-headings-list'.
Elsewhere, add position to `org-hop-add-marker-to-list'.
If VERBOSE is non-nil, show messages in echo area."
  (interactive)
  (let ((pos (point)))
    (if (and (eq major-mode 'org-mode)
             (buffer-file-name) ; NOTE: ignores indirect/capture buffers
             (or (org-at-heading-p)
                 (and (re-search-backward org-heading-regexp nil t)
                      (org-at-heading-p))))
        (progn
          (goto-char (point-at-eol))  ; NOTE: make sure we get the right heading
          (let ((heading (org-hop-get-heading)))
            (org-hop-add-heading heading)
            (if (or verbose (called-interactively-p 'any))
                (message (format "Saved %s" (car heading))))))
      (org-hop-add-marker-to-list verbose))
    (goto-char pos)))


;;;;; Actions

(defun org-hop-get-coordinates (candidate)
  "Return coordinates to hop to CANDIDATE."
  (let* ((type   (plist-get (car candidate) :type))
         (marker (plist-get (car candidate) :marker))
         (file   (plist-get (car candidate) :file))
         (line   (plist-get (car candidate) :line))
         (buffer (or (marker-buffer marker)
                     (find-buffer-visiting file)
                     (find-file-noselect file)))
         (char   (marker-position marker)))
    (if buffer
        `(:type ,type :buffer ,buffer :line ,line :char ,char)
      (if (equal type "marker")
          (org-hop-remove-marker candidate)
        (org-hop-remove-heading candidate))
      (message "Buffer %s vanished." buffer)
      nil)))

(defun org-hop-goto-char-or-line (char line)
  "If CHAR is non-nil, go to char.  Else, go to LINE."
  (if char (goto-char char)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun org-hop-to-entry (candidate)
  "Hop to entry CANDIDATE in buffer."
  (let* ((entry  (org-hop-get-coordinates candidate))
         (buffer (plist-get entry :buffer))
         (char   (plist-get entry :char))
         (line   (plist-get entry :line)))
    (when entry
      (if org-hop-mark-ring-push (org-mark-ring-push))
      (if org-hop-switch-to-buffer-other-window
          (switch-to-buffer-other-window buffer)
        (switch-to-buffer buffer))
      (org-hop-goto-char-or-line char line)
      ;; post-hop actions
      (cond ((and (eq major-mode 'org-mode) (org-at-heading-p))
             (org-hop-remove-heading candidate)
             (org-hop-add-heading (org-hop-get-heading))
             (goto-char (point-at-bol))
             (org-fold-show-context)
             (org-fold-show-entry)
             (org-fold-show-children))
            (t
             (org-hop-remove-marker candidate)
             (org-hop-add-marker (org-hop-get-marker))))
      (recenter org-hop-recenter))))


;;;; Remove items from recent lists

(defmacro org-hop-remove (item list &optional verbose)
  "Remove an ITEM from a recent from LIST."
  `(let ((entry (rassoc ,item ,list)))
     (setq ,list (remove entry ,list))
     (when (or ,verbose (called-interactively-p 'any))
       (message (format "Removed from recent list: %s" (car entry))))))

(defun org-hop-remove-heading (item &optional verbose)
  "Remove ITEM from recent headings list."
  (org-hop-remove item org-hop-headings-list verbose))

(defun org-hop-remove-marker (item &optional verbose)
  "Remove ITEM from recent marker list."
  (org-hop-remove item org-hop-markers-list verbose))

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

(defun org-hop-remove-marker-from-list (&optional arg)
  "Remove an item from a recent list using `completing-read'.
With optional argument ARG, set list to nil."
  (interactive "P")
  (org-hop-remove-from-list org-hop-markers-list arg))


;;;; Main commands

;;;###autoload
(defun org-hop (&optional arg)
  "Hop to a Org heading.
With optional argument ARG, force refresh all lists."
  (interactive "P")
  (when arg (org-hop-reset))
  (let* ((org-headings (append org-hop-headings-list
                               org-hop-markers-list
                               (org-hop-all-headings arg)))
         (item (completing-read "Hop to: " org-headings))
         (entry (cdr (assoc item org-headings))))
    (org-hop-to-entry entry)))

;;;###autoload
(defun org-hop-add-heading-or-marker (&optional arg)
  "Add current heading to recent list.
With optional argument ARG, add current position as a marker."
  (interactive "P")
  (if arg
      (org-hop-add-marker-to-list t)
    (org-hop-add-heading-to-list t)))

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

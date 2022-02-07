;;; org-hop.el --- Hop to Org headings -*- lexical-binding: t -*-

;; Copyright (C) 2022 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/org-hop
;; Version: 0.1
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
  "Which Org files should be scanned by `org-hop-headings-scan'.
This variable controls the `org-hop-files' function. Default values are:

  'agenda          list of Org agenda files
  'buffers         list of open Org buffers
  'org-files-list  list of Org agenda files + open Org buffers

Alternatively, this variable can be a custom list of Org files."
  :group 'org-hop
  :type 'sexp)

(defcustom org-hop-files-main nil
  "List of Org files that must always be scanned for headings.
This list is appended to `org-hop-files'."
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
  "Idle interval, in seconds, to add current Org heading to `org-hop-recent-list'. See `org-hop-recent-mode'."
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


;;;; Variables

(defvar org-hop-cache nil
  "Org headings cache. See `org-hop-headings-scan'.")

(defvar org-hop-last-scan 0
  "Time of the last `org-hop-cache'. See `org-hop-headings-scan'.")

(defvar org-hop-recent-list nil
  "List of Org headings recently visited. See `org-hop-add-recent'.")

(defvar org-hop-marker-list nil
  "List of saved point-markers. See `org-hop-add-marker'.")


;;;; Functions

(defun org-hop-reset ()
  "Reset lists."
  (setq org-hop-cache nil
        org-hop-recent-list nil
        org-hop-marker-list nil))

(defun org-hop-file-attr-modified (file)
  "Return FILE modification time attribute in seconds."
  (string-to-number
   (format-time-string
    "%s" (file-attribute-modification-time (file-attributes file)))))

(defun org-hop-files-truename (files)
  "Return the truenames of a list of FILES."
  (interactive)
  (let ((files-truename-list nil))
    (dolist (file files)
      (cl-pushnew (file-truename file) files-truename-list))
    (remove nil (delete-dups (reverse files-truename-list)))))

(defun org-hop-files ()
  "Return a list of Org files using their truenames.
This function is controlled by the `org-hop-files' variable."
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
    (org-hop-files-truename (if org-hop-files-main
                                (append org-hop-files-main org-files)
                              org-files))))

(defun org-hop-get-heading (&optional org-file)
  "Get Org heading at point."
  (interactive)
  (let ((org-filename (file-name-nondirectory
                       (or org-file (buffer-file-name))))
        (tags (org-get-tags)))
    (identity `(,(concat (org-format-outline-path
                          (org-get-outline-path t t)
                          org-hop-headings-width
                          (if org-hop-headings-with-filename
                              (format "%s:" org-filename))
                          "/")
                         (if (and org-hop-headings-with-tags tags)
                             (format " %s" (org-make-tag-string tags))))
                . ,(point-marker)))))

(defun org-hop-headings-file (org-file)
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

(defun org-hop-headings-scan (&optional force)
  "Scan the files returned by `org-hop-files' for Org headings.

The Org headings are stored in `org-hop-cache'.
The scan only happens when files' modification times are greater
than `org-hop-last-scan'; or when files are missing
from `org-hop-cache'.

When FORCE is non-nil, force the scan of all files.

See `org-hop-headings-file'."
  (let ((org-hop-cache-new nil)
        (org-file-modified-time nil))
    (dolist (org-file (org-hop-files))
      (setq org-file-modified-time (org-hop-file-attr-modified org-file))
      (cond
       ((or force ;; re-read org-file headings
            (not org-hop-cache)
            (> org-file-modified-time org-hop-last-scan)
            (not (assoc org-file org-hop-cache))
            (not (find-buffer-visiting org-file)))
        (message (format "Updating cache for %s..."
                         (file-name-nondirectory org-file)))
        (cl-pushnew `(,org-file . ,(list (org-hop-headings-file org-file)))
                    org-hop-cache-new  :test #'equal))
       (t         ;; or to just copy them from cache
        (cl-pushnew (assoc org-file org-hop-cache)
                    org-hop-cache-new  :test #'equal))))
    (setq org-hop-cache org-hop-cache-new)
    (setq org-hop-last-scan (string-to-number (format-time-string "%s")))))

(defun org-hop-headings (&optional force)
  "Build the Org headings list."
  (let ((org-headings-list))
    (org-hop-headings-scan force)
    (dotimes (_ (length org-hop-cache) org-headings-list)
      (setq org-headings-list (append (cadr (nth _ org-hop-cache))
                                      org-headings-list)))))


;;;; Recent Org headings

(defun org-hop-add-recent ()
  "Add Org heading to `org-hop-recent-list'."
  (interactive)
  (let ((heading (org-hop-get-heading)))
    ;; NOTE: always remove duplicate keys
    (setq org-hop-recent-list (cl-remove-duplicates
                               org-hop-recent-list
                               :test #'equal :key #'cdr :from-end t))
    (setq org-hop-recent-list
          (cons heading (remove heading org-hop-recent-list)))))

(defun org-hop-add-heading-to-recent (&optional add-marker verbose)
  "When under an Org heading, add it to `org-hop-recent-list'.
If VERBOSE is non-nil, show messages.

If ADD-MARKER is non-nil, add current marker to `org-hop-marker-list' if position is not under an Org heading."
  (interactive)
  (let ((position (point)))
    (cond ((and (eq major-mode 'org-mode)
                (buffer-file-name) ; NOTE: ignores indirect/capture buffers
                (or (org-at-heading-p)
                    (and (re-search-backward org-heading-regexp nil t)
                         (org-at-heading-p))))
           ;; NOTE: move to eol to make sure marker gets the right heading
           (goto-char (point-at-eol))
           (org-hop-add-recent)
           (if verbose
               (message (format "Saved %s" (car (org-hop-get-heading))))))
          (add-marker
           (org-hop-add-marker)))
    (goto-char position)))

(defun org-hop-add-marker ()
  "Save current point-marker to `org-hop-marker-list'."
  (interactive)
  (let ((buffer (if (buffer-file-name)
                    (file-name-nondirectory (buffer-file-name))
                  (buffer-name)))
        (line-number (line-number-at-pos))
        (line (thing-at-point 'line)))
    (cl-pushnew `(,(replace-regexp-in-string
                    "\n" "" (format "%s:%s %s" buffer line-number line))
                  . ,(point-marker))
                org-hop-marker-list)
    (message (format "Saved marker %s:%s" buffer line-number))))


;;;;; Actions on markers

(defun org-hop-to-marker (marker)
  "Hop to MARKER in buffer."
  (if (not (marker-buffer marker))
      (progn
        (org-hop-remove-recent marker)
        (message "Buffer vanished."))
    (if org-hop-mark-ring-push (org-mark-ring-push))
    (if org-hop-switch-to-buffer-other-window
        (switch-to-buffer-other-window (marker-buffer marker))
      (switch-to-buffer (marker-buffer marker)))
    (goto-char (marker-position marker))
    (when (and (eq major-mode 'org-mode) (org-at-heading-p))
      (org-hop-add-recent)
      (org-show-context)
      (re-search-backward org-heading-regexp nil t)
      (org-show-entry)
      (org-show-children))))

(defun org-hop-remove-recent (marker)
  "Remove Org heading from `org-hop-recent-list'."
  (let ((heading (rassoc marker org-hop-recent-list)))
    (setq org-hop-recent-list (remove heading org-hop-recent-list))
    (message (format "Removed from recent list: %s" (car heading)))))

(defun org-hop-remove-marker (marker)
  "Remove Org heading from `org-hop-marker-list'."
  (let ((line (rassoc marker org-hop-marker-list)))
    (setq org-hop-marker-list (remove line org-hop-marker-list))
    (message (format "Removed from marker list: %s"
                     (car (split-string
                           (substring-no-properties (car line)) " "))))))

(defun org-hop-remove-from-recent-list (&optional arg)
  "Remove a heading from `org-hop-recent-list'.
With C-u, reset the list."
  (interactive "P")
  (if arg (setq org-hop-recent-list nil)
    (let* ((entry (completing-read "Remove from list: " org-hop-recent-list))
           (marker (cdr (assoc entry org-hop-recent-list))))
      (org-hop-remove-recent marker))))

(defun org-hop-remove-from-marker-list (&optional arg)
  "Remove a marker from `org-hop-marker-list'.
With C-u, reset the list."
  (interactive "P")
  (if arg (setq org-hop-marker-list nil)
    (let* ((entry (completing-read "Remove from list: " org-hop-marker-list))
           (marker (cdr (assoc entry org-hop-marker-list))))
      (org-hop-remove-marker marker))))


;;;; Main commands

;;;###autoload
(defun org-hop (&optional arg)
  "Hop to a Org heading.
With C-u, force refresh all lists."
  (interactive "P")
  (when arg (org-hop-reset))
  (let* ((org-headings (append org-hop-recent-list
                               org-hop-marker-list
                               (org-hop-headings arg)))
         (entry (completing-read "Hop to: " org-headings))
         (marker (cdr (assoc entry org-headings))))
    (org-hop-to-marker marker)))

;;;###autoload
(defun org-hop-add-heading-or-marker ()
  (interactive)
  (org-hop-add-heading-to-recent t t))

(define-minor-mode org-hop-recent-mode
  "Toggle org-hop-recent mode.
When idle, add current Org heading to `org-hop-recent-list'."
  :init-value nil
  :lighter nil
  :group 'org-hop
  (if org-hop-recent-mode
      (run-with-idle-timer org-hop-recent-idle-interval t
                           #'org-hop-add-heading-to-recent)
    (cancel-function-timers #'org-hop-add-heading-to-recent)))


(provide 'org-hop)

;;; org-hop.el ends here

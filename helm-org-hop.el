;;; helm-org-hop.el --- Helm for org-hop -*- lexical-binding: t -*-

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

;; Helm interface for org-hop.

;;; Code:

(require 'org-hop)
(require 'helm)
(require 'helm-source)


;;;; Custom variables

(defgroup helm-org-hop nil
  "Group for `helm-org-hop' customizations."
  :group 'org-hop)

(defcustom helm-org-hop-capture-key "n"
  "The string associated with a template in org-capture-templates."
  :group 'helm-org-hop
  :type 'string)

(defcustom helm-org-hop-capture-insert-input t
  "If non-nil, insert user input in org-capture buffer."
  :group 'helm-org-hop
  :type 'boolean)

(defcustom helm-org-hop-headings-actions
  '(("Hop to heading          " . org-hop-to-marker)
    ("Store link to heading"    . helm-org-hop-headings-store-link)
    ("Insert link to heading"   . helm-org-hop-headings-insert-link))
  "Default actions alist for `helm-org-hop-headings-source'."
  :group 'helm-org-hop
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-hop-recent-actions
  '(("Hop to heading"           . org-hop-to-marker)
    ("Store link to heading"    . helm-org-hop-headings-store-link)
    ("Insert link to heading"   . helm-org-hop-headings-insert-link)
    ("Remove heading from list" . helm-org-hop-remove-recent-multi))
  "Default actions alist for `helm-org-hop-recent-source'."
  :group 'helm-org-hop
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-hop-marker-actions
  '(("Hop to marker"           . org-hop-to-marker)
    ("Store link to marker"    . helm-org-hop-marker-store-link)
    ("Insert link to marker"   . helm-org-hop-marker-insert-link)
    ("Remove marker from list" . helm-org-hop-remove-marker-multi))
  "Default actions alist for `helm-org-hop-marker-source'."
  :group 'helm-org-hop
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-hop-capture-actions
  '(("Capture note"            . helm-org-hop-capture-note))
  "Default actions alist for `helm-org-hop-capture-source'."
  :group 'helm-org-hop
  :type '(alist :key-type string :value-type function))


;;;; Variables

(defvar helm-org-hop-headings-source nil
  "Helm source for `org-hop-headings'.")

(defvar helm-org-hop-recent-source nil
  "Helm source for `org-hop-recent-list'.")

(defvar helm-org-hop-marker-source nil
  "Helm source for `org-hop-marker-list'.")

(defvar helm-org-hop-capture-source nil
  "Helm source for note capturing.")


;;;; Functions

;;;;; Headings actions

(defun helm-org-hop-headings-store-link (marker)
  (let ((position (point)))
    (with-current-buffer (marker-buffer marker)
      (goto-char (marker-position marker))
      (call-interactively 'org-store-link))
    (goto-char position)))

(defun helm-org-hop-headings-insert-link (marker)
  (let ((position (point)))
    (with-current-buffer (marker-buffer marker)
      (goto-char (marker-position marker))
      (call-interactively 'org-store-link))
    (goto-char position)
    (org-insert-all-links 1 "" "")))

(defun helm-org-hop-remove-recent-multi (marker)
  (let ((size (length (helm-marked-candidates))))
    (dolist (entry (helm-marked-candidates))
      (org-hop-remove-recent entry))
    (if (> size 1)
        (message (format "Removed %s headings from recent list" size)))))


;;;;; Markers actions

(defun helm-org-hop-store-marker-link ()
  (let ((file (buffer-file-name))
        (line (line-number-at-pos)))
    (add-to-list 'org-stored-links
                 `(,(format "file:%s::%s" file line)
                   ,(format "%s::%s" (file-name-nondirectory file) line))
                 t)))

(defun helm-org-hop-marker-store-link (marker)
  (let ((position (point)))
    (with-current-buffer (marker-buffer marker)
      (goto-char (marker-position marker))
      (helm-org-hop-store-marker-link))
    (goto-char position)))

(defun helm-org-hop-marker-insert-link (marker)
  (let ((position (point)))
    (with-current-buffer (marker-buffer marker)
      (goto-char (marker-position marker))
      (helm-org-hop-store-marker-link))
    (goto-char position)
    (org-insert-all-links 1 "" "")))

(defun helm-org-hop-remove-marker-multi (marker)
  (let ((size (length (helm-marked-candidates))))
    (dolist (entry (helm-marked-candidates))
      (org-hop-remove-marker entry))
    (if (> size 1)
        (message (format "Removed %s markers from recent list" size)))))


;;;;; Capture actions

(defun helm-org-hop-capture-note (input)
  (org-capture nil helm-org-hop-capture-key)
  (if helm-org-hop-capture-insert-input (insert input)))



;;;;; Helm build sources

(defun helm-org-hop-build-sources (&optional force)
  (when force (org-hop-reset))
  (setq helm-org-hop-recent-source
        (helm-build-sync-source "Recent Org headings: "
          :action 'helm-org-hop-recent-actions
          :candidates org-hop-recent-list))

  (setq helm-org-hop-marker-source
        (helm-build-sync-source "Hop to marker: "
          :action 'helm-org-hop-marker-actions
          :candidates org-hop-marker-list))

  (setq helm-org-hop-headings-source
        (helm-build-sync-source "Org headings: "
          :action 'helm-org-hop-headings-actions
          :candidates (org-hop-headings force)))

  (setq helm-org-hop-capture-source
        (helm-build-dummy-source "Create note"
          :action 'helm-org-hop-capture-actions)))

;;;###autoload
(defun helm-org-hop (&optional arg)
  "Helm for Org headings.
With C-u, force refresh all lists."
  (interactive "P")
  (helm-org-hop-build-sources arg)
  (helm :buffer "*helm-org-hop*"
        :sources '(helm-org-hop-recent-source
                   helm-org-hop-marker-source
                   helm-org-hop-headings-source
                   helm-org-hop-capture-source)))


(provide 'helm-org-hop)

;;; helm-org-hop.el ends here

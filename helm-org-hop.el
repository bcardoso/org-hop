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

(defvar helm-source-org-hop-headings nil
  "Helm source for `org-hop-headings'.")

(defvar helm-source-org-hop-recent nil
  "Helm source for `org-hop-recent-list'.")

(defvar helm-source-org-hop-marker nil
  "Helm source for `org-hop-marker-list'.")

(defvar helm-org-hop-headings-actions
  '(("Hop to heading          " . org-hop-to-marker)
    ("Store link to heading"    . helm-org-hop-headings-store-link)
    ("Insert link to heading"   . helm-org-hop-headings-insert-link))
  "Default actions alist for `helm-source-org-hop-headings'.")

(defvar helm-org-hop-recent-actions
  '(("Hop to heading"           . org-hop-to-marker)
    ("Store link to heading"    . helm-org-hop-headings-store-link)
    ("Insert link to heading"   . helm-org-hop-headings-insert-link)
    ("Remove heading from list" . helm-org-hop-remove-recent-multi))
  "Default actions alist for `helm-source-org-hop-recent'.")

(defvar helm-org-hop-marker-actions
  '(("Hop to marker"           . org-hop-to-marker)
    ("Store link to marker"    . helm-org-hop-marker-store-link)
    ("Insert link to marker"   . helm-org-hop-marker-insert-link)
    ("Remove marker from list" . helm-org-hop-remove-marker-multi))
  "Default actions alist for `helm-source-org-hop-marker'.")


;;;; Headings actions

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
  (dolist (entry (helm-marked-candidates))
    (org-hop-remove-recent entry)))


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
  (dolist (entry (helm-marked-candidates))
    (org-hop-remove-marker entry)))


;;;; Helm build sources

(defun helm-org-hop-build-sources (&optional force)
  (when force (org-hop-reset))
  (setq helm-source-org-hop-recent
        (helm-build-sync-source "Recent Org headings: "
          :action 'helm-org-hop-recent-actions
          :candidates org-hop-recent-list))

  (setq helm-source-org-hop-marker
        (helm-build-sync-source "Hop to marker: "
          :action 'helm-org-hop-marker-actions
          :candidates org-hop-marker-list))

  (setq helm-source-org-hop-headings
        (helm-build-sync-source "Org headings: "
          :action 'helm-org-hop-headings-actions
          :candidates (org-hop-headings force))))

;;;###autoload
(defun helm-org-hop (&optional arg)
  "Helm for Org headings.
With C-u, force refresh all lists."
  (interactive "P")
  (helm-org-hop-build-sources arg)
  (helm :buffer "*helm-org-hop*"
        :sources '(helm-source-org-hop-recent
                   helm-source-org-hop-marker
                   helm-source-org-hop-headings)))


(provide 'helm-org-hop)

;;; helm-org-hop.el ends here

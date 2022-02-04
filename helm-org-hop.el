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

(defvar helm-org-hop-headings-source nil
  "Helm source for `org-hop-headings'.")

(defvar helm-org-hop-recent-source nil
  "Helm source for `org-hop-recent-list'.")

(defvar helm-org-hop-marker-source nil
  "Helm source for `org-hop-marker-list'.")

(defvar helm-org-hop-headings-actions
  '(("Hop to heading" . org-hop-to-marker))
  "Default actions alist for `helm-org-hop-headings-source'.")

(defvar helm-org-hop-recent-actions
  '(("Hop to heading"           . org-hop-to-marker)
    ("Remove heading from list" . org-hop-remove-recent))
  "Default actions alist for `helm-org-hop-recent-source'.")

(defvar helm-org-hop-marker-actions
  '(("Hop to marker"           . org-hop-to-marker)
    ("Remove marker from list" . org-hop-remove-marker))
  "Default actions alist for `helm-org-hop-marker-source'.")

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
          :candidates (org-hop-headings force))))

;;;###autoload
(defun helm-org-hop (&optional arg)
  "Helm for Org headings.
With C-u, force refresh all lists."
  (interactive "P")
  (helm-org-hop-build-sources arg)
  (helm :buffer "helm-org-hop"
        :sources '(helm-org-hop-recent-source
                   helm-org-hop-marker-source
                   helm-org-hop-headings-source)))


(provide 'helm-org-hop)

;;; helm-org-hop.el ends here

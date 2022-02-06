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
  "The key string associated with a template in org-capture-templates."
  :group 'helm-org-hop
  :type 'string)

(defcustom helm-org-hop-capture-insert-input t
  "If non-nil, insert user input in org-capture buffer."
  :group 'helm-org-hop
  :type 'boolean)

(defcustom helm-org-hop-headings-actions
  '(("Hop to heading"                   . org-hop-to-marker)
    ("Store link to heading  `C-c l`"   . helm-org-hop-headings-store-link)
    ("Insert link to heading `C-c C-l`" . helm-org-hop-headings-insert-link))
  "Default actions alist for `helm-org-hop-headings-source'."
  :group 'helm-org-hop
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-hop-recent-actions
  '(("Hop to heading"                   . org-hop-to-marker)
    ("Store link to heading `C-c l`"    . helm-org-hop-headings-store-link)
    ("Insert link to heading `C-c C-l`" . helm-org-hop-headings-insert-link)
    ("Remove heading from list `M-D`"   . helm-org-hop-remove-recent))
  "Default actions alist for `helm-org-hop-recent-source'."
  :group 'helm-org-hop
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-hop-marker-actions
  '(("Hop to marker"                    . org-hop-to-marker)
    ("Store link to marker `C-c l`"     . helm-org-hop-marker-store-link)
    ("Insert link to marker `C-c C-l`"  . helm-org-hop-marker-insert-link)
    ("Remove marker from list `M-D`"    . helm-org-hop-remove-marker))
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

(defun helm-org-hop-remove (type marker)
  (let ((num (length (helm-marked-candidates))))
    (dolist (entry (helm-marked-candidates))
      (if (eq type 'heading)
          (org-hop-remove-recent entry)
        (org-hop-remove-marker entry)))
    (if (> num 1)
        (message (format "Removed %s entries from %s list" num type)))))

(defun helm-org-hop-insert-link (type marker)
  (let ((org-link-file-path-type 'absolute)
        (position (point))
        (num (length (helm-marked-candidates))))
    (dolist (entry (reverse (helm-marked-candidates)))
      (with-current-buffer (marker-buffer entry)
        (goto-char (marker-position entry))
        (let ((inhibit-message t))
          (if (eq type 'heading)
              (call-interactively 'org-store-link)
            (helm-org-hop-store-marker-link)))))
    (goto-char position)
    (if (> num 1)
        (org-insert-all-links num "- " "\n")
      (org-insert-all-links 1 "" ""))))

(defun helm-org-hop-store-link (type marker)
  (let ((position (point))
        (num (length (helm-marked-candidates))))
    (dolist (entry (helm-marked-candidates))
      (with-current-buffer (marker-buffer entry)
        (goto-char (marker-position entry))
        (if (eq type 'heading)
            (call-interactively 'org-store-link)
          (helm-org-hop-store-marker-link))))
    (goto-char position)
    (if (> num 1)
        (message (format "Stored %s links" num)))))

(defun helm-org-hop-store-marker-link ()
  (let* ((file (buffer-file-name))
         (filename (file-name-nondirectory file))
         (line (line-number-at-pos)))
         ;;(line (org-link--normalize-string (org-current-line-string) t)))
    (add-to-list 'org-stored-links
                 `(,(format "file:%s::%s" file line)
                   ,(format "%s:%s" filename line))
                 t)
    (message "Stored link for %s:%s" filename line)))


;;;;; Headings actions

(defun helm-org-hop-headings-store-link (marker)
  (helm-org-hop-store-link 'heading marker))

(defun helm-org-hop-headings-insert-link (marker)
  (helm-org-hop-insert-link 'heading marker))

(defun helm-org-hop-remove-recent (marker)
  (helm-org-hop-remove 'heading marker))


;;;;; Markers actions

(defun helm-org-hop-marker-store-link (marker)
  (helm-org-hop-store-link 'marker marker))

(defun helm-org-hop-marker-insert-link (marker)
  (helm-org-hop-insert-link 'marker marker))

(defun helm-org-hop-remove-marker (marker)
  (helm-org-hop-remove 'marker marker))


;;;;; Capture actions

(defun helm-org-hop-capture-note (input)
  (org-capture nil helm-org-hop-capture-key)
  (if helm-org-hop-capture-insert-input (insert input)))


;;;;; Helm build sources

(defun helm-org-hop-run-headings-store-link ()
  "Run interactively `helm-org-hop-headings-store-link'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-hop-headings-store-link)))

(defun helm-org-hop-run-headings-insert-link ()
  "Run interactively `helm-org-hop-headings-insert-link'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-hop-headings-insert-link)))

(defun helm-org-hop-run-remove-recent ()
  "Run interactively `helm-org-hop-remove-recent'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-hop-remove-recent)))

(defun helm-org-hop-run-marker-store-link ()
  "Run interactively `helm-org-hop-marker-store-link'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-hop-marker-store-link)))

(defun helm-org-hop-run-marker-insert-link ()
  "Run interactively `helm-org-hop-marker-insert-link'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-hop-marker-insert-link)))

(defun helm-org-hop-run-remove-marker ()
  "Run interactively `helm-org-hop-remove-marker'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-hop-remove-marker)))

(defvar helm-org-hop-headings-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c l")   'helm-org-hop-run-headings-store-link)
    (define-key map (kbd "C-c C-l") 'helm-org-hop-run-headings-insert-link)
    map)
  "Keymap for `helm-org-hop-headings-source'.")

(defvar helm-org-hop-recent-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c l")   'helm-org-hop-run-headings-store-link)
    (define-key map (kbd "C-c C-l") 'helm-org-hop-run-headings-insert-link)
    (define-key map (kbd "M-D")     'helm-org-hop-run-remove-recent)
    map)
  "Keymap for `helm-org-hop-recent-source'.")

(defvar helm-org-hop-marker-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c l")   'helm-org-hop-run-marker-store-link)
    (define-key map (kbd "C-c C-l") 'helm-org-hop-run-marker-insert-link)
    (define-key map (kbd "M-D")     'helm-org-hop-run-remove-marker)
    map)
  "Keymap for `helm-org-hop-marker-source'.")

(defun helm-org-hop-build-sources (&optional force)
  (when force (org-hop-reset))
  (setq helm-org-hop-recent-source
        (helm-build-sync-source "Recent Org headings: "
          :action 'helm-org-hop-recent-actions
          :keymap helm-org-hop-recent-map
          :candidates org-hop-recent-list))

  (setq helm-org-hop-marker-source
        (helm-build-sync-source "Hop to marker: "
          :action 'helm-org-hop-marker-actions
          :keymap helm-org-hop-marker-map
          :candidates org-hop-marker-list))

  (setq helm-org-hop-headings-source
        (helm-build-sync-source "Org headings: "
          :action 'helm-org-hop-headings-actions
          :keymap helm-org-hop-headings-map
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

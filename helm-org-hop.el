;;; helm-org-hop.el --- Helm for org-hop -*- lexical-binding: t -*-

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

;; Helm interface for org-hop.

;;; Code:

(require 'org)
(require 'org-hop)
(require 'helm)
(require 'helm-source)


;;;; Custom variables

(defgroup helm-org-hop nil
  "Group for `helm-org-hop' customizations."
  :group 'org-hop)

(defcustom helm-org-hop-capture-key "n"
  "The key string associated with a template in `org-capture-templates'."
  :group 'helm-org-hop
  :type 'string)

(defcustom helm-org-hop-capture-insert-input t
  "If non-nil, insert user input in `org-capture' buffer."
  :group 'helm-org-hop
  :type 'boolean)

(defcustom helm-org-hop-headings-actions
  '(("Hop to heading"                   . org-hop-to-entry)
    ("Store link to heading  `C-c l`"   . helm-org-hop-headings-store-link)
    ("Insert link to heading `C-c C-l`" . helm-org-hop-headings-insert-link))
  "Default actions alist for `helm-org-hop-all-headings-source'."
  :group 'helm-org-hop
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-hop-recent-headings-actions
  '(("Hop to heading"                     . org-hop-to-entry)
    ("Store link to heading    `C-c l`"   . helm-org-hop-headings-store-link)
    ("Insert link to heading   `C-c C-l`" . helm-org-hop-headings-insert-link)
    ("Remove heading from list `M-D`"     . helm-org-hop-remove-heading))
  "Default actions alist for `helm-org-hop-headings-source'."
  :group 'helm-org-hop
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-hop-lines-actions
  '(("Hop to line"                     . org-hop-to-entry)
    ("Store link to line    `C-c l`"   . helm-org-hop-lines-store-link)
    ("Insert link to line   `C-c C-l`" . helm-org-hop-lines-insert-link)
    ("Remove line from list `M-D`"     . helm-org-hop-remove-line))
  "Default actions alist for `helm-org-hop-lines-source'."
  :group 'helm-org-hop
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-hop-capture-actions
  '(("Capture note" . helm-org-hop-capture-note))
  "Default actions alist for `helm-org-hop-capture-source'."
  :group 'helm-org-hop
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-hop-default-sources '(helm-org-hop-headings-source
                                          helm-org-hop-lines-source
                                          helm-org-hop-all-headings-source
                                          helm-org-hop-capture-source)
  "Default sources list used in `helm-org-hop'."
  :group 'helm-org-hop
  :type '(repeat (choice symbol)))


;;;; Variables

(defvar helm-org-hop-all-headings-source nil
  "Helm source for `org-hop-all-headings'.")

(defvar helm-org-hop-headings-source nil
  "Helm source for `org-hop-headings-list'.")

(defvar helm-org-hop-lines-source nil
  "Helm source for `org-hop-lines-list'.")

(defvar helm-org-hop-capture-source nil
  "Helm source for note capturing.")


;;;; Actions

(defun helm-org-hop-remove (type)
  "Remove candidates in variable `helm-marked-candidates' from recent list.
Argument TYPE indicates if candidate is a 'heading or 'line."
  (let* ((num (length (helm-marked-candidates)))
         (verbose (eq num 1)))
    (dolist (entry-data (helm-marked-candidates))
      (if (eq type 'heading)
          (org-hop-remove-heading entry-data verbose)
        (org-hop-remove-line entry-data verbose)))
    (if (> num 1)
        (message (format "Removed %s entries from %s list." num type)))))

(defun helm-org-hop-insert-link (type)
  "Insert Org links to candidates in variable `helm-marked-candidates'.
Argument TYPE indicates if candidate is a 'heading or 'line."
  (let ((org-link-file-path-type 'absolute)
        (current-buffer (current-buffer))
        (point (point))
        (num (length (helm-marked-candidates))))
    (dolist (entry-data (reverse (helm-marked-candidates)))
      (let ((entry (org-hop-get-coordinates entry-data)))
        (when entry
          (with-current-buffer (plist-get entry :buffer)
            (org-hop-to-char-or-line (plist-get entry :char)
                                     (plist-get entry :line))
            (let ((inhibit-message t))
              (if (eq type 'heading)
                  (call-interactively 'org-store-link)
                (helm-org-hop-store-line-link)))))))
    (switch-to-buffer current-buffer)
    (goto-char point)
    (if (> num 1)
        (org-insert-all-links num "- " "\n")
      (org-insert-all-links 1 "" ""))))

(defun helm-org-hop-store-link (type)
  "Store Org links to candidates in variable `helm-marked-candidates'.
Argument TYPE indicates if candidate is a 'heading or 'line."
  (let ((current-buffer (current-buffer))
        (point (point))
        (num (length (helm-marked-candidates))))
    (dolist (entry-data (helm-marked-candidates))
      (let ((entry (org-hop-get-coordinates entry-data)))
        (when entry
          (with-current-buffer (plist-get entry :buffer)
            (org-hop-to-char-or-line (plist-get entry :char)
                                     (plist-get entry :line))
            (if (eq type 'heading)
                (call-interactively 'org-store-link)
              (helm-org-hop-store-line-link))))))
    (switch-to-buffer current-buffer)
    (goto-char point)
    (if (> num 1)
        (message (format "Stored %s links" num)))))

(defun helm-org-hop-store-line-link ()
  "Store custom Org links to candidates in variable `helm-marked-candidates'."
  (let* ((file        (buffer-file-name))
         (filename    (file-name-nondirectory file))
         (line-number (line-number-at-pos)))
    (add-to-list 'org-stored-links
                 `(,(format "file:%s::%s" file line-number)
                   ,(format "%s:%s" filename line-number))
                 t)
    (message "Stored link for %s:%s" filename line-number)))


;;;;; Headings actions

(defun helm-org-hop-headings-store-link (&optional candidate)
  "Helm action to store an Org link to CANDIDATE."
  (ignore candidate)
  (helm-org-hop-store-link 'heading))

(defun helm-org-hop-headings-insert-link (&optional candidate)
  "Helm action to insert an Org link to CANDIDATE."
  (ignore candidate)
  (helm-org-hop-insert-link 'heading))

(defun helm-org-hop-remove-heading (&optional candidate)
  "Helm action to remove CANDIDATE from recent list."
  (ignore candidate)
  (helm-org-hop-remove 'heading))


;;;;; Lines actions

(defun helm-org-hop-lines-store-link (&optional candidate)
  "Helm action to store an Org link to CANDIDATE."
  (ignore candidate)
  (helm-org-hop-store-link 'line))

(defun helm-org-hop-lines-insert-link (&optional candidate)
  "Helm action to insert an Org link to CANDIDATE."
  (ignore candidate)
  (helm-org-hop-insert-link 'line))

(defun helm-org-hop-remove-line (&optional candidate)
  "Helm action to remove CANDIDATE from recent list."
  (ignore candidate)
  (helm-org-hop-remove 'line))


;;;;; Capture actions

(defun helm-org-hop-capture-note (input)
  "Helm action to `org-capture' a note based on INPUT."
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
  "Run interactively `helm-org-hop-remove-heading'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-hop-remove-heading)))

(defun helm-org-hop-run-line-store-link ()
  "Run interactively `helm-org-hop-lines-store-link'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-hop-lines-store-link)))

(defun helm-org-hop-run-lines-insert-link ()
  "Run interactively `helm-org-hop-lines-insert-link'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-hop-lines-insert-link)))

(defun helm-org-hop-run-remove-line ()
  "Run interactively `helm-org-hop-remove-line'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-hop-remove-line)))

(defvar helm-org-hop-headings-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c l")   'helm-org-hop-run-headings-store-link)
    (define-key map (kbd "C-c C-l") 'helm-org-hop-run-headings-insert-link)
    map)
  "Keymap for `helm-org-hop-all-headings-source'.")

(defvar helm-org-hop-recent-headings-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c l")   'helm-org-hop-run-headings-store-link)
    (define-key map (kbd "C-c C-l") 'helm-org-hop-run-headings-insert-link)
    (define-key map (kbd "M-D")     'helm-org-hop-run-remove-recent)
    map)
  "Keymap for `helm-org-hop-headings-source'.")

(defvar helm-org-hop-lines-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c l")   'helm-org-hop-run-line-store-link)
    (define-key map (kbd "C-c C-l") 'helm-org-hop-run-lines-insert-link)
    (define-key map (kbd "M-D")     'helm-org-hop-run-remove-line)
    map)
  "Keymap for `helm-org-hop-lines-source'.")

(defun helm-org-hop-build-sources (&optional force)
  "Build Helm sources for all lists.
Optional argument FORCE will reset all lists."
  (when force (org-hop-reset))
  (setq helm-org-hop-headings-source
        (helm-build-sync-source "Recent Org headings: "
          :action 'helm-org-hop-recent-headings-actions
          :keymap helm-org-hop-recent-headings-map
          :candidates org-hop-headings-list))

  (setq helm-org-hop-lines-source
        (helm-build-sync-source "Hop to line: "
          :action 'helm-org-hop-lines-actions
          :keymap helm-org-hop-lines-map
          :candidates org-hop-lines-list))

  (setq helm-org-hop-all-headings-source
        (helm-build-sync-source "Org headings: "
          :action 'helm-org-hop-headings-actions
          :keymap helm-org-hop-headings-map
          :candidates (org-hop-all-headings force)))

  (setq helm-org-hop-capture-source
        (helm-build-dummy-source "Create note"
          :action 'helm-org-hop-capture-actions)))

;;;###autoload
(defun helm-org-hop (&optional arg)
  "Helm for Org headings.
With optional argument ARG, reset all lists."
  (interactive "P")
  (helm-org-hop-build-sources arg)
  (helm :buffer "*helm-org-hop*"
        :ff-transformer-show-only-basename nil
        :sources helm-org-hop-default-sources))


(provide 'helm-org-hop)

;;; helm-org-hop.el ends here

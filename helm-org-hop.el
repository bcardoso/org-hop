;;; helm-org-hop.el --- Helm for org-hop -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/org-hop
;; Version: 0.3.1
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

;; Helm interface for org-hop.

;;; Code:

(require 'org)
(require 'org-hop)
(require 'org-ql)
(require 'helm)
(require 'helm-lib)
(require 'helm-mode)
(require 'helm-source)
(require 'helm-org-ql)


;;;; Custom variables

(defgroup helm-org-hop nil
  "Group for `helm-org-hop' customizations."
  :group 'org-hop)

(defcustom helm-org-hop-default-sources
  '(helm-org-hop-recent-headings-source
    helm-org-hop-recent-lines-source
    helm-org-hop-headings-source
    helm-org-hop-smart-source
    helm-org-hop-capture-source)
  "Default sources list used in `helm-org-hop'."
  :group 'helm-org-hop
  :type '(repeat (choice symbol)))

(defcustom helm-org-hop-capture-key "n"
  "The key string associated with a template in `org-capture-templates'."
  :group 'helm-org-hop
  :type 'string)

(defcustom helm-org-hop-capture-insert-input t
  "If non-nil, insert user input in `org-capture' buffer."
  :group 'helm-org-hop
  :type 'boolean)

(defcustom helm-org-hop-headings-actions
  (helm-make-actions
   "Hop to heading"                        'org-hop-to-entry
   "Hop to heading other window `C-c o'"   'helm-org-hop-entry-other-window
   "Hop to indirect buffer      `C-c i'"   'helm-org-hop-entry-indirect
   "Store link to heading       `C-c l`"   'helm-org-hop-store-link-heading
   "Insert link to heading      `C-c C-l`" 'helm-org-hop-insert-link-heading)
  "Default actions alist for `helm-org-hop-headings-source'."
  :group 'helm-org-hop
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-hop-recent-headings-actions
  (helm-make-actions
   "Hop to heading"                        'org-hop-to-entry
   "Hop to heading other window `C-c o'"   'helm-org-hop-entry-other-window
   "Hop to indirect buffer      `C-c i'"   'helm-org-hop-entry-indirect
   "Store link to heading       `C-c l`"   'helm-org-hop-store-link-heading
   "Insert link to heading      `C-c C-l`" 'helm-org-hop-insert-link-heading
   "Remove heading from list    `M-D`"     'helm-org-hop-remove-heading)
  "Default actions alist for `helm-org-hop-recent-headings-source'."
  :group 'helm-org-hop
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-hop-recent-lines-actions
  (helm-make-actions
   "Hop to line"                        'org-hop-to-entry
   "Hop to line other window `C-c o'"   'helm-org-hop-entry-other-window
   "Store link to line       `C-c l`"   'helm-org-hop-store-link-line
   "Insert link to line      `C-c C-l`" 'helm-org-hop-insert-link-line
   "Remove line from list    `M-D`"     'helm-org-hop-remove-line)
  "Default actions alist for `helm-org-hop-recent-lines-source'."
  :group 'helm-org-hop
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-hop-capture-actions
  '(("Capture note" . helm-org-hop-capture-note))
  "Default actions alist for `helm-org-hop-capture-source'."
  :group 'helm-org-hop
  :type '(alist :key-type string :value-type function))


;;;; Actions

;;;;; Entry actions

(defun helm-org-hop-entry-other-window (entry)
  "Helm action to store an Org link to ENTRY."
  (org-hop-to-entry entry 'other-window))

(defun helm-org-hop-entry-indirect (entry)
  "Helm action to create indirect buffer and narrow it to current ENTRY."
  (org-hop-with-entry-buffer entry (org-tree-to-indirect-buffer)))


;;;;; Store link

(defun helm-org-hop-store-link-to-line ()
  "Store custom Org link to line at point."
  (let* ((file        (abbreviate-file-name (buffer-file-name)))
         (filename    (file-name-nondirectory file))
         (line-number (line-number-at-pos)))
    (add-to-list 'org-stored-links
                 `(,(format "file:%s::%s" file line-number)
                   ,(format "%s:%s" filename line-number)))
    (message "Stored link for %s:%s" filename line-number)))

(defun helm-org-hop-store-link (type)
  "Store Org links to candidates in variable `helm-marked-candidates'.
Argument TYPE indicates if candidate is a \\='heading or \\='line."
  (let ((org-link-file-path-type 'absolute)
        (num (length (helm-marked-candidates))))
    (dolist (entry (helm-marked-candidates))
      (org-hop-with-entry-buffer entry
        (if (eq type 'heading)
            (call-interactively 'org-store-link)
          (helm-org-hop-store-link-to-line))))
    (when (> num 1)
      (message (format "Stored %s links" num)))))

(defun helm-org-hop-store-link-heading (&optional _entry)
  "Helm action to store an Org link to ENTRY."
  (helm-org-hop-store-link 'heading))

(defun helm-org-hop-store-link-line (&optional _entry)
  "Helm action to store an Org link to ENTRY."
  (helm-org-hop-store-link 'line))


;;;;; Insert link

(defun helm-org-hop-insert-link (type)
  "Insert Org links to candidates in variable `helm-marked-candidates'.
Argument TYPE indicates if candidate is a \\='heading or \\='line."
  (let ((org-link-file-path-type 'absolute)
        (num (length (helm-marked-candidates))))
    (dolist (entry (reverse (helm-marked-candidates)))
      (org-hop-with-entry-buffer entry
        (if (eq type 'heading)
            (let ((inhibit-message t))
              (call-interactively 'org-store-link))
          (helm-org-hop-store-link-to-line))))
    (if (> num 1)
        (org-insert-all-links num "- " "\n")
      (org-insert-all-links 1 "" ""))))

(defun helm-org-hop-insert-link-heading (&optional _entry)
  "Helm action to insert an Org link to ENTRY."
  (helm-org-hop-insert-link 'heading))

(defun helm-org-hop-insert-link-line (&optional _entry)
  "Helm action to insert an Org link to ENTRY."
  (helm-org-hop-insert-link 'line))


;;;;; Remove from recent list

(defun helm-org-hop-remove (type)
  "Remove candidates in variable `helm-marked-candidates' from recent list.
Argument TYPE indicates if candidate is a \\='heading or \\='line."
  (let* ((num (length (helm-marked-candidates)))
         (verbose (eq num 1)))
    (dolist (entry (helm-marked-candidates))
      (if (eq type 'heading)
          (org-hop-remove-heading entry verbose)
        (org-hop-remove-line entry verbose)))
    (when (> num 1)
      (message (format "Removed %s entries from recent %s list." num type)))))

(defun helm-org-hop-remove-heading (&optional _entry)
  "Helm action to remove ENTRY from recent list."
  (helm-org-hop-remove 'heading))

(defun helm-org-hop-remove-line (&optional _entry)
  "Helm action to remove ENTRY from recent list."
  (helm-org-hop-remove 'line))


;;;;; Org Capture

(defun helm-org-hop-capture-note (input)
  "Helm action to `org-capture' a note based on INPUT."
  (org-capture nil helm-org-hop-capture-key)
  (when helm-org-hop-capture-insert-input (insert input)))


;;;;; Keymaps

(helm-make-command-from-action helm-org-hop-run-entry-other-window
  "Run interactively `helm-org-hop-entry-other-window'."
  'helm-org-hop-entry-other-window)

(helm-make-command-from-action helm-org-hop-run-store-link-heading
  "Run interactively `helm-org-hop-store-link-heading'."
  'helm-org-hop-store-link-heading)

(helm-make-command-from-action helm-org-hop-run-insert-link-heading
  "Run interactively `helm-org-hop-insert-link-heading'."
  'helm-org-hop-insert-link-heading)

(helm-make-command-from-action helm-org-hop-run-remove-heading
  "Run interactively `helm-org-hop-remove-heading'."
  'helm-org-hop-remove-heading)

(helm-make-command-from-action helm-org-hop-run-store-link-line
  "Run interactively `helm-org-hop-store-link-line'."
  'helm-org-hop-store-link-line)

(helm-make-command-from-action helm-org-hop-run-insert-link-line
  "Run interactively `helm-org-hop-insert-link-line'."
  'helm-org-hop-insert-link-line)

(helm-make-command-from-action helm-org-hop-run-remove-line
  "Run interactively `helm-org-hop-remove-line'."
  'helm-org-hop-remove-line)

(defvar-keymap helm-org-hop-headings-map
  :doc "Keymap for `helm-org-hop-headings-source'."
  :parent helm-map
  "C-c o"   #'helm-org-hop-run-entry-other-window
  "C-c l"   #'helm-org-hop-run-store-link-heading
  "C-c C-l" #'helm-org-hop-run-insert-link-heading)

(defvar-keymap helm-org-hop-recent-headings-map
  :doc "Keymap for `helm-org-hop-recent-headings-source'."
  :parent helm-map
  "C-c o"   #'helm-org-hop-run-entry-other-window
  "C-c l"   #'helm-org-hop-run-store-link-heading
  "C-c C-l" #'helm-org-hop-run-insert-link-heading
  "M-D"     #'helm-org-hop-run-remove-heading)

(defvar-keymap helm-org-hop-recent-lines-map
  :doc "Keymap for `helm-org-hop-recent-lines-source'."
  :parent helm-map
  "C-c o"   #'helm-org-hop-run-entry-other-window
  "C-c l"   #'helm-org-hop-run-store-link-line
  "C-c C-l" #'helm-org-hop-run-insert-link-line
  "M-D"     #'helm-org-hop-run-remove-line)


;;;; Sources

(cl-defun helm-org-hop-make-source (&key (pred 'heading) min sort)
  "Build a Helm source for Org headings.
PRED is a `org-ql-predicates' predicate, defaults to \\='heading.
MIN is the mininum length of the input pattern.
SORT is a list of defined `org-ql' sorting methods."
  (let ((smart (or (eq pred 'rifle) (eq pred 'smart))))
    (helm-make-source
        (if smart "Org headings & contents" "Org headings")
        'helm-source-sync
      :init
      (when (eq pred 'heading)
        (lambda () (org-hop-headings-list-update)))
      :candidates
      (lambda ()
        (if smart
            (org-hop-headings :pred pred :pattern helm-pattern :sort sort)
          org-hop-headings-list))
      :match (when smart #'identity)
      :nohighlight smart
      :requires-pattern min
      :volatile t
      :diacritics 'helm-mode-ignore-diacritics
      :action 'helm-org-hop-headings-actions
      :keymap 'helm-org-hop-headings-map)))

(defvar helm-org-hop-headings-source
  (helm-org-hop-make-source)
  "Helm source for Org Headings.")

(defvar helm-org-hop-smart-source
  (helm-org-hop-make-source :pred 'smart :min 8)
  "Helm source for Org Headings with `smart' predicate.")

(defmacro helm-org-hop-build-recent-source (name action keymap candidates)
  "Macro for building the Helm sources.

NAME is a string which define the source name.
ACTION is a list of actions for the currently selected candidate.
KEYMAP is the specific keymap for this source.
CANDIDATES is the list of candidates."
  (declare (indent defun))
  `(helm-make-source ,name 'helm-source-sync
     :diacritics helm-mode-ignore-diacritics
     :action ,action
     :keymap ,keymap
     :candidates ,candidates))

(defvar helm-org-hop-recent-headings-source
  (helm-org-hop-build-recent-source "Recent Org headings"
    helm-org-hop-recent-headings-actions
    helm-org-hop-recent-headings-map
    (lambda () org-hop-recent-headings-list))
  "Helm source for `org-hop-recent-headings-list'.")

(defvar helm-org-hop-recent-lines-source
  (helm-org-hop-build-recent-source "Hop to line"
    helm-org-hop-recent-lines-actions
    helm-org-hop-recent-lines-map
    (lambda () org-hop-recent-lines-list))
  "Helm source for `org-hop-recent-lines-list'.")

(defvar helm-org-hop-capture-source
  (helm-build-dummy-source "Create note"
    :action 'helm-org-hop-capture-actions)
  "Helm source for note capturing.")


;;;; Commands

;;;###autoload
(defun helm-org-hop (&optional arg)
  "Helm for `org-hop'.
With optional argument ARG, run `org-hop-reset', which see."
  (interactive "P")
  (org-hop-reset arg)
  (helm :buffer "*helm-org-hop*"
        :ff-transformer-show-only-basename nil
        :sources helm-org-hop-default-sources))

;;;###autoload
(defun helm-org-hop-current-buffer (&optional buffers-files)
  "Helm for Org headings in current buffer.
When BUFFERS-FILES is a list of Org buffers or files, use it instead."
  (interactive)
  (if (or buffers-files (derived-mode-p 'org-mode))
      (helm
       :buffer "*helm-org-hop*"
       :ff-transformer-show-only-basename nil
       :sources (helm-org-hop-build-recent-source
                 (format "Org headings for %s" (buffer-name))
                 helm-org-hop-headings-actions
                 helm-org-hop-headings-map
                 (org-hop-headings
                  :buffers-files (or buffers-files (current-buffer)))))
    (helm-outline)))


(provide 'helm-org-hop)

;;; helm-org-hop.el ends here

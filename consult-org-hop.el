;;; consult-org-hop.el --- Consult for org-hop -*- lexical-binding: t -*-

;; Copyright (C) 2024 Bruno Cardoso

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

;; Consult interface for org-hop.

;;; Code:

(require 'org-hop)
(require 'consult)

(defmacro consult-org-hop-build-source (name narrow category items)
  "Build Consult source with properties NAME, NARROW, CATEGORY, and ITEMS."
  (declare (indent defun))
  `(list :name     ,name
         :prompt   "Hop to: "
         :narrow   ,narrow
         :category ,category
         :action   #'org-hop-to-entry
         :items    ,items
         :sort     nil))

(defvar consult-org-hop--headings-source
  (consult-org-hop-build-source
    "Org headings" ?h 'org-heading
    (lambda ()
      (org-hop-headings-list-update)
      (mapcar #'car org-hop-headings-list)))
  "Consult source for Org Headings.")

(defvar consult-org-hop--recent-headings-source
  (consult-org-hop-build-source
    "Recent Org headings" ?r 'org-heading
    (lambda () (mapcar #'car org-hop-recent-headings-list)))
  "Consult source for recent Org Headings.")

(defvar consult-org-hop--recent-lines-source
  (consult-org-hop-build-source
    "Hop to line" ?l 'consult-location
    (lambda () (mapcar #'car org-hop-recent-lines-list)))
  "Consult source for saved lines.")

(defcustom consult-org-hop-sources
  '(consult-org-hop--recent-headings-source
    consult-org-hop--recent-lines-source
    consult-org-hop--headings-source)
  "Source list for `consult-org-hop'."
  :group 'org-hop
  :type '(repeat (choice symbol)))

(defcustom consult-org-hop-capture-key "n"
  "Org capture template key for new note from minibuffer input."
  :group 'org-hop
  :type 'string)

(defvar-keymap consult-org-hop-map
  :doc "Keymap for `consult-org-hop'."
  "M-i" #'consult-org-hop-to-org-ql-find
  "M-N" #'consult-org-hop--capture-note)

(declare-function org-ql-find "org-ql-find")
(declare-function embark--quit-and-run "embark")

;; REVIEW 2024-06-23: works, but is there a better way?
(defun consult-org-hop-to-org-ql-find ()
  "Quit command and run `org-ql-find' with the current minibuffer input."
  (interactive)
  (let ((input (minibuffer-contents-no-properties))
        (org-ql-default-predicate 'rifle))
    (embark--quit-and-run
     (lambda ()
       (minibuffer-with-setup-hook
           (lambda () (insert input))
         (org-ql-find (org-buffer-list)))))))

(defun consult-org-hop--capture-note ()
  "Run Org capture from current minibuffer input."
  (interactive)
  (embark--quit-and-run
   (lambda (input)
     (org-capture nil consult-org-hop-capture-key)
     (insert input))
   (minibuffer-contents-no-properties)))

;;;###autoload
(defun consult-org-hop (&optional arg)
  "Consult for `org-hop'.
With optional argument ARG, run `org-hop-reset', which see."
  (interactive "P")
  (org-hop-reset arg)
  (consult--multi consult-org-hop-sources
                  :sort nil
                  :keymap consult-org-hop-map))

;;;###autoload
(defun consult-org-hop-current-buffer (&optional buffers-files)
  "Consult for Org headings in current buffer.
When BUFFERS-FILES is a list of Org buffers or files, use it instead."
  (interactive)
  (if (or buffers-files (derived-mode-p 'org-mode))
      (consult--multi
       (list
        (consult-org-hop-build-source
          nil nil 'org-heading
          (mapcar #'car
                  (org-hop-headings
                   :buffers-files (or buffers-files (current-buffer))))))
       :sort nil
       :keymap consult-org-hop-map)
    (consult-imenu)))


(provide 'consult-org-hop)

;;; consult-org-hop.el ends here

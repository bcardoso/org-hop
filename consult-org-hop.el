;;; consult-org-hop.el --- Consult for org-hop -*- lexical-binding: t -*-

;; Copyright (C) 2024 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/org-hop
;; Version: 0.1
;; Package-Requires: ((emacs "29.3"))

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

(require 'consult)

(defvar consult-org-hop--headings-source
  (list :name     "Org headings"
        :prompt   "Hop to: "
        :narrow   ?h
        :sort     nil
        :category 'org-heading
        :action   #'org-hop-to-entry
        :items    (lambda ()
                    (org-hop-headings-list-update)
                    org-hop-headings-list))
  "Consult source for Org Headings.")

(defvar consult-org-hop--recent-headings-source
  (list :name     "Recent Org headings"
        :prompt   "Hop to: "
        :narrow   ?r
        :sort     nil
        :category 'org-heading
        :action   #'org-hop-to-entry
        :items    (lambda () org-hop-recent-headings-list))
  "Consult source for recent Org Headings.")

(defvar consult-org-hop--recent-lines-source
  (list :name     "Hop to line"
        :prompt   "Hop to: "
        :narrow   ?l
        :sort     nil
        :category 'consult-location
        :action   #'org-hop-to-entry
        :items    (lambda () org-hop-recent-lines-list))
  "Consult source for saved lines.")

;;;###autoload
(defun consult-org-hop ()
  (interactive)
  (consult--multi
   (list consult-org-hop--recent-headings-source
         consult-org-hop--recent-lines-source
         consult-org-hop--headings-source)
   :sort nil))

(provide 'consult-org-hop)

;;; consult-org-hop.el ends here

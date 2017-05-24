;;; emgi-faces.el ---

;; Copyright (C) 2017 Chris Gregory czipperz@gmail.com

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Chris Gregory "czipperz"
;; Email: czipperz@gmail.com
;; Version: 0.0.1

;;; Commentary:
;;; Code:

(require 'emgi-groups (expand-file-name "./emgi-groups.el"))

(defface emgi-log-slash-face
  '()
  "The face for the graphing symbols `/|' in a git log --graph."
  :group 'emgi-group)
(defvar emgi-log-slash-face 'emgi-log-slash-face)

(defface emgi-log-stars-face
  '((((class color) (background light)) :foreground "red")
    (((class color) (background dark)) :foreground "red"))
  "The face for the `*'s in a git log --graph."
  :group 'emgi-group)
(defvar emgi-log-stars-face 'emgi-log-stars-face)

(defface emgi-commit-hash-face
  '((((class color) (background light)) :foreground "darkgoldenrod")
    (((class color) (background dark)) :foreground "gold"))
  "The face for a git commit hash."
  :group 'emgi-group)
(defvar emgi-commit-hash-face 'emgi-commit-hash-face)

(defface emgi-author-face
  '((((class color) (background light)) :foreground "limegreen")
    (((class color) (background dark)) :foreground "green"))
  "The face for an author of a git commit."
  :group 'emgi-group)
(defvar emgi-author-face 'emgi-author-face)

(defface emgi-branch-face
  '((((class color) (background light)) :foreground "darkblue" :box t)
    (((class color) (background dark)) :foreground "dodgerblue" :box t))
  "The face for a git branch name."
  :group 'emgi-group)
(defvar emgi-branch-face 'emgi-branch-face)

(defface emgi-log-message-face
  '((((class color) (background light)) :foreground "black" :underline t)
    (((class color) (background dark)) :foreground "white" :underline t))
  "The face for the message at the top of the git log."
  :group 'emgi-group)
(defvar emgi-log-message-face 'emgi-log-message-face)

(defmacro emgi-invisible-face (match)
  "A function to be `eval'd that renders text matched to not display.

The text matched lies between the beginning and end of the MATCH
match, according to `match-beginning' and `match-end'."
  ;; The double quote makes it so that when the macro returns, it
  ;; evaluates the outer quote.  Then when it is `eval'd it evaluates
  ;; to the inner code expression.
  `'(progn (add-text-properties (match-beginning ,match)
                                (match-end ,match)
                                '(invisible t))
           ;; then return defualt so as to not confuse renderer.
           'default))

(provide 'emgi-faces)
;;; emgi-faces.el ends here

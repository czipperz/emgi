;;; emgi-status.el ---

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

(require 'emgi-repo (expand-file-name "./emgi-repo.el"))

(defvar emgi-commands-map
  (let ((keymap (make-sparse-keymap)))
    ;;; high-level commands (porecelain)
    ;; main porcelain
    (define-key keymap (kbd "add") nil)
    (define-key keymap (kbd "am") nil)
    (define-key keymap (kbd "archive") nil)
    (define-key keymap (kbd "bisect") nil)
    (define-key keymap (kbd "branch") nil)
    (define-key keymap (kbd "bundle") nil)
    (define-key keymap (kbd "checkout") nil)
    (define-key keymap (kbd "cherrypick") nil)
    (define-key keymap (kbd "clean") nil)
    (define-key keymap (kbd "clone") nil)
    (define-key keymap (kbd "commit") nil)
    (define-key keymap (kbd "describe") nil)
    (define-key keymap (kbd "diff") nil)
    (define-key keymap (kbd "fetc h") nil)
    (define-key keymap (kbd "formatpatch") nil)
    (define-key keymap (kbd "gc") nil)
    (define-key keymap (kbd "grep") nil)
    (define-key keymap (kbd "log") nil)
    (define-key keymap (kbd "merge") nil)
    (define-key keymap (kbd "mv") nil)
    (define-key keymap (kbd "notes") nil)
    (define-key keymap (kbd "pull") nil)
    (define-key keymap (kbd "push") nil)
    (define-key keymap (kbd "rebase") nil)
    (define-key keymap (kbd "reset") nil)
    (define-key keymap (kbd "revert") nil)
    (define-key keymap (kbd "rm") nil)
    (define-key keymap (kbd "shortlog") nil)
    (define-key keymap (kbd "show") nil)
    (define-key keymap (kbd "stash") nil)
    (define-key keymap (kbd "status") nil)
    (define-key keymap (kbd "submodule") nil)
    (define-key keymap (kbd "tag") nil)
    (define-key keymap (kbd "worktree") nil)
    ;; ancillary
    (define-key keymap (kbd "config") nil)
    (define-key keymap (kbd "fastexport") nil)
    (define-key keymap (kbd "filterbranch") nil)
    (define-key keymap (kbd "mergetool") nil)
    (define-key keymap (kbd "pack-refs") nil)
    (define-key keymap (kbd "prune") nil)
    (define-key keymap (kbd "reflog") nil)
    (define-key keymap (kbd "remote") nil)
    (define-key keymap (kbd "repack") nil)
    (define-key keymap (kbd "replace") nil)
    (define-key keymap (kbd "annotate") nil)
    (define-key keymap (kbd "blame") nil)
    (define-key keymap (kbd "cherry") nil)
    (define-key keymap (kbd "count-objects") nil)
    (define-key keymap (kbd "difftool") nil)
    (define-key keymap (kbd "fsck") nil)
    (define-key keymap (kbd "gettarcommitid") nil)
    (define-key keymap (kbd "help") nil)
    (define-key keymap (kbd "mergetree") nil)
    (define-key keymap (kbd "rerere") nil)
    (define-key keymap (kbd "revparse") nil)
    (define-key keymap (kbd "showbranch") nil)
    (define-key keymap (kbd "verifycommit") nil)
    (define-key keymap (kbd "verifytag") nil)
    (define-key keymap (kbd "whatchanged") nil)
    ;; interacting with others
    (define-key keymap (kbd "archimport") nil)
    (define-key keymap (kbd "cvsexportcommit") nil)
    (define-key keymap (kbd "cvsimport") nil)
    (define-key keymap (kbd "cvsserver") nil)
    (define-key keymap (kbd "imapsend") nil)
    (define-key keymap (kbd "p4") nil)
    (define-key keymap (kbd "quiltimport") nil)
    (define-key keymap (kbd "requestpull") nil)
    (define-key keymap (kbd "sendemail") nil)
    ;;; low-level commands (plumbing)
    ;; manipulation
    (define-key keymap (kbd "apply") nil)
    ;; I decided to cut out other ones that I don't find useful.
    keymap)
  "The mapping for raw input of a command.")

(defvar emgi-status-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "s" emgi-commands-map)
    keymap)
  "The mapping for `emgi-status-mode'.")

(define-derived-mode emgi-status-mode nil "Emgi"
  "Mode for the emgi status buffer, showing the status of the Git repository.")

(defun emgi-status (&optional directory)
  "Launch a window where you can interactively interact with the git repository.

DIRECTORY is used in the call to `emgi-find-repo'."
  (interactive)
  (setq directory (emgi-find-repo directory))
  (switch-to-buffer
   (get-buffer-create (format "*emgi: %s*"
                              (emgi-repo-name directory))))
  (emgi-status-mode))

(provide 'emgi-status)

;;; emgi-status.el ends here

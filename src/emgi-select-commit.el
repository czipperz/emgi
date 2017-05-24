;;; emgi-select-commit.el --- Define `emgi-select-commit'.

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

(require 'emgi-config (expand-file-name "./emgi-config.el"))
(require 'emgi-faces (expand-file-name "./emgi-faces.el"))

(defvar-local emgi-select-commit-callback nil
  "What to do when a commit is selected.")

(defvar emgi-select-commit-font-lock-keywords
  `(("^\\([|/ ]*\\)\\* " 1 emgi-log-slash-face)
    ("^[|/ ]*\\(\\*\\) " 1 emgi-log-stars-face)
    ("^[|/ ]*\\* \x01[a-f0-9]\\{8,\\} \x01\\([^\x01]+\\)\x01" 1 emgi-author-face)
    ("^[|/ ]*\\* \x01[a-f0-9]\\{8,\\} \x01[^\x01]+\x01 (\\([^)]+?\\))" 1 emgi-branch-face)

    ;; make invisible the `\x01's
    ("^[|/ ]*\\* \\(\x01\\)" 1 ,(emgi-invisible-face 1))
    ("^[|/ ]*\\* \x01[a-f0-9]\\{8,\\} \\(\x01\\)" 1 ,(emgi-invisible-face 1))
    ("^[|/ ]*\\* \x01[a-f0-9]\\{8,\\} \x01[^\x01]+\\(\x01\\)" 1 ,(emgi-invisible-face 1))
    ("^[|/ ]*\\* \x01[a-f0-9]\\{8,\\} \x01[^\x01]+\x01\\( ([^)]+?)\\)?\\(\x01\\)" 2 ,(emgi-invisible-face 2))

    ;; make invisible the parens around the branch name
    ("^[|/ ]*\\* \x01[a-f0-9]\\{8,\\} \x01[^\x01]+\x01 \\((\\)" 1 ,(emgi-invisible-face 1))
    ("^[|/ ]*\\* \x01[a-f0-9]\\{8,\\} \x01[^\x01]+\x01 ([^)]+?\\()\\)" 1 ,(emgi-invisible-face 1))

    ;; This works by doing a "negative match" that highlights nothing
    ;; if it doesn't apply instead of causing an error.  If the
    ;; negative match (the left side of group 1) is taken, then group
    ;; 2 is the empty one at the end.  If the negative match is not
    ;; taken, then match 2 is the entire line, the message.
    ("^\\([|/ ]*\\* \\|\\(.*\\)\\)\\(\\)" 2 emgi-log-message-face))
  "Keywords for `emgi-select-commit-mode'.")

(defvar emgi-select-commit-font-lock-visible-commit-hash
  '("^[|/ ]*\\* \x01\\([a-f0-9]\\{8,\\} \\)" 1 emgi-commit-hash-face))
(defvar emgi-select-commit-font-lock-invisible-commit-hash
  `("^[|/ ]*\\* \x01\\([a-f0-9]\\{8,\\} \\)" 1 ,(emgi-invisible-face 1)))

(defun emgi-select-commit-select ()
  "Send the commit on this line to `emgi-select-commit-callback'."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "[|/ ]*\\* \x01\\([a-f0-9]\\{8,\\}\\) \x01")
      (user-error "Not on a line specifying a commit")))
  ;; we have to save these so that after `kill-buffer' they are still valid
  (let ((commit-hash (buffer-substring-no-properties (match-beginning 1)
                                                     (match-end 1)))
        (callback emgi-select-commit-callback))
    (kill-buffer)
    (when callback (funcall callback commit-hash))))

(defun emgi-select-commit-quit ()
  "Quit the buffer meant to select a commit."
  (interactive)
  (kill-buffer))

(defun emgi-select-commit-enter-hash (hash)
  "Allow the user to manually enter a hash HASH.

If the hash is less than 8 characters it is expanded to 8
characters based on the first matching commit in the buffer."
  (interactive "sCommit hash: ")
  (when (> (length hash) 8)
    (setq hash (substring hash 0 8)))
  (save-excursion
    (goto-char (point-min))
    (setq hash
          (catch 'end-search
            (while (search-forward hash nil t)
              (forward-line 0)
              (when (looking-at (concat "[|/ ]*\\* \x01\\(" hash "\\)"))
                (assert (looking-at "[|/ ]*\\* \x01\\([a-f0-9]\\{8,\\}\\) \x01"))
                (throw 'end-search
                       (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
              (forward-line 1))
            (user-error "Invalid commit hash: %s" hash))))
  (let ((callback emgi-select-commit-callback))
    (kill-buffer)
    (when callback (funcall callback hash))))

(defvar emgi-select-commit-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") 'emgi-select-commit-select)
    (define-key keymap (kbd "C-c C-k") 'emgi-select-commit-quit)
    (define-key keymap (kbd "q") 'emgi-select-commit-quit)
    (define-key keymap (kbd "p") 'previous-line)
    (define-key keymap (kbd "n") 'next-line)
    (define-key keymap (kbd "h") 'emgi-select-commit-enter-hash)
    keymap)
  "Keymap for `emgi-select-commit-mode'.")

(define-derived-mode emgi-select-commit-mode nil "Emgi"
  "Mode for a buffer used for selecting a commit."
  (setq font-lock-defaults
        (list (cons (if emgi-log-show-commit-hash
                        emgi-select-commit-font-lock-visible-commit-hash
                      emgi-select-commit-font-lock-invisible-commit-hash)
                    emgi-select-commit-font-lock-keywords))))

(defun emgi-select-commit (callback &optional message directory)
  "Select a commit in the repository where DIRECTORY lies.

If a command is selected, it is passed to CALLBACK.
If MESSAGE is provided, it will be shown at the top of the window."
  (setq directory (emgi-find-repo directory))
  (switch-to-buffer
   (get-buffer-create (format "*emgi-select-commit: %s*"
                              (emgi-repo-name directory))))
  (emgi-select-commit-mode)
  (when (require 'evil nil t) (turn-off-evil-mode))
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (insert (or message "Select a commit") "\n")
    (emgi-git-process directory
                      "log" "--graph" "--decorate" "--abbrev=8"
                      "--color=never" "--all"
                      "--pretty=format:\x01%h \x01%aN\x01%d\x01 %s")
    (insert "\n"))
  (goto-char (point-min))
  (setq emgi-select-commit-callback callback)
  (turn-on-font-lock)
  (font-lock-flush)
  nil)

(provide 'emgi-select-commit)
;;; emgi-select-commit.el ends here

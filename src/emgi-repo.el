;;; emgi-repo.el ---

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

(require 'emgi-git-process (expand-file-name "./emgi-git-process.el"))

;;;###autoload
(defun emgi-find-repo (&optional directory)
  "Find the git repository root directory starting at DIRECTORY.

If there isn't a git repository above DIRECTORY in the directory tree, throws a `user-error'."
  (or (emgi-find-repo-noexcept directory)
      (user-error "Couldn't find a git repository")))

;;;###autoload
(defun emgi-find-repo-noexcept (&optional directory)
  "Find the git repository root directory starting at DIRECTORY.

If there isn't a git repository above DIRECTORY in the directory tree, returns nil."
  (setq directory (expand-file-name ".git" directory))
  (let ((p ""))
    (while (if (string-equal p directory)
               (setq directory nil)
             (not (file-directory-p directory)))
      (setq p directory)
      (setq directory (concat (file-name-directory
                               (directory-file-name
                                (file-name-directory directory)))
                              ".git"))))
  (if directory (file-name-directory directory)))

;;;###autoload
(defun emgi-repo-name (directory)
  "Get the name of the repository located at DIRECTORY."
  (file-name-base (directory-file-name directory)))

;;;###autoload
(defun emgi-branch-name (&optional directory)
  "Get the name of the branch HEAD is pointing at for the repository located at DIRECTORY."
  (substring (shell-command-to-string "git rev-parse --abbrev-ref HEAD") 0 -1))

(provide 'emgi-repo)
;;; emgi-repo.el ends here

;;; emgi.el ---

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
(require 'emgi-repo (expand-file-name "./emgi-repo.el"))
(require 'emgi-groups (expand-file-name "./emgi-groups.el"))
(require 'emgi-faces (expand-file-name "./emgi-faces.el"))
(require 'emgi-git-process (expand-file-name "./emgi-git-process.el"))
(require 'emgi-select-commit (expand-file-name "./emgi-select-commit.el"))
(require 'emgi-status (expand-file-name "./emgi-status.el"))

(provide 'emgi)
;;; emgi.el ends here

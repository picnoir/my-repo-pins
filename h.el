;;; h.el --- Project navigation and remote checkout -*- lexical-binding: t -*-

;; Copyright (C) 2022 Félix Baylac Jacqué
;; Author: Félix Baylac Jacqué <felix at alternativebit.fr>
;; Maintainer: Félix Baylac Jacqué <felix at alternativebit.fr>
;; Version: 1.14.0
;; Homepage: https://alternativebit.fr/TODO
;; Package-Requires: ((emacs "25.1"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>

;;; Commentary:

;; TODO before publish

;;; Code:

(eval-when-compile (require 'subr-x))

(defgroup h-group nil
  "Variables used to setup the h.el code folder manager."
  :group 'Communication)


(defcustom h-code-root nil
  "Directory containing the git projects.
h.el organise the git repos you'll checkout in a tree fashion.

All the code fetched using h.el will end up in this root directory. A
tree of subdirectories will be created mirroring the remote URI.

For instance, after checking out
https://git.savannah.gnu.org/git/emacs/org-mode.git, the source code
will live in the h-code-root/git.savannah.gnu.org/git/emacs/org-mode/
local directory"
  :type 'directory
  :group 'h-group)

(defcustom h-git-bin "git"
  "Path pointing to the git binary.
By default, it'll look for git in the current $PATH."
  :type 'file
  :group 'h-group)

(defun h--git-path ()
  "Find the git binary path using `h-git-bin`.

Errors out if we can't find it."
  (if (file-executable-p h-git-bin)
      h-git-bin
    (let ((git-from-bin-path (locate-file h-git-bin exec-path)))
      (if (file-executable-p git-from-bin-path)
          git-from-bin-path
          (error "Can't find git. Is h-git-bin correctly set?")))))

(defun h--call-git-in-dir (dir args)
  "Call the git binary as pointed by `h-git-bin` in DIR with ARGS."
  (let ((default-directory dir))
    (process-file (h--git-path) nil nil nil args)))

(defun h--is-git-repo (dir)
  "Check if DIR is a git repo using a pretty weak heuristic."
    (file-directory-p (concat (file-name-as-directory dir) ".git")))

(defun h--get-code-root-projects (code-root)
  "Retrieve the projects contained in the CODE-ROOT directory.
We're going to make some hard assumptions about how the `h-code-root`
directory should look like. First of all, if a directory seem to be a
git repository, it'll automatically be considered as a project root.

It means that after encountering a git repository, we won't recurse
any further.

If the directory pointed by h-code-root does not exists yet, returns
an empty list."
  (if (not (file-directory-p code-root))
      '()
    (let*
        ((is-not-git-repo (lambda (dir) (not (h--is-git-repo dir))))
         (remove-code-root-prefix
          (lambda (path) (string-remove-prefix (concat (file-name-as-directory code-root)) path)))
       ;;; PERF: Using directory-files-recursively is pretty
         ;;; inneficient. We have to list the dir content twice:
         ;;; 1. when directory-files-recursively checks.
         ;;; 2. when we filter the intermediate dirs from this list.
         (recursively-found-dirs
          (directory-files-recursively code-root "" t is-not-git-repo))
         (projects-absolute-path (seq-filter (lambda (e) (h--is-git-repo e)) recursively-found-dirs))
         (projects-relative-to-code-root
          (mapcar remove-code-root-prefix projects-absolute-path)))
      projects-relative-to-code-root)))

(provide 'h)
;;; h.el ends here

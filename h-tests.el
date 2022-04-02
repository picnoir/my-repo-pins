;;; h-tests.el --- Project navigation and remote checkout -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Félix Baylac Jacqué
;; Author: Félix Baylac Jacqué <felix at alternativebit.fr>
;; Maintainer: Félix Baylac Jacqué <felix at alternativebit.fr>
;; Version: 1.14.0

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

(require 'ert)
(require 'h)

; Test Helpers
;;;;;;;;;;;;;;

(defun h--tests-with-temp-dir (func)
  "Run the FUNC function in a temporary directory.

FUNC gets called with the temp dir as parameter.
The directory gets deleted once we exit FUNC."
  (let ((temp-dir (make-temp-file "h-test-" t)))
    (unwind-protect
          (funcall func (file-name-as-directory temp-dir))
      (delete-directory temp-dir t))))

(defun h--tests-init-fake-git-repo (dir)
  "Create a dummy git repo at DIR.

If DIR doesn't exists, we create it first."
  (let ((d (file-name-as-directory dir)))
    (progn
       (unless (file-directory-p d) (make-directory d t))
       (h--call-git-in-dir d "init" ))))

; Test Dirs Setup
;;;;;;;;;;;;;;;;;

(defun h--tests-run-on-testroot-1 (func)
  "Run the FUNC function on testroot1.

FUNC is called with the directory cotaining test root 1 as parameter.

For reference: test-root-1 looks like this:
    test-root-1
    ├── example1.tld
    │   ├── user1
    │   │   ├── proj1
    │   │   └── proj2
    │   └── user2
    │       └── proj1
    └── example2.tld
        └── user1
            └── proj1"
  (h--tests-with-temp-dir
   (lambda (temp-dir)
     (progn
       (h--tests-init-fake-git-repo (concat temp-dir "/example1.tld/user1/proj1"))
       (h--tests-init-fake-git-repo (concat temp-dir "/example1.tld/user1/proj2"))
       (h--tests-init-fake-git-repo (concat temp-dir "/example1.tld/user2/proj1"))
       (h--tests-init-fake-git-repo (concat temp-dir "/example2.tld/user1/proj1"))
       (funcall func temp-dir)
       ))))

; Tests
;;;;;;;

(ert-deftest h--tests-get-code-root-projects ()
  "Testing the `h--get-code-root-projects with test-root-1 setup."
  (let
      ((results
        (h--tests-run-on-testroot-1 (lambda (root) (h--get-code-root-projects root))))
       )
    (should (member "example1.tld/user1/proj1" results))
    (should (member "example1.tld/user1/proj2" results))
    (should (member "example1.tld/user2/proj1" results))
    (should (member "example2.tld/user1/proj1" results))
    (should (eq (length results) 4))
  ))

(h--get-code-root-projects "./test/fixtures/test-root-1")

(file-name-as-directory "~/code-root")



(provide 'h-tests)
;;; h-tests.el ends here

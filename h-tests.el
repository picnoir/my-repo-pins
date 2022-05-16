;;; h-tests.el --- Project navigation and remote checkout -*- lexical-binding: t; -*-

;;; Copyright (C) 2022 Félix Baylac Jacqué
;;; Author: Félix Baylac Jacqué <felix at alternativebit.fr>
;;; Maintainer: Félix Baylac Jacqué <felix at alternativebit.fr>
;;; Version: 1.14.0

;;; License:

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>

;;; Commentary:

;;; TODO before publish

;;; Code:

(require 'ert)
(require 'h)

;; Test Helpers
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
       (h--call-git-in-dir d "init"))))

;; Test Dirs Setup
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

(defun h--tests-run-on-testroot-2 (func)
  "Run the FUNC function on testroot2.

FUNC is called with the directory cotaining test root 2 as parameter.

For reference: test-root-2 looks like this:
    test-root-2
    ├── example1.tld
    │   ├── user1
    │   │   ├── proj1
    │   │   └── proj2 (NOT A GIT REPO)
    │   └── user2
    │       └── proj1
    └── example2.tld
        └── user1
            └── proj1"
  (h--tests-with-temp-dir
   (lambda (temp-dir)
     (progn
       (h--tests-init-fake-git-repo (concat temp-dir "/example1.tld/user1/proj1"))
       (make-directory (concat (file-name-as-directory temp-dir) "/example1.tld/user1/proj2"))
       (h--tests-init-fake-git-repo (concat temp-dir "/example1.tld/user2/proj1"))
       (h--tests-init-fake-git-repo (concat temp-dir "/example2.tld/user1/proj1"))
       (funcall func temp-dir)))))


(defun h--tests-run-on-empty-testroot (func)
  "Run the FUNC function on testroot1.

FUNC is called with a empty test root.

For reference: a empty test root looks like this:
    test-root"
  (h--tests-with-temp-dir
   (lambda (temp-dir)
     (progn
       (funcall func temp-dir)))))

;; Tests
;;;;;;;

(ert-deftest h--tests-get-code-root-projects-coderoot-1 ()
  "Test the `h--get-code-root-projects with test-root-1 setup."
  (let
      ((results
        (h--tests-run-on-testroot-1 (lambda (root) (h--get-code-root-projects root))))
       )
    (should (member "example1.tld/user1/proj1" results))
    (should (member "example1.tld/user1/proj2" results))
    (should (member "example1.tld/user2/proj1" results))
    (should (member "example2.tld/user1/proj1" results))
    (should (eq (length results) 4))))


(ert-deftest h--tests-find-git-dirs-recursively-coderoot-1 ()
  "Test the `h--get-code-root-projects with test-root-1 setup."
  (let*
      ((r nil)
       (results
        (h--tests-run-on-testroot-1
         (lambda (root)
           (progn (setq r root)
                  (h--find-git-dirs-recursively root))))))
    (should (member (concat r "example1.tld/user1/proj1/") results))
    (should (member (concat r "example1.tld/user1/proj2/") results))
    (should (member (concat r "example1.tld/user2/proj1/") results))
    (should (member (concat r "example2.tld/user1/proj1/") results))
    (should (eq (length results) 4))))

(ert-deftest h--tests-get-code-root-projects-coderoot-2 ()
  "Test the `h--get-code-root-projects with test-root-2 setup."
  (let
      ((results
        (h--tests-run-on-testroot-2 (lambda (root) (h--get-code-root-projects root))))
       )
    (should (member "example1.tld/user1/proj1" results))
    (should (member "example1.tld/user2/proj1" results))
    (should (member "example2.tld/user1/proj1" results))
    (should (eq (length results) 3))))

(ert-deftest h--tests-find-git-dirs-recursively-coderoot-2 ()
  "Test the `h--get-code-root-projects with test-root-2 setup."
  (let*
      ((r nil)
       (results
        (h--tests-run-on-testroot-2
         (lambda (root)
           (progn (setq r root)
                  (h--find-git-dirs-recursively root))))))
    (should (member (concat r "example1.tld/user1/proj1/") results))
    (should (member (concat r "example1.tld/user2/proj1/") results))
    (should (member (concat r "example2.tld/user1/proj1/") results))
    (should (eq (length results) 3))))

(ert-deftest h--tests-get-code-root-projects-empty-coderoot ()
  "Test the `h--get-code-root-projects with a empty coderoot."
  (let
      ((results
        (h--tests-run-on-empty-testroot (lambda (root) (h--get-code-root-projects root))))
       )
    (should (seq-empty-p results))))

(ert-deftest h--tests-find-git-dirs-recursively-empty-coderoot ()
  "Test the `h--get-code-root-projects with a empty coderoot."
  (let
      ((results
        (h--tests-run-on-empty-testroot (lambda (root) (h--find-git-dirs-recursively root))))
       )
    (should (seq-empty-p results))))

(ert-deftest h--tests-get-code-root-projects-no-coderoot ()
  "Test the `h--get-code-root-projects with a non-existing coderoot."
  (let
      ((results (h--get-code-root-projects "/does/not/exist")))
    (should (seq-empty-p results))))


;; Test Fetchers
;;;;;;;;;;;;;;;;;

;; Github

(ert-deftest h--tests-fetch-github-parse-response-ok ()
  "Test h--tests-fetch-github-parse-response with a fixture."
    (with-temp-buffer
      (insert-file-contents "./tests/fixtures/github-get-request-ok.txt")
      (should (equal (h--fetch-github-parse-response (current-buffer))
                  '((ssh . "git@github.com:NinjaTrappeur/h.el.git")
                    (https . "https://github.com/NinjaTrappeur/h.el.git"))))))

(ert-deftest h--tests-fetch-github-parse-response-ko ()
  "Test h--tests-fetch-github-parse-response with a fixture."
    (with-temp-buffer
      (insert-file-contents "./tests/fixtures/github-get-request-ko.txt")
      (should (equal (h--fetch-github-parse-response (current-buffer)) nil))))

;; Test repo URI parser
;;;;;;;;;;;;;;;;;

(ert-deftest h--test-parse-repo-idetifier ()
  "Test h--parse-repo-identifier."
  (should (equal (h--parse-repo-identifier "https://github.com/Ninjatrappeur/h.el") 'full-url))
  (should (equal (h--parse-repo-identifier "github.com/Ninjatrappeur/h.el") 'full-url))
  (should (equal (h--parse-repo-identifier "Ninjatrappeur/h.el") 'owner-repo))
  (should (equal (h--parse-repo-identifier "h.el") 'repo)))

(ert-deftest h--test-filepath-from-clone-url ()
  "Test h--filepath-from-clone-url."
  ;; HTTP/HTTPS
  (should (equal (h--filepath-from-clone-url "http://github.com/NinjaTrappeur/h.el.git") "github.com/NinjaTrappeur/h.el"))
  (should (equal (h--filepath-from-clone-url "http://github.com/NinjaTrappeur/h.el") "github.com/NinjaTrappeur/h.el"))
  (should (equal (h--filepath-from-clone-url "https://github.com/NinjaTrappeur/h.el.git") "github.com/NinjaTrappeur/h.el"))
  (should (equal (h--filepath-from-clone-url "https://github.com/NinjaTrappeur/h.el") "github.com/NinjaTrappeur/h.el"))
  ;; SSH
  (should (equal (h--filepath-from-clone-url "ssh://git@github.com:NinjaTrappeur/h.el.git") "github.com/NinjaTrappeur/h.el"))
  (should (equal (h--filepath-from-clone-url "ssh://git@github.com:NinjaTrappeur/h.el") "github.com/NinjaTrappeur/h.el"))
  (should (equal (h--filepath-from-clone-url "git@github.com:NinjaTrappeur/h.el.git") "github.com/NinjaTrappeur/h.el"))
  (should (equal (h--filepath-from-clone-url "git@github.com:NinjaTrappeur/h.el") "github.com/NinjaTrappeur/h.el")))

(ert-deftest h--test-git-clone-in-dir ()
  "Test the h--git-clone-in-dir function."
    (h--tests-run-on-testroot-1
     (lambda (dir)
       (let
           ((tmpdir (make-temp-file "h-test-" t)))
         (progn
           (h--git-clone-in-dir
            (format "file://%s" (concat dir "example1.tld/user1/proj1/"))
            tmpdir)
           (should (file-exists-p (format "%s/.git" tmpdir)))
           (delete-directory tmpdir t))))))

(ert-deftest h--test-pick-relevant-forges ()
  "Test the h--pick-relevant-forges function."
  (let
      ((forge-list
        '(
          (forge1 . ((query . h--query-github) (url . "https://forge1.com/.*/.*")))
          (forge2 . ((query . h--query-github) (url . "https://forge2.com/.*/.*"))))))
    (should (equal (h--pick-relevant-forges "owner/repo" forge-list) forge-list))
    (should (equal (h--pick-relevant-forges "repo" forge-list) forge-list))
    (should (equal
             (h--pick-relevant-forges "https://forge1.com/owner/repo" forge-list)
             '((forge1 . ((query . h--query-github) (url . "https://forge1.com/.*/.*"))))))))

(provide 'h-tests)
;;; h-tests.el ends here

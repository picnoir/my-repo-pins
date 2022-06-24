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
  (let* ((d (file-name-as-directory dir))
         (git-process
          (progn
            (make-directory d t)
            (h--call-git-in-dir d
                                nil
                                "init"))))
    (progn
      (unless (file-directory-p d) (make-directory d t))
      ;; ERT does not handle async processes gracefully for the time
      ;; being. Blocking and waiting for the git process to exit
      ;; before moving on.
      (while (accept-process-output git-process)))))

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
       (h--tests-init-fake-git-repo (concat temp-dir "example1.tld/user1/proj1"))
       (h--tests-init-fake-git-repo (concat temp-dir "example1.tld/user1/proj2"))
       (h--tests-init-fake-git-repo (concat temp-dir "example1.tld/user2/proj1"))
       (h--tests-init-fake-git-repo (concat temp-dir "example2.tld/user1/proj1"))
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
       (h--tests-init-fake-git-repo (concat temp-dir "example1.tld/user1/proj1"))
       (make-directory (concat (file-name-as-directory temp-dir) "example1.tld/user1/proj2"))
       (h--tests-init-fake-git-repo (concat temp-dir "example1.tld/user2/proj1"))
       (h--tests-init-fake-git-repo (concat temp-dir "example2.tld/user1/proj1"))
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

;; Gitea

(ert-deftest h--tests-fetch-gitea-parse-response-ok ()
  "Test h--tests-fetch-gitea-parse-response with a fixture."
    (with-temp-buffer
      (insert-file-contents "./tests/fixtures/gitea-get-request-ok.txt")
      (should (equal (h--fetch-gitea-parse-response (current-buffer))
                  '((ssh . "gitea@git.alternativebit.fr:NinjaTrappeur/h.el.git")
                    (https . "https://git.alternativebit.fr/NinjaTrappeur/h.el.git"))))))

(ert-deftest h--tests-fetch-gitea-parse-response-ko ()
  "Test h--tests-fetch-gitea-parse-response with a fixture."
    (with-temp-buffer
      (insert-file-contents "./tests/fixtures/gitea-get-request-ko.txt")
      (should (equal (h--fetch-gitea-parse-response (current-buffer)) nil))))

;; Test repo URI parser
;;;;;;;;;;;;;;;;;

(ert-deftest h--test-parse-repo-identifier ()
  "Test h--parse-repo-identifier."
  (should (equal
           (h--parse-repo-identifier "https://github.com/Ninjatrappeur/h.el")
           '((tag . full-url) (full-url . "https://github.com/Ninjatrappeur/h.el"))))
  (should (equal
           (h--parse-repo-identifier "github.com/Ninjatrappeur/h.el")
           '((tag . full-url) (full-url . "github.com/Ninjatrappeur/h.el"))))
  (should (equal
           (h--parse-repo-identifier "Ninjatrappeur/h.el")
           '((tag . owner-repo) (owner . "Ninjatrappeur") (repo . "h.el"))))
  (should (equal
           (h--parse-repo-identifier "h.el")
           '((tag . repo) (repo . "h.el")))))

(ert-deftest h--test-filepath-from-clone-url ()
  "Test h--filepath-from-clone-url."
  ;; HTTP/HTTPS
  (should (equal (h--filepath-from-clone-url "http://github.com/NinjaTrappeur/h.el.git") "github.com/NinjaTrappeur/h.el"))
  (should (equal (h--filepath-from-clone-url "http://github.com/NinjaTrappeur/h.el") "github.com/NinjaTrappeur/h.el"))
  (should (equal (h--filepath-from-clone-url "https://github.com/NinjaTrappeur/h.el.git") "github.com/NinjaTrappeur/h.el"))
  (should (equal (h--filepath-from-clone-url "https://github.com/NinjaTrappeur/h.el") "github.com/NinjaTrappeur/h.el"))
  (should (equal (h--filepath-from-clone-url "http://git.savannah.gnu.org/cgit/emacs/elpa.git") "git.savannah.gnu.org/cgit/emacs/elpa"))
  (should (equal (h--filepath-from-clone-url "https://git.savannah.gnu.org/git/emacs.git") "git.savannah.gnu.org/git/emacs"))
  ;; SSH
  (should (equal (h--filepath-from-clone-url "ssh://git@github.com:NinjaTrappeur/h.el.git") "github.com/NinjaTrappeur/h.el"))
  (should (equal (h--filepath-from-clone-url "ssh://git@github.com:NinjaTrappeur/h.el") "github.com/NinjaTrappeur/h.el"))
  (should (equal (h--filepath-from-clone-url "git@github.com:NinjaTrappeur/h.el.git") "github.com/NinjaTrappeur/h.el"))
  (should (equal (h--filepath-from-clone-url "git@github.com:NinjaTrappeur/h.el") "github.com/NinjaTrappeur/h.el")))

(ert-deftest h--test-git-clone-in-dir ()
  "Test the h--git-clone-in-dir function."
    (h--tests-run-on-testroot-1
     (lambda (dir)
       (let*
           ((tmpdir (make-temp-file "h-test-" t))
            (git-process (h--git-clone-in-dir
                          (format "file://%s" (concat dir "example1.tld/user1/proj1/"))
                          tmpdir)))
         (progn
           (while (accept-process-output git-process))
           (should (file-exists-p (format "%s/.git" tmpdir)))
           (delete-directory tmpdir t))))))

;;; State Management tests

(ert-deftest h--test-init-forges-state ()
  "Test the h--init-forges-state function."
  (let* ((forge-fetchers
          '(("GitHub.com" .
             ((query-user-repo . h--query-github-owner-repo)))
            ("GitLab.com" .
             ((query-user-repo . (lambda (owner repo cb) (h--query-gitlab-owner-repo "gitlab.com" owner repo cb)))))
            ("git.sr.ht" .
             ((query-user-repo . (lambda (owner repo cb) (h--query-sourcehut-owner-repo "git.sr.ht" owner repo cb)))))
            ("Codeberg.org" .
             ((query-user-repo . (lambda (owner repo cb) (h--query-gitea-owner-repo "codeberg.org" owner repo cb)))))))
         (result (h--init-forges-state forge-fetchers)))
    (should (equal (alist-get "GitHub.com" result nil nil 'equal) 'loading))
    (should (equal (alist-get "GitLab.com" result nil nil 'equal) 'loading))
    (should (equal (alist-get "git.sr.ht" result nil nil 'equal) 'loading))
    (should (equal (alist-get "Codeberg.org" result nil nil 'equal) 'loading))))

;;; UI-related tests

(ert-deftest h--test-add-keys-to-forge-status ()
  "Test the h--add-keys-to-forge-status function."
  (let
      ((dummy-forge-query-status-one-result
        '(("GitHub"
          (ssh . "git@github.com:NinjaTrappeur/h.el.git")
          (https . "https://github.com/NinjaTrappeur/h.el.git"))
         ("GitLab" . not-found)))
       (expected-forge-query-status-with-keys-one-result
        `(("GitHub"
           (status
            (ssh . "git@github.com:NinjaTrappeur/h.el.git")
            (https . "https://github.com/NinjaTrappeur/h.el.git"))
          (key . ,?1))
          ("GitLab" (status . not-found))))
       (dummy-forge-query-status-two-results
        '(("GitHub"
           (ssh . "git@github.com:NinjaTrappeur/h.el.git")
           (https . "https://github.com/NinjaTrappeur/h.el.git"))
          ("Codeberg" . not-found)
          ("GitLab"
           (ssh . "git@gitlab.com:NinjaTrappeur/h.el.git")
           (https . "https://gitlab.com/NinjaTrappeur/h.el.git"))))
       (expected-forge-query-status-with-keys-two-results
        `(("GitHub"
           (status
            (ssh . "git@github.com:NinjaTrappeur/h.el.git")
            (https . "https://github.com/NinjaTrappeur/h.el.git"))
           (key . ,'?1))
          ("Codeberg" (status . not-found))
          ("GitLab"
           (status
            (ssh . "git@gitlab.com:NinjaTrappeur/h.el.git")
            (https . "https://gitlab.com/NinjaTrappeur/h.el.git"))
           (key . ,'?2)))))

    (should (equal
             expected-forge-query-status-with-keys-one-result
             (h--add-keys-to-forge-status dummy-forge-query-status-one-result)))
    (should (equal
             expected-forge-query-status-with-keys-two-results
             (h--add-keys-to-forge-status dummy-forge-query-status-two-results)))))

(provide 'h-tests)
;;; h-tests.el ends here

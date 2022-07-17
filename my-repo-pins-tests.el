;;; my-repo-pins-tests.el --- Project navigation and remote checkout -*- lexical-binding: t; -*-

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
(require 'my-repo-pins)

;; Test Helpers
;;;;;;;;;;;;;;

(defun my-repo-pins--tests-with-temp-dir (func)
  "Run the FUNC function in a temporary directory.

FUNC gets called with the temp dir as parameter.
The directory gets deleted once we exit FUNC."
  (let ((temp-dir (make-temp-file "my-repo-pins-test-" t)))
    (unwind-protect
          (funcall func (file-name-as-directory temp-dir))
      (delete-directory temp-dir t))))

(defun my-repo-pins--tests-init-fake-git-repo (dir)
  "Create a dummy git repo at DIR.

If DIR doesn't exists, we create it first."
  (let* ((d (file-name-as-directory dir))
         (git-process
          (progn
            (make-directory d t)
            (my-repo-pins--call-git-in-dir d
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

(defun my-repo-pins--tests-run-on-testroot-1 (func)
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
  (my-repo-pins--tests-with-temp-dir
   (lambda (temp-dir)
     (progn
       (my-repo-pins--tests-init-fake-git-repo (concat temp-dir "example1.tld/user1/proj1"))
       (my-repo-pins--tests-init-fake-git-repo (concat temp-dir "example1.tld/user1/proj2"))
       (my-repo-pins--tests-init-fake-git-repo (concat temp-dir "example1.tld/user2/proj1"))
       (my-repo-pins--tests-init-fake-git-repo (concat temp-dir "example2.tld/user1/proj1"))
       (funcall func temp-dir)
       ))))

(defun my-repo-pins--tests-run-on-testroot-2 (func)
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
  (my-repo-pins--tests-with-temp-dir
   (lambda (temp-dir)
     (progn
       (my-repo-pins--tests-init-fake-git-repo (concat temp-dir "example1.tld/user1/proj1"))
       (make-directory (concat (file-name-as-directory temp-dir) "example1.tld/user1/proj2"))
       (my-repo-pins--tests-init-fake-git-repo (concat temp-dir "example1.tld/user2/proj1"))
       (my-repo-pins--tests-init-fake-git-repo (concat temp-dir "example2.tld/user1/proj1"))
       (funcall func temp-dir)))))

(defun my-repo-pins--tests-run-on-nested-testroot (func)
  "Run the FUNC function on testroot2.

FUNC is called with the directory cotaining test root 2 as parameter.

For reference: test-root-2 looks like this:
    test-root-2
    ├── example1.tld
    │   ├── user1
    │   │   ├── proj1
    │   │   ├── nested
    │   │   │   └── repo
    │   │   └── nested2
    │   │       └── git
    │   │           └── repo
    │   └── user2
    │       └── proj1
    └── example2.tld
        └── user1
            └── proj1"
  (my-repo-pins--tests-with-temp-dir
   (lambda (temp-dir)
     (progn
       (my-repo-pins--tests-init-fake-git-repo (concat temp-dir "example1.tld/user1/proj1"))
       (my-repo-pins--tests-init-fake-git-repo (concat temp-dir "example1.tld/user1/nested/repo"))
       (my-repo-pins--tests-init-fake-git-repo (concat temp-dir "example1.tld/user1/nested2/git/repo"))
       (my-repo-pins--tests-init-fake-git-repo (concat temp-dir "example1.tld/user2/proj1"))
       (my-repo-pins--tests-init-fake-git-repo (concat temp-dir "example2.tld/user1/proj1"))
       (funcall func temp-dir)))))


(defun my-repo-pins--tests-run-on-empty-testroot (func)
  "Run the FUNC function on testroot1.

FUNC is called with a empty test root.

For reference: a empty test root looks like this:
    test-root"
  (my-repo-pins--tests-with-temp-dir
   (lambda (temp-dir)
     (progn
       (funcall func temp-dir)))))

;; Tests
;;;;;;;

(ert-deftest my-repo-pins--tests-get-code-root-projects-coderoot-1 ()
  "Test the `my-repo-pins--get-code-root-projects with test-root-1 setup."
  (let
      ((results
        (my-repo-pins--tests-run-on-testroot-1 (lambda (root) (my-repo-pins--get-code-root-projects root 3))))
       )
    (should (member "example1.tld/user1/proj1" results))
    (should (member "example1.tld/user1/proj2" results))
    (should (member "example1.tld/user2/proj1" results))
    (should (member "example2.tld/user1/proj1" results))
    (should (eq (length results) 4))))


(ert-deftest my-repo-pins--tests-find-git-dirs-recursively-coderoot-1 ()
  "Test the `my-repo-pins--get-code-root-projects with test-root-1 setup."
  (let*
      ((r nil)
       (results
        (my-repo-pins--tests-run-on-testroot-1
         (lambda (root)
           (progn (setq r root)
                  (my-repo-pins--find-git-dirs-recursively root 3))))))
    (should (member (concat r "example1.tld/user1/proj1/") results))
    (should (member (concat r "example1.tld/user1/proj2/") results))
    (should (member (concat r "example1.tld/user2/proj1/") results))
    (should (member (concat r "example2.tld/user1/proj1/") results))
    (should (eq (length results) 4))))

(ert-deftest my-repo-pins--tests-get-code-root-projects-coderoot-2 ()
  "Test the `my-repo-pins--get-code-root-projects with test-root-2 setup."
  (let
      ((results
        (my-repo-pins--tests-run-on-testroot-2 (lambda (root) (my-repo-pins--get-code-root-projects root 3))))
       )
    (should (member "example1.tld/user1/proj1" results))
    (should (member "example1.tld/user2/proj1" results))
    (should (member "example2.tld/user1/proj1" results))
    (should (eq (length results) 3))))

(ert-deftest my-repo-pins--tests-get-code-root-projects-nested-coderoot-max-depth-2 ()
  "Test the `my-repo-pins--get-code-root-projects with nested-test-root setup."
  (let
      ((results
        (my-repo-pins--tests-run-on-nested-testroot (lambda (root) (my-repo-pins--get-code-root-projects root 2))))
       )
    (should (member "example1.tld/user1/proj1" results))
    (should (member "example1.tld/user2/proj1" results))
    (should (member "example2.tld/user1/proj1" results))
    (should (eq (length results) 3))))

(ert-deftest my-repo-pins--tests-get-code-root-projects-nested-coderoot-max-depth-3 ()
  "Test the `my-repo-pins--get-code-root-projects with nested-test-root setup."
  (let
      ((results
        (my-repo-pins--tests-run-on-nested-testroot (lambda (root) (my-repo-pins--get-code-root-projects root 3))))
       )
    (should (member "example1.tld/user1/proj1" results))
    (should (member "example1.tld/user2/proj1" results))
    (should (member "example2.tld/user1/proj1" results))
    (should (member "example1.tld/user1/nested/repo" results))
    (should (not (member "example1.tld/user1/nested2/git/repo" results)))
    (should (eq (length results) 4))))

(ert-deftest my-repo-pins--tests-get-code-root-projects-nested-coderoot-max-depth-no-limit ()
  "Test the `my-repo-pins--get-code-root-projects with nested-test-root setup."
  (let
      ((results
        (my-repo-pins--tests-run-on-nested-testroot (lambda (root) (my-repo-pins--get-code-root-projects root nil)))))
    (should (member "example1.tld/user1/proj1" results))
    (should (member "example1.tld/user2/proj1" results))
    (should (member "example2.tld/user1/proj1" results))
    (should (member "example1.tld/user1/nested/repo" results))
    (should (member "example1.tld/user1/nested2/git/repo" results))
    (should (eq (length results) 5))))

(ert-deftest my-repo-pins--tests-find-git-dirs-recursively-coderoot-2 ()
  "Test the `my-repo-pins--get-code-root-projects with test-root-2 setup."
  (let*
      ((r nil)
       (results
        (my-repo-pins--tests-run-on-testroot-2
         (lambda (root)
           (progn (setq r root)
                  (my-repo-pins--find-git-dirs-recursively root 3))))))
    (should (member (concat r "example1.tld/user1/proj1/") results))
    (should (member (concat r "example1.tld/user2/proj1/") results))
    (should (member (concat r "example2.tld/user1/proj1/") results))
    (should (eq (length results) 3))))

(ert-deftest my-repo-pins--tests-get-code-root-projects-empty-coderoot ()
  "Test the `my-repo-pins--get-code-root-projects with a empty coderoot."
  (let
      ((results
        (my-repo-pins--tests-run-on-empty-testroot (lambda (root) (my-repo-pins--get-code-root-projects root 3))))
       )
    (should (seq-empty-p results))))

(ert-deftest my-repo-pins--tests-find-git-dirs-recursively-empty-coderoot ()
  "Test the `my-repo-pins--get-code-root-projects with a empty coderoot."
  (let
      ((results
        (my-repo-pins--tests-run-on-empty-testroot (lambda (root) (my-repo-pins--find-git-dirs-recursively root 3))))
       )
    (should (seq-empty-p results))))

(ert-deftest my-repo-pins--tests-get-code-root-projects-no-coderoot ()
  "Test the `my-repo-pins--get-code-root-projects with a non-existing coderoot."
  (let
      ((results (my-repo-pins--get-code-root-projects "/does/not/exist" 3)))
    (should (seq-empty-p results))))


;; Test Fetchers
;;;;;;;;;;;;;;;;;

;; Github

(ert-deftest my-repo-pins--tests-fetch-github-parse-response-ok ()
  "Test my-repo-pins--tests-fetch-github-parse-response with a fixture."
    (with-temp-buffer
      (insert-file-contents "./tests/fixtures/github-get-request-ok.txt")
      (should (equal (my-repo-pins--fetch-github-parse-response (current-buffer))
                  '((ssh . "git@github.com:NinjaTrappeur/my-repo-pins.el.git")
                    (https . "https://github.com/NinjaTrappeur/my-repo-pins.el.git"))))))

(ert-deftest my-repo-pins--tests-fetch-github-parse-response-ko ()
  "Test my-repo-pins--tests-fetch-github-parse-response with a fixture."
    (with-temp-buffer
      (insert-file-contents "./tests/fixtures/github-get-request-ko.txt")
      (should (equal (my-repo-pins--fetch-github-parse-response (current-buffer)) nil))))

;; Gitea

(ert-deftest my-repo-pins--tests-fetch-gitea-parse-response-ok ()
  "Test my-repo-pins--tests-fetch-gitea-parse-response with a fixture."
    (with-temp-buffer
      (insert-file-contents "./tests/fixtures/gitea-get-request-ok.txt")
      (should (equal (my-repo-pins--fetch-gitea-parse-response (current-buffer))
                  '((ssh . "gitea@git.alternativebit.fr:NinjaTrappeur/my-repo-pins.el.git")
                    (https . "https://git.alternativebit.fr/NinjaTrappeur/my-repo-pins.el.git"))))))

(ert-deftest my-repo-pins--tests-fetch-gitea-parse-response-ko ()
  "Test my-repo-pins--tests-fetch-gitea-parse-response with a fixture."
    (with-temp-buffer
      (insert-file-contents "./tests/fixtures/gitea-get-request-ko.txt")
      (should (equal (my-repo-pins--fetch-gitea-parse-response (current-buffer)) nil))))

;; Test repo URI parser
;;;;;;;;;;;;;;;;;

(ert-deftest my-repo-pins--test-parse-repo-identifier ()
  "Test my-repo-pins--parse-repo-identifier."
  (should (equal
           (my-repo-pins--parse-repo-identifier "https://github.com/Ninjatrappeur/my-repo-pins.el")
           '((tag . full-url) (full-url . "https://github.com/Ninjatrappeur/my-repo-pins.el"))))
  (should (equal
           (my-repo-pins--parse-repo-identifier "github.com/Ninjatrappeur/my-repo-pins.el")
           '((tag . full-url) (full-url . "github.com/Ninjatrappeur/my-repo-pins.el"))))
  (should (equal
           (my-repo-pins--parse-repo-identifier "Ninjatrappeur/my-repo-pins.el")
           '((tag . owner-repo) (owner . "Ninjatrappeur") (repo . "my-repo-pins.el"))))
  (should (equal
           (my-repo-pins--parse-repo-identifier "my-repo-pins.el")
           '((tag . repo) (repo . "my-repo-pins.el")))))

(ert-deftest my-repo-pins--test-filepath-from-clone-url ()
  "Test my-repo-pins--filepath-from-clone-url."
  ;; HTTP/HTTPS
  (should (equal (my-repo-pins--filepath-from-clone-url "http://github.com/NinjaTrappeur/my-repo-pins.el.git") "github.com/NinjaTrappeur/my-repo-pins.el"))
  (should (equal (my-repo-pins--filepath-from-clone-url "http://github.com/NinjaTrappeur/my-repo-pins.el") "github.com/NinjaTrappeur/my-repo-pins.el"))
  (should (equal (my-repo-pins--filepath-from-clone-url "https://github.com/NinjaTrappeur/my-repo-pins.el.git") "github.com/NinjaTrappeur/my-repo-pins.el"))
  (should (equal (my-repo-pins--filepath-from-clone-url "https://github.com/NinjaTrappeur/my-repo-pins.el") "github.com/NinjaTrappeur/my-repo-pins.el"))
  (should (equal (my-repo-pins--filepath-from-clone-url "http://git.savannah.gnu.org/cgit/emacs/elpa.git") "git.savannah.gnu.org/cgit/emacs/elpa"))
  (should (equal (my-repo-pins--filepath-from-clone-url "https://git.savannah.gnu.org/git/emacs.git") "git.savannah.gnu.org/git/emacs"))
  ;; SSH
  (should (equal (my-repo-pins--filepath-from-clone-url "ssh://git@github.com:NinjaTrappeur/my-repo-pins.el.git") "github.com/NinjaTrappeur/my-repo-pins.el"))
  (should (equal (my-repo-pins--filepath-from-clone-url "ssh://git@github.com:NinjaTrappeur/my-repo-pins.el") "github.com/NinjaTrappeur/my-repo-pins.el"))
  (should (equal (my-repo-pins--filepath-from-clone-url "git@github.com:NinjaTrappeur/my-repo-pins.el.git") "github.com/NinjaTrappeur/my-repo-pins.el"))
  (should (equal (my-repo-pins--filepath-from-clone-url "git@github.com:NinjaTrappeur/my-repo-pins.el") "github.com/NinjaTrappeur/my-repo-pins.el")))

(ert-deftest my-repo-pins--test-git-clone-in-dir ()
  "Test the my-repo-pins--git-clone-in-dir function."
    (my-repo-pins--tests-run-on-testroot-1
     (lambda (dir)
       (let*
           ((tmpdir (make-temp-file "my-repo-pins-test-" t))
            (git-process (my-repo-pins--git-clone-in-dir
                          (format "file://%s" (concat dir "example1.tld/user1/proj1/"))
                          tmpdir)))
         (progn
           (while (accept-process-output git-process))
           (should (file-exists-p (format "%s/.git" tmpdir)))
           (delete-directory tmpdir t))))))

;;; State Management tests

(ert-deftest my-repo-pins--test-init-forges-state ()
  "Test the my-repo-pins--init-forges-state function."
  (let* ((forge-fetchers
          '(("GitHub.com" .
             ((query-user-repo . my-repo-pins--query-github-owner-repo)))
            ("GitLab.com" .
             ((query-user-repo . (lambda (owner repo cb) (my-repo-pins--query-gitlab-owner-repo "gitlab.com" owner repo cb)))))
            ("git.sr.ht" .
             ((query-user-repo . (lambda (owner repo cb) (my-repo-pins--query-sourcehut-owner-repo "git.sr.ht" owner repo cb)))))
            ("Codeberg.org" .
             ((query-user-repo . (lambda (owner repo cb) (my-repo-pins--query-gitea-owner-repo "codeberg.org" owner repo cb)))))))
         (result (my-repo-pins--init-forges-state forge-fetchers)))
    (should (equal (alist-get "GitHub.com" result nil nil 'equal) 'loading))
    (should (equal (alist-get "GitLab.com" result nil nil 'equal) 'loading))
    (should (equal (alist-get "git.sr.ht" result nil nil 'equal) 'loading))
    (should (equal (alist-get "Codeberg.org" result nil nil 'equal) 'loading))))

;;; UI-related tests

(ert-deftest my-repo-pins--test-add-keys-to-forge-status ()
  "Test the my-repo-pins--add-keys-to-forge-status function."
  (let
      ((dummy-forge-query-status-one-result
        '(("GitHub"
          (ssh . "git@github.com:NinjaTrappeur/my-repo-pins.el.git")
          (https . "https://github.com/NinjaTrappeur/my-repo-pins.el.git"))
         ("GitLab" . not-found)))
       (expected-forge-query-status-with-keys-one-result
        `(("GitHub"
           (status
            (ssh . "git@github.com:NinjaTrappeur/my-repo-pins.el.git")
            (https . "https://github.com/NinjaTrappeur/my-repo-pins.el.git"))
          (key . ,?1))
          ("GitLab" (status . not-found))))
       (dummy-forge-query-status-two-results
        '(("GitHub"
           (ssh . "git@github.com:NinjaTrappeur/my-repo-pins.el.git")
           (https . "https://github.com/NinjaTrappeur/my-repo-pins.el.git"))
          ("Codeberg" . not-found)
          ("GitLab"
           (ssh . "git@gitlab.com:NinjaTrappeur/my-repo-pins.el.git")
           (https . "https://gitlab.com/NinjaTrappeur/my-repo-pins.el.git"))))
       (expected-forge-query-status-with-keys-two-results
        `(("GitHub"
           (status
            (ssh . "git@github.com:NinjaTrappeur/my-repo-pins.el.git")
            (https . "https://github.com/NinjaTrappeur/my-repo-pins.el.git"))
           (key . ,'?1))
          ("Codeberg" (status . not-found))
          ("GitLab"
           (status
            (ssh . "git@gitlab.com:NinjaTrappeur/my-repo-pins.el.git")
            (https . "https://gitlab.com/NinjaTrappeur/my-repo-pins.el.git"))
           (key . ,'?2)))))

    (should (equal
             expected-forge-query-status-with-keys-one-result
             (my-repo-pins--add-keys-to-forge-status dummy-forge-query-status-one-result)))
    (should (equal
             expected-forge-query-status-with-keys-two-results
             (my-repo-pins--add-keys-to-forge-status dummy-forge-query-status-two-results)))))

(provide 'my-repo-pins-tests)
;;; my-repo-pins-tests.el ends here

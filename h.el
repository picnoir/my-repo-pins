;;; h.el --- Project navigation and remote checkout -*- lexical-binding: t -*-

;;; Copyright (C) 2022 Félix Baylac Jacqué
;;; Author: Félix Baylac Jacqué <felix at alternativebit.fr>
;;; Maintainer: Félix Baylac Jacqué <felix at alternativebit.fr>
;;; Version: 1.14.0
;;; Homepage: https://alternativebit.fr/TODO
;;; Package-Requires: ((emacs "25.1"))

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

;;;; Commentary:

;;; TODO before publish

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
;; Required to batch eval the module: the substring functions are
;; loaded by default in interactive emacs, not in batch-mode emacs.
(eval-when-compile (require 'subr-x))

(defgroup h-group nil
  "Variables used to setup the h.el project manager."
  :group 'Communication)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal: git primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom h-git-bin "git"
  "Path pointing to the git binary.
By default, it'll look for git in the current $PATH."
  :type 'file
  :group 'h-group)

(defun h--git-path ()
  "Find the git binary path using ‘h-git-bin’.

Errors out if we can't find it."
  (if (file-executable-p h-git-bin)
      h-git-bin
    (let ((git-from-bin-path (locate-file h-git-bin exec-path)))
      (if (file-executable-p git-from-bin-path)
          git-from-bin-path
          (error "Can't find git. Is h-git-bin correctly set?")))))

(defun h--call-git-in-dir (dir &rest args)
  "Call the git binary as pointed by ‘h-git-bin’ in DIR with ARGS."
  (let ((default-directory dir))
    (apply 'process-file (seq-concatenate 'list `(,(h--git-path) nil "*h git log*" nil) args))))

(defun h--git-clone-in-dir (clone-url checkout-filepath)
  "Clone the CLONE-URL repo at CHECKOUT-FILEPATH."
  (h--call-git-in-dir "~/" "clone" clone-url checkout-filepath))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal: builtin fetchers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generic fetcher infrastructure
(defvar h--builtins-forge-fetchers
  '(("GitHub" .
            ((query-user-repo . h--query-github-owner-repo)
             (url . "https?://github.com/.*"))))

  "Fetchers meant to be used in conjunction with ‘h-forge-fetchers’.

This variable contains fetchers for:
- github.com")

(defcustom h-forge-fetchers
  h--builtins-forge-fetchers
  "List of forges for which we want to remote fetch projects."
  :type '(alist
          :key-type symbol
          :value-type (alist
                        :key-type symbol
                        :value-type (choice function string)))
  :group 'h-group)

(defvar h--forge-fetchers-state '())

;;; Github Fetcher
(defun h--fetch-github-parse-response (response-buffer)
  "Parse the RESPONSE-BUFFER containing a GET response from the GitHub API.

Parsing a response from a GET https://api.github.com/repos/user/repo request.

If the repo does exists, returns a alist in the form of:

`(
  (ssh . SSH-CHECKOUT-URL)
  (https . HTTPS-CHECKOUT-URL)
  (forge-str . \"Github\")
)

Returns nil if the repo does not exists."
  (progn (set-buffer response-buffer)
         (goto-char (point-min))
         (if (not(eq(re-search-forward "^HTTP/1.1 200 OK$" nil t) nil))
             (progn
               (goto-char (point-min))
               (re-search-forward "^$")
               (delete-region (point) (point-min))
               (let* ((parsed-buffer (json-read))
                      (ssh-url (alist-get 'ssh_url parsed-buffer))
                      (https-url (alist-get 'clone_url parsed-buffer)))
                 `((ssh . ,ssh-url)
                   (https . ,https-url))))
           nil)))

(defun h--query-github-owner-repo (user-name repo-name forge callback)
  "Queries the GitHub API to retrieve some infos about a GitHub repo.
This function will first try to determine whether
github.com/USER-NAME/REPO-NAME exists.

If so, calls the CALLBACK function with a alist containing the ssh and
https clone URLs. If the repo does not exists, calls the callback with
nil as parameter."
  (progn
    (url-retrieve
     (format "https://api.github.com/repos/%s/%s" user-name repo-name)
     (lambda (&rest _rest) (funcall callback (h--fetch-github-parse-response(current-buffer)) forge)))))

;;; Gitea Fetcher
(defun h--query-gitea (instance-url user-name repo-name callback)
  "Queries the INSTANCE-URL gitea instance to retrieve a repo informations.
This function will first try to dertermine whether the
USER-NAME/REPO-NAME exists.

If so, calls the CALLBACK function with a alist containing the ssh and
https clone URLs. If the repo does not exists, calls the callback with
nil as parameter."
  (url-retrieve
   (format "%s/api/v1/repos/%s/%s" instance-url user-name repo-name)
   (lambda (&rest _rest) (funcall callback (h--fetch-gitea-parse-response(current-buffer))))))
; Get /repos/owner/repo


;;; Gitlab Fetcher

(defun h--query-gitlab (instance-url user-name repo-name callback)
  "Queries the INSTANCE-URL gitlab instance to retrieve a repo informations.
This function will first try to dertermine whether the
USER-NAME/REPO-NAME exists.

If so, calls the CALLBACK function with a alist containing the ssh and
https clone URLs. If the repo does not exists, calls the callback with
nil as parameter."
  (url-retrieve
   (format "%s/api/v4/users/%s/projects" instance-url user-name)
   (lambda (&rest _rest) (funcall callback nil))))
;1. Find project in
;https://gitlab.com/api/v4/users/ninjatrappeur/projects

;;; Generic fetcher infrastructure

(defun h--dispatch-fetcher (query-string)
  "Try to download QUERY-STRING via the fetchers registered in ‘h-forge-fetchers’."
  (cond ((string-match-p "github.com" query-string)
         (apply 'h--query-github (h--parse-query-string-for-forge query-string)))
        ((string-match-p "gitlab.com" query-string)
         (apply 'h--query-gitlab (h--parse-query-string-for-forge query-string)))
        (t (error (format "No fetcher for %s" query-string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal: repo URI parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun h--parse-repo-identifier (query-str)
  "Do its best to figure out which repo the user meant by QUERY-STR.

A valid QUERY-STR is in one of the 4 following formats:

1. project
   Jump to the project if available, do not fetch a remote forge
   project.
2. owner/project
   Open a promp with available projects + fetch all the remote
   forges.
3. forge.tld/owner/project
   Open the project is available, fetch it if not.
4. https://forge.tld/owner/project
   Open the project is available, fetch it if not.

This function will return a tagged union in the form of a alist. For
each kind of format, it'll return something along the line of:

\(('tag . 'full-url) ('full-url .\
\"https://full-url.org/path/to/git/repo/checkout\"))
or
\(('tag . 'owner-repo) ('owner . \"NinjaTrappeur\") ('repo\
. \"h.el\"))
or
\(('tag . 'repo) ('repo . \"h.el\"))"
  (cond
   ;; Full-url case
   ((or (string-match "^https?://.*/.*/.*$" query-str)
        (string-match "^.*/.*/.*$" query-str))
    `((tag . full-url) (full-url . ,query-str)))
   ;; owner/repo case
   ((string-match "^.*/.*$" query-str)
    (let*
        ((splitted-query (split-string "Ninjatrappeur/h.el" "/"))
         (owner (car splitted-query))
         (repo (cadr splitted-query)))
    `((tag . owner-repo) (owner . ,owner) (repo . ,repo))))
   ;; repo case
   (t `((tag . repo) (repo . ,query-str)))))

(defun h--filepath-from-clone-url (clone-url)
  "Return the relative path relative to the coderoot for CLONE-URL.

CLONE-STR being the git clone URL we want to find the local path for."
  (let*
      ((is-http (string-match-p "^https?://.*$" clone-url))
       (is-ssh (string-match-p "^\\(ssh://\\)?.*@.*:.*$" clone-url)))
    (cond (is-http
           (string-remove-suffix
            ".git"
            (cadr(split-string clone-url "//"))))
          (is-ssh
           (let*
               ((url-without-user (cadr(split-string clone-url "@")))
                (colon-split (split-string url-without-user ":"))
                (fqdn (car colon-split))
                (repo-url (string-remove-suffix ".git" (cadr colon-split))))
             (format "%s/%s" fqdn repo-url))))))

(defun h--pick-relevant-forges (query-string forges-alist)
  "Filters out relevant FORGES-ALIST entries for QUERY-STRING.

If QUERY-STRING is in the form of owner/repo or just a repo name, do
not filter anything.

If QUERY-STRING is a fully qualified URL, exclusively use the relevant forge."
  (let*
      ((query-string-type
        (alist-get 'tag (h--parse-repo-identifier query-string))))
    (cond
     ;; query-string is a full URL. Let's filter out the irrelevant forges.
     ((eq query-string-type 'full-url)
      (seq-filter
       (lambda (e)
         (let* ((forge-url-regex (alist-get 'url e)))
           (string-match-p forge-url-regex query-string)))
       forges-alist))
     ((eq query-string-type 'owner-repo) forges-alist)
     ((eq query-string-type 'repo) forges-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal: code-root management functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom h-code-root nil
  "Root directory containing all your projects.
h.el organise the git repos you'll checkout in a tree fashion.

All the code fetched using h.el will end up in this root directory. A
tree of subdirectories will be created mirroring the remote URI.

For instance, after checking out
https://git.savannah.gnu.org/git/emacs/org-mode.git, the source code
will live in the h-code-root/git.savannah.gnu.org/git/emacs/org-mode/
local directory"
  :type 'directory
  :group 'h-group)

(defun h--safe-get-code-root ()
    "Ensure ‘h-code-root’ is correctly set, then canonalize the path.
Errors out if ‘h-code-root’ has not been set yet."
    (progn (when (not h-code-root)
             (error "h-code-root has not been set. Please point it to your code root"))
           (file-name-as-directory h-code-root)))


(defun h--find-git-dirs-recursively (dir)
  "Vendored, slightly modified version of ‘directory-files-recursively’.

This library isn't available for Emacs > 25.1. Vendoring it for
backward compatibility.

We take advantage of vendoring this function to taylor it a bit more
for our needs.

Return list of all git repositories under directory DIR. This function works
recursively. Files are returned in \"depth first\" order, and files
from each directory are sorted in alphabetical order. Each file name
appears in the returned list in its absolute form.

By default, the returned list excludes directories, but if
optional argument INCLUDE-DIRECTORIES is non-nil, they are
included."
  (let* ((projects nil)
         (recur-result nil)
         (dir (directory-file-name dir)))
    (dolist (file (sort (file-name-all-completions "" dir)
			'string<))
      (unless (member file '("./" "../"))
	(if (directory-name-p file)
	      ;; Don't follow symlinks to other directories.
            (let ((full-file (concat dir "/" file)))
	      (when (not (file-symlink-p full-file))
                (if (file-directory-p (concat full-file ".git"))
                    ;; It's a git repo, let's stop here.
                    (progn (setq projects (nconc projects (list full-file))))
                  ;; It's not a git repo, let's recurse into it.
                  (setq recur-result
                        (nconc recur-result
                               (h--find-git-dirs-recursively full-file)))))))))
   (nconc recur-result (nreverse projects))))


(defun h--get-code-root-projects (code-root)
  "Retrieve the projects contained in the CODE-ROOT directory.
We're going to make some hard assumptions about how the ‘h-code-root’
directory should look like. First of all, if a directory seem to be a
git repository, it'll automatically be considered as a project root.

It means that after encountering a git repository, we won't recurse
any further.

If the directory pointed by h-code-root does not exists yet, returns
an empty list."
  (if (not (file-directory-p code-root))
      '()
    (let*
        ((remove-code-root-prefix-and-trailing-slash
          (lambda (path)
            (let ((path-without-prefix (string-remove-prefix code-root path)))
                (substring path-without-prefix 0 (1- (length path-without-prefix))))))
         (projects-absolute-path (h--find-git-dirs-recursively code-root))
         (projects-relative-to-code-root
          (mapcar remove-code-root-prefix-and-trailing-slash projects-absolute-path)))
      projects-relative-to-code-root)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal: UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun h--draw-forge-status (forge-result)
  "Draws a FORGE-RESULT status to the current buffer.

FORGE-STATUS being a alist in the form of (FORGE-NAME . LOOKUP-STATUS)"
  (let*
      ((status (alist-get 'status forge-result))
       (key (alist-get 'key forge-result))
       (forge-name (car forge-result))
       (status-text (cond
              ((eq status 'loading) (format "[?] %s (loading...)" forge-name))
              ((eq status 'not-found) (format "[X] %s" forge-name))
              ((eq status 'found) (format "[✓] %s" forge-name))
              (t (error (format "h--draw-forge-status: Invalid forge status %s" status)))))
       (text (format "%s [%s]\n" status-text (char-to-string key)))
       (font-color (cond
                    ((eq status 'loading) "orange")
                    ((eq status 'not-found) "red")
                    ((eq status 'found) "green")
                    (t (error (format "h--draw-forge-status: Invalid forge status %s" status)))))
       (original-point (point)))
  (progn
    (local-set-key (kbd (format "%s" (char-to-string key)))
                   (lambda () (interactive) (message (format "Checking out: %s" forge-name))))
    (insert text)
    ;; Set color for status indicator
    (set-text-properties original-point
                         (+ original-point 4)
                         `(face (:foreground ,font-color :weight bold)))
    ;; Set color for key binding
    (set-text-properties (- (point) 4) (point)
                         '(face (:foreground "orange" :weight bold))))))

(defun h--find-next-available-key-binding (cur-key-binding)
  "Find a key binding starting CUR-KEY-BINDING for the h buffer.

We're using the 1-9 numbers, then, once all the numbers are already in
use, we start allocating the a-Z letters."
  (cond ((= cur-key-binding ?9) ?a)
        ((= cur-key-binding ?z) (error "Keys exhausted, can't bind any more"))
        (t (+ cur-key-binding 1))))

(defun h--draw-ui-buffer (forge-status)
  "Draws the UI depending on the app state.

FORGE-STATUS being a alist in the form of (FORGE-NAME . LOOKUP-STATUS)
where FORGE-NAME is a string representing the name of a forge,
LOOKUP-STATUS an atom that is either 'loading, 'not-found or 'found."
  (let* (
        (h-buffer (get-buffer-create "h.el"))
        (previous-buffer (current-buffer))
        (dummy-forge-status
         '(
           ("GitHub" . loading)
           ("GitLab" . not-found)
           ("Codeberg" . found)))
        (forge-status-with-keys
         (reverse
          (cdr
           (cl-reduce
            (lambda
              (acc e)
              (append `(,(h--find-next-available-key-binding (car acc))
                        (,(car e) .
                         ((status . ,(cdr e))
                          (key . ,(car acc)))))
                      (cdr acc)))
            forge-status
            :initial-value '(?1 . ()))))))
    (progn
      (set-buffer h-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (local-set-key (kbd "q") 'delete-window)
      (seq-map (lambda (e) (h--draw-forge-status e)) forge-status-with-keys)
      (setq buffer-read-only t)
      (set-buffer previous-buffer)
      (display-buffer h-buffer))))

(defun h--update-forges-state (forge-name new-state)
  "Update ‘h--forge-fetchers-state’ for FORGE-NAME with NEW-STATE."
  (progn
    (setq h--forge-fetchers-state (assq-delete-all forge-name h--forge-fetchers-state))
    (setq h--forge-fetchers-state (cons `(,forge-name . ,new-state) h--forge-fetchers-state))))

(defun h--query-forge-fetchers (repo-query)
  "Find repo matches to the relevant forges for REPO-QUERY."
  (let* ((relevant-forges (h--pick-relevant-forges repo-query h-forge-fetchers))
         (parsed-repo-query (h--parse-repo-identifier repo-query))
         (repo-query-kind (alist-get 'tag parsed-repo-query)))
    (cond
     ((equal repo-query-kind 'owner-repo)
      (seq-map
       (lambda (forge)
         (let* ((owner (alist-get 'owner parsed-repo-query))
                (repo (alist-get 'repo parsed-repo-query))
                (fetch-func (alist-get 'query-user-repo forge))
                (forge-id (car forge)))
           (apply `(,fetch-func
                    ,owner
                    ,repo
                    ,forge
                    (lambda (result forge)
                        (h--update-forges-state (car forge) result))))))
       relevant-forges))
     ((equal repo-query-kind 'repo) (error (format "Can't checkout %s (for now), please specify a owner" repo-query)))
     ((equal repo-query-kind 'full-url) (error "TODO: Can't checkout a full URL (for now)"))
     (t (error repo-query-kind)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive Commands
;;;;;;;;;;;;;;;;;;;;;;;;

(defun h-checkout-project (user-query)
  (interactive "s")
  (progn
    (h--query-forge-fetchers user-query)
   ))

(defun h-jump-to-project ()
  "Open a project contained in the ‘h-code-root’ directory.
If the project is not here yet, check it out from the available sources."
  (interactive)
  (let* ((selected-project-from-coderoot
          (completing-read
           "Available projects: "
           (h--get-code-root-projects (h--safe-get-code-root))
           nil t ""))
         (selected-project-absolute-path (concat (h--safe-get-code-root) selected-project-from-coderoot)))
    (if (file-directory-p selected-project-absolute-path)
        (find-file selected-project-absolute-path)
      (error "NOT IMPLEMENTED: cannot checkout a new project for now"))))

(provide 'h)
;;; h.el ends here

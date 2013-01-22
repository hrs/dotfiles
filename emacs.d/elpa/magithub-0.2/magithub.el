;;; magithub.el --- Magit extensions for using GitHub

;; Copyright (c) 2010 Nathan Weizenbaum
;; Licensed under the same terms as Emacs.

;; Author: Nathan Weizenbaum
;; URL: http://github.com/nex3/magithub
;; Version: 0.2
;; Created: 2010-06-06
;; By: Nathan Weizenbaum
;; Keywords: git, github, magit
;; Package-Requires: ((magit "0.8") (json "1.2"))

;;; Commentary:

;; This package does two things.  First, it extends Magit's UI with
;; assorted GitHub-related functionality, similar to the github-gem
;; tool (http://github.com/defunkt/github-gem).  Second, it uses
;; Magit's excellent Git library to build an Elisp library for
;; interfacing with GitHub's API.

(require 'magit)
(require 'url)
(require 'json)
(require 'crm)
(eval-when-compile (require 'cl))


;;; Variables

(defvar magithub-api-base "https://github.com/api/v2/json/"
  "The base URL for accessing the GitHub API.")

(defvar magithub-github-url "https://github.com/"
  "The URL for the main GitHub site.

This is used for some calls that aren't supported by the official API.")

(defvar magithub-use-ssl nil
  "If non-nil, access GitHub via HTTPS.
This is more secure, but slower.")

(defvar magithub-gist-url "http://gist.github.com/"
  "The URL for the Gist site.")

(defvar magithub-view-gist t
  "Whether or not to open new Gists in the browser.")

(defvar magithub-request-data nil
  "An assoc list of parameter names to values.

This is meant to be dynamically bound around `magithub-retrieve'
and `magithub-retrieve-synchronously'.")

(defvar magithub-parse-response t
  "Whether to parse responses from GitHub as JSON.
Used by `magithub-retrieve' and `magithub-retrieve-synchronously'.
This should only ever be `let'-bound, not set outright.")

(defvar magithub-users-history nil
  "A list of users selected via `magithub-read-user'.")

(defvar magithub-repos-history nil
  "A list of repos selected via `magithub-read-repo'.")

(defvar magithub--repo-obj-cache (make-hash-table :test 'equal)
  "A hash from (USERNAME . REPONAME) to decoded JSON repo objects (plists).
This caches the result of `magithub-repo-obj' and
`magithub-cached-repo-obj'.")


;;; Utilities

(defun magithub--remove-if (predicate seq)
  "Remove all items satisfying PREDICATE from SEQ.
Like `remove-if', but without the cl runtime dependency."
  (loop for el being the elements of seq
        if (not (funcall predicate el)) collect el into els
        finally return els))

(defun magithub--position (item seq)
  "Return the index of ITEM in SEQ.
Like `position', but without the cl runtime dependency.

Comparison is done with `eq'."
  (loop for el in seq until (eq el item) count t))

(defun magithub--cache-function (fn)
  "Return a lambda that will run FN but cache its return values.
The cache is a very naive assoc from arguments to returns.
The cache will only last as long as the lambda does.

FN may call magithub--use-cache, which will use a pre-cached
value if available or recursively call FN if not."
  (lexical-let ((fn fn) cache cache-fn)
    (setq cache-fn
          (lambda (&rest args)
            (let ((cached (assoc args cache)))
              (if cached (cdr cached)
                (flet ((magithub--use-cache (&rest args) (apply cache-fn args)))
                  (let ((val (apply fn args)))
                    (push (cons args val) cache)
                    val))))))))

(defun magithub-make-query-string (params)
  "Return a query string constructed from PARAMS.
PARAMS is an assoc list of parameter names to values.

Any parameters with a nil values are ignored."
  (replace-regexp-in-string
   "&+" "&"
   (mapconcat
    (lambda (param)
      (when (cdr param)
        (concat (url-hexify-string (car param)) "="
                (url-hexify-string (cdr param)))))
    params "&")))

(defun magithub-parse-repo (repo)
  "Parse a REPO string of the form \"username/repo\".
Return (USERNAME . REPO), or raise an error if the format is
incorrect."
  (condition-case err
      (destructuring-bind (username repo) (split-string repo "/")
        (cons username repo))
    (wrong-number-of-arguments (error "Invalid GitHub repository %s" repo))))

(defun magithub-repo-url (username repo &optional sshp)
  "Return the repository URL for USERNAME/REPO.
If SSHP is non-nil, return the SSH URL instead.  Otherwise,
return the HTTP URL."
  (format (if sshp "git@github.com:%s/%s.git" "http://github.com/%s/%s.git")
          username repo))

(defun magithub-remote-info (remote)
  "Return (USERNAME REPONAME SSHP) for the given REMOTE.
Return nil if REMOTE isn't a GitHub remote.

USERNAME is the owner of the repo, REPONAME is the name of the
repo, and SSH is non-nil if it's checked out via SSH."
  (block nil
    (let ((url (magit-get "remote" remote "url")))
      (unless url (return))
      (when (string-match "\\(?:git\\|https?\\)://github\\.com/\\(.*?\\)/\\(.*\\)\.git" url)
        (return (list (match-string 1 url) (match-string 2 url) nil)))
      (when (string-match "git@github\\.com:\\(.*?\\)/\\(.*\\)\\.git" url)
        (return (list (match-string 1 url) (match-string 2 url) t)))
      (return))))

(defun magithub-remote-for-commit (commit)
  "Return the name of the remote that contains COMMIT.
If no remote does, return nil.  COMMIT should be the full SHA1
commit hash.

If origin contains the commit, it takes precedence.  Otherwise
the priority is nondeterministic."
  (flet ((name-rev (remote commit)
           (magit-git-string "name-rev" "--name-only" "--no-undefined" "--refs"
                             ;; I'm not sure why the initial * is required,
                             ;; but if it's not there this always returns nil
                             (format "*remotes/%s/*" remote) commit)))
    (let ((remote (or (name-rev "origin" commit) (name-rev "*" commit))))
      (when (and remote (string-match "^remotes/\\(.*?\\)/" remote))
        (match-string 1 remote)))))

(defun magithub-remote-info-for-commit (commit)
  "Return information about the GitHub repo for the remote that contains COMMIT.
If no remote does, return nil.  COMMIT should be the full SHA1
commit hash.

The information is of the form returned by `magithub-remote-info'.

If origin contains the commit, it takes precedence.  Otherwise
the priority is nondeterministic."
  (let ((remote (magithub-remote-for-commit commit)))
    (when remote (magithub-remote-info remote))))

(defun magithub-branches-for-remote (remote)
  "Return a list of branches in REMOTE, as of the last fetch."
  (let ((lines (magit-git-lines "remote" "show" "-n" remote)) branches)
    (while (not (string-match-p "^  Remote branches:" (pop lines)))
      (unless lines (error "Unknown output from `git remote show'")))
    (while (string-match "^    \\(.*\\)" (car lines))
      (push (match-string 1 (pop lines)) branches))
    branches))

(defun magithub-repo-relative-path ()
  "Return the path to the current file relative to the repository root.
Only works within `magithub-minor-mode'."
  (let ((filename buffer-file-name))
    (with-current-buffer magithub-status-buffer
      (file-relative-name filename default-directory))))

(defun magithub-name-rev-for-remote (rev remote)
  "Return a human-readable name for REV that's valid in REMOTE.
Like `magit-name-rev', but sanitizes things referring to remotes
and errors out on local-only revs."
  (setq rev (magit-name-rev rev))
  (if (and (string-match "^\\(remotes/\\)?\\(.*?\\)/\\(.*\\)" rev)
           (equal (match-string 2 rev) remote))
      (match-string 3 rev)
    (unless (magithub-remote-contains-p remote rev)
      (error "Commit %s hasn't been pushed"
             (substring (magit-git-string "rev-parse" rev) 0 8)))
    (cond
     ;; Assume the GitHub repo will have all the same tags as we do,
     ;; since we can't actually check without performing an HTTP request.
     ((string-match "^tags/\\(.*\\)" rev) (match-string 1 rev))
     ((and (not (string-match-p "^remotes/" rev))
           (member rev (magithub-branches-for-remote remote))
           (magithub-ref= rev (concat remote "/" rev)))
      rev)
     (t (magit-git-string "rev-parse" rev)))))

(defun magithub-remotes-containing-ref (ref)
  "Return a list of remotes containing REF."
  (loop with remotes
        for line in (magit-git-lines "branch" "-r" "--contains" ref)
        if (and (string-match "^ *\\(.+?\\)/" line)
                (not (string= (match-string 1 line) (car remotes))))
           do (push (match-string 1 line) remotes)
        finally return remotes))

(defun magithub-remote-contains-p (remote ref)
  "Return whether REF exists in REMOTE, in any branch.
This does not fetch origin before determining existence, so it's
possible that its result is based on stale data."
  (member remote (magithub-remotes-containing-ref ref)))

(defun magithub-ref= (ref1 ref2)
  "Return whether REF1 refers to the same commit as REF2."
  (string= (magit-rev-parse ref1) (magit-rev-parse ref2)))


;;; Reading Input

(defun magithub--lazy-completion-callback (fn &optional noarg)
  "Converts a simple string-listing FN into a lazy-loading completion callback.
FN should take a string (the contents of the minibuffer) and
return a list of strings (the candidates for completion).  This
method takes care of any caching and makes sure FN isn't called
until completion needs to happen.

If NOARG is non-nil, don't pass a string to FN."
  (lexical-let ((fn (magithub--cache-function fn)) (noarg noarg))
    (lambda (string predicate allp)
      (let ((strs (if noarg (funcall fn) (funcall fn string))))
        (if allp (all-completions string strs predicate)
          (try-completion string strs predicate))))))

(defun magithub-read-user (&optional prompt predicate require-match initial-input
                                     hist def inherit-input-method)
  "Read a GitHub username from the minibuffer with completion.

PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD work as in `completing-read'.  PROMPT
defaults to \"GitHub user: \".  HIST defaults to
'magithub-users-history.

WARNING: This function currently doesn't work fully, since
GitHub's user search API only returns an apparently random subset
of users."
  (setq hist (or hist 'magithub-users-history))
  (completing-read (or prompt "GitHub user: ")
                   (magithub--lazy-completion-callback
                    (lambda (s)
                      (mapcar (lambda (user) (plist-get user :name))
                              (magithub-user-search s))))
                   predicate require-match initial-input hist def inherit-input-method))

(defun magithub-read-repo-for-user (user &optional prompt predicate require-match
                                         initial-input hist def inherit-input-method)
  "Read a GitHub repository from the minibuffer with completion.
USER is the owner of the repository.

PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD work as in `completing-read'.  PROMPT
defaults to \"GitHub repo: <user>/\"."
  (lexical-let ((user user))
    (completing-read (or prompt (concat "GitHub repo: " user "/"))
                     (magithub--lazy-completion-callback
                      (lambda ()
                        (mapcar (lambda (repo) (plist-get repo :name))
                                (magithub-repos-for-user user)))
                      'noarg)
                     predicate require-match initial-input hist def
                     inherit-input-method)))

(defun magithub-read-repo (&optional prompt predicate require-match initial-input
                                     hist def inherit-input-method)
  "Read a GitHub user-repository pair with completion.
Return (USERNAME . REPO), or nil if the user enters no input.

PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD work as in `completing-read'.  PROMPT
defaults to \"GitHub repo (user/repo): \".  HIST defaults to
'magithub-repos-history.  If REQUIRE-MATCH is non-nil and the
user enters no input, raises an error.

WARNING: This function currently doesn't work fully, since
GitHub's user search API only returns an apparently random subset
of users, and also has no way to search for users whose names
begin with certain characters."
  (setq hist (or hist 'magithub-repos-history))
  (let ((result (completing-read
                 (or prompt "GitHub repo (user/repo): ")
                 (magithub--lazy-completion-callback 'magithub--repo-completions)
                 predicate require-match initial-input hist def inherit-input-method)))
    (if (string= result "")
        (when require-match (error "No repository given"))
      (magithub-parse-repo result))))

(defun magithub--repo-completions (string)
  "Try completing the given GitHub user/repository pair.
STRING is the text already in the minibuffer, PREDICATE is a
predicate that the string must satisfy."
  (destructuring-bind (username . rest) (split-string string "/")
    (if (not rest) ;; Need to complete username before we start completing repo
        (mapcar (lambda (user) (concat (plist-get user :name) "/"))
                (magithub-user-search username))
      (if (not (string= (car rest) ""))
          (magithub--use-cache (concat username "/"))
        (mapcar (lambda (repo) (concat username "/" (plist-get repo :name)))
                (magithub-repos-for-user username))))))

(defun magithub-read-pull-request-recipients ()
  "Read a list of recipients for a GitHub pull request."
  (let ((collabs (magithub-repo-parent-collaborators))
        (network (magithub-repo-network)))
    (magithub--remove-if
     (lambda (s) (string= s ""))
     (completing-read-multiple
      "Send pull request to: "
      (mapcar (lambda (repo) (plist-get repo :owner)) (magithub-repo-network))
      nil nil (concat (mapconcat 'identity collabs crm-separator)
                      (if (= (length collabs) (length network)) "" crm-separator))))))

(defun magithub-read-untracked-fork ()
  "Read the name of a fork of this repo that we aren't yet tracking.
This will accept either a username or a username/repo pair,
and return (USERNAME . REPONAME)."
  (let ((fork
         (completing-read
          "Track fork (user or user/repo): "
          (magithub--lazy-completion-callback
           (lambda ()
             (mapcar (lambda (repo) (concat (plist-get repo :owner) "/"
                                       (plist-get repo :name)))
                     (magithub-untracked-forks)))
           'noarg)
          nil nil nil 'magithub-repos-history)))
    (cond
     ((string= fork "") (error "No fork given"))
     ((string-match "/" fork) (magithub-parse-repo fork))
     (t (cons fork (magithub-repo-name))))))


;;; Bindings

(define-prefix-command 'magithub-prefix 'magithub-map)
(define-key magithub-map (kbd "C") 'magithub-create-from-local)
(define-key magithub-map (kbd "c") 'magithub-clone)
(define-key magithub-map (kbd "f") 'magithub-fork-current)
(define-key magithub-map (kbd "p") 'magithub-pull-request)
(define-key magithub-map (kbd "t") 'magithub-track)
(define-key magithub-map (kbd "g") 'magithub-gist-repo)
(define-key magithub-map (kbd "S") 'magithub-toggle-ssh)
(define-key magithub-map (kbd "b") 'magithub-browse-item)
(define-key magit-mode-map (kbd "'") 'magithub-prefix)


;;; Requests

(defun magit-request-url (path)
  "Return the full GitHub URL for the resource PATH.

PATH can either be a string or a list of strings.
In the latter case, they're URL-escaped and joined with \"/\".

If `url-request-method' is GET, the returned URL will include
`url-request-data' as the query string."
  (let ((url
         (concat magithub-api-base
                 (if (stringp path) path (mapconcat 'url-hexify-string path "/"))
                 (if (string= url-request-method "GET")
                     (concat "?" url-request-data)
                   ""))))
    (if magithub-use-ssl url
      (replace-regexp-in-string "^https" "http" url))))

(defmacro magithub-with-auth (&rest body)
  "Runs BODY with GitHub authorization info in `magithub-request-data'."
  (declare (indent 0))
  (let ((auth (gensym)))
    `(let* ((,auth (magithub-auth-info))
            (magithub-request-data (append (list
                                            (cons "login" (car ,auth))
                                            (cons "token" (cdr ,auth)))
                                           magithub-request-data)))
       ,@body)))

(defun magithub-handle-errors (status)
  "Handle any errors reported in a `url-retrieve' callback.
STATUS is the first argument passed to the callback.

If there is an error and GitHub returns an error message, that
message is printed with `error'.  Otherwise, the HTTP error is
signaled."
  (loop for (name val) on status by 'cddr
        do (when (eq name :error)
             (if (not magithub-handle-errors)
                 (signal (car val) (cdr val))
               (condition-case err
                   (let* ((json-object-type 'plist)
                          (data (json-read))
                          (err (plist-get data :error)))
                     (unless err (signal 'json-readtable-error nil))
                     (error "GitHub error: %s" err))
                 (json-readtable-error (signal (car val) (cdr val))))))))

(defun magithub-retrieve (path callback &optional cbargs)
  "Retrieve GitHub API PATH asynchronously.
Call CALLBACK with CBARGS when finished.

PATH can either be a string or a list of strings.
In the latter case, they're URL-escaped and joined with \"/\".

Like `url-retrieve', except for the following:
* PATH is an API resource path, not a full URL.
* GitHub authorization is automatically enabled.
* `magithub-request-data' is used instead of `url-request-data'.
* CALLBACK is passed a decoded JSON object (as a plist) rather
  than a list of statuses.  Basic error handling is done by `magithub-retrieve'.

If `magithub-parse-response' is nil, CALLBACK is just passed nil
rather than the JSON response object."
  (magithub-with-auth
    (let ((url-request-data (magithub-make-query-string magithub-request-data)))
      (lexical-let ((callback callback) (magithub-parse-response magithub-parse-response))
        (url-retrieve (magit-request-url path)
                      (lambda (status &rest cbargs)
                        (when magithub-parse-response
                          (search-forward "\n\n" nil t)) ; Move past headers
                        (magithub-handle-errors status)
                        (apply callback
                               (if (not magithub-parse-response)
                                   (current-buffer)
                                 (let* ((json-object-type 'plist)
                                        (obj (json-read)))
                                   (kill-buffer)
                                   obj))
                               cbargs))
                      cbargs)))))

(defun magithub-retrieve-synchronously (path)
  "Retrieve GitHub API PATH synchronously.

PATH can either be a string or a list of strings.
In the latter case, they're URL-escaped and joined with \"/\".

Like `url-retrieve-synchronously', except for the following:
* PATH is an API resource path, not a full URL.
* GitHub authorization is automatically enabled.
* `magithub-request-data' is used instead of `url-request-data'.
* Return a decoded JSON object (as a plist) rather than a buffer
  containing the response unless `magithub-parse-response' is nil."
  (magithub-with-auth
    (let ((url-request-data (magithub-make-query-string magithub-request-data)))
      (with-current-buffer (url-retrieve-synchronously (magit-request-url path))
        (goto-char (point-min))
        (if (not magithub-parse-response) (current-buffer)
          (search-forward "\n\n" nil t) ; Move past headers
          (let* ((data (let ((json-object-type 'plist)) (json-read)))
                 (err (plist-get data :error)))
            (when err (error "GitHub error: %s" err))
            (kill-buffer)
            data))))))


;;; Configuration
;; This API was taken from gist.el (http://github.com/defunkt/gist.el),
;; and renamed to avoid conflict.  The code also uses Magit rather
;; than relying on the Git executable directly.

(defun magithub-config (key)
  "Returns a GitHub specific value from the global Git config."
  (magit-git-string "config" "--global" (concat "github." key)))

(defun magithub-set-config (key value)
  "Sets a GitHub specific value to the global Git config."
  (magit-git-string "config" "--global" (concat "github." key) value))

(defun magithub-auth-info ()
  "Returns the user's GitHub authorization information.
Searches for a GitHub username and token in the global git config,
and returns (USERNAME . TOKEN). If nothing is found, prompts
for the info then sets it to the git config."
  (interactive)

  (let* ((user (magithub-config "user"))
         (token (magithub-config "token")))

    (when (not user)
      (setq user (read-string "GitHub username: "))
      (magithub-set-config "user" user))

    (when (not token)
      (setq token (read-string "GitHub API token: "))
      (magithub-set-config "token" token))

    (cons user token)))


;;; GitHub Information

(defun magithub-repos-for-user (user)
  "Return an array of all repos owned by USER.
The repos are decoded JSON objects (plists)."
  (let ((url-request-method "GET"))
    (plist-get
     (magithub-retrieve-synchronously
      (list "repos" "show" user))
     :repositories)))

(defun magithub-user-search (user)
  "Run a GitHub user search for USER.
Return an array of all matching users.

WARNING: WARNING: This function currently doesn't work fully,
since GitHub's user search API only returns an apparently random
subset of users."
  (if (string= user "") []
    (let ((url-request-method "GET"))
      (plist-get
       (magithub-retrieve-synchronously
        (list "user" "search" string))
       :users))))

(defun magithub-repo-obj (&optional username repo)
  "Return an object representing the repo USERNAME/REPO.
Defaults to the current repo.

The returned object is a decoded JSON object (plist)."
  (setq username (or username (magithub-repo-owner)))
  (setq repo (or repo (magithub-repo-name)))
  (remhash (cons username repo) magithub--repo-obj-cache)
  (magithub-cached-repo-obj username repo))

(defun magithub-cached-repo-obj (&optional username repo)
  "Return a (possibly cached) object representing the repo USERNAME/REPO.
Defaults to the current repo.

The returned object is a decoded JSON object (plist).

This differs from `magithub-repo-obj' in that it returns a cached
copy of the repo object if one exists.  This is useful for
properties such as :parent and :fork that are highly unlikely to
change."
  (setq username (or username (magithub-repo-owner)))
  (setq repo (or repo (magithub-repo-name)))
  (let ((cached (gethash (cons username repo) magithub--repo-obj-cache)))
    (or cached
        (let* ((url-request-method "GET")
               (obj (plist-get
                     (magithub-retrieve-synchronously
                      (list "repos" "show" username repo))
                     :repository)))
          (puthash (cons username repo) obj magithub--repo-obj-cache)
          obj))))

(defun magithub-repo-collaborators (&optional username repo)
  "Return an array of names of collaborators on USERNAME/REPO.
Defaults to the current repo."
  (setq username (or username (magithub-repo-owner)))
  (setq repo (or repo (magithub-repo-name)))
  (let ((url-request-method "GET"))
    (plist-get
     (magithub-retrieve-synchronously
      (list "repos" "show" username repo "collaborators"))
     :collaborators)))

(defun magithub-repo-network (&optional username repo)
  "Return an array of forks and/or parents of USERNAME/REPO.
Defaults to the current repo.

Each fork is a decoded JSON object (plist)."
  (setq username (or username (magithub-repo-owner)))
  (setq repo (or repo (magithub-repo-name)))
  (let ((url-request-method "GET"))
    (plist-get
     (magithub-retrieve-synchronously
      (list "repos" "show" username repo "network"))
     :network)))

(defun magithub-repo-parent-collaborators (&optional username repo)
  "Return an array of names of collaborators on the parent of USERNAME/REPO.
These are the default recipients of a pull request for this repo.
Defaults to the current repo.

If this repo has no parents, return the collaborators for it instead."
  (let ((parent (plist-get (magithub-cached-repo-obj username repo) :parent)))
    (if (not parent) (magithub-repo-collaborators username repo)
      (destructuring-bind (parent-owner . parent-repo) (magithub-parse-repo parent)
        (magithub-repo-collaborators parent-owner parent-repo)))))

(defun magithub-untracked-forks ()
  "Return a list of forks of this repo that aren't being tracked as remotes.
Returned repos are decoded JSON objects (plists)."
  (lexical-let ((remotes (magit-git-lines "remote")))
    (delq "origin" remotes)
    (push (magithub-repo-owner) remotes)
    (magithub--remove-if
     (lambda (repo) (member-ignore-case (plist-get repo :owner) remotes))
     (magithub-repo-network))))


;;; Local Repo Information

(defun magithub-repo-info ()
  "Return information about this GitHub repo.
This is of the form given by `magithub-remote-info'.

Error out if this isn't a GitHub repo."
  (or (magithub-remote-info "origin")
      (error "Not in a GitHub repo")))

(defun magithub-repo-owner ()
  "Return the name of the owner of this GitHub repo.

Error out if this isn't a GitHub repo."
  (car (magithub-repo-info)))

(defun magithub-repo-name ()
  "Return the name of this GitHub repo.

Error out if this isn't a GitHub repo."
  (cadr (magithub-repo-info)))

(defun magithub-repo-ssh-p ()
  "Return non-nil if this GitHub repo is checked out via SSH.

Error out if this isn't a GitHub repo."
  (caddr (magithub-repo-info)))


;;; Diff Information

(defun magithub-section-index (section)
  "Return the index of SECTION as a child of its parent section."
  (magithub--position section (magit-section-children (magit-section-parent section))))

(defun magithub-hunk-lines ()
  "Return the two line numbers for the current line (which should be in a hunk).
The first number is the line number in the original file, the
second is the line number in the new file.  They're returned
as (L1 L2).  If either doesn't exist, it will be nil.

If something goes wrong (e.g. we're not in a hunk or it's in an
unknown format), return nil."
  (block nil
    (let ((point (point)))
      (save-excursion
        (beginning-of-line)
        (when (looking-at "@@") ;; Annotations don't have line numbers,
          (forward-line)        ;; so we'll approximate with the next line.
          (setq point (point)))
        (goto-char (magit-section-beginning (magit-current-section)))
        (unless (looking-at "@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)") (return))
        (let ((l (- (string-to-number (match-string 1)) 1))
              (r (- (string-to-number (match-string 2)) 1)))
          (forward-line)
          (while (<= (point) point)
            (unless (looking-at "\\+") (incf l))
            (unless (looking-at "-") (incf r))
            (forward-line))
          (forward-line -1)
          (list (unless (looking-at "\\+") l) (unless (looking-at "-") r)))))))


;;; Network

(defun magithub-track (username &optional repo fetch)
  "Track USERNAME/REPO as a remote.
If FETCH is non-nil, fetch that remote.

Interactively, prompts for the username and repo.  With a prefix
arg, fetches the remote."
  (interactive
   (destructuring-bind (username . repo) (magithub-read-untracked-fork)
     (list username repo current-prefix-arg)))
  (magit-run-git "remote" "add" username (magithub-repo-url username repo))
  (when fetch (magit-run-git-async "remote" "update" username))
  (message "Tracking %s/%s%s" username repo
           (if fetch ", fetching..." "")))


;;; Browsing

(defun magithub-browse (&rest path-and-anchor)
  "Load http://github.com/PATH#ANCHOR in a web browser and add it to the kill ring.
Any nil elements of PATH are ignored.

\n(fn &rest PATH [:anchor ANCHOR])"
  (destructuring-bind (path anchor)
      (loop for el on path-and-anchor
            if (car el)
              unless (eq (car el) :anchor) collect (car el) into path
              else return (list path (cadr el))
            finally return (list path nil))
    (let ((url (concat "http://github.com/" (mapconcat 'identity path "/"))))
      (when anchor (setq url (concat url "#" anchor)))
      (kill-new url)
      (browse-url url))))

(defun magithub-browse-current (&rest path-and-anchor)
  "Load http://github.com/USER/REPO/PATH#ANCHOR in a web browser.
With ANCHOR, loads the URL with that anchor.

USER is `magithub-repo-owner' and REPO is `magithub-repo-name'.

\n(fn &rest PATH [:anchor ANCHOR])"
  (apply 'magithub-browse (magithub-repo-owner) (magithub-repo-name) path-and-anchor))

(defun magithub-browse-repo ()
  "Show the GitHub webpage for the current branch of this repository."
  ;; Don't use name-rev-for-remote here because we want it to work
  ;; even if the branches are out-of-sync.
  (magithub-browse-current "tree" (magit-name-rev "HEAD")))

(defun magithub-browse-commit (commit &optional anchor)
  "Show the GitHub webpage for COMMIT.
COMMIT should be the SHA of a commit.

If ANCHOR is given, it's used as the anchor in the URL."
  (let ((info (magithub-remote-info-for-commit commit)))
    (if info (magithub-browse (car info) (cadr info) "commit" commit :anchor anchor)
      (error "Commit %s hasn't been pushed" (substring commit 0 8)))))

(defun magithub-browse-commit-diff (diff-section)
  "Show the GitHub webpage for the diff displayed in DIFF-SECTION.
This must be a diff for `magit-currently-shown-commit'."
  (magithub-browse-commit
   magit-currently-shown-commit
   (format "diff-%d" (magithub-section-index diff-section))))

(defun magithub-browse-commit-hunk-at-point ()
  "Show the GitHub webpage for the hunk at point.
This must be a hunk for `magit-currently-shown-commit'."
  (destructuring-bind (l r) (magithub-hunk-lines)
    (magithub-browse-commit
     magit-currently-shown-commit
     (format "L%d%s" (magithub-section-index (magit-section-parent
                                              (magit-current-section)))
             (if l (format "L%d" l) (format "R%d" r))))))

(defun magithub-name-ref-for-compare (ref remote)
  "Return a human-readable name for REF that's valid in the compare view for REMOTE.
This is like `magithub-name-rev-for-remote', but takes into
account comparing across repos.

To avoid making an HTTP request, this method assumes that if REV
is in a remote, that repo is a GitHub fork."
  (let ((remotes (magithub-remotes-containing-ref ref)))
    ;; If remotes is empty, we let magithub-name-rev-for-remote's
    ;; error-handling deal with it.
    (if (or (member remote remotes) (null remotes))
        (magithub-name-rev-for-remote ref remote)
      (let ((remote-for-ref (car remotes)))
        (concat remote-for-ref ":"
                (magithub-name-rev-for-remote ref remote-for-ref))))))

(defun magithub-browse-compare (from to &optional anchor)
  "Show the GitHub webpage comparing refs FROM and TO.

If ANCHOR is given, it's used as the anchor in the URL."
  (magithub-browse-current
   "compare" (format "%s...%s"
                     (magithub-name-ref-for-compare from "origin")
                     (magithub-name-ref-for-compare to "origin"))
   :anchor anchor))

(defun magithub-browse-diffbuff (&optional anchor)
  "Show the GitHub webpage comparing refs corresponding to the current diff buffer.

If ANCHOR is given, it's used as the anchor in the URL."
  (when (and (listp magit-current-range) (null (cdr magit-current-range)))
    (setq magit-current-range (car magit-current-range)))
  (if (stringp magit-current-range)
      (progn
        (unless (magit-everything-clean-p)
          (error "Diff includes dirty working directory"))
        (magithub-browse-compare magit-current-range
                                 (magithub-name-rev-for-remote "HEAD" "origin")
                                 anchor))
    (magithub-browse-compare (car magit-current-range) (cdr magit-current-range) anchor)))

(defun magithub-browse-diff (section)
  "Show the GitHub webpage for the diff displayed in DIFF-SECTION.
This must be a diff from a *magit-diff* buffer."
  (magithub-browse-diffbuff (format "diff-%d" (magithub-section-index diff-section))))

(defun magithub-browse-hunk-at-point ()
  "Show the GitHub webpage for the hunk at point.
This must be a hunk from a *magit-diff* buffer."
  (destructuring-bind (l r) (magithub-hunk-lines)
    (magithub-browse-diffbuff
     (format "L%d%s" (magithub-section-index (magit-section-parent
                                              (magit-current-section)))
             (if l (format "L%d" l) (format "R%d" r))))))

(defun magithub-browse-blob (path &optional anchor)
  "Show the GitHub webpage for the blob at PATH.

If ANCHOR is given, it's used as the anchor in the URL."
  (magithub-browse-current "blob" (magithub-name-rev-for-remote "HEAD" "origin")
                           path :anchor anchor))

(defun magithub-browse-item ()
  "Load a GitHub webpage describing the item at point.
The URL of the webpage is added to the kill ring."
  (interactive)
  (or
   (magit-section-action (item info "browse")
     ((commit) (magithub-browse-commit info))
     ((diff)
      (case magit-submode
        (commit (magithub-browse-commit-diff (magit-current-section)))
        (diff (magithub-browse-diff (magit-current-section)))))
     ((hunk)
      (case magit-submode
        (commit (magithub-browse-commit-hunk-at-point))
        (diff (magithub-browse-hunk-at-point))))
     (t
      (case magit-submode
        (commit (magithub-browse-commit magit-currently-shown-commit))
        (diff (magithub-browse-diffbuff)))))
   (magithub-browse-repo)))

(defun magithub-browse-file ()
  "Show the GitHub webpage for the current file.
The URL for the webpage is added to the kill ring.  This only
works within `magithub-minor-mode'.

In Transient Mark mode, if the mark is active, highlight the
contents of the region."
  (interactive)
  (let ((path (magithub-repo-relative-path))
        (start (line-number-at-pos (region-beginning)))
        (end (line-number-at-pos (region-end))))
    (when (eq (char-before (region-end)) ?\n) (decf end))
    (with-current-buffer magithub-status-buffer
      (magithub-browse-blob
       path (when (and transient-mark-mode mark-active)
              (if (eq start end) (format "L%d" start)
                (format "L%d-%d" start end)))))))


;;; Creating Repos

(defun magithub-gist-repo (&optional private)
  "Upload the current repo as a Gist.
If PRIVATE is non-nil or with a prefix arg, the Gist is private.

Copies the URL of the Gist into the kill ring.  If
`magithub-view-gist' is non-nil (the default), opens the gist in
the browser with `browse-url'."
  (interactive "P")
  (let ((url-max-redirections 0)
        (url-request-method "POST")
        (magithub-api-base magithub-gist-url)
        (magithub-request-data
         `(,@(if private '(("private" . "1")))
           ("file_ext[gistfile1]" . ".dummy")
           ("file_name[gistfile1]" . "dummy")
           ("file_contents[gistfile1]" .
            "Dummy Gist created by Magithub. To be replaced with a real repo.")))
        magithub-parse-response)
    (let (url)
      (with-current-buffer (magithub-retrieve-synchronously "gists")
        (goto-char (point-min))
        (re-search-forward "^Location: \\(.*\\)$")
        (setq url (match-string 1))
        (kill-buffer))
      (kill-new url)
      (let ((ssh-url (replace-regexp-in-string
                      "^http://gist\\.github\\.com/"
                      "git@gist.github.com:" url)))
        (magit-run-git "remote" "add" "origin" ssh-url)
        (magit-set "origin" "branch" "master" "remote")
        (magit-set "refs/heads/master" "branch" "master" "merge")
        (magit-run-git-async "push" "-v" "-f" "origin" "master")
        (when magithub-view-gist (browse-url url))
        (message "Gist created: %s" url)))))

(defun magithub-create-from-local (name &optional description homepage private)
  "Create a new GitHub repository for the current Git repository.
NAME is the name of the GitHub repository, DESCRIPTION describes
the repository, URL is the location of the homepage.  If PRIVATE
is non-nil, a private repo is created.

When called interactively, prompts for NAME, DESCRIPTION, and
HOMEPAGE.  NAME defaults to the name of the current Git
directory.  By default, creates a public repo; with a prefix arg,
creates a private repo."
  (interactive
   (list (read-string "Repository name: "
                      (file-name-nondirectory
                       (directory-file-name
                        (expand-file-name
                         (magit-get-top-dir default-directory)))))
         (read-string "Description: ")
         (read-string "Homepage: ")
         current-prefix-arg))

  (let ((url-request-method "POST")
        (magithub-request-data `(("name" . ,name)
                                 ("description" . ,description)
                                 ("homepage" . ,homepage)
                                 ("private" . ,(if private "0" "1")))))
    (magithub-retrieve "repos/create"
                       (lambda (data name)
                         (magit-git-string
                          "remote" "add" "origin"
                          (magithub-repo-url (magithub-config "user") name 'ssh))
                         (magit-set "origin" "branch" "master" "remote")
                         (magit-set "refs/heads/master" "branch" "master" "merge")
                         (magit-run-git-async "push" "-v" "origin" "master")
                         (message "GitHub repository created: %s"
                                  (plist-get (plist-get data :repository) :url)))
                       (list name))))

;;;###autoload
(defun magithub-clone (username repo dir &optional sshp)
  "Clone GitHub repo USERNAME/REPO into directory DIR.
If SSHP is non-nil, clone it using the SSH URL.  Once the repo is
cloned, switch to a `magit-status' buffer for it.

Interactively, prompts for the repo name and directory.  With a
prefix arg, clone using SSH."
  (interactive
   (destructuring-bind (username . repo) (magithub-read-repo "Clone repo (user/repo): ")
     (list username repo (read-directory-name "Parent directory: ") current-prefix-arg)))
  ;; The trailing slash is necessary for Magit to be able to figure out
  ;; that this is actually a directory, not a file
  (let ((dir (concat (directory-file-name (expand-file-name dir)) "/" repo "/")))
    (magit-run-git "clone" (magithub-repo-url username repo sshp) dir)
    (magit-status dir)))


;;; Message Mode

(defvar magithub-message-mode-hook nil "Hook run by `magithub-message-mode'.")

(defvar magithub-message-confirm-cancellation magit-log-edit-confirm-cancellation
  "If non-nil, confirm when cancelling the editing of a `magithub-message-mode' buffer.")

(defconst magithub-message-buffer-name "*magithub-edit-message*"
  "Buffer name for composing messages.")

(defconst magithub-message-header-end "-- End of Magithub header --\n")

(defvar magithub-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'magithub-message-send)
    (define-key map (kbd "C-c C-k") 'magithub-message-cancel)
    (define-key map (kbd "C-c C-]") 'magithub-message-cancel)
    map)
  "The keymap for `magithub-message-mode'.")

(defvar magithub-pre-message-window-configuration nil)

(macrolet
    ((define-it (parent-mode)
       `(define-derived-mode magithub-message-mode ,parent-mode "Magithub Message Edit"
          "A mode for editing pull requests and other GitHub messages."
          (run-mode-hooks 'magithub-message-mode-hook))))
  (if (featurep 'markdown-mode) (define-it markdown-mode)
    (define-it text-mode)))

(defmacro with-magithub-message-mode (&rest body)
  "Runs BODY with Magit's log-edit functions usable with Magithub's message mode."
  (declare (indent 0))
  `(let ((magit-log-edit-buffer-name magithub-message-buffer-name)
         (magit-log-header-end magithub-message-header-end)
         (magit-log-edit-confirm-cancellation
          magithub-message-confirm-cancellation)
         (magit-pre-log-edit-window-configuration
          magithub-pre-message-window-configuration))
     (unwind-protect (progn ,@body)
       (setq magithub-pre-message-window-configuration
             magit-pre-log-edit-window-configuration))))

(defun magithub-pop-to-message (operation)
  "Open up a `magithub-message-mode' buffer and switch to it.
OPERATION is the name of what will happen when C-c C-c is used,
printed as a message when the buffer is opened."
  (let ((dir default-directory)
	(buf (get-buffer-create magithub-message-buffer-name)))
    (setq magithub-pre-message-window-configuration
	  (current-window-configuration))
    (pop-to-buffer buf)
    (setq default-directory dir)
    (magithub-message-mode)
    (message "Type C-c C-c to %s (C-c C-k to cancel)." operation)))

(defun magithub-message-send ()
  "Finish writing the message and send it."
  (interactive)
  (let ((recipients (with-magithub-message-mode
                      (magit-log-edit-get-field 'recipients))))
    (with-magithub-message-mode (magit-log-edit-set-fields nil))
    (magithub-send-pull-request
     (buffer-string) (split-string recipients crm-separator))
    (let (magithub-message-confirm-cancellation)
      (magithub-message-cancel))))

(defun magithub-message-cancel ()
  "Abort and erase message being composed."
  (interactive)
  (with-magithub-message-mode (magit-log-edit-cancel-log-message)))


;;; Forking Repos

(defun magithub-fork-current ()
  "Fork the current repository in place."
  (interactive)
  (destructuring-bind (owner repo _) (magithub-repo-info)
    (let ((url-request-method "POST"))
      (magithub-retrieve (list "repos" "fork" owner repo)
                         (lambda (obj repo buffer)
                           (with-current-buffer buffer
                             (magit-with-refresh
                               (magit-set (magithub-repo-url
                                           (car (magithub-auth-info))
                                           repo 'ssh)
                                          "remote" "origin" "url")))
                           (message "Forked %s/%s" owner repo))
                         (list repo (current-buffer))))))

(defun magithub-send-pull-request (text recipients)
  "Send a pull request with text TEXT to RECIPIENTS.
RECIPIENTS should be a list of usernames."
  (let ((url-request-method "POST")
        (magithub-request-data (cons (cons "message[body]" text)
                                     (mapcar (lambda (recipient)
                                               (cons "message[to][]" recipient))
                                             recipients)))
        (magithub-api-base magithub-github-url)
        (url-max-redirections 0) ;; GitHub will try to redirect, but we don't care
        magithub-parse-response)
    (magithub-retrieve (list (magithub-repo-owner) (magithub-repo-name)
                             "pull_request" (magithub-name-rev-for-remote "HEAD" "origin"))
                       (lambda (_)
                         (kill-buffer)
                         (message "Your pull request was sent.")))))

(defun magithub-pull-request (recipients)
  "Compose a pull request and send it to RECIPIENTS.
RECIPIENTS should be a list of usernames.

Interactively, reads RECIPIENTS via `magithub-read-pull-request-recipients'.
For non-interactive pull requests, see `magithub-send-pull-request'."
  (interactive (list (magithub-read-pull-request-recipients)))
  (with-magithub-message-mode
    (magit-log-edit-set-field
     'recipients (mapconcat 'identity recipients crm-separator)))
  (magithub-pop-to-message "send pull request"))

(defun magithub-toggle-ssh (&optional arg)
  "Toggle whether the current repo is checked out via SSH.
With ARG, use SSH if and only if ARG is positive."
  (interactive "P")
  (if (null arg) (setq arg (if (magithub-repo-ssh-p) -1 1))
    (setq arg (prefix-numeric-value arg)))
  (magit-set (magithub-repo-url (magithub-repo-owner) (magithub-repo-name) (> arg 0))
             "remote" "origin" "url")
  (magit-refresh-status))


;;; Minor Mode

(defvar magithub-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ' b") 'magithub-browse-file)
    map))

(defvar magithub-status-buffer nil
  "The Magit status buffer for the current buffer's Git repository.")
(make-variable-buffer-local 'magithub-status-buffer)

(define-minor-mode magithub-minor-mode
  "Minor mode for files in a GitHub repository.

\\{magithub-minor-mode-map}"
  :keymap magithub-minor-mode-map)

(defun magithub-try-enabling-minor-mode ()
  "Activate `magithub-minor-mode' in this buffer if it's a Git buffer.
This means it's visiting a Git-controlled file and a Magit buffer
is open for that file's repo."
  (block nil
    (if magithub-minor-mode (return))
    (unless buffer-file-name (return))
    ;; Try to find the Magit status buffer for this file.
    ;; If it doesn't exist, don't activate magithub-minor-mode.
    (let* ((topdir (magit-get-top-dir (file-name-directory buffer-file-name)))
           (status (magit-find-buffer 'status topdir)))
      (unless status (return))
      (magithub-minor-mode 1)
      (setq magithub-status-buffer status))))

(defun magithub-try-disabling-minor-mode ()
  "Deactivate `magithub-minor-mode' in this buffer if it's no longer a Git buffer.
See `magithub-try-enabling-minor-mode'."
  (when (and magithub-minor-mode (buffer-live-p magithub-status-buffer))
    (magithub-minor-mode -1)))

(defun magithub-try-enabling-minor-mode-everywhere ()
  "Run `magithub-try-enabling-minor-mode' on all buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf (magithub-try-enabling-minor-mode))))

(defun magithub-try-disabling-minor-mode-everywhere ()
  "Run `magithub-try-disabling-minor-mode' on all buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf (magithub-try-disabling-minor-mode))))

(magithub-try-enabling-minor-mode-everywhere)


;;; Hooks into Magit and Emacs

(defun magithub-magit-init-hook ()
  (when (y-or-n-p "Create GitHub repo? ")
    (call-interactively 'magithub-create-from-local)))
(add-hook 'magit-init-hook 'magithub-magit-init-hook)

(defun magithub-magit-mode-hook ()
  "Enable `magithub-minor-mode' in buffers that are now in a Magit repo.
If the new `magit-mode' buffer is a status buffer, try enabling
`magithub-minor-mode' in all buffers."
  (when (derived-mode-p 'magit-status-mode)
    (magithub-try-enabling-minor-mode-everywhere)))
(add-hook 'magit-mode-hook 'magithub-magit-mode-hook)

(defun magithub-kill-buffer-hook ()
  "Clean up `magithub-minor-mode'.
That is, if the buffer being killed is a Magit status buffer,
deactivate `magithub-minor-mode' on all buffers in its repository."
  (when (and (eq major-mode 'magit-mode) (derived-mode-p 'magit-status-mode))
    (magithub-try-disabling-minor-mode-everywhere)))
(add-hook 'kill-buffer-hook 'magithub-kill-buffer-hook)

(add-hook 'find-file-hook 'magithub-try-enabling-minor-mode)


(provide 'magithub)

;;;###autoload
(eval-after-load 'magit
  '(unless (featurep 'magithub)
     (require 'magithub)))

;;; magithub.el ends here

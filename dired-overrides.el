;;; dired-overrides.el --- Dired misc utils -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/dired-overrides
;; Version: 0.1.0
;; Keywords: files
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Dired misc utils

;;; Code:

(declare-function dired-mark-pop-up "dired")
(declare-function dired-mark-prompt "dired")

(defcustom dired-overrides-multi-source-restore-last-input t
  "Whether to insert last typed source input."
  :group 'dired-overrides
  :type 'boolean)

(defvar dired-overrides-multi-source--sources-list nil
  "Normalized sources.")

(defvar dired-overrides-multi-source-last-input nil
  "Last typed input in minibuffer.")

(defvar dired-overrides-multi-source--current-index nil
  "Index of active source.")

(defvar dired-overrides-multi-source--sources-list nil
  "List of sources for multi-source Dired overrides.")

(defvar dired-overrides-multi-source-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C->") #'dired-overrides-multi-source-select-next)
    (define-key map (kbd "C-<") #'dired-overrides-multi-source-select-prev)
    (define-key map (kbd "C-.") #'dired-overrides-multi-source-read-source)
    map)
  "Keymap to use in minibuffer.")


(defun dired-overrides--format-plural (count singular-str)
  "Format COUNT with SINGULAR-STR, adding \"s\" for plural.

Argument COUNT is an integer representing the quantity to consider for
pluralization.

Argument SINGULAR-STR is a string representing the singular form of the word to
be potentially pluralized."
  (concat (format "%d " count)
          (concat singular-str
                  (if (= count 1) "" "s"))))

(defun dired-overrides-format-time-diff (time)
  "Format a human-readable string representing TIME difference.

Argument TIME is a time value representing the number of seconds since the epoch
\=(January 1, 1970, 00:00:00 GMT)."
  (let ((diff-secs
         (- (float-time (encode-time (append (list 0)
                                             (cdr (decode-time
                                                   (current-time))))))
            (float-time
             (encode-time (append (list 0)
                                  (cdr (decode-time time))))))))
    (if (zerop (round diff-secs))
        "Now"
      (let* ((past (> diff-secs 0))
             (diff-secs-int (if past diff-secs (- diff-secs)))
             (suffix (if past "ago" "from now"))
             (minutes-secs 60)
             (hours-secs (* 60 minutes-secs))
             (day-secs (* 24 hours-secs))
             (month-secs (* 30 day-secs))
             (year-secs (* 365 day-secs))
             (res
              (cond ((< diff-secs-int minutes-secs)
                     (dired-overrides--format-plural (truncate diff-secs-int)
                                                     "second"))
                    ((< diff-secs-int hours-secs)
                     (dired-overrides--format-plural (truncate (/ diff-secs-int
                                                                  minutes-secs))
                                                     "minute"))
                    ((< diff-secs-int day-secs)
                     (dired-overrides--format-plural (truncate
                                                      (/ diff-secs-int hours-secs))
                                                     "hour"))
                    ((< diff-secs-int month-secs)
                     (dired-overrides--format-plural (truncate (/ diff-secs-int
                                                                  day-secs))
                                                     "day"))
                    ((< diff-secs-int year-secs)
                     (dired-overrides--format-plural (truncate
                                                      (/ diff-secs-int month-secs))
                                                     "month"))
                    (t
                     (let* ((months (truncate (/ diff-secs-int month-secs)))
                            (years (/ months 12))
                            (remaining-months (% months 12)))
                       (string-join
                        (delq nil
                              (list
                               (when (> years 0)
                                 (dired-overrides--format-plural years "year"))
                               (when (> remaining-months 0)
                                 (dired-overrides--format-plural
                                  remaining-months "month"))))
                        " "))))))
        (concat res " " suffix)))))

(defun dired-overrides-multi-source-select-next ()
  "Throw to the catch tag ='next with 1."
  (interactive)
  (throw 'next
         1))

(defun dired-overrides-multi-source-select-prev ()
  "Navigate to the previous item in a multi-source selection."
  (interactive)
  (throw 'next
         -1))


(defun dired-overrides-multi-source-read-source ()
  "Prompt user to select a source and calculate index offset."
  (interactive)
  (let* ((source-label
          (completing-read "Source: " (nth 1
                                           dired-overrides-multi-source--sources-list)))
         (pos (seq-position (nth 1 dired-overrides-multi-source--sources-list)
                            source-label)))
    (throw 'next (- pos dired-overrides-multi-source--current-index))))

(defun dired-overrides-multi-source-read (sources)
  "Combine minibuffer SOURCES into a command with several alternatives.

Every alternative should be a function that reads data from minibuffer.

By default the first source is called and user can switch between
alternatives dynamically with commands:

 `dired-overrides-multi-source-select-next' (bound
to \\<dired-overrides-multi-source-minibuffer-map>\
`\\[dired-overrides-multi-source-select-next]') - select next alternative.
 `dired-overrides-multi-source-select-prev' (bound
to \\<dired-overrides-multi-source-minibuffer-map>\
`\\[dired-overrides-multi-source-select-prev]') - select previus alternative.
 `dired-overrides-multi-source-read-source' (bound to
\\<dired-overrides-multi-source-minibuffer-map>\
`\\[dired-overrides-multi-source-read-source]') - select from completions list.

Allowed forms for SOURCES are
 - a list of functions
 - a plist of backend's name and corresponding function,
-  an alist of backend's name, corresponding function and optionally extra
 arguments to pass."
  (setq dired-overrides-multi-source--sources-list
        (dired-overrides-multi-source-map-sources sources))
  (setq dired-overrides-multi-source--current-index 0)
  (setq dired-overrides-multi-source-last-input nil)
  (let ((curr)
        (fns (nth 0 dired-overrides-multi-source--sources-list))
        (args (nth 2 dired-overrides-multi-source--sources-list))
        (window-selection-change-functions nil))
    (while
        (numberp
         (setq curr
               (catch 'next
                 (minibuffer-with-setup-hook
                     (lambda ()
                       (use-local-map
                        (let ((map
                               (copy-keymap
                                dired-overrides-multi-source-minibuffer-map)))
                          (set-keymap-parent map (current-local-map))
                          map))
                       (when (minibuffer-window-active-p
                              (selected-window))
                         (when (and dired-overrides-multi-source-restore-last-input
                                    dired-overrides-multi-source-last-input
                                    (string-empty-p (minibuffer-contents-no-properties)))
                           (insert
                            dired-overrides-multi-source-last-input))
                         (add-hook
                          'post-command-hook
                          #'dired-overrides-multi-source-set-last-input
                          nil t)
                         (add-hook
                          'minibuffer-exit-hook
                          #'dired-overrides-multi-source-set-last-input
                          nil t)
                         (add-hook
                          'post-self-insert-hook
                          #'dired-overrides-multi-source-set-last-input
                          nil t)))
                   (let ((window-selection-change-functions nil))
                     (apply (nth dired-overrides-multi-source--current-index fns)
                            (nth dired-overrides-multi-source--current-index args)))))))
      (setq dired-overrides-multi-source--current-index
            (dired-overrides-multi-source-switcher curr
                                                   dired-overrides-multi-source--current-index
                                                   fns)))
    (setq dired-overrides-multi-source-last-input nil)
    (setq dired-overrides-multi-source--sources-list nil)
    curr))


(defun dired-overrides-multi-source-switcher (step current-index switch-list)
  "Increase or decrease CURRENT-INDEX depending on STEP value and SWITCH-LIST."
  (cond ((> step 0)
         (if (>= (+ step current-index)
                 (length switch-list))
             0
           (+ step current-index)))
        ((< step 0)
         (if (or (<= 0 (+ step current-index)))
             (+ step current-index)
           (1- (length switch-list))))
        (t current-index)))

(defun dired-overrides-multi-source-set-last-input ()
  "Save last typed input in mininubbfer."
  (when (minibufferp)
    (setq dired-overrides-multi-source-last-input
          (buffer-substring (minibuffer-prompt-end)
                            (point)))))

(defun dired-overrides-multi-source-map-sources (sources)
  "Normalize SOURCES to list of functions, labels and arguments."
  (let ((curr)
        (labels)
        (args)
        (fns))
    (while (setq curr (pop sources))
      (pcase curr
        ((pred stringp)
         (let ((fn (pop sources)))
           (push curr labels)
           (push fn fns)
           (push nil args)))
        ((pred functionp)
         (let ((label
                (if (symbolp curr)
                    (symbol-name curr)
                  "")))
           (push label labels)
           (push curr fns)
           (push nil args)))
        ((pred listp)
         (let* ((label (car curr))
                (rest (cdr curr))
                (fn (if (listp rest)
                        (car rest)
                      rest))
                (extra-args
                 (when (listp rest)
                   (cdr rest))))
           (push label labels)
           (push (if (or (functionp fn)
                         (symbolp fn))
                     fn
                   `(lambda () ,fn))
                 fns)
           (push extra-args args)))))
    (list (reverse fns)
          (reverse labels)
          (reverse args))))

(defun dired-overrides-get-active-buffers-dirs ()
  "Return list of uniq default directories from all buffers."
  (let* ((curr-buf (current-buffer))
         (live-buffers (seq-sort-by (lambda (it)
                                      (if (get-buffer-window it)
                                          (if (eq curr-buf it)
                                              1
                                            2)
                                        -1))
                                    #'>
                                    (buffer-list))))
    (delete-dups
     (delq nil (mapcar (lambda (buff)
                         (when-let ((dir (buffer-local-value
                                          'default-directory
                                          buff)))
                           (unless (or (file-remote-p dir)
                                       (not (file-accessible-directory-p
                                             dir)))
                             (expand-file-name dir))))
                       live-buffers)))))

(defun dired-overrides-get-active-buffers-files ()
  "Return list of uniq files from all buffers."
  (let* ((curr-buf (current-buffer))
         (live-buffers (seq-sort-by (lambda (it)
                                      (if (get-buffer-window it)
                                          (if (eq curr-buf it)
                                              1
                                            2)
                                        -1))
                                    #'>
                                    (buffer-list))))
    (delete-dups
     (delq nil (mapcar (lambda (buff)
                         (when-let ((file (buffer-local-value
                                           'buffer-file-name
                                           buff)))
                           (when (file-readable-p file)
                             (expand-file-name file))))
                       live-buffers)))))
(when-let ((project (ignore-errors (project-current))))
  (if (fboundp 'project-root)
      (project-root project)
    (with-no-warnings
      (car (project-roots project)))))


(defun dired-overrides-get-all-projects-files ()
  "Return a list of all unique project files from live buffers."
  (let* ((curr-buf (current-buffer))
         (live-buffers (seq-sort-by (lambda (it)
                                      (if (get-buffer-window it)
                                          (if (eq curr-buf it)
                                              1
                                            2)
                                        -1))
                                    #'>
                                    (buffer-list))))
    (delete-dups
     (delq nil (mapcan (lambda (buff)
                         (when-let ((project
                                     (ignore-errors
                                       (project-current nil
                                                        (buffer-local-value
                                                         'default-directory
                                                         buff)))))
                           (when (fboundp 'project-files)
                             (project-files project))))
                       live-buffers)))))


(defun dired-overrides-minibuffer-get-metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun dired-overrides-minibuffer-ivy-selected-cand ()
  "Return the currently selected item in Ivy."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get (ignore-errors (dired-overrides-minibuffer-get-metadata))
                              'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun dired-overrides-minibuffer-get-default-candidates ()
  "Return all current completion candidates from the minibuffer."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point)
                           (minibuffer-prompt-end)))))
           (last (last all)))
      (when last (setcdr last nil))
      (cons
       (completion-metadata-get (dired-overrides-minibuffer-get-metadata) 'category)
       all))))


(defun dired-overrides-get-minibuffer-get-default-completion ()
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target."
  (when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (dired-overrides-minibuffer-get-default-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(defvar dired-overrides-minibuffer-targets-finders
  '(dired-overrides-minibuffer-ivy-selected-cand
    dired-overrides-get-minibuffer-get-default-completion))

(defun dired-overrides-minibuffer-get-current-candidate ()
  "Return cons filename for current completion candidate."
  (let (target)
    (run-hook-wrapped
     'dired-overrides-minibuffer-targets-finders
     (lambda (fun)
       (when-let ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr-safe result))
                    (not (string-empty-p (cdr-safe result))))
           (setq target result)))
       (and target (minibufferp))))
    target))

(defun dired-overrides-minibuffer-exit-with-action (action)
  "Call ACTION with current candidate and exit minibuffer."
  (pcase-let ((`(,_category . ,current)
               (dired-overrides-minibuffer-get-current-candidate)))
    (progn (run-with-timer 0.1 nil action current)
           (abort-minibuffers))))

(defun dired-overrides-minibuffer-web-restore-completions-wind ()
  "Restore *Completions* window height."
  (when (eq this-command 'minibuffer-next-completion)
    (remove-hook 'post-command-hook
                 #'dired-overrides-minibuffer-web-restore-completions-wind)
    (when-let ((win (get-buffer-window "*Completions*" 0)))
      (fit-window-to-buffer win completions-max-height))))

(defun dired-overrides-minibuffer-action-no-exit (action)
  "Execute ACTION on current minibuffer candidate.

Argument ACTION is a function to be called with the current minibuffer
candidate."
  (pcase-let ((`(,_category . ,current)
               (dired-overrides-minibuffer-get-current-candidate)))
    (when-let ((win (get-buffer-window "*Completions*" 0)))
      (minimize-window win)
      (add-hook 'post-command-hook
                #'dired-overrides-minibuffer-web-restore-completions-wind))
    (with-minibuffer-selected-window
      (funcall action current))))

(defun dired-overrides-minibuffer-preview-file ()
  "Preview file from Dired minibuffer without closing it."
  (interactive)
  (dired-overrides-minibuffer-action-no-exit 'find-file))

(defvar dired-overrides-minibuffer-file-override-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o")
                #'dired-overrides-find-file-other-window)
    map))

(defvar dired-overrides-minibuffer-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j")
                #'dired-overrides-minibuffer-preview-file)
    map))

(defun dired-overrides-completing-read-with-keymap (prompt collection &optional keymap
                                                           predicate require-match
                                                           initial-input hist def
                                                           inherit-input-method)
  "Read COLLECTION in minibuffer with PROMPT and KEYMAP.
See `completing-read' for PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD."
  (let ((collection (if (stringp (car-safe collection))
                        (copy-tree collection)
                      collection)))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (minibufferp)
            (when keymap
              (let ((map (make-composed-keymap keymap
                                               (current-local-map))))
                (use-local-map map)))))
      (completing-read prompt
                       collection
                       predicate
                       require-match initial-input hist
                       def inherit-input-method))))

(defun dired-overrides--files-to-alist (files &optional proj-root)
  "Sort FILES by modification time and return as an associative list.

Argument FILES is a list of FILES to be processed by the function.

Optional argument PROJ-ROOT is a string representing the root directory of the
project, with no default value."
  (when proj-root (setq proj-root (file-name-directory
                                   (abbreviate-file-name proj-root))))
  (nreverse (seq-sort-by
             (pcase-lambda (`(,_k . ,v)) v)
             #'time-less-p
             (mapcar
              (lambda (file)
                (cons
                 (if proj-root
                     (substring-no-properties (abbreviate-file-name
                                               file)
                                              (length proj-root))
                   (abbreviate-file-name file))
                 (file-attribute-modification-time
                  (file-attributes
                   file))))
              files))))

(defun dired-overrides--completing-read-files (prompt files &rest args)
  "PROMPT for file selection with modification time annotations.

Argument PROMPT is a string displayed as the prompt in the minibuffer.

Argument FILES is a list of file names to be processed.

Remaining arguments ARGS are passed to
`dired-overrides-completing-read-with-keymap'."
  (let* ((alist (dired-overrides--files-to-alist
                 files))
         (len 120)
         (annotf (lambda (str)
                   (or
                    (when-let ((time (cdr (assoc str alist))))
                      (when (length> str len)
                        (setq len (1+ (length str))))
                      (concat
                       (propertize " " 'display
                                   (list 'space :align-to
                                         len))
                       (dired-overrides-format-time-diff time)))
                    "")))
         (category 'file)
         (cands (mapcar #'car alist)))
    (apply #'dired-overrides-completing-read-with-keymap
           prompt
           (lambda (str pred action)
             (if (eq action 'metadata)
                 `(metadata
                   (annotation-function . ,annotf)
                   (category . ,category))
               (complete-with-action action cands
                                     str pred)))
           (make-composed-keymap dired-overrides-minibuffer-file-map
                                 dired-overrides-minibuffer-file-override-map)
           args)))


(defun dired-overrides--get-parents-and-subdirs (dirs)
  "Collect existing, unique parent directories and subdirectories from DIRS.

Argument DIRS is a list of directory paths."
  (let ((dirs (seq-filter #'file-accessible-directory-p
                          (delete-dups
                           (mapcar
                            (lambda (d)
                              (file-name-as-directory
                               (if (file-name-absolute-p d)
                                   d
                                 (expand-file-name d default-directory))))
                            dirs)))))
    (seq-reduce
     (lambda (acc curr)
       (let ((parent (file-name-parent-directory curr))
             (children (directory-files curr t
                                        directory-files-no-dot-files-regexp)))
         (unless (or (not parent)
                     (member parent acc))
           (push parent acc)
           (setq children (nconc (directory-files
                                  parent t
                                  directory-files-no-dot-files-regexp)
                                 children)))
         (dolist (child children)
           (when (file-accessible-directory-p child)
             (let ((dir (file-name-as-directory child)))
               (unless (member dir acc)
                 (push dir acc))))))
       acc)
     dirs
     dirs)))

(defun dired-overrides-expand-pattern (curr pattern)
  "Recursively expand PATTERN for string CURR using regexps or functions.

Argument CURR is the current string to be matched or processed.

Argument PATTERN is the pattern to match against CURR, which can be a
function, a string, or a list of functions and strings."
  (pcase pattern
    ((pred functionp)
     (funcall pattern curr))
    ((pred stringp)
     (string-match-p pattern curr))
    (_ (seq-find
        (apply-partially #'dired-overrides-expand-pattern curr)
        pattern))))

(defun dired-overrides-find-in-dir (dir &optional pattern non-visit-pattern
                                        max-depth transform-fn include-dirs)
  "Return list of files that matches PATTERN in DIR at MAX-DEPTH.

Both PATTERN and NON-VISIT-PATTERN, if non nil, will be tested against the
directory to visit.

It should be either:
- regexp that will be tested against the current file name of the directory.
- function (will be called with one argument local directory name)
- list of patterns, that will be tested until first non nil result.

If PATTERN matches, it will be added to result, and not be visited.

If NON-VISIT-PATTERN matches, directory will not be visited.

If TRANSFORM-FN is non nil, it should be a function that will be called with one
argument - full directory name.

By default, the returned list excludes directories, but if
optional argument INCLUDE-DIRECTORIES is non-nil, they are
included."
  (let ((found-dirs nil)
        (queue (list (cons (expand-file-name (file-name-as-directory dir)) 1))))
    (unless max-depth (setq max-depth 1))
    (while queue
      (let* ((current (pop queue))
             (current-dir (car current))
             (current-depth (cdr current))
             (default-directory current-dir)
             (files (directory-files default-directory nil
                                     directory-files-no-dot-files-regexp t)))
        (when (<= current-depth max-depth)
          (while files
            (let* ((curr (pop files))
                   (full-name (expand-file-name curr))
                   (is-dir (file-directory-p full-name)))
              (cond ((and is-dir (not (file-accessible-directory-p full-name))))
                    ((and non-visit-pattern
                          (dired-overrides-expand-pattern curr non-visit-pattern)))
                    (t
                     (when (or (not pattern)
                               (dired-overrides-expand-pattern curr
                                                               pattern))
                       (unless (and is-dir (not include-dirs))
                         (push (if transform-fn (funcall transform-fn
                                                         full-name)
                                 full-name)
                               found-dirs)))
                     (when is-dir
                       (push (cons full-name (1+ current-depth)) queue)))))))))
    found-dirs))

;; (defun dired-overrides-find-in-dir (dir &optional pattern non-visit-pattern
;;                                         max-depth transform-fn current-depth)
;;   "Return list of files that matches PATTERN in DIR at MAX-DEPTH.

;; Both PATTERN and NON-VISIT-PATTERN, if non nil, will be tested against the
;; directory to visit.

;; It should be either:
;; - regexp that will be tested against the current file name of the directory.
;; - function (will be called with one argument local directory name)
;; - list of patterns, that will be tested until first non nil result.

;; If PATTERN matches, it will be added to result, and not be visited.

;; If NON-VISIT-PATTERN matches, directory will not be visited.

;; If TRANSFORM-FN is non nil, it should be a function that will be called with one
;; argument - full directory name.

;; CURRENT-DEPTH is used for recoursive purposes."
;;   (setq current-depth (or current-depth 1))
;;   (unless max-depth (setq max-depth 1))
;;   (when (>= max-depth current-depth)
;;     (let ((non-essential t))
;;       (let ((found-dirs))
;;         (let
;;             ((default-directory (expand-file-name (file-name-as-directory dir))))
;;           (dolist
;;               (curr
;;                (directory-files default-directory nil
;;                                 directory-files-no-dot-files-regexp t))
;;             (let ((full-name (expand-file-name curr))
;;                   (tramp-archive-enabled nil)
;;                   (is-dir))
;;               (setq is-dir (file-directory-p full-name))
;;               (cond ((and
;;                       is-dir
;;                       (not (file-accessible-directory-p full-name))))
;;                     ((and non-visit-pattern
;;                           (dired-overrides-expand-pattern curr non-visit-pattern)))
;;                     ((and pattern
;;                           (dired-overrides-expand-pattern curr pattern))
;;                      (setq found-dirs
;;                            (push (if transform-fn
;;                                      (funcall transform-fn full-name)
;;                                    full-name)
;;                                  found-dirs)))
;;                     (t
;;                      (unless pattern
;;                        (setq found-dirs
;;                              (push (if transform-fn
;;                                        (funcall transform-fn full-name)
;;                                      full-name)
;;                                    found-dirs)))
;;                      (when is-dir
;;                        (when-let
;;                            ((subdirs
;;                              (dired-overrides-find-in-dir full-name
;;                                                           pattern
;;                                                           non-visit-pattern
;;                                                           max-depth
;;                                                           transform-fn
;;                                                           (1+
;;                                                            current-depth))))
;;                          (setq found-dirs
;;                                (if found-dirs
;;                                    (nconc
;;                                     found-dirs
;;                                     subdirs)
;;                                  subdirs)))))))))
;;         found-dirs))))

(defun dired-overrides--guess-initial-dirs (&optional directories)
  "Get directories by filtering writable ones from active buffers and paths.

Optional argument DIRECTORIES is a list of directory paths to include."
  (seq-filter
   #'file-writable-p
   (seq-uniq (nconc
              (dired-overrides-get-active-buffers-dirs)
              (dired-overrides--get-parents-and-subdirs
               (delq nil
                     (append (list default-directory)
                             directories)))))))

(defun dired-overrides-read-active-buffers-directory ()
  "Read directory name, completing with default directories from all buffers."
  (interactive)
  (dired-overrides-completing-read-with-keymap
   "Directory: "
   (dired-overrides-get-active-buffers-dirs)
   (make-composed-keymap
    dired-overrides-minibuffer-file-override-map
    dired-overrides-minibuffer-file-map)))

(defun dired-overrides-read-file (prompt &optional initial-dir default)
  "PROMPT for a directory or filename, showing active buffer directories first.

Argument PROMPT is a string that is used to PROMPT the user for input.
Optional argument INITIAL-DIR is a string representing the initial directory.
If not provided, it defaults to nil.
Optional argument DEFAULT is a string representing the DEFAULT directory.
If not provided, it defaults to nil."
  (let
      ((choices (dired-overrides--guess-initial-dirs (list initial-dir default-directory))))
    (dired-overrides-multi-source-read `(("Active Buffers Directories"
                                          dired-overrides--completing-read-files
                                          ,prompt
                                          ,choices)
                                         ("Other directory" read-directory-name
                                          ,prompt
                                          ,initial-dir ,default)
                                         ("Other filename" read-file-name
                                          ,prompt ,initial-dir
                                          ,default)))))

(defun dired-overrides-get-xdg-dirs ()
  "Return a list of user directories from the XDG configuration file."
  (when
      (require 'xdg nil t)
    (when (and (fboundp 'xdg--user-dirs-parse-file)
               (fboundp 'xdg-config-home))
      (ignore-errors (mapcar #'cdr
                             (xdg--user-dirs-parse-file
                              (expand-file-name "user-dirs.dirs"
                                                (xdg-config-home))))))))

(defun dired-overrides-read-file-name ()
  "Complete a directory or filename, showing active buffer directories first.

Argument PROMPT is a string that is used to PROMPT the user for input.
Optional argument INITIAL-DIR is a string representing the initial directory.
If not provided, it defaults to nil.
Optional argument DEFAULT is a string representing the DEFAULT directory.
If not provided, it defaults to nil."
  (let* ((active-files (dired-overrides-get-active-buffers-files))
         (active-file-parents (delete-dups (mapcar #'file-name-parent-directory
                                                   active-files)))
         (files  (mapcan (lambda (dir)
                           (unless (file-equal-p dir "~/")
                             (directory-files-recursively
                              dir ".*" nil
                              (lambda (it)
                                (and (file-readable-p
                                      it)
                                     (not (string-match-p
                                           "/node_modules\\|.venv\\|env\\|venv\\|.env\\'"
                                           it)))))))
                         active-file-parents))
         (all-files
          (delete-dups
           (append active-files files
                   (dired-overrides-get-all-projects-files)))))
    (dired-overrides-multi-source-read `(("Active Files Directories"
                                          dired-overrides--completing-read-files
                                          ,"File name"
                                          ,all-files)
                                         ("Other directory" read-directory-name
                                          ,"Directory name")
                                         ("Other filename" read-file-name
                                          "File name")))))

(defun dired-overrides-mark-read-file-name (prompt dir op-symbol arg files
                                                   &optional default)
  "Read arguments for a marked-files command that wants a file name for operations.

Argument PROMPT is a string that will be formatted and displayed to the user.
Argument DIR is a string representing the directory in which the operation will
take place.
Argument OP-SYMBOL is a symbol that represents the operation to be performed.
ARG is the prefix arg and indicates whether the files came from marks (ARG=nil)
or a repeat factor (integerp ARG).
Argument FILES is a list of FILES on which the operation will be performed.
Optional argument DEFAULT is a value that will be used as the DEFAULT if no
other value is provided."
  (require 'dired)
  (dired-mark-pop-up
   nil op-symbol files
   #'dired-overrides-read-file
   (format prompt (dired-mark-prompt arg files)) dir default))

;;;###autoload
(define-minor-mode dired-overrides-mode
  "Enable or disable enhancements for operational functionality in Dired Mode.

Currently, this mode overrides the `dired-mark-read-file-name' function. When
activated, it suggests directories from all active buffers and offers additional
sources such as manual inputs for directory and file names."
  :lighter " dir-ov"
  :group 'dired
  :global t
  (advice-remove 'dired-mark-read-file-name
                 #'dired-overrides-mark-read-file-name)
  (advice-remove 'read-file-name
                 #'dired-overrides-read-file-name-with-keymap-fn)
  (when dired-overrides-mode
    (advice-add 'dired-mark-read-file-name :override
                #'dired-overrides-mark-read-file-name)
    (advice-add 'read-file-name :around
                #'dired-overrides-read-file-name-with-keymap-fn)))

(defvar dired-overrides-multiple-files-completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map
                (kbd "C-<return>")
                #'dired-overrides-throw-done)
    (define-key map
                (kbd "RET")
                #'dired-overrides-throw-visit-or-done)
    (define-key map (kbd "C-M-j")
                #'dired-overrides-throw-done)
    (define-key map (kbd "C-SPC")
                #'dired-overrides-throw-mark)
    map)
  "Keymap used in `dired-overrides-read-multiple-files'.")

(defun dired-overrides-throw-mark ()
  "Throw the symbol \\='mark to exit a catch block."
  (interactive)
  (throw 'action
         `(mark . ,(cdr (dired-overrides-minibuffer-get-current-candidate)))))

(defun dired-overrides-throw-visit-or-done ()
  "Throw the symbol \\='action to exit a catch block."
  (interactive)
  (let ((dir (cdr (dired-overrides-minibuffer-get-current-candidate))))
    (throw 'action
           (cons (if (file-directory-p dir)
                     'visit
                   'done)
                 dir))))

(defun dired-overrides-throw-done ()
  "Throw the symbol \\='done to exit a catch block."
  (interactive)
  (throw 'done t))

(defun dired-overrides--all-pass (&rest filters)
  "Create an unary predicate function from FILTERS.
Return t if every one of the provided predicates is satisfied by provided
argument."
  (lambda (item)
    (not (catch 'found
           (dolist (filter filters)
             (unless (funcall filter item)
               (throw 'found t)))))))

(defun dired-overrides-find-file-other-window ()
  "Open the selected file in another window."
  (interactive)
  (dired-overrides-minibuffer-exit-with-action
   #'find-file-other-window))

(defun dired-overrides--read-directory-name (prompt &optional dir
                                                    default-dirname mustmatch
                                                    initial)
  "Read a directory name with a custom keymap for the minibuffer.

Argument PROMPT is the string to prompt with.

Optional argument DIR is the directory to start in.

Optional argument DEFAULT-DIRNAME is the default directory name.

Optional argument MUSTMATCH specifies whether the user must match an existing
directory.

Optional argument INITIAL is the initial input."
  (minibuffer-with-setup-hook
      (lambda ()
        (use-local-map
         (make-composed-keymap
          (list dired-overrides-minibuffer-file-map
                dired-overrides-minibuffer-file-override-map)
          (current-local-map))))
    (read-directory-name prompt dir default-dirname mustmatch
                         initial)))

(defun dired-overrides-read-file-name-with-keymap-fn (fn &rest args)
  "Apply minibuffer function FN with ARGS with extended keymap.

The keymap:

\\<dired-overrides-minibuffer-file-map>\\{dired-overrides-minibuffer-file-map}."
  (minibuffer-with-setup-hook
      (lambda ()
        (use-local-map
         (make-composed-keymap
          dired-overrides-minibuffer-file-override-map
          (current-local-map))))
    (apply fn args)))

(defun dired-overrides-enable-advice-read-file-name ()
  "Add advice to `read-file-name' to use a custom keymap.

During completion the next commands are available:

\\<dired-overrides-minibuffer-file-map>\\{dired-overrides-minibuffer-file-map}."
  (interactive)
  (advice-add 'read-file-name :around
              #'dired-overrides-read-file-name-with-keymap-fn))

(defun dired-overrides-disable-advice-read-file-name ()
  "Remove the advice function from `read-file-name'."
  (interactive)
  (advice-remove 'read-file-name
                 #'dired-overrides-read-file-name-with-keymap-fn))

(defun dired-overrides-read-multiple-files (prompt &optional dir
                                                   default-filename mustmatch
                                                   initial predicate)
  "PROMPT user to select multiple files with completion and actions.

Argument PROMPT is the string to display as the prompt.

Optional argument DIR is the directory to read files from; defaults to
`default-directory'.

Optional argument DEFAULT-FILENAME is the default file name offered to the user.

Optional argument MUSTMATCH, when non-nil, means the user can only select from
existing files.

Optional argument INITIAL is the initial input to the minibuffer.

Optional argument PREDICATE is a function to filter the files shown; it takes a
file name and returns non-nil if the file should be shown."
  (unless dir (setq dir default-directory))
  (let* ((choices)
         (default-pred
          (lambda (file)
            (let ((full (if (file-name-absolute-p file)
                            file
                          (expand-file-name file dir))))
              (not (seq-find
                    (lambda (it)
                      (file-equal-p full it))
                    choices)))))
         (pred (if (functionp predicate)
                   (dired-overrides--all-pass
                    default-pred
                    predicate)
                 default-pred)))
    (catch 'done (while
                     (let ((curr (catch 'action
                                   (let ((last-dir dir))
                                     (minibuffer-with-setup-hook
                                         (lambda ()
                                           (use-local-map
                                            (make-composed-keymap
                                             (list
                                              dired-overrides-multiple-files-completion-map
                                              dired-overrides-minibuffer-file-map
                                              dired-overrides-minibuffer-file-override-map)
                                             (current-local-map))))
                                       (let ((composed-prompt
                                              (concat prompt
                                                      (truncate-string-to-width
                                                       (if choices
                                                           (format "(%s)"
                                                                   (string-join
                                                                    choices ", "))
                                                         "")
                                                       (window-width)))))
                                         (read-file-name composed-prompt
                                                         last-dir
                                                         default-filename
                                                         mustmatch
                                                         initial pred)))))))
                       (pcase (car-safe curr)
                         ('visit
                          (unless (equal dir (cdr curr))
                            (setq dir (cdr curr))))
                         ('mark
                          (when-let ((file (cdr curr)))
                            (setq dir (or (file-name-parent-directory file) dir))
                            (setq choices (if (member file choices)
                                              (remove file choices)
                                            (append choices (list file))))
                            t))
                         ('done
                          (when-let ((file (cdr curr)))
                            (setq choices (if (member file choices)
                                              (remove file choices)
                                            (append choices (list file))))
                            nil))
                         ((guard (and (stringp curr)
                                      (not choices)))
                          (setq choices (append choices (list curr)))
                          nil)
                         ((guard (stringp curr))
                          (setq choices (if (member curr choices)
                                            (remove curr choices)
                                          (append choices (list curr))))
                          t)))))
    choices))

(defun dired-overrides-act-on-files (action prompt &optional initial-dir &rest
                                            args)
  "Apply ACTION to selected files, handling directories with user prompt.

Argument ACTION is a function to be applied to each file.

Argument PROMPT is a string displayed to the user when asking for files.

Optional argument INITIAL-DIR is the directory to start with.

Remaining arguments ARGS are passed to the function
`dired-overrides-read-multiple-files'."
  (let ((files (apply #'dired-overrides-read-multiple-files prompt initial-dir
                      args)))
    (while files
      (let ((file (pop files)))
        (if (file-directory-p file)
            (pcase (car (read-multiple-choice
                         (format (concat "File %s is directory, " prompt) file)
                         '((?a "all files")
                           (?y "one by one")
                           (?p "prompt")
                           (?n "no, skip"))))
              (?y
               (setq files (append files
                                   (directory-files
                                    file t
                                    directory-files-no-dot-files-regexp))))
              (?Y
               (setq files (append files
                                   (directory-files-recursively
                                    file directory-files-no-dot-files-regexp))))
              (?p
               (setq files (append files (dired-overrides-read-multiple-files
                                          prompt file)))))
          (funcall action file))))))


(provide 'dired-overrides)
;;; dired-overrides.el ends here
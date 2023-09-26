;;; dired-overrides.el --- Dired misc utils -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/dired-overrides
;; Version: 0.1.0
;; Keywords: files
;; Package-Requires: ((emacs "28.1"))
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
  "Normalized sources.")

(defvar dired-overrides-multi-source-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C->") #'dired-overrides-multi-source-select-next)
    (define-key map (kbd "C-<") #'dired-overrides-multi-source-select-prev)
    (define-key map (kbd "C-.") #'dired-overrides-multi-source-read-source)
    map)
  "Keymap to use in minibuffer.")

(defun dired-overrides-multi-source-select-next ()
  "Throw to the catch tag ='next with 1."
  (interactive)
  (throw 'next
         1))

(defun dired-overrides-multi-source-select-prev ()
  "Throw to the catch tag ='next with -1."
  (interactive)
  (throw 'next
         -1))


(defun dired-overrides-multi-source-read-source ()
  "Throw to the catch tag ='next with -1."
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
                           (unless (file-remote-p dir)
                             (expand-file-name dir))))
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
  "Call ACTION with minibuffer candidate in its original window."
  (pcase-let ((`(,_category . ,current)
               (dired-overrides-minibuffer-get-current-candidate)))
    (when-let ((win (get-buffer-window "*Completions*" 0)))
      (minimize-window win)
      (add-hook 'post-command-hook
                #'dired-overrides-minibuffer-web-restore-completions-wind))
    (with-minibuffer-selected-window
      (funcall action current))))

(defun dired-overrides-minibuffer-preview-file ()
  "Call ACTION with minibuffer candidate in its original window."
  (interactive)
  (dired-overrides-minibuffer-action-no-exit 'find-file))

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


(defun dired-overrides-read-active-buffers-directory ()
  "Read directory name, completing with default directories from all buffers."
  (interactive)
  (dired-overrides-completing-read-with-keymap
   "Directory: "
   (dired-overrides-get-active-buffers-dirs)
   dired-overrides-minibuffer-file-map))

(defun dired-overrides-read-file (prompt &optional initial-dir default)
  "PROMPT for a directory or filename, showing active buffer directories first.

Argument PROMPT is a string that is used to PROMPT the user for input.
Optional argument INITIAL-DIR is a string representing the initial directory.
If not provided, it defaults to nil.
Optional argument DEFAULT is a string representing the DEFAULT directory.
If not provided, it defaults to nil."
  (let ((choices (seq-uniq (append
                            (mapcar (lambda (d)
                                      (if (file-name-absolute-p d)
                                          d
                                        (expand-file-name d
                                                          default-directory)))
                                    (delq nil (list initial-dir default)))
                            (dired-overrides-get-active-buffers-dirs)))))
    (dired-overrides-multi-source-read `(("Active Buffers Directories"
                                          dired-overrides-completing-read-with-keymap
                                          ,prompt
                                          ,choices
                                          ,dired-overrides-minibuffer-file-map)
                                         ("Other directory" read-directory-name
                                          ,prompt
                                          ,initial-dir ,default)
                                         ("Other filename" read-file-name
                                          ,prompt ,initial-dir
                                          ,default)))))

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
  (if dired-overrides-mode
      (advice-add 'dired-mark-read-file-name :override
                  #'dired-overrides-mark-read-file-name)
    (advice-remove 'dired-mark-read-file-name
                   #'dired-overrides-mark-read-file-name)))


(provide 'dired-overrides)
;;; dired-overrides.el ends here
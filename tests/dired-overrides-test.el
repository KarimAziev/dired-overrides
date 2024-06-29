;;; dired-overrides-test.el --- Tests for dired-overrides -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>

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

;; Tests for dired-overrides

;;; Code:




(require 'ert)

(require 'dired-overrides)

;; Test when pattern is a function
(ert-deftest test-dired-overrides-expand-pattern-function ()
  (let ((pattern (lambda (s) (string= s "test"))))
    (should (dired-overrides-expand-pattern "test" pattern))
    (should-not (dired-overrides-expand-pattern "not-test" pattern))))

;; Test when pattern is a string (regex)
(ert-deftest test-dired-overrides-expand-pattern-string ()
  (let ((pattern "^test$"))
    (should (dired-overrides-expand-pattern "test" pattern))
    (should-not (dired-overrides-expand-pattern "not-test" pattern))
    (should-not (dired-overrides-expand-pattern "test123" pattern))))

;; Test when pattern is a list of functions and strings
(ert-deftest test-dired-overrides-expand-pattern-list ()
  (let ((pattern (list "^test$" (lambda (s) (string= s "example")))))
    (should (dired-overrides-expand-pattern "test" pattern))
    (should (dired-overrides-expand-pattern "example" pattern))
    (should-not (dired-overrides-expand-pattern "not-test" pattern))
    (should-not (dired-overrides-expand-pattern "test123" pattern))))

;; Test when pattern is an empty list
(ert-deftest test-dired-overrides-expand-pattern-empty-list ()
  (let ((pattern '()))
    (should-not (dired-overrides-expand-pattern "test" pattern))))

;; Test when pattern is a list with only functions
(ert-deftest test-dired-overrides-expand-pattern-list-functions ()
  (let ((pattern (list (lambda (s) (string= s "test"))
                       (lambda (s) (string= s "example")))))
    (should (dired-overrides-expand-pattern "test" pattern))
    (should (dired-overrides-expand-pattern "example" pattern))
    (should-not (dired-overrides-expand-pattern "not-test" pattern))))

;; Test when pattern is a list with only strings
(ert-deftest test-dired-overrides-expand-pattern-list-strings ()
  (let ((pattern (list "^test$" "^example$")))
    (should (dired-overrides-expand-pattern "test" pattern))
    (should (dired-overrides-expand-pattern "example" pattern))
    (should-not (dired-overrides-expand-pattern "not-test" pattern))
    (should-not (dired-overrides-expand-pattern "test123" pattern))))

;; Test when pattern is a nested list
(ert-deftest test-dired-overrides-expand-pattern-nested-list ()
  (let ((pattern (list "^test$" (list (lambda (s) (string= s "example")) "^nested$"))))
    (should (dired-overrides-expand-pattern "test" pattern))
    (should (dired-overrides-expand-pattern "example" pattern))
    (should (dired-overrides-expand-pattern "nested" pattern))
    (should-not (dired-overrides-expand-pattern "not-test" pattern))
    (should-not (dired-overrides-expand-pattern "test123" pattern))))

;; Mocking directory structure for testing
(defun dired-overrides--mock-directory-structure ()
  "Create temporaririly directory with such structure.

tmp/test-generated-dir:
    ├── file3.txt
    ├── file2.log
    ├── file1.txt
    ├── dir3/
    ├── dir2/
    │   ├── subnested-file.txt
    │   ├── subdir/
    │   │   ├── nested-file.txt
    ├── dir1/
    │   ├── file.txt."
  (let ((root (make-temp-file "test-dir" t)))
    (dolist (dir '("dir1" "dir2/subdir" "dir3"))
      (make-directory (expand-file-name dir root) t))
    (dolist (file '("dir1/file.txt" "dir2/subdir/nested-file.txt"
                    "dir2/subnested-file.txt" "file1.txt" "file2.log"
                    "file3.txt"))
      (write-region "" nil (expand-file-name file root)))
    root))




;; Test when pattern is a string (regex)
(ert-deftest test-dired-overrides-find-in-dir-pattern-string ()
  (let ((root (dired-overrides--mock-directory-structure)))
    (unwind-protect
        (let ((result (dired-overrides-find-in-dir root ".*\\.txt$")))
          (should (equal (sort result 'string<)
                         (sort (list (expand-file-name "file1.txt" root)
                                     (expand-file-name "file3.txt" root))
                               'string<))))
      (delete-directory root t))))


;; Test when pattern is a function
(ert-deftest test-dired-overrides-find-in-dir-pattern-function ()
  (let ((root (dired-overrides--mock-directory-structure)))
    (unwind-protect
        (let ((result (dired-overrides-find-in-dir root (lambda (s) (string-suffix-p ".log" s)))))
          (should (equal result (list (expand-file-name "file2.log" root)))))
      (delete-directory root t))))


(ert-deftest test-dired-overrides-find-in-dir-non-visit-pattern ()
  (let ((root (dired-overrides--mock-directory-structure)))
    (unwind-protect
        (let ((result (dired-overrides-find-in-dir root ".*\\.txt$" "dir2")))
          (should (equal (sort result 'string<)
                         (sort (list (expand-file-name "file1.txt" root)
                                     (expand-file-name "file3.txt" root))
                               'string<))))
      (delete-directory root t))))

(ert-deftest test-dired-overrides-find-in-dir-max-depth-1 ()
  (let ((root (dired-overrides--mock-directory-structure)))
    (unwind-protect
        (let ((result (dired-overrides-find-in-dir root nil nil 1)))
          (should (equal (sort result 'string<)
                         (sort (list
                                (expand-file-name "file1.txt" root)
                                (expand-file-name "file2.log" root)
                                (expand-file-name "file3.txt" root))
                               'string<))))
      (delete-directory root t))))

(ert-deftest test-dired-overrides-find-in-dir-max-depth-1-with-pattern ()
  (let ((root (dired-overrides--mock-directory-structure)))
    (unwind-protect
        (let ((result (dired-overrides-find-in-dir root ".*\\.txt$" nil 1)))
          (should (equal (sort result 'string<)
                         (sort (list (expand-file-name "file1.txt" root)
                                     (expand-file-name "file3.txt" root))
                               'string<))))
      (delete-directory root t))))

(ert-deftest test-dired-overrides-find-in-dir-max-depth-2-txt ()
  (let ((root (dired-overrides--mock-directory-structure)))
    (unwind-protect
        (let ((result (dired-overrides-find-in-dir root "\\.txt\\'" nil 2)))
          (should (equal (sort result 'string<)
                         (sort (list
                                (expand-file-name "file1.txt" root)
                                (expand-file-name "file3.txt" root)
                                (expand-file-name "dir1/file.txt" root)
                                (expand-file-name "dir2/subnested-file.txt"
                                                  root))
                               'string<))))
      (delete-directory root t))))

(ert-deftest test-dired-overrides-find-in-dir-max-depth-2 ()
  (let ((root (dired-overrides--mock-directory-structure)))
    (unwind-protect
        (let ((result (dired-overrides-find-in-dir root nil nil 2 nil t)))
          (should (equal (sort result 'string<)
                         (sort (list
                                (expand-file-name "dir1" root)
                                (expand-file-name "dir2" root)
                                (expand-file-name "dir3" root)
                                (expand-file-name "file1.txt" root)
                                (expand-file-name "file2.log" root)
                                (expand-file-name "file3.txt" root)
                                (expand-file-name "dir1/file.txt" root)
                                (expand-file-name "dir2/subdir" root)
                                (expand-file-name "dir2/subnested-file.txt"
                                                  root))
                               'string<))))
      (delete-directory root t))))

(ert-deftest test-dired-overrides-find-in-dir-max-depth-2-log ()
  (let ((root (dired-overrides--mock-directory-structure)))
    (unwind-protect
        (let ((result (dired-overrides-find-in-dir root ".*\\.log$" nil 2)))
          (should (equal (sort result 'string<)
                         (list
                          (expand-file-name "file2.log" root)))))
      (delete-directory root t))))

(ert-deftest test-dired-overrides-find-in-dir-max-depth-3-txt ()
  (let ((root (dired-overrides--mock-directory-structure)))
    (unwind-protect
        (let ((result (dired-overrides-find-in-dir root ".*\\.txt$" nil 3)))
          (should (equal (sort result 'string<)
                         (sort (list
                                (expand-file-name "file1.txt" root)
                                (expand-file-name "file3.txt" root)
                                (expand-file-name "dir1/file.txt" root)
                                (expand-file-name
                                 "dir2/subnested-file.txt" root)
                                (expand-file-name
                                 "dir2/subdir/nested-file.txt" root))
                               'string<))))
      (delete-directory root t))))

(ert-deftest test-dired-overrides-find-in-dir-max-depth-3 ()
  (let ((root (dired-overrides--mock-directory-structure)))
    (unwind-protect
        (let ((result (dired-overrides-find-in-dir root nil nil 3 nil t)))
          (should (equal (sort result 'string<)
                         (sort (list
                                (expand-file-name "dir1" root)
                                (expand-file-name "dir2" root)
                                (expand-file-name "dir3" root)
                                (expand-file-name "file1.txt" root)
                                (expand-file-name "file2.log" root)
                                (expand-file-name "file3.txt" root)
                                (expand-file-name "dir1/file.txt" root)
                                (expand-file-name "dir2/subdir" root)
                                (expand-file-name "dir2/subnested-file.txt" root)
                                (expand-file-name "dir2/subdir/nested-file.txt"
                                                  root))
                               'string<))))
      (delete-directory root t))))

(ert-deftest test-dired-overrides-find-in-dir-max-depth-10000 ()
  (let ((root (dired-overrides--mock-directory-structure)))
    (unwind-protect
        (let ((result (dired-overrides-find-in-dir root nil nil 10000 nil t)))
          (should (equal (sort result 'string<)
                         (sort (list
                                (expand-file-name "dir1" root)
                                (expand-file-name "dir2" root)
                                (expand-file-name "dir3" root)
                                (expand-file-name "file1.txt" root)
                                (expand-file-name "file2.log" root)
                                (expand-file-name "file3.txt" root)
                                (expand-file-name "dir1/file.txt" root)
                                (expand-file-name "dir2/subdir" root)
                                (expand-file-name "dir2/subnested-file.txt" root)
                                (expand-file-name "dir2/subdir/nested-file.txt"
                                                  root))
                               'string<))))
      (delete-directory root t))))

(ert-deftest test-dired-overrides-find-in-dir-max-depth-10000-without-dirs ()
  (let ((root (dired-overrides--mock-directory-structure)))
    (unwind-protect
        (let ((result (dired-overrides-find-in-dir root nil nil 10000)))
          (should (equal (sort result 'string<)
                         (sort (list
                                (expand-file-name "file1.txt" root)
                                (expand-file-name "file2.log" root)
                                (expand-file-name "file3.txt" root)
                                (expand-file-name "dir1/file.txt" root)
                                (expand-file-name "dir2/subnested-file.txt" root)
                                (expand-file-name "dir2/subdir/nested-file.txt"
                                                  root))
                               'string<))))
      (delete-directory root t))))

;; Test with transform-fn
(ert-deftest test-dired-overrides-find-in-dir-transform-fn ()
  (let ((root (dired-overrides--mock-directory-structure)))
    (unwind-protect
        (let
            ((result
              (dired-overrides-find-in-dir root ".*\\.txt$" nil nil
                                           (lambda (s)
                                             (concat
                                              s
                                              "-transformed")))))
          (should (equal (sort result 'string<)
                         (sort (list (concat (expand-file-name "file1.txt" root)
                                             "-transformed")
                                     (concat (expand-file-name "file3.txt" root)
                                             "-transformed"))
                               'string<))))
      (delete-directory root t))))


(provide 'dired-overrides-test)
;;; dired-overrides-test.el ends here
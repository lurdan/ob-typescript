;;; ob-typescript.el --- org-babel functions for typescript evaluation  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 KURASHIKI Satoru

;; Author: KURASHIKI Satoru
;; Keywords: literate programming, reproducible research, typescript
;; Homepage: https://github.com/lurdan/ob-typescript
;; Version: 0.1
;; Package-Requires: ((emacs "24") (org "8.0"))

;;; License:

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Exec typescript in org-babel code blocks.

;;; Requirements:
;; You need to install node.js and typescript to use this extension.

;;; Code:
(require 'ob)
;;(require 'ob-ref)
;;(require 'ob-comint)
;;(require 'ob-eval)

;;(require 'typescript)

(add-to-list 'org-babel-tangle-lang-exts '("typescript" . "ts"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:typescript '((:cmdline . "--noImplicitAny")))

(defvar org-babel-command:typescript "tsc"
  "Command run by ob-typescript to launch tsc compiler")

(defun org-babel-variable-assignments:typescript (params)
  "Return list of typescript statements assigning the block's variables."
  (mapcar (lambda (pair) (format "let %s=%s;"
                                 (car pair) (org-babel-typescript-var-to-typescript (cdr pair))))
          (org-babel--get-vars params)))

(defun org-babel-typescript-var-to-typescript (var)
  "Convert an elisp var into a string of typescript source code
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-typescript-var-to-typescript var ", ") "]")
    (replace-regexp-in-string "\n" "\\\\n" (format "%S" var))))

(defun org-babel-execute:typescript (body params)
  "Execute a block of Typescript code with org-babel.
This function is called by `org-babel-execute-src-block'.

The block is compiled with `--outDir'/`--rootDir' instead of the
`--outFile'/`--out' options, which were removed in TypeScript 7
\(`--outFile' only ever worked together with AMD/System modules, and
the native compiler no longer supports it).  `--outDir' works the same
way in all TypeScript versions."
  (let* ((tmp-src-file (org-babel-temp-file "ts-src-" ".ts"))
         (tmp-dir (file-name-directory tmp-src-file))
         ;; With --outDir equal to --rootDir, tsc emits <basename>.js
         ;; right next to the temporary source file.
         (tmp-out-file (concat (file-name-sans-extension tmp-src-file) ".js"))
         (cmdline (cdr (assoc :cmdline params)))
         (cmdline (if cmdline (concat " " cmdline) ""))
         (wrap (assoc :wrap params))
         (jsexec (if wrap ""
                   (concat " ; node " (org-babel-process-file-name tmp-out-file)))))
    (with-temp-file tmp-src-file
      (insert (org-babel-expand-body:generic
               body params (org-babel-variable-assignments:typescript params))))
    (let ((results (org-babel-eval
                    (format "%s%s --rootDir %s --outDir %s %s%s"
                            org-babel-command:typescript
                            cmdline
                            (org-babel-process-file-name tmp-dir)
                            (org-babel-process-file-name tmp-dir)
                            (org-babel-process-file-name tmp-src-file)
                            jsexec)
                    "")))
      (if wrap
          (with-temp-buffer
            (insert-file-contents tmp-out-file)
            (buffer-substring-no-properties (point-min) (point-max)))
        results))))

(provide 'ob-typescript)

;;; ob-typescript.el ends here

;;; macos.el --- macOS utilities with native integration.
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Alvaro Ramirez
;;
;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/EmacsMacOSModule
;;
;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The `macos' package is a set of macOS utilites with native
;; integrations via EmacsMacOSModule.

;;; Code:

(defvar macos-lib-name "libEmacsMacOSModule.dylib")

(defvar macos-module-install-dir (expand-file-name (file-name-concat user-emacs-directory "modules" "macos")))

(defvar macos-module-path (file-name-concat macos-module-install-dir macos-lib-name))

(declare-function macos-module--share "ext:macos-module" t)

(declare-function macos-module--reveal-in-finder "ext:macos-module" t)

(defun macos-reveal-in-finder ()
  "Reveal file(s) in macOS Finder.

If visiting a buffer with associated file, reveal it.

While in `dired', any selected files, reveal those.  If region is
active, reveal files in region.  Otherwise reveal file at point."
  (interactive)
  (macos-module--reveal-in-finder (vconcat (macos--files-dwim))))

(defun macos-show-emoji-picker ()
  "Show macOS emoji picker."
  (interactive)
  (macos-module--show-emoji-picker))

(defun macos-share ()
  "Share file(s) with other macOS apps.

If visiting a buffer with associated file, share it.

While in `dired', any selected files, share those.  If region is
active, share files in region.  Otherwise share file at point."
  (interactive)
  (macos-module--share (vconcat (macos--files-dwim))))

(defun macos--files-dwim ()
  "Return buffer file (if available) or marked/region files for a `dired' buffer."
  (cl-assert (not (and (use-region-p) (let ((files (dired-get-marked-files nil nil nil t)))
                                     ;; Based on `dired-number-of-marked-files'.
                                     (cond ((null (cdr files))
                                            nil)
                                           ((and (= (length files) 2)
                                                 (eq (car files) t))
                                            t)
                                           (t
                                            (not (seq-empty-p files)))))))
             nil "Region and marked files both active. Choose one only.")
  (if (buffer-file-name)
      (list (buffer-file-name))
    (or
     (macos--dired-paths-in-region)
     (dired-get-marked-files))))

(defun macos--dired-paths-in-region ()
  "If `dired' buffer, return region files.  nil otherwise."
  (when (and (equal major-mode 'dired-mode)
             (use-region-p))
    (let ((start (region-beginning))
          (end (region-end))
          (paths))
      (save-excursion
        (save-restriction
          (goto-char start)
          (while (< (point) end)
            ;; Skip non-file lines.
            (while (and (< (point) end) (dired-between-files))
              (forward-line 1))
            (when (dired-get-filename nil t)
              (setq paths (append paths (list (dired-get-filename nil t)))))
            (forward-line 1))))
      paths)))

(defun macos--emacs-point-x ()
  "Return the x coordinate at point."
  (car (posn-x-y (posn-at-point (point)))))

(defun macos--emacs-point-y ()
  "Return the y coordinate at point."
  (cdr (posn-x-y (posn-at-point (point)))))

(defun macos-load-module ()
  "Loads cached module.

Builds and loads if no cache available.

To explicitly rebuild and reload, use `macos-rebuild-module-and-reload'."
  (let ((module-path (macos--built-module-path)))
    (if (file-exists-p module-path)
        (module-load module-path)
      (macos-rebuild-module-and-reload))))

(defun macos-rebuild-module-and-reload ()
  "Rebuild and reload native module."
  (interactive)
  (let ((compilation-ask-about-save nil))
    (compile (format "swift build && %s -ne '(module-load \"%s\")'"
                     (executable-find "emacsclient")
                     (macos--built-module-path)))))

(defun macos--module-source-root ()
  "Return the source root directory for the native module."
  (let ((project-root (expand-file-name (file-name-directory (symbol-file 'macos--module-source-root 'defun)))))
    (unless project-root
      (error "Not in macos project"))
    (unless (file-exists-p (file-name-concat project-root "macos.el"))
      (error "Not in macos project"))
    project-root))

(defun macos--built-module-path ()
  "Return the path to the built module."
  (file-name-concat (macos--module-source-root) ".build" "debug" macos-lib-name))

(provide 'macos)

;;; macos.el ends here

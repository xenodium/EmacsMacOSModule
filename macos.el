;;; macos.el --- macOS utilities with native integration.  -*- lexical-binding: t; -*-

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

(require 'map)

(defvar macos-lib-name "libEmacsMacOSModule.dylib")

(defvar macos-module-install-dir (expand-file-name (file-name-concat user-emacs-directory "modules" "macos")))

(defvar macos-module-path (file-name-concat macos-module-install-dir macos-lib-name))

(declare-function macos-module--share "ext:macos-module" t)

(declare-function macos-module--reveal-in-finder "ext:macos-module" t)

(declare-function macos-module--quick-look "ext:macos-module" t)

(declare-function macos-module--quick-look-hide "ext:macos-module" t)

(declare-function macos-module--quick-look-visible-p "ext:macos-module" t)

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

(defun macos-quick-look ()
  "Preview file(s) with macOS Quick Look.

If visiting a buffer with associated file, preview it.

While in `dired', any selected files, preview those.  If region is
active, preview files in region.  Otherwise preview file at point."
  (interactive)
  (macos-module--quick-look (vconcat (macos--files-dwim)) 0
                            (macos--quick-look-rect-vector
                             (macos--quick-look-source-rect))))

(defun macos--quick-look-source-rect ()
  "Return the file name rectangle at point as an alist.

Return nil if there is no file name at point or it is off screen.
Coordinates are Emacs display pixels.  Quick Look zooms its panel
out of (and back into) this rectangle.

For a Dired line whose file name is drawn 90 pixels wide, 38 pixels
from the left of the display and 275 from the top, on a 21 pixel line:

  (macos--quick-look-source-rect)
  => ((:x . 38) (:y . 275) (:width . 90) (:height . 21))"
  (save-excursion
    (when-let* (((derived-mode-p 'dired-mode))
                (start (dired-move-to-filename))
                (top-left (window-absolute-pixel-position start))
                (top-right (window-absolute-pixel-position
                            (progn (dired-move-to-end-of-filename t) (point)))))
      (list (cons :x (car top-left))
            (cons :y (cdr top-left))
            (cons :width (max 1 (- (car top-right) (car top-left))))
            (cons :height (default-line-height))))))

(defun macos--quick-look-rect-vector (rect)
  "Convert RECT into the vector the native module expects.

RECT is an alist as returned by `macos--quick-look-source-rect', or
nil for no zoom origin, which makes Quick Look fade the panel in.

  (macos--quick-look-rect-vector
   \\='((:x . 38) (:y . 275) (:width . 90) (:height . 21)))
  => [38 275 90 21]

  (macos--quick-look-rect-vector nil)
  => []"
  (if rect
      (vector (map-elt rect :x)
              (map-elt rect :y)
              (map-elt rect :width)
              (map-elt rect :height))
    []))

(defun macos-quick-look-dismiss ()
  "Dismiss the macOS Quick Look panel."
  (interactive)
  (macos-module--quick-look-hide))

(defun macos-quick-look-visible-p ()
  "Return non-nil if the macOS Quick Look panel is visible."
  (macos-module--quick-look-visible-p))

(defvar-local macos--dired-quick-look-file nil
  "File last handed to Quick Look in this buffer.")

(define-minor-mode macos-dired-quick-look-mode
  "Toggle macOS Quick Look previews following point in `dired'.

Quick Look does not take keyboard focus, so navigating `dired'
updates the preview in place."
  :lighter " QL"
  :global nil
  (cond (macos-dired-quick-look-mode
         (unless (derived-mode-p 'dired-mode)
           (setq macos-dired-quick-look-mode nil)
           (user-error "Not in a Dired buffer"))
         (add-hook 'post-command-hook #'macos--dired-quick-look-update nil t)
         (add-hook 'kill-buffer-hook #'macos-quick-look-dismiss nil t)
         (macos--dired-quick-look-update))
        (t
         (remove-hook 'post-command-hook #'macos--dired-quick-look-update t)
         (remove-hook 'kill-buffer-hook #'macos-quick-look-dismiss t)
         (setq macos--dired-quick-look-file nil)
         (macos-quick-look-dismiss))))

(defun macos--dired-quick-look-update ()
  "Preview the `dired' file at point, if it changed since last command."
  (when-let* (((derived-mode-p 'dired-mode))
              (file (dired-get-filename nil t))
              ((not (equal file macos--dired-quick-look-file))))
    (setq macos--dired-quick-look-file file)
    (macos-module--quick-look (vector file) 0
                              (macos--quick-look-rect-vector
                               (macos--quick-look-source-rect)))))

(provide 'macos)

;;; macos.el ends here

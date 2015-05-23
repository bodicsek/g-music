;;; g-music.el --- Support for google music all access

;; Copyright (C) 2015 David Nabraczky

;; Author: David Nabraczky <david.nabraczky@gmail.com>
;; Created: 23 May 2015
;; Keywords: google music

;; This file is not part of GNU Emacs.

;; This file is free software…
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

(defconst g-music-buffer "*GMusic*"
  "GMusic buffer name.")

(defvar g-music-mode-hook nil
  "*List of functions to call when entering g-music mode.")

(defvar g-music-mode-map nil
  "Keymap for g-music major mode.")

(if g-music-mode-map
    nil      ;do nothing if g-music-mode-map exists
  (setq quip-mode-map (make-sparse-keymap))
  (define-key quip-mode-map "g" 'g-music-load-playlists))

(defun g-music-buffer-setup ()
  "Render the file browser in the *GMusic* buffer."
  (setq deft-window-width (window-width))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (g-music-print-header)

  ;; Print the playlist names plus *collection*

  (use-local-map g-music-mode-map)
  (widget-setup)
  (goto-char 1)
  (forward-line 2))

(defun g-music-print-header ()
  (widget-insert "GMusic")
  (widget-insert "\n\n"))

(defun g-music-mode ()
  "Major mode for listening music from google music."
  (interactive)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'g-music-mode)
  (setq mode-name "GMusic")
  (g-music-buffer-setup)
  (run-hooks 'g-music-mode-hook))

;;;###autoload
(defun g-music ()
  "Switch to *GMusic* buffer and load playlists."
  (interactive)
  (switch-to-buffer g-music-buffer)
  (if (not (eq major-mode 'g-music-mode))
      (g-music-mode)))

(provide 'g-music)

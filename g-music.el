;;; g-music.el --- Support for google music all access

;; Copyright (C) 2015 David Nabraczky

;; Author: David Nabraczky <david.nabraczky@gmail.com>
;; Created: 23 May 2015
;; Keywords: google music
;; Version: 0.0.1
;; Package-Requires: ((request "0.1.0"))

;; This file is not part of GNU Emacs.

;; This file is free software…
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'cl-macs)
(require 'wid-edit)
(require 'request)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurable (public) variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface g-music-playlist-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for GMusic playlists."
  :group 'g-music-faces)

(defvar g-music-mode-hook nil
  "*List of functions to call when entering g-music mode.")

(defvar *g-music-proxy-addr* "http://127.0.0.1:9999")

(defconst *g-music-buffer* "*GMusic*"
  "GMusic buffer name.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal (private) variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A list of playlists
;; A playlist is represented as a property list:
;; :plname  - the displayed name of the playlist
;; :plurl   - the url to be used to get the playlist content
;; :content - a list of songs
;; A song is represented as a cons:
;; (name . url)
(defvar *db* (list (list :plname "collection"
                         :plurl (concat *g-music-proxy-addr* "/get_collection")
                         :content nil)))

(defun g-music-db-get-playlist (name db)
  (cl-find name db :key (cl-function (lambda (pl) (cl-getf pl :plname))) :test 'equal))

(defun g-music-db-get-playlist-name (playlist)
  (cl-getf playlist :plname))

(defun g-music-db-get-playlist-url (playlist)
  (cl-getf playlist :plurl))

(defun g-music-db-get-playlist-content (playlist)
  (cl-getf playlist :content))

(defun g-music-db-set-playlist-content (playlist content)
  (setf (cl-getf playlist :content) content))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode map and its handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar g-music-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'g-music-refresh)
    (define-key map (kbd "RET") 'g-music-complete)
    map)
  "Keymap for g-music major mode.")

;; (if g-music-mode-map
;;     nil      ;do nothing if g-music-mode-map exists
;;   (setq g-music-mode-map (make-sparse-keymap))
;;   (define-key g-music-mode-map "g" 'g-music-load-playlists)
;;   (define-key g-music-mode-map "RET" 'g-music-expand-playlist))

(defun g-music-refresh ()
  (interactive)
  (request (concat g-music-proxy-addr "/get_all_playlists")
           :parser 'buffer-string
           :success (cl-function (lambda (&key data &allow-other-keys)
                                   (message data)))))

(defun g-music-complete ()
  (interactive)
  (if (widget-at)
      (widget-button-press (point))
    (message "Nothing to do.")))


;; TODO: make the proxy command configurable
;; TODO: wait for the process to initialize
(defun g-music-start-proxy ()
  (start-process "GMusicProxy" "*GMusicProxy*" "GMusicProxy")
  (display-buffer "*GMusicProxy*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun g-music-buffer-setup ()
  "Render the file browser in the *GMusic* buffer."
  (setq deft-window-width (window-width))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (g-music-print-header)

  ;; Print the playlist names plus *collection*
  (g-music-create-playlist-widget "collection" "")

  (widget-setup)
  (goto-char 1)
  (forward-line 2))

(defun g-music-print-header ()
  (widget-insert "GMusic")
  (widget-insert "\n\n"))

(defun g-music-create-playlist-widget (value tag)
  (widget-create 'link
                 :button-prefix ""
                 :button-suffix ""
                 :button-face 'g-music-playlist-face
                 :format "* %[%v%]"
                 :tag tag
                 :help-echo "Expand this playlist"
                 :notify (lambda (widget &rest ignore)
                           (message "Expanding.."))
                 value)
  (widget-insert "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun g-music-mode ()
  "Major mode for listening music from google music."
  (interactive)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'g-music-mode)
  (setq mode-name "GMusic")
  (use-local-map g-music-mode-map)
  (g-music-start-proxy)
  (g-music-buffer-setup)
  (run-hooks 'g-music-mode-hook))

;;;###autoload
(defun g-music ()
  "Switch to *GMusic* buffer and load playlists."
  (interactive)
  (switch-to-buffer *g-music-buffer*)
  (if (not (eq major-mode 'g-music-mode))
      (g-music-mode)))

(provide 'g-music)

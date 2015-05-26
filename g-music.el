;;; g-music.el --- Support for google music all access-*- lexical-binding: t -*-

;; Copyright (C) 2015 David Nabraczky

;; Author: David Nabraczky <david.nabraczky@gmail.com>
;; Created: 23 May 2015
;; Keywords: google music
;; Version: 0.0.1
;; Package-Requires: ((request "0.1.0") (dash "2.10.0"))

;; This file is not part of GNU Emacs.

;; This file is free software…
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'cl-macs)
(require 'wid-edit)
(require 'request)
(require 'dash)

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
(defvar *db* nil
  "A list of playlists
   A playlist is represented as a property list:
   :plname  - the displayed name of the playlist
   :plurl   - the url to be used to get the playlist content
   :content - a list of songs
   A song is represented as a cons:
   (name . url)")

(defun g-music-db-init ()
  (setq *db* (list (g-music-db-create-playlist "collection"
                                               (concat *g-music-proxy-addr* "/get_collection")))))

(defun g-music-db-create-playlist (name url)
  (list :plname name :plurl url :content nil))

(defun g-music-db-get-playlist (name db)
  (cl-find name db :key (cl-function (lambda (pl) (cl-getf pl :plname))) :test 'equal))

(defun g-music-db-set-playlist (playlist db)
  (let ((new-db (cons playlist db)))
    (setq *db* new-db) ;; ugly side effect
    new-db))

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

(defun g-music-refresh ()
  "Reinitializes the cache and redraws the buffer."
  (interactive)
  (request (concat *g-music-proxy-addr* "/get_all_playlists")
           :parser 'buffer-string
           :success (cl-function (lambda (&key data &allow-other-keys)
                                   (g-music-db-init)
                                   (g-music-extm3u-update-playlists data)
                                   (g-music-buffer-setup)))))

(defun g-music-complete ()
  "Runs the action of the widget at point."
  (interactive)
  (if (widget-at)
      (widget-button-press (point))
    (message "Nothing to do.")))

(defun g-music-refresh-playlist-content (playlist)
  "Retrieves the content of the given playlist, updates the cache and redraws the buffer."
  (let ((url (g-music-db-get-playlist-url playlist)))
    (request url
             :parser 'buffer-string
             :success (cl-function (lambda (&key data &allow-other-keys)
                                     (g-music-extm3u-update-playlist-content playlist data)
                                     (g-music-buffer-setup))))))

;; TODO: make the proxy command configurable
;; TODO: wait for the process to initialize
(defun g-music-start-proxy ()
  (start-process "GMusicProxy" "*GMusicProxy*" "GMusicProxy")
  (display-buffer "*GMusicProxy*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extm3u parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun g-music-extm3u-update-playlist-content (playlist data)
  "Parses the playlist content extm3u data and updates the given playlist's :content."
  (let ((content (g-music-extm3u-map (-lambda ((name url)) (cons name url))
                                     (g-music-extm3u-parse data))))
    (g-music-db-set-playlist-content playlist content)))

(defun g-music-extm3u-update-playlists (data)
  "Parses the playlist extm3u data and updates the *db*."
  (g-music-extm3u-map (-lambda ((name url))
                        (g-music-db-set-playlist (g-music-db-create-playlist name url) *db*))
                      (g-music-extm3u-parse data)))

(defun g-music-extm3u-map (f seq)
  "Maps an extm3u parse list with the given function f.
The function f should have two parameters: name and url."
  (-map f (-partition-in-steps 2 2 seq)))

(defun g-music-extm3u-parse (data)
  "Parses the given extm3u string into a list of names and urls.
So it returns (\"song1\" \"http://song1\")"
  (let ((regex "^#EXTINF:.*[0-9]+,\\(.+\\)
\\(http.+\\)$"))
    (g-music-match-regex data regex 0)))

(defun g-music-match-regex (str regex start)
  "Returns all the matching groups of regex in str as a list."
  (if (string-match-p regex str start)
      (save-match-data
        (string-match regex str start)
        (-let* ((all-match-pos (-partition-in-steps 2 2 (match-data t)))
                ((_ new-start) (car all-match-pos))
                (group-matches (-map (-lambda ((start end)) (substring str start end)) (cdr all-match-pos))))
          (-union group-matches (g-music-match-regex str regex new-start))))
    '()))
                             
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

  (-map (lambda (pl)
          (let ((content (g-music-db-get-playlist-content pl)))
            (g-music-create-playlist-widget pl)
            (when (not (null content))
              (-map 'g-music-create-playlist-widget content))))
        *db*)

  (widget-setup)
  ;; TODO: preserve point location
  (goto-char 1)
  (forward-line 2))

(defun g-music-print-header ()
  (widget-insert "GMusic")
  (widget-insert "\n\n"))

(defun g-music-create-playlist-widget (playlist)
  (let ((value (g-music-db-get-playlist-name playlist)))
    (widget-create 'link
                   :button-prefix ""
                   :button-suffix ""
                   :button-face 'g-music-playlist-face
                   :format "%[%v%]\n"
                   :tag playlist
                   :help-echo "Expand this playlist"
                   :notify (lambda (widget &rest ignore)
                             (g-music-refresh-playlist-content (widget-get :tag)))
                   (concat "* " value))))

(defun g-music-create-song-widget (song)
  (-let (((name . url) song))
    (widget-create 'link
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%v%]\n"
                   :tag url
                   :help-echo "Play this song."
                   :notify (lambda (widget &rest ignore)
                             (message "Playing song..."))
                   (concat "  " name))))

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
  (g-music-db-init)
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

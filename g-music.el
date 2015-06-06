;;; g-music.el --- Support for google music all access-*- lexical-binding: t -*-

;; Copyright (C) 2015 David Nabraczky

;; Author: David Nabraczky <david.nabraczky@gmail.com>
;; Created: 23 May 2015
;; Keywords: google music
;; Version: 0.0.1
;; Package-Requires: ((request) (dash) (libmpdee))

;; This file is not part of GNU Emacs.

;; This file is free software…
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'cl-macs)
(require 'wid-edit)
(require 'request)
(require 'dash)
(require 'libmpdee)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurable (public) variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface g-music-playlist-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for GMusic playlists."
  :group 'g-music-faces)

(defface g-music-playlist-content-face
  '((t :inherit font-lock-string-face))
  "Face for GMusic playlist content."
  :group 'g-music-faces)

(defvar g-music-mode-hook nil
  "*List of functions to call when entering g-music mode.")

(defvar *g-music-proxy-addr* "127.0.0.1")
(defvar *g-music-proxy-port* 9999)

(defvar *g-music-mpd-addr* "127.0.0.1")
(defvar *g-music-mpd-port* 6600)

(defconst *g-music-buffer* "*GMusic*"
  "GMusic buffer name.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal (private) variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *db* nil
  "A list of playlists
   A playlist is represented as a property list:
   :plname            - the displayed name of the playlist
   :plurl             - the url to be used to get the playlist content
   :content           - a list of songs
   :content-displayed - bool value, that shows if the content should be displayed
   A song is represented as a cons:
   (name . url)")

(defun g-music-db-init ()
  (setq *db* (list (g-music-db-create-playlist
                    "collection"
                    (g-music--get-url *g-music-proxy-addr* *g-music-proxy-port* "/get_collection")))))

(defun g-music-db-create-playlist (name url)
  (list :plname name :plurl url :content nil :content-display nil))

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

(defun g-music-db-get-playlist-content-display (playlist)
  (cl-getf playlist :content-display))

(defun g-music-db-set-playlist-content-display (playlist flag)
  (setf (cl-getf playlist :content-display) flag))

(defun g-music-db-exclusive-set-playlist-content-display (playlist db)
  (g-music-db-clear-all-playlist-content-display db)
  (g-music-db-set-playlist-content-display playlist t))

(defun g-music-db-clear-all-playlist-content-display (db)
  (-map (-lambda (pl) (g-music-db-set-playlist-content-display pl nil)) db))

(defvar *mpd* nil
  "The mpd connection.")

(defun g-music-mpd-init ()
  (setf *mpd* (mpd-conn-new *g-music-mpd-addr* *g-music-mpd-port*))
  (mpd-clear-playlist *mpd*))

(defun g-music--get-url (host port &optional rest)
  (concat "http://" host ":" (number-to-string port) rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode map and its handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar g-music-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") 'g-music-refresh)
    (define-key map (kbd "RET") 'g-music-complete)
    map)
  "Keymap for g-music major mode.")

(defun g-music-refresh ()
  "Reinitializes the cache and redraws the buffer."
  (interactive)
  (request (g-music--get-url *g-music-proxy-addr* *g-music-proxy-port* "/get_all_playlists")
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
  (let ((content (g-music-db-get-playlist-content playlist))
        (content-display (g-music-db-get-playlist-content-display playlist))
        (url (g-music-db-get-playlist-url playlist)))
    (if (not (null content-display))
        (progn (g-music-db-clear-all-playlist-content-display *db*)
               (g-music-buffer-setup))
      (if (null content)
          (request url
                   :parser 'buffer-string
                   :success (cl-function (lambda (&key data &allow-other-keys)
                                           (g-music-extm3u-update-playlist-content playlist data)
                                           (g-music-db-exclusive-set-playlist-content-display playlist *db*)
                                           (g-music-buffer-setup))))
        (progn (g-music-db-exclusive-set-playlist-content-display playlist *db*)
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
  (let ((content nil))
    (g-music-extm3u-parse (-lambda ((name url)) (setq content (cons (cons name url) content))) data)
    (g-music-db-set-playlist-content playlist (reverse content))))

(defun g-music-extm3u-update-playlists (data)
  "Parses the playlist extm3u data and updates the *db*."
  (g-music-extm3u-parse (-lambda ((name url))
                          (g-music-db-set-playlist (g-music-db-create-playlist name url) *db*))
                        data))

(defun g-music-extm3u-parse (fn data)
  "Parses the given extm3u string and calls fn with every match.
So it calls fn with (\"song1\" \"http://song1\")"
  (let ((regex "^#EXTINF:.*[0-9]+,\\(.+\\)
\\(http.+\\)$"))
    (g-music-match-regex data regex fn)))

(defun g-music-match-regex (str regex fn)
  "Calls fn with the matching groups of regex in str one by one."
  (let ((start 0)
        (data  '()))
    (save-match-data
      (while (string-match regex str start)
             (-let* ((all-match-pos (-partition-in-steps 2 2 (match-data t)))
                     ((_ new-start) (car all-match-pos))
                     (group-matches (-map (-lambda ((start end)) (substring str start end)) (cdr all-match-pos))))
               (funcall fn group-matches)
               (setq start new-start))))))
                             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun g-music-buffer-setup ()
  "Render the file browser in the *GMusic* buffer."
  (setq orig-point (point))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (g-music-print-header)

  (-map (lambda (pl)
          (let ((content (g-music-db-get-playlist-content pl))
                (display (g-music-db-get-playlist-content-display pl)))
            (g-music-create-playlist-widget pl)
            (when (and (not (null content))
                       display)
              (-map-indexed 'g-music-create-song-widget content))))
        *db*)

  (widget-setup)
  (goto-char orig-point))

(defun g-music-print-header ()
  (widget-insert "GMusic Connection: ")
  (widget-insert (g-music--get-url *g-music-proxy-addr* *g-music-proxy-port*))
  (widget-insert "\n")
  (widget-insert "MPD Connection:    ")
  (widget-insert (g-music--get-url *g-music-mpd-addr* *g-music-mpd-port*))
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
                             (g-music-refresh-playlist-content (widget-get widget :tag)))
                   (concat "* " value))))

(defun g-music-create-song-widget (pos song)
  (-let (((name . url) song))
    (widget-create 'link
                   :button-prefix ""
                   :button-suffix ""
                   :button-face 'g-music-playlist-content-face
                   :format "%[%v%]\n"
                   :tag url
                   :pos pos
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
  (g-music-mpd-init)
  
  (setq deft-window-width (window-width))
  (remove-overlays)
  (g-music-buffer-setup)
  (goto-char 1)
  (forward-line 2)
  
  (run-hooks 'g-music-mode-hook))

;;;###autoload
(defun g-music ()
  "Switch to *GMusic* buffer and load playlists."
  (interactive)
  (switch-to-buffer *g-music-buffer*)
  (if (not (eq major-mode 'g-music-mode))
      (g-music-mode)))

(provide 'g-music)

;;; g-music.el --- Support for google music all access-*- lexical-binding: t -*-

;; Copyright (C) 2015 David Nabraczky

;; Author: David Nabraczky <david.nabraczky@gmail.com>
;; Created: 23 May 2015
;; Keywords: google music
;; Version: 0.0.1
;; Package-Requires: ((request) (dash) (libmpdee) (s))

;; This file is not part of GNU Emacs.

;; This file is free software…
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'wid-edit)

(require 's)
(require 'dash)
(require 'request)
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
  "List of functions to call when entering g-music mode.")

(defvar *g-music-proxy-addr* "127.0.0.1"
  "GMusicProxy host.")
(defvar *g-music-proxy-port* 9999
  "GMusicProxy port.")

(defvar *g-music-mpd-addr* "127.0.0.1"
  "MPD host.")
(defvar *g-music-mpd-port* 6600
  "MPD port.")

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
  "Initializes the g-music db with the default entries."
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
  "Creates a new MPD connection and clears the default MPD playlist."
  (setf *mpd* (mpd-conn-new *g-music-mpd-addr* *g-music-mpd-port*))
  (mpd-clear-playlist *mpd*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: make the proxy command configurable
;; TODO: wait for the process to initialize
;; TODO: check if we alredy have a running instance
(defun g-music-start-proxy ()
  (start-process "GMusicProxy" "*GMusicProxy*" "GMusicProxy")
  (display-buffer "*GMusicProxy*"))

(defun g-music-mpd-url (&optional rest)
  (g-music--get-url *g-music-mpd-addr* *g-music-mpd-port* rest))

(defun g-music-proxy-url (&optional rest)
  (g-music--get-url *g-music-proxy-addr* *g-music-proxy-port* rest))

(defun g-music--get-url (host port &optional rest)
  (concat "http://" host ":" (number-to-string port) rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar g-music-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'g-music-play-pause-toggle)
    (define-key map (kbd "s") 'g-music-stop)
    (define-key map (kbd "G") 'g-music-refresh)
    (define-key map (kbd "RET") 'g-music-complete)
    map)
  "Keymap for g-music major mode.")

(defun g-music-stop ()
  "Stops the playback of the active song widget."
  (interactive)
  (mpd-stop *mpd*)
  (g-music-reflect-mpd-status))

(defun g-music-play-pause-toggle ()
  "Toggles play/pause on the active song widget"
  (interactive)
  (g-music-song-widget-notify-handler *active-song-widget*))

(defun g-music-refresh ()
  "Reinitializes the db and redraws the buffer."
  (interactive)
  (request (g-music-proxy-url "/get_all_playlists")
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
;; MPD Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun g-music-mpd-setup (mpd-conn db)
  "Reinitializes the active MPD playlist based on the current state of the db"
  (mpd-clear-playlist mpd-conn)
  (-each
      (-filter (lambda (pl) (g-music-db-get-playlist-content-display pl)) db)
    (lambda (pl) (g-music-mpd-enqueue-playlist mpd-conn pl))))

(defun g-music-mpd-enqueue-playlist (mpd-conn playlist)
  "Enqueues the given playlist's content into the default MPD playlist."
  (let ((content (g-music-db-get-playlist-content playlist)))
    (-each content (-lambda ((name . url)) (mpd-enqueue mpd-conn url)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *active-song-widget* nil
  "The active song widget that is selected for action or played by MPD.")

(defun g-music-buffer-setup ()
  "Renders the db in the *GMusic* buffer."
  (setq orig-point (point))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (g-music-print-header)

  (let* ((have-displayable-content (-any-p (lambda (pl) (g-music-db-get-playlist-content-display pl)) *db*))
         (widgets                  (-map (lambda (pl)
                                           (let ((pl-content         (g-music-db-get-playlist-content pl))
                                                 (pl-display-content (g-music-db-get-playlist-content-display pl))
                                                 (pl-widget          (g-music-create-playlist-widget pl have-displayable-content)))
                                             (when pl-display-content
                                               (widget-put pl-widget
                                                           :children
                                                           (-map-indexed
                                                            (-lambda (pos song)
                                                              (g-music-create-song-widget pl-widget pos song))
                                                            pl-content)))
                                             pl-widget))
                                         *db*)))

    (widget-setup)
    (goto-char orig-point)
    widgets))

(defun g-music-print-header ()
  (widget-insert "GMusic Connection: ")
  (widget-insert (g-music-proxy-url))
  (widget-insert "\n")
  (widget-insert "MPD Connection:    ")
  (widget-insert (g-music-mpd-url))
  (widget-insert "\n\n"))

(defun g-music-create-playlist-widget (playlist is-default-disabled)
  (let* ((value (g-music-db-get-playlist-name playlist))
         (display (g-music-db-get-playlist-content-display playlist))
         (widget (widget-create 'link
                                :button-prefix ""
                                :button-suffix ""
                                :button-face 'g-music-playlist-face
                                :format "%[%v%]\n"
                                :tag playlist
                                :help-echo "Expand this playlist"
                                :notify (lambda (widget &rest ignore)
                                          (g-music-playlist-widget-notify-handler widget))
                                (concat "* " value))))
    ;; by default the widget should be disabled
    ;; but if we have content to display activate it
    (if (and is-default-disabled (not display))
        (widget-apply widget :deactivate)
      (widget-apply widget :activate))
    widget))

(defun g-music-playlist-widget-notify-handler (widget)
  "Retrieves the content of the given playlist, updates the cache and redraws the buffer."
  (let* ((playlist        (widget-get widget :tag))
         (content         (g-music-db-get-playlist-content playlist))
         (content-display (g-music-db-get-playlist-content-display playlist))
         (url             (g-music-db-get-playlist-url playlist)))
    (cond
     ;;no playlist content, so request it
     ((null content)
      (request url
               :parser 'buffer-string
               :success (cl-function (lambda (&key data &allow-other-keys)
                                       (g-music-extm3u-update-playlist-content playlist data)
                                       (g-music-playlist-widget-notify-handler widget)))))
     ;;content-display is true, let's hide it
     ((not (null content-display))
      (g-music-db-clear-all-playlist-content-display *db*)
      (g-music-mpd-setup *mpd* *db*)
      (g-music-buffer-setup)
      (setf *active-song-widget* nil))
     ;;content-display is false, let's show it
     ((null content-display)
      (g-music-db-exclusive-set-playlist-content-display playlist *db*)
      (g-music-mpd-setup *mpd* *db*)
      (let* ((widgets (g-music-buffer-setup))
             (new-widget (-first (lambda (w) (equal playlist (widget-get w :tag))) widgets)))
        (setf *active-song-widget* (car (widget-get new-widget :children))))
      (g-music-reflect-mpd-status)))))

(defun g-music-create-song-widget (parent-widget pos song)
  (-let (((name . url) song))
    (widget-create 'link
                   :button-prefix ""
                   :button-suffix ""
                   :button-face 'g-music-playlist-content-face
                   :format "%[%v%]\n"
                   :tag url
                   :pos pos
                   :parent parent-widget
                   :help-echo "Play this song."
                   :notify (lambda (widget &rest ignore)
                             (g-music-song-widget-notify-handler widget))
                   (concat "   " name))))

(defun g-music-song-widget-notify-handler (widget)
  (let* ((mpd-status-info  (mpd-get-status *mpd*))
         (mpd-state        (plist-get mpd-status-info 'state))
         (mpd-song-pos     (plist-get mpd-status-info 'song))
         (actual-song-pos  (widget-get widget :pos)))
    (cond
     ((equal mpd-state 'play)
      (if (equal mpd-song-pos actual-song-pos)
          ;; pause the current song
          ;; update the UI to show the new song state
          (progn (mpd-pause *mpd*)
                 (g-music-song-widget-set-marker widget 'pause))
        ;; it is a new song -> stop the current playback and start to play the new song
        ;; update the UI to show the new state
        (g-music-song-play-other widget)))
     ((equal mpd-state 'pause)
      (if (equal mpd-song-pos actual-song-pos)
          ;; resume the play of the current song
          ;; update the UI to show the new song state
          (progn (mpd-pause *mpd*)
                 (g-music-song-widget-set-marker widget 'play))
        ;; it is a new song -> stop the current playback and start to play the new song
        ;; update the UI to show the new state
        (g-music-song-play-other widget)))
     ((equal mpd-state 'stop)
      ;; start to play the song at the current position
      ;; update the UI to show the new song state
      (g-music-song-widget-clear-marker *active-song-widget*)
      (mpd-play *mpd* actual-song-pos)
      (g-music-song-widget-set-marker widget 'play)))
    (widget-setup)
    (setf *active-song-widget* widget)))

(defun g-music-song-play-other (new-widget)
  (mpd-stop *mpd*)
  (mpd-play *mpd* (widget-get new-widget :pos))
  (g-music-song-widget-clear-marker *active-song-widget*)
  (g-music-song-widget-set-marker new-widget 'play))

(defun g-music-reflect-mpd-status ()
  (when *active-song-widget*
    (let* ((mpd-status       (mpd-get-status *mpd*))
           (mpd-state        (plist-get mpd-status 'state))
           (mpd-song-pos     (plist-get mpd-status 'song))
           (active-song-pos  (widget-get *active-song-widget* :pos)))
      (with-current-buffer *g-music-buffer*
        (when (and (not (null mpd-song-pos))
                   (not (equal mpd-song-pos active-song-pos)))
          (g-music-song-widget-clear-marker *active-song-widget*)
          (let* ((active-playlist-widget (widget-get *active-song-widget* :parent))
                 (mpd-song-widget        (-first
                                          (lambda (w) (equal (widget-get w :pos) mpd-song-pos))
                                          (widget-get active-playlist-widget :children))))
            (setf *active-song-widget* mpd-song-widget)))
        (g-music-song-widget-set-marker *active-song-widget* mpd-state)
        (widget-setup)))))

(defun g-music-song-widget-clear-marker (widget)
  (let ((widget-name (g-music-song-widget-get-name widget)))
    (widget-value-set widget (concat "   " widget-name))))

(defun g-music-song-widget-set-marker (widget state)
  (let ((widget-name  (g-music-song-widget-get-name widget)))
       (cond
        ((equal state 'play)
         (widget-value-set widget (concat ">  " widget-name)))
        ((equal state 'pause)
         (widget-value-set widget (concat "|| " widget-name)))
        ((equal state 'stop)
         (widget-value-set widget (concat "#  " widget-name))))))

(defun g-music-song-widget-get-name (widget)
  (let ((value (widget-get widget :value)))
    (s-chop-prefix (s-left 3 value) value)))

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
  (forward-line 3)

  (run-at-time t 15 'g-music-reflect-mpd-status)
  
  (run-hooks 'g-music-mode-hook))

;;;###autoload
(defun g-music ()
  "Switch to *GMusic* buffer and load playlists."
  (interactive)
  (switch-to-buffer *g-music-buffer*)
  (if (not (eq major-mode 'g-music-mode))
      (g-music-mode)))

(provide 'g-music)

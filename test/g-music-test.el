;;-*- lexical-binding: t -*-

(require 'cl)
(require 'cl-lib)
(require 'el-mock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; db tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ert-deftest db-test/g-music-db-init ()
  "Should initialize the db with the default collection playlist"
  (g-music-db-init)
  (should (not (equal (g-music-db-get-playlist "collection" g-music-*db*)
                      nil))))

(ert-deftest db-test/g-music-db-get-playlist ()
  "Should find the list element whose playlist name matches"
  (let ((db (list (list :plname "collection"
                        :plurl "http://testproxy:9999/get_collection"
                        :content nil)
                  (list :plname "jazz"
                        :plurl "http://testproxy:9999/get_playlist?id=1"
                        :content nil)
                  (list :plname "jazz bonus"
                        :plurl "http://testproxy:9999/get_playlist?id=2"
                        :content nil))))
    (should (equal (cl-getf (cl-second db) :plurl)
                   (cl-getf (g-music-db-get-playlist "jazz" db) :plurl)))))

(ert-deftest db-test/g-music-db-set-playlist ()
  "Should insert the given playlist into the db"
  (reinit-g-music-db)
  (let* ((new-playlist (list :plname "new"
                             :plurl "http://new"
                             :content nil))
         (db (g-music-db-set-playlist new-playlist nil)))
    (should (equal new-playlist
                   (g-music-db-get-playlist "new" db)))))

(ert-deftest db-test/g-music-db-set-playlist--side-effect ()
  "Should update the gloabl *db* during setup"
  (reinit-g-music-db)
  (let ((new-playlist (list :plname "new"
                            :plurl "http://new"
                            :content nil)))
    (g-music-db-set-playlist new-playlist g-music-*db*)
    (should (equal new-playlist
                   (g-music-db-get-playlist "new" g-music-*db*)))))

(ert-deftest db-test/g-music-db-get-playlist-name ()
  "Should return the name of the given playlist"
  (let ((playlist (list :plname "playlist1")))
    (should (equal (cl-getf playlist :plname)
                   (g-music-db-get-playlist-name playlist)))))

(ert-deftest db-test/g-music-db-get-playlist-url ()
  "Should return the url of the given playlist"
  (let ((playlist (list :plurl "http://playlist1")))
    (should (equal (cl-getf playlist :plurl)
                   (g-music-db-get-playlist-url playlist)))))

(ert-deftest db-test/g-music-db-get-playlist-content ()
  "Should return all the songs associated with the given playlist"
  (let ((playlist (list :content (cons "song1" "http://song1"))))
    (should (equal (cl-getf playlist :content)
                   (g-music-db-get-playlist-content playlist)))))

(ert-deftest db-test/g-music-db-set-playlist-content ()
  "Should set the songs for the given playlist"
  (let ((playlist (list :content nil))
        (new-content (cons "song" "http://song")))
    (g-music-db-set-playlist-content playlist new-content)
    (should (equal new-content
                   (g-music-db-get-playlist-content playlist)))))

(ert-deftest db-test/g-music-db-exclusive-set-playlist-content-display ()
    "Should set the given playlist's flag to true, all the others should be false"
  (let* ((db (list (list :plname "pl1" :content-display t)
                   (list :plname "pl2" :content-display nil)
                   (list :plname "pl3" :content-display nil)))
         (pl1 (g-music-db-get-playlist "pl1" db))
         (pl3 (g-music-db-get-playlist "pl3" db)))
    (g-music-db-exclusive-set-playlist-content-display pl3 db)
    (should (and (equal (g-music-db-get-playlist-content-display pl1)
                        nil)
                 (equal (g-music-db-get-playlist-content-display pl3)
                        t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ert-deftest utils-test/g-music--get-url ()
  "Should return http://addr:port when addr is a string port is a number"
  (should (equal "http://127.0.0.1:6600"
                 (g-music--get-url "127.0.0.1" 6600))))

(ert-deftest utils-test/g-music--get-url--rest ()
  "Should return http://addr:port/rest if rest is provided"
  (should (equal "http://127.0.0.1:6600/get"
                 (g-music--get-url "127.0.0.1" 6600 "/get"))))

(ert-deftest utils-test/g-music--get-url--non-string-addr ()
  "Should throw an error if addr is not a string"
  (should-error (g-music--get-url 127001 6600)))

(ert-deftest utils-test/g-music--get-url--non-number-port ()
  "Should throw an error if port is not a number"
  (should-error (g-music--get-url "something" "port")))

(ert-deftest utils-test/g-music--get-url--non-string-rest ()
  "Should throw an error if rest is not a string"
  (should-error (g-music--get-url "localhost" 1024 1)))

(ert-deftest utils-test/g-music-mpd-url ()
  "Should return http://localhost:1024"
  (setf *g-music-mpd-addr* "localhost")
  (setf *g-music-mpd-port* 1024)
  (should (equal "http://localhost:1024"
                 (g-music-mpd-url))))

(ert-deftest utils-test/g-music-mpd-url--rest ()
  "Should return http://remote:9999/get_something"
  (setf *g-music-mpd-addr* "remote")
  (setf *g-music-mpd-port* 9999)
  (should (equal "http://remote:9999/get_something"
                 (g-music-mpd-url "/get_something"))))

(ert-deftest utils-test/g-music-proxy-url ()
  "Should return http://localproxy:1111"
  (setf *g-music-proxy-addr* "localproxy")
  (setf *g-music-proxy-port* 1111)
  (should (equal "http://localproxy:1111"
                 (g-music-proxy-url))))

(ert-deftest utils-test/g-music-proxy-url--rest ()
  "Should return http://remoteproxy:2222/something"
  (setf *g-music-proxy-addr* "remoteproxy")
  (setf *g-music-proxy-port* 2222)
  (should (equal "http://remoteproxy:2222/something"
                 (g-music-proxy-url "/something"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extm3u tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ert-deftest extm3u-test/g-music-match-regex--group-1 ()
  "Should return the one and only group match"
  (let ((str "1 orange")
        (re  "\\([0-9]\\)")
        (result nil))
    (g-music-match-regex str re (-lambda ((x)) (setq result (cons x result))))
    (should (equal '("1")
                   result))))

(ert-deftest extm3u-test/g-music-match-regex--group-2 ()
  "Should return 2 group matches"
  (let ((str "1 orange, 2 apples")
        (re  "\\([0-9]\\)")
        (result nil))
    (g-music-match-regex str re (-lambda ((x)) (setq result (cons x result))))
    (should (equal '("2" "1")
                   result))))

(ert-deftest extm3u-test/g-music-match-regex--single-extm3u-entry ()
  "Should return 2 group matches of extm3u entry"
  (let ((str "#EXTINF:-1,Jazz bonus set 1
http://192.168.2.115:9999/get_playlist?id=df2f1c84-3bdb-4420-a656-65e21d9ea3b3")
        (re  "^#EXTINF:.*[0-9]+,\\(.+\\)
\\(http.+\\)$")
        (result nil))
    (g-music-match-regex str re (-lambda ((name url)) (setq result (cons name (cons url result)))))
    (should (equal '("Jazz bonus set 1" "http://192.168.2.115:9999/get_playlist?id=df2f1c84-3bdb-4420-a656-65e21d9ea3b3")
                   result))))

(ert-deftest extm3u-test/g-music-match-regex--two-extm3u-entries ()
  "Should return 4 group matches of 2 extm3u entries"
  (let ((str "#EXTINF:-1,Jazz bonus set 1
http://192.168.2.115:9999/get_playlist?id=df2f1c84-3bdb-4420-a656-65e21d9ea3b3
#EXTINF:-1,Jazz
http://192.168.2.115:9999/get_playlist?id=594dc46c-77df-4d83-beae-3f3043ad3133")
        (re  "^#EXTINF:.*[0-9]+,\\(.+\\)
\\(http.+\\)$")
        (result nil))
    (g-music-match-regex str re (-lambda ((name url)) (setq result (cons name (cons url result)))))
    (should (equal '("Jazz" "http://192.168.2.115:9999/get_playlist?id=594dc46c-77df-4d83-beae-3f3043ad3133"
                     "Jazz bonus set 1" "http://192.168.2.115:9999/get_playlist?id=df2f1c84-3bdb-4420-a656-65e21d9ea3b3")
                   result))))

(ert-deftest extm3u-test/g-music-extm3u-update-playlists ()
  "Should update the global *db* instance from the given extm3u data"
  (reinit-g-music-db)
  (let ((data "#EXTINF:-1,Jazz bonus set 1
http://192.168.2.115:9999/get_playlist?id=df2f1c84-3bdb-4420-a656-65e21d9ea3b3
#EXTINF:-1,Jazz
http://192.168.2.115:9999/get_playlist?id=594dc46c-77df-4d83-beae-3f3043ad3133"))
    (g-music-extm3u-update-playlists data)
    (should (equal '("Jazz" "Jazz bonus set 1")
                   (-map 'g-music-db-get-playlist-name g-music-*db*)))))

(ert-deftest extm3u-test/g-music-extm3u-update-playlist-content ()
  "Should update the given playlist's content based on the extm3u data."
  (let ((playlist (list :plname "pl1" :plurl "http://pl1" :content nil))
        (data "#EXTM3U
#EXTINF:192,Count Basie - Blues In Hoss's Flat
http://192.168.2.115:9999/get_song?id=Twecus4qdtuz3pbsv325jskgk4u
#EXTINF:307,Charlie Parker - Now's The Time
http://192.168.2.115:9999/get_song?id=Tjk22ydqo4cvklyoxgcoche4moq"))
    (g-music-extm3u-update-playlist-content playlist data)
    (should (equal '("Count Basie - Blues In Hoss's Flat" "Charlie Parker - Now's The Time")
                   (-map (-lambda ((name . _)) name) (g-music-db-get-playlist-content playlist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mpd tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ert-deftest mpd-test/g-music-mpd-enqueue-playlist ()
  "Should call the mpd-enqueue mock for every song in the playlist"
  (let ((playlist (list :content (list '("song1" . "url1")))))
    (with-mock
     (mock (mpd-enqueue * *) :times 1)
     (g-music-mpd-enqueue-playlist nil playlist))))

(ert-deftest mpd-test/g-music-mpd-setup ()
  "Should call mpd-clear-playlist once and mpd-enqueue for each song"
  (let ((db (list (list :content-display t :content (list '("song1" . "url1")
                                                          '("song2" . "url2")))
                  (list :content-display nil :content (list '("song3" . "url3"))))))
    (with-mock
     (mock (mpd-clear-playlist *) :times 1)
     (mock (mpd-enqueue * *) :times 2)
     (g-music-mpd-setup nil db))))

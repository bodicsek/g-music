;;-*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; db tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ert-deftest db-test/g-music-db-init ()
  "Should initialize the db with the default collection playlist"
  (g-music-db-init)
  (should (not (equal (g-music-db-get-playlist "collection" *db*)
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
    (g-music-db-set-playlist new-playlist *db*)
    (should (equal new-playlist
                   (g-music-db-get-playlist "new" *db*)))))

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

(ert-deftest db-test/g-music-sb-set-playlist-content ()
  "Should set the songs for the given playlist"
  (let ((playlist (list :content nil))
        (new-content (cons "song" "http://song")))
    (g-music-db-set-playlist-content playlist new-content)
    (should (equal new-content
                   (g-music-db-get-playlist-content playlist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extm3u tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ert-deftest extm3u-test/g-music-match-regex--group-1 ()
  "Should return the one and only group match"
  (let ((str "1 orange")
        (re  "\\([0-9]\\)"))
    (should (equal '("1")
                   (g-music-match-regex str re 0)))))

(ert-deftest extm3u-test/g-music-match-regex--group-2 ()
  "Should return 2 group matches"
  (let ((str "1 orange, 2 apples")
        (re  "\\([0-9]\\)"))
    (should (equal '("1" "2")
                   (g-music-match-regex str re 0)))))

(ert-deftest extm3u-test/g-music-match-regex--single-extm3u-entry ()
  "Should return 2 group matches of extm3u entry"
  (let ((str "#EXTINF:-1,Jazz bonus set 1
http://192.168.2.115:9999/get_playlist?id=df2f1c84-3bdb-4420-a656-65e21d9ea3b3")
        (re  "^#EXTINF:.*[0-9]+,\\(.+\\)
\\(http.+\\)$"))
    (should (equal '("Jazz bonus set 1" "http://192.168.2.115:9999/get_playlist?id=df2f1c84-3bdb-4420-a656-65e21d9ea3b3")
                   (g-music-match-regex str re 0)))))

(ert-deftest extm3u-test/g-music-match-regex--two-extm3u-entries ()
  "Should return 4 group matches of 2 extm3u entries"
  (let ((str "#EXTINF:-1,Jazz bonus set 1
http://192.168.2.115:9999/get_playlist?id=df2f1c84-3bdb-4420-a656-65e21d9ea3b3
#EXTINF:-1,Jazz
http://192.168.2.115:9999/get_playlist?id=594dc46c-77df-4d83-beae-3f3043ad3133")
        (re  "^#EXTINF:.*[0-9]+,\\(.+\\)
\\(http.+\\)$"))
    (should (equal '("Jazz bonus set 1" "http://192.168.2.115:9999/get_playlist?id=df2f1c84-3bdb-4420-a656-65e21d9ea3b3"
                     "Jazz" "http://192.168.2.115:9999/get_playlist?id=594dc46c-77df-4d83-beae-3f3043ad3133")
                   (g-music-match-regex str re 0)))))

(ert-deftest extm3u-test/g-music-extm3u-map ()
  "Should apply the given fn to the result list of the extm3u regex match"
  (let ((regex-result '("song1" "http://song1" "song2" "http://song2"))        
        (expected-result (list (list :plname "song1" :plurl "http://song1" :content nil)
                               (list :plname "song2" :plurl "http://song2" :content nil))))
        (should (equal expected-result
                       (g-music-extm3u-map (-lambda ((name url)) (list :plname name :plurl url :content nil)) regex-result)))))

(ert-deftest extm3u-test/g-music-extm3u-update-playlists ()
  "Should update the global *db* instance from the given extm3u data"
  (reinit-g-music-db)
  (let ((data "#EXTINF:-1,Jazz bonus set 1
http://192.168.2.115:9999/get_playlist?id=df2f1c84-3bdb-4420-a656-65e21d9ea3b3
#EXTINF:-1,Jazz
http://192.168.2.115:9999/get_playlist?id=594dc46c-77df-4d83-beae-3f3043ad3133"))
    (g-music-extm3u-update-playlists data)
    (should (equal '("Jazz bonus set 1" "Jazz")
                   (-map 'g-music-db-get-playlist-name *db*)))))

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

(ert-deftest db-test/g-music-db-get-playlist-name ()
  (let ((playlist (list :plname "playlist1")))
    (should (equal (cl-getf playlist :plname)
                   (g-music-db-get-playlist-name playlist)))))

(ert-deftest db-test/g-music-db-get-playlist-url ()
  (let ((playlist (list :plurl "http://playlist1")))
    (should (equal (cl-getf playlist :plurl)
                   (g-music-db-get-playlist-url playlist)))))

(ert-deftest db-test/g-music-db-get-playlist-content ()
  (let ((playlist (list :content (cons "song1" "http://song1"))))
    (should (equal (cl-getf playlist :content)
                   (g-music-db-get-playlist-content playlist)))))

(ert-deftest db-test/g-music-sb-set-playlist-content ()
  (let ((playlist (list :content nil))
        (new-content (cons "song" "http://song")))
    (g-music-db-set-playlist-content playlist new-content)
    (should (equal new-content
                   (g-music-db-get-playlist-content playlist)))))

;; parsing example
;; #EXTM3U
;; #EXTINF:-1,Jazz bonus set 1
;; http://192.168.2.115:9999/get_playlist?id=df2f1c84-3bdb-4420-a656-65e21d9ea3b3
;; #EXTINF:-1,Jazz bonus set 2
;; http://192.168.2.115:9999/get_playlist?id=2ebc3e7f-8f2c-4cd3-9374-028ac928deb7
;; #EXTINF:-1,Jazz deep listening 1
;; http://192.168.2.115:9999/get_playlist?id=382d6b85-9ee3-4b76-bd37-74bef74011fb
;; #EXTINF:-1,Jazz deep listening 2
;; http://192.168.2.115:9999/get_playlist?id=f435d939-2a37-437e-9114-09bfa247b8e3
;; #EXTINF:-1,Jazz
;; http://192.168.2.115:9999/get_playlist?id=594dc46c-77df-4d83-beae-3f3043ad3133


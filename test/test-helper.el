(require 'f)

(defvar test-path
  (f-dirname (f-this-file)))

(defvar code-path
  (f-parent test-path))

(require 'g-music (f-expand "g-music.el" code-path))


(in-package :zbucium-stump)

(ql:quickload :zbucium)

(defcommand zbucium-play-song (artist)
    ((:string "Artist: "))
  (when artist
    (let ((song (select-from-menu
                 (current-screen)
                 ;; Enable dynamic searching by having a list of lists
                 (mapcar #'list
                         (lastfm:artist-gettoptracks artist 20))
                 (format nil "~a songs: " artist))))
      (when song
        (bt:make-thread
         (lambda ()
           (play-song artist (first song))))))))

(defcommand zbucium-play-artist (artist random)
    ((:string "Artist: ")
     (:y-or-n "Random? "))
  (play-artist artist 20 random))

(defcommand zbucium-play-similar-artist (artist)
    ((:string "Artist: "))
  (play-artist-similar-artists artist 10 10))

(defcommand zbucium-what-is-playing () ()
  (if-let ((song (what-is-playing-as-string)))
    (message song)
    (message "Player is stopped")))

(defcommand zbucium-lyrics () ()
  (let ((*suppress-echo-timeout* t))
    (if-let ((song (what-is-playing-as-string)))
      (message (format nil "~a ~% ~a" song (song-lyrics)))
      (message "Player is stopped"))))

(defcommand zbucium-search-song (lyrics)
    ((:string "Search lyrics: "))
  (let ((selected-song
          (select-from-menu
           (current-screen)
           (mapcar (lambda (e)
                     (let ((artist (first e))
                           (song (second e))
                           (lyrics (third e)))
                       ;; Only the first element of this list is presented to
                       ;; the user, but the whole list is returned as the user
                       ;; selection
                       (list (format nil "~a - ~a: ~a" artist song lyrics)
                             artist song)))
                   (search-song lyrics)))))
    (when selected-song
      (bt:make-thread
       (lambda ()
         (play-song (second selected-song)
                    (third selected-song)))))))

(defun album-song-switcher (menu)
  (let ((selected (nth (menu-state-selected menu)
                       (menu-state-table menu))))
    (cond ((string-equal (third selected) "album")
           (setf (menu-state-table menu)
                 (mapcar (lambda (song)
                           (list song (second selected) "song"))
                         (lastfm:album-getinfo (second selected) (first selected)))))
          ((string-equal (third selected) "song")
           (setf (menu-state-table menu)
                 (mapcar (lambda (alb)
                           (list alb (second selected) "album"))
                         (lastfm:artist-gettopalbums (second selected) 5))))
          (t nil))
    (setf (menu-state-selected menu) 0)))

(defcommand zbucium-play-album (artist)
    ((:string "Artist: "))
  (let ((albums (mapcar (lambda (alb)
                          (list alb artist "album"))
                        (lastfm:artist-gettopalbums artist 5))))
    (when albums
      (let ((selected
              (select-from-menu
               (current-screen)
               albums "Play song or album: "
               0
               (let ((m (stumpwm:make-sparse-keymap)))
                 (define-key m (kbd "C-l") #'album-song-switcher)
                 m))))
        (when selected
          (if (string-equal (third selected) "album")
              (play-artist-album (second selected) (first selected))
              (play-song (second selected) (first selected))))))))

(defcommand zbucium-play/pause () ()
  (play/pause))

(defcommand zbucium-replay () ()
  (replay))

(defcommand zbucium-seek-forward () ()
  (seek 5)
  (song-progress))

(defcommand zbucium-seek-backward () ()
  (seek -5)
  (song-progress))

(defcommand zbucium-percent-pos () ()
  (message (percent-pos)))

(defun string-to-time (str)
  (let* ((time (parse-integer str))
         (minutes (round (/ time 60)))
         (seconds (mod time 60))
         (m-string (if (>= minutes 10)
                       "~a"
                       "0~a"))
         (m-seconds (if (>= seconds 10)
                        "~a"
                        "0~a")))
    (format nil (format nil "~a:~a" m-string m-seconds)
            minutes seconds)))

(defun time-pos-string ()
  (string-to-time (time-pos)))

(defun total-time-string ()
  (string-to-time (duration)))

(define-interactive-keymap seek-song ()
    ;; (:on-enter #'song-progress)
  ((kbd ",") "zbucium-seek-backward")
  ((kbd ".") "zbucium-seek-forward"))

(defun progress-bar (current total)
  (let* ((done
           (loop for i from 0 upto current
                 collect "-"))
         (remaining
           (loop for i from current upto total
                 collect " ")))
    (reduce (lambda (char rest)
              (concatenate 'string char rest))
            (append '("0:00 ") done `(,(time-pos-string)) remaining `(,(total-time-string))))))

(defun song-progress ()
  (message (progress-bar (/ (round (percent-pos)) 2) 50)))

(defcommand zbucium-time-pos () ()
  (message (time-pos)))

(defcommand zbucium-switch-to-browser () ()
  (switch-to-browser))

(defcommand zbucium-turn-video-on () ()
  (turn-video-on))

(defcommand zbucium-next-song () ()
  (next-song))

(defcommand zbucium-stop () ()
  (stop))

(defcommand zbucium-love-song () ()
  (love-song))

(defcommand zbucium-unlove-song () ()
  (unlove-song))

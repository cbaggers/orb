(in-package #:orb)

;;------------------------------------------------------------

(define-audio
  (bullets :channels 20)
  (reset :channels 2))

(setf *screen-height-in-game-units* 1200f0)

(defvar *orb* nil)
(defvar *ship* nil)
(defparameter *dirs*
  (list (list (v! 1 0) 270)
        (list (v! 0 -1) 180)
        (list (v! -1 0) 90)
        (list (v! 0 1) 0)))

(defvar *colors* '(:red :green :blue))
(defvar *since* (now))
(defvar *best* 0f0)

(defun spawn-wall ()
  (let* ((ang (* 0.5pi-f (random 4)))
         (dist 1000f0)
         (far-center (v2:*s (v2:from-angle ang) dist))
         (step-size 15f0)
         (count 150)
         (half-count (float (floor count 2) 0f0))
         (to-spawn (elt '(wall-red
                          wall-green
                          wall-blue)
                        (random 3)))
         (wibble-rand (random 2pi-f)))
    (loop
       :for i :from (- half-count) :to half-count
       :for step := (v! i (sin (+ wibble-rand (* 0.1 i))))
       :for pos := (v2:rotate (v2:*s step step-size) ang)
       :do
       (spawn to-spawn (v2:+ far-center pos)
              ;;:orb *orb*
              :ang (+ (degrees ang) 180)))))

(define-god ((missile-timer (make-stepper (seconds 30)
                                          (seconds 30)))
             (wall-timer (make-stepper (seconds 4)
                                       (seconds 4)))
             (bkg (spawn 'background (v! 0 0)) t))
  (:setup
   (when *orb*
     (kill *orb*))
   (setf *orb* (spawn 'orb (v! 0 0)))
   (play-track "media/audio/far-away.ogg")
   (when *ship*
     (kill *ship*))
   (setf *ship* (spawn 'ship (v! 0 0) :orb *orb*))
   (setf *since* (now))
   (spawn-time (v! 0 550))
   (change-state :main))
  (:main
   (setf *best* (max *best* (- (now) *since*)))
   (when (funcall missile-timer)
     (let ((mpos (v2:*s (v2:from-angle (random 2pi-f))
                        2200f0)))
       (spawn 'missile mpos;; :orb *orb*
              )))
   (when (key-down-p key.escape)
     (stop))
   (when (funcall wall-timer)
     (spawn-wall))))

(defun reset ()
  (as *god*
    (change-state :setup)))

(define-actor val-counter
    ((:visual "media/nums/nums.png")
     (:tile-count (10 1))
     (:default-depth 2)
     (multiple 1 t)
     (counts (lambda () 0f0)))
  (:main
   (set-frame (mod (floor (funcall counts) multiple) 10))))

(defun since ()
  (- (now) *since*))

(defun best ()
  *best*)

(defun spawn-time (pos)
  (setf *since* (now))
  (kill-all-of 'val-counter)

  (spawn! 'val-counter (v2:+ pos (v! 10 0)) :multiple 1
          :counts #'since)
  (spawn! 'val-counter (v2:+ pos (v! -10 0)) :multiple 10
          :counts #'since)
  (spawn! 'val-counter (v2:+ pos (v! -30 0)) :multiple 100
          :counts #'since)

  (spawn! 'val-counter (v2:+ pos (v! 10 -40)) :multiple 1
          :counts #'best)
  (spawn! 'val-counter (v2:+ pos (v! -10 -40)) :multiple 10
          :counts #'best)
  (spawn! 'val-counter (v2:+ pos (v! -30 -40)) :multiple 100
          :counts #'best))

(define-actor background ((:visual "media/background.png")
                          (:default-depth 95)))

(define-actor orb ((:visual "media/orb.png")
                   (:default-depth 70)
                   (:tile-count (3 1))
                   (swap-up nil nil)
                   (spark (each (seconds 0.05)
                            (let* ((ang (random 2pi-f))
                                   (dir (v2:from-angle ang)))
                              (spawn 'sparkle (v2:*s dir 20f0)
                                     :ang (degrees ang)
                                     :speed 15
                                     :col (get-frame)))))
                   (reset (load-audio "media/audio/start-level.wav")))
  (:setup
   (next-frame)
   (change-state :main))
  (:main
   (funcall spark)
   (setf swap-up (color-control swap-up))
   (failed reset)))

(defun color-control (swap-up)
  (flet ((spawn-stars ()
           (loop :for i :below 360 :by 5 :do
              (spawn 'dead-white (v! 0 0) :ang i))))
    (let ((left 9)
          (right 10))
      (when (and swap-up (pad-button left))
        (last-frame)
        (setf swap-up nil)
        (spawn-stars))
      (when (and swap-up
                 (or (pad-button right)
                     (key-down-p key.space)))
        (next-frame)
        (setf swap-up nil)
        (spawn-stars))
      (when (and (not (pad-button left))
                 (not (pad-button right))
                 (not (key-down-p key.space)))
        (setf swap-up t))))
  swap-up)

(defun failed (&optional sound)
  (cond
    ((and (coll-with 'wall-red)
          (/= (get-frame) 0))
     (kill-all-of 'wall-red)
     (hit-wall sound))
    ((and (coll-with 'wall-green)
          (/= (get-frame) 1))
     (kill-all-of 'wall-green)
     (hit-wall sound))
    ((and (coll-with 'wall-blue)
          (/= (get-frame) 2))
     (kill-all-of 'wall-blue)
     (hit-wall sound))))

(defun hit-wall (sound)
  (flet ((splode-all (kind-name)
           (loop :for x :across
              (daft::this-frames-actors
               (daft::get-actor-kind-by-name *current-scene* kind-name))
              :do (as x (change-state :splode)))))
    (splode-all 'wall-red)
    (splode-all 'wall-green)
    (splode-all 'wall-blue)
    (when sound
      (play-sound 'reset sound))
    (spawn-time (v! 0 550))))

;; ship VERY fps dependent
(define-actor ship ((:visual "media/ship.png")
                    (:tile-count (3 1))
                    (:default-depth 60)
                    (orb nil t)
                    (swap-up nil)
                    (vel (v! 0 0))
                    (last-shot (now))
                    (last-sound (now))
                    (bullet (load-audio "media/audio/pew.wav"))
                    (reset (load-audio "media/audio/start-level.wav"))
                    (shot-time 0.01)
                    (shot-sound-mult 3)
                    (face-move-p t t)
                    (c-up t))
  (:main
   (let* ((pad (gamepad *default-gamepad-id*))
          (dir (cond
                 ((key-down-p key.left)
                  (v! -1 0))
                 ((key-down-p key.right)
                  (v! 1 0))
                 ((key-down-p key.up)
                  (v! 0 1))
                 ((key-down-p key.down)
                  (v! 0 -1))
                 (t (gamepad-2d pad 0)))))
     (v2:incf vel (v2:*s dir (per-second 80f0)))
     (compass-dir-move vel)
     (setf vel (v2:*s vel (- 1f0 (per-second 5))))
     (if face-move-p
         (let ((ang (degrees (v2:angle-from (compass-dir) vel))))
           (turn-left (per-second (* ang 10f0))))
         (set-angle-from-analog 1 pad))
     (if (and (key-down-p key.c) c-up)
         (progn
           (setf face-move-p (not face-move-p))
           (setf c-up nil))
         (setf c-up t))
     (setf swap-up (color-control swap-up))
     (failed reset)
     (when (and (or (> (pad-1d 1 pad) 0)
                    (key-down-p key.lshift))
                (> (- (now) last-shot) shot-time))
       (when (> (- (now) last-sound) (* shot-time shot-sound-mult))
         (play-sound 'bullets bullet)
         (setf last-sound (now)))
       (setf last-shot (now))
       (spawn 'bullet (v! 17 10))
       (spawn 'bullet (v! -17 10))))))

(define-actor bullet ((:visual "media/shot.png")
                      (:default-depth 70))
  (:main
   (move-forward 10)
   (unless (in-world-p)
     (die))))

(define-actor dead-white ((:visual "media/deadWhite.png")
                          (:tile-count (1 3))
                          (:default-depth 90)
                          (ang 0f0 t)
                          (speed 500 t)
                          (do-scale
                              (then
                                (before (seconds 3)
                                  (setf (scale) (max 0 (- 1f0 %progress%))))
                                (once (die)))))
  (:setup
   (turn-left ang)
   (change-state :main))
  (:main
   (advance-frame (per-second 30))
   (move-forward (per-second speed))
   (funcall do-scale)
   (unless (in-world-p)
     (die))))

(define-actor sparkle ((:visual "media/sparkles.png")
                       (:tile-count (3 3))
                       (:default-depth 90)
                       (col 0)
                       (ang 0f0 t)
                       (speed 500 t)
                       (scale 1f0)
                       (time 1.4)
                       (do-scale nil t)
                       (range nil t))
  (:setup
   (let ((scale (float scale 0f0)))
     (setf do-scale
           (then
             (before (seconds 1.4)
               (setf (scale) (max 0 (- scale (* scale %progress%)))))
             (once (die)))))
   (turn-left ang)
   (setf range (list (* col 3)
                     (- (* (1+ col) 3) 1)))
   (set-frame (* col 3))
   (change-state :main))
  (:main
   ;;(advance-frame (per-second 30) range)
   (move-forward (per-second speed))
   (funcall do-scale)
   (unless (in-world-p)
     (die))))

(define-actor missile ((:visual "media/missile2.png")
                       (:default-depth 60))
  (:main
   (unless *orb*
     (die))
   (move-towards *orb* (per-second 25))
   (turn-towards *orb* (per-second 18))
   (when (coll-with 'bullet)
     (change-state :dying))
   (when (coll-with 'orb)
     (print "GAAME OOVER MAAAAN")
     (change-state :dying)))
  (:dying
   (let ((turn 10))
     (loop :for i :below 5 :do
        (loop :for j :below 360 :by turn :do
           (spawn 'sparkle
                  (v! 0 0)
                  :scale 2f0
                  :ang j
                  :col (random 3)
                  :speed (* (1+ i) 25f0)))))
   (die)))

(defun do-wall ()
  (when (< (random 500f0) 1f0)
    (spawn 'dead-white (v! 0 0) :speed 0f0))
  (move-forward (per-second 120f0)))

(defvar *wall-time* 30f0)

(define-actor wall-red ((:visual "media/wallPart.png")
                        (:tile-count (3 1))
                        (:default-depth 90)
                        (ang nil t)
                        (started nil t)
                        (lifespawn (after (seconds *wall-time*) t) t))
  (:setup
   (turn-left ang)
   (set-frame 0)
   (change-state :main))
  (:main
   (do-wall)
   (when (funcall lifespawn) (die)))
  (:splode
   (spawn 'dead-white (v! 0 0)
          :speed -20f0)
   (die)))

(define-actor wall-green ((:visual "media/wallPart.png")
                          (:tile-count (3 1))
                          (:default-depth 90)
                          (ang nil t)
                          (started nil t)
                          (lifespawn (after (seconds *wall-time*) t) t))
  (:setup
   (turn-left ang)
   (set-frame 1)
   (change-state :main))
  (:main
   (do-wall)
   (when (funcall lifespawn) (die)))
  (:splode
   (spawn 'dead-white (v! 0 0)
          :speed -20f0)
   (die)))

(define-actor wall-blue ((:visual "media/wallPart.png")
                         (:tile-count (3 1))
                         (:default-depth 90)
                         (ang nil t)
                         (started nil t)
                         (lifespawn (after (seconds *wall-time*) t) t))
  (:setup
   (turn-left ang)
   (set-frame 2)
   (change-state :main))
  (:main
   (do-wall)
   (when (funcall lifespawn) (die)))
  (:splode
   (spawn 'dead-white (v! 0 0)
          :speed -20f0)
   (die)))

;;------------------------------------------------------------

(defun kill-all-of (kind-name)
  ;; hack: only for dev
  (loop :for x :across
     (daft::this-frames-actors
      (daft::get-actor-kind-by-name *current-scene* kind-name))
     :do (as x (die))))

(defun names-of (kind-name)
  ;; hack: only for dev
  (loop :for x :across
     (daft::this-frames-actors
      (daft::get-actor-kind-by-name *current-scene* kind-name))
     :collect (slot-value x 'daft::debug-name)))

;;------------------------------------------------------------

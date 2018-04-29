(in-package #:orb)

;;------------------------------------------------------------

(setf daft::*system-hack* :orb)
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

(defun spawn-wall ()
  (let* ((ang (* 0.5pi-f (random 4)))
         (dist 1000f0)
         (far-center (v2:*s (v2:from-angle ang) dist))
         (step-size 15f0)
         (count 100)
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
              :orb *orb*
              :ang (+ (degrees ang) 180)))))

(define-god ((missile-timer (make-stepper (seconds 60)
                                          (seconds 60)))
             (wall-timer (make-stepper (seconds 4)
                                       (seconds 4)))
             (bkg (spawn 'background (v! 0 0)) t))
  (:setup
   (when *orb*
     (kill *orb*))
   (setf *orb* (spawn 'orb (v! 0 0)))
   (when *ship*
     (kill *ship*))
   (setf *ship* (spawn 'ship (v! 0 0) :orb *orb*))
   (setf *since* (now))
   (spawn-time (v! 0 550))
   (change-state :main))
  (:main
   (when (funcall missile-timer)
     (let ((mpos (v2:*s (v2:from-angle (random 2pi-f))
                        2200f0)))
       (spawn 'missile mpos :orb *orb*)))
   (when (funcall wall-timer)
     (spawn-wall))))

(defun reset ()
  (as *god*
    (change-state :setup)))

(define-actor time-counter
    ((:visual "media/nums/nums.png")
     (:tile-count (10 1))
     (:default-depth 2)
     (multiple 1 t))
  (:main
   (set-frame (mod (floor (- (now) *since*) multiple) 10))))

(defun spawn-time (pos)
  (setf *since* (now))
  (kill-all-of 'time-counter)
  (spawn! 'time-counter (v2:+ pos (v! 30 0)) :multiple 0.1)
  (spawn! 'time-counter (v2:+ pos (v! 10 0)) :multiple 1)
  (spawn! 'time-counter (v2:+ pos (v! -10 0)) :multiple 10)
  (spawn! 'time-counter (v2:+ pos (v! -30 0)) :multiple 100))

(define-actor background ((:visual "media/background.png")
                          (:default-depth 95))
  (:main))

(define-actor orb ((:visual "media/orb.png")
                   (:default-depth 95)
                   (:tile-count (3 1))
                   (swap-up nil nil))
  (:setup
   (next-frame)
   (change-state :main))
  (:main
   (setf swap-up (color-control swap-up))
   (failed)))

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
      (when (and swap-up (pad-button right))
        (next-frame)
        (setf swap-up nil)
        (spawn-stars))
      (when (and (not (pad-button left))
                 (not (pad-button right)))
        (setf swap-up t))))
  swap-up)

(defun failed ()
  (cond
    ((and (coll-with 'wall-red)
          (/= (get-frame) 0))
     (kill-all-of 'wall-red)
     (hit-wall))
    ((and (coll-with 'wall-green)
          (/= (get-frame) 1))
     (kill-all-of 'wall-green)
     (hit-wall))
    ((and (coll-with 'wall-blue)
          (/= (get-frame) 2))
     (kill-all-of 'wall-blue)
     (hit-wall))))

(defun hit-wall ()
  (flet ((splode-all (kind-name)
           (loop :for x :across
              (daft::this-frames-actors
               (daft::get-actor-kind-by-name *current-scene* kind-name))
              :do (as x (change-state :splode)))))
    (splode-all 'wall-red)
    (splode-all 'wall-green)
    (splode-all 'wall-blue)
    (spawn-time (v! 0 550))))

(define-actor ship ((:visual "media/ship.png")
                    (:tile-count (3 1))
                    (:default-depth 60)
                    (orb nil t)
                    (swap-up nil)
                    (vel (v! 0 0))
                    (last-shot (now)))
  (:main
   (let* ((dir (gamepad-2d (gamepad) 0)))
     (v2:incf vel (v2:*s dir (per-second 80f0)))
     (compass-dir-move vel)
     (setf vel (v2:*s vel (- 1f0 (per-second 5))))
     (let ((ang (degrees (v2:angle-from (compass-dir) vel))))
       (turn-left (per-second (* ang 10f0)))))
   (setf swap-up (color-control swap-up))
   (failed)
   (when (and (> (pad-1d 1) 0)
              (> (- (now) last-shot) 0.01))
     (setf last-shot (now))
     (spawn 'bullet (v! 17 10))
     (spawn 'bullet (v! -17 10)))))

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

(define-actor missile ((:visual "media/missile2.png")
                       (:default-depth 60)
                       (orb nil t))
  (:main
   (unless orb
     (die))
   (move-towards orb 0.3)
   (turn-towards orb 0.3)
   (when (coll-with 'bullet)
     (die))
   (when (coll-with 'orb)
     (print "GAAME OOVER MAAAAN")
     (die))))

(defun do-wall ()
  (when (< (random 500f0) 1f0)
    (spawn 'dead-white (v! 0 0) :speed 0f0))
  (move-forward 2f0)
  (unless (in-world-p)
    (die)))

(define-actor wall-red ((:visual "media/wallPart.png")
                        (:tile-count (3 1))
                        (:default-depth 90)
                        (orb nil t)
                        (ang nil t)
                        (started nil t)
                        (lifespawn (after (seconds 10) t) t))
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
                          (orb nil t)
                          (ang nil t)
                          (started nil t)
                          (lifespawn (after (seconds 10) t) t))
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
                         (orb nil t)
                         (ang nil t)
                         (started nil t)
                         (lifespawn (after (seconds 10) t) t))
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

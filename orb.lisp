(in-package #:orb)

;;------------------------------------------------------------

(setf daft::*system-hack* :orb)
(setf *screen-height-in-game-units* 1600f0)

(defvar *orb* nil)
(defvar *ship* nil)
(defparameter *dirs*
  (list (list (v! 1 0) 270)
        (list (v! 0 -1) 180)
        (list (v! -1 0) 90)
        (list (v! 0 1) 0)))

(defvar *colors* '(:red :green :blue))

(defun spawn-wall ()
  (let* ((r (random 4))
         (dir (first (elt *dirs* r)))
         (step (first (elt *dirs* (mod (1+ r) 4))))
         (ang (second (elt *dirs* r)))
         (start-pos (v2:*s (v2:negate dir) 1000f0))
         (cn (random 3))
         (col (elt '(wall-red
                     wall-green
                     wall-blue)
                   cn)))
    (loop :for i :below 120
       :for j := (* (float (- i 60) 0f0) 20) :do
       (spawn col
              (v2:+ start-pos (v2:*s step j))
              :orb *orb*
              :ang ang
              :col cn))))

(define-god ((missile-timer (make-stepper (seconds 60)
                                          (seconds 60)))
             (wall-timer (make-stepper (seconds 4)
                                       (seconds 4)))
             (bkg (spawn 'background (v! 0 0)) t))
  (:setup
   (when *orb*
     (kill *orb*))
   (setf *orb* (spawn 'orb (v! 0 0)))
   (incf (slot-value *orb* 'daft::anim-frame))
   (when *ship*
     (kill *ship*))
   (setf *ship* (spawn 'ship (v! 0 0) :orb *orb*))
   (change-state :main))
  (:main
   (when (funcall missile-timer)
     (let ((mpos (v2:*s (v2:from-angle (random 2pi-f))
                        2200f0)))
       (spawn 'missile mpos :orb *orb*)))
   (when (funcall wall-timer)
     (spawn-wall))))

(define-actor background ((:visual "media/background.png"))
  (:main))

(define-actor orb ((:visual "media/orb.png")
                   (:tile-count (3 1))
                   (swap-up nil nil))
  (:main
   (setf swap-up (color-control swap-up))
   (failed)))

(defun color-control (swap-up)
  (let ((left 9)
        (right 10))
    (when (and swap-up (pad-button left))
      (last-frame)
      (setf swap-up nil))
    (when (and swap-up (pad-button right))
      (next-frame)
      (setf swap-up nil))
    (when (and (not (pad-button left))
               (not (pad-button right)))
      (setf swap-up t)))
  swap-up)

(defun failed ()
  (cond
    ((and (coll-with 'wall-red)
          (/= (get-frame) 0))
     ;;(kill-actor-kind! 'wall-red)
     ;;(print "FAIL")
     )
    ((and (coll-with 'wall-green)
          (/= (get-frame) 1))
     ;;(kill-actor-kind! 'wall-green)
     ;;(print "FAIL")
     )
    ((and (coll-with 'wall-blue)
          (/= (get-frame) 2))
     ;;(kill-actor-kind! 'wall-blue)
     ;;(print "FAIL")
     )))

(define-actor ship ((:visual "media/ship.png")
                    (:tile-count (3 1))
                    (:default-depth 60)
                    (orb nil t)
                    (swap-up nil))
  (:main
   (setf swap-up (color-control swap-up))
   (set-position-relative-to
    orb
    (let* ((cpos (gamepad-2d (gamepad) 0)))
      (v2:*s cpos 500f0)))
   (set-angle-from-analog 1 0f0)
   (failed)
   (when (> (pad-1d 1) 0)
     (spawn 'bullet (v! 0 0)))))

(define-actor bullet ((:visual "media/shot.png"))
  (:main
   (move-forward 25)
   (unless (in-world-p)
     (die))))

(define-actor missile ((:visual "media/missile2.png")
                       (:tile-count (3 1))
                       (orb nil t))
  (:main
   (unless orb
     (die))
   (move-towards orb 0.3)
   (turn-towards orb 0.3)
   ;;(setf (scale) 2f0)
   (when (coll-with 'bullet)
     (die))
   (when (< (distance-to orb) 4)
     (die))))

(defun do-wall (orb started ang)
  (when orb
    (unless started
      (turn-left ang)
      (setf (scale) 4f0)))
   (move-forward 14f0)
   (unless (in-world-p)
     (die))
   t)

(define-actor wall-red ((:visual "media/wallPart.png")
                         (:tile-count (3 1))
                         (orb nil t)
                         (ang nil t)
                         (started nil t))
  (:main
   (set-frame 0)
   (setf started (do-wall orb started ang ))))

(define-actor wall-green ((:visual "media/wallPart.png")
                         (:tile-count (3 1))
                         (orb nil t)
                         (ang nil t)
                         (started nil t))
  (:main
   (set-frame 1)
   (setf started (do-wall orb started ang ))))

(define-actor wall-blue ((:visual "media/wallPart.png")
                         (:tile-count (3 1))
                         (orb nil t)
                         (ang nil t)
                         (started nil t))
  (:main
   (set-frame 2)
   (setf started (do-wall orb started ang ))))

;;------------------------------------------------------------

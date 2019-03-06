(in-package :orb)

;;------------------------------------------------------------

(def-shipping-manifest :orb run
  :compression #+windows nil #-windows -1
  :libs-to-include (cl-soil::soil
                    (sdl2-mixer::libsdl2-mixer :recur)
                    (sdl2::libsdl2 :recur))
  "media/")

(setf daft::*system-hack* :orb)
(set-local-path-function
 (lambda (sys path) (shipshape:local-path path sys)))

;;------------------------------------------------------------

(defun run (&optional fullscreen)
  (daft :start -1 fullscreen))

(defun stop ()
  (daft :stop))

;;------------------------------------------------------------

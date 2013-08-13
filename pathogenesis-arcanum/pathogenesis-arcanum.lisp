(in-package #:pathogenesis-arcanum)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defun init-opengl ()
  (gl:clear-color 0 0 0 0)

  ;; Initialize viewing values.
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 1 0 1 -1 1)

  (gl:enable :depth-test)
  (gl:depth-func :less)

  )

(defun display ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (gl:color 1 1 1)

  (gl:with-primitive :lines
    (gl:vertex -1.0 -1.0 0.0)
    (gl:vertex 1.0 1.0 0.0)))

(defun main ()
  (sdl:with-init ()
    (sdl:window 640 640
                :title-caption "Pathogenesis Arcanum"
                :icon-caption "Pathogenesis Arcanum"
                :opengl t
                :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1))
                :fps (make-instance 'sdl:fps-unlocked))

    (init-opengl)

    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
                       (case key
                         (:sdl-key-q (sdl:push-quit-event))))
      (:idle ()
             (display)
             ;; Start processing buffered OpenGL routines.
             (gl:flush)
             (sdl:update-display)))))

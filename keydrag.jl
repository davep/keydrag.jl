;;; keydrag.jl --- Functions for moving windows from the keyboard.
;; Copyright 1999,2000,2002 by Dave Pearson <davep@davep.org>
;; $Revision: 1.7 $

;; keydrag.jl is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; keydrag.jl provides a set of functions that allow you to drag windows
;; around using only the keyboard. To use these functions simply add this:
;;
;; (require 'keydrag)
;;
;; to your ~/.sawfishrc file and then use the sawfish keyboard bindings
;; dialog to bind the various dragging functions to keys.
;;
;; The impatient user might like to skip the above binding step and instead
;; add this:
;;
;; (keydrag-keypad-bindings)
;;
;; to their ~/.sawfishrc after the above `require'. This will result in a
;; set of numeric keypad based bindings. The various keypad keys in
;; conjunction with ALT will quickly move the window in that direction (the
;; keydrag-*-step variables control the defintion of "quickly").
;;
;; The keypad keys in conjunction with CTRL-ALT will move the window slowly,
;; allowing fine control of the position of the window.
;;
;; See the documentation for `keydrag-keypad-bindings' for more information.

;;; Thanks:
;;
;; Ryan Pavlik for pointing out that some people need the cursor to track
;; the movement of the window (which inspired the addition of the hooks).

;;; Code:

;; Things we need:

(require 'sawfish.wm.placement)

;; Customise options.

(defgroup keydrag "Keyboard window dragging")

(defcustom keydrag-horizontal-step 10
  "Number of pixels to step horizontally."
  :group     keydrag
  :type      number
  :allow-nil nil)

(defcustom keydrag-vertical-step 10
  "Number of pixels to step vertically."
  :group     keydrag
  :type      number
  :allow-nil nil)

;; Hook variables.

(defvar keydrag-before-move-hook nil
  "Evaluated before a window is moved.

Each function in the hook will be passed three parameters:

W is the window that will be moved.
H is an indication horizontal movement.
V is an indication of vertical movement.")

(defvar keydrag-after-move-hook nil
  "Evaluated after a window is moved.

Each function in the hook will be passed three parameters:

W is the window that has been moved. H is an indication horizontal movement.
V is an indication of vertical movement.")

(defvar keydrag-before-center-hook nil
  "Evaluated before a window is centered.

Each function in the hook will be passed the handle of the window that is
about the be centered.")

(defvar keydrag-after-center-hook nil
  "Evaluated after a window is centered.

Each function in the hook will be passed the handle of the window that has
just been centered.")

;; Main code.

(defun keydrag-move-window (w h v)
  "Reposition window W moving in the direction indicated by H and V."
  (call-hook 'keydrag-before-move-hook (list w h v))
  (let ((pos (window-position w)))
    (move-window-to w
                    (+ (car pos) (* h keydrag-horizontal-step))
                    (+ (cdr pos) (* v keydrag-vertical-step))))
  (call-hook 'keydrag-after-move-hook (list w h v)))

(defmacro keydrag-make-movement-command (v h slow)
  "Create a keyboard dragging command.

V should be UP, DOWN or NIL.

H should be LEFT, RIGHT or NIL.

When SLOW is non-nil the resulting command will move the window by a single
point."
  `(defun ,(intern (format nil "keydrag-%s%s%s"
                           (or v "")
                           (or h "")
                           (if slow "-slowly" ""))) (w)
    ,(format nil "%sove the current window %s."
             (if slow "Slowly m" "M")
             (if (and v h)
                 (format nil "%s and to the %s" v h)
               (format nil "%s" (or v h))))
    (interactive "%W")
    (let ,(when slow
                '((keydrag-horizontal-step 1)
                  (keydrag-vertical-step 1)))
      (keydrag-move-window w
                           ,(case h ((left) -1) ((right) 1) (t 0))
                           ,(case v ((up) -1)   ((down) 1)  (t 0))))))

;; Create all the different keydragging commands.
(let ((h '(up down ()))
      (v '(left right ())))
  (mapc (lambda (x)
          (mapc (lambda (y)
                  (when (or x y)
                    ;; This eval feels wrong but I can't call the above
                    ;; macro with variable arguments without doing this. I
                    ;; guess I'm missing something in the above macro.
                    (eval
                     `(progn
                       (keydrag-make-movement-command ,x ,y ())
                       (keydrag-make-movement-command ,x ,y t)))))
                v))
        h))

(defun keydrag-center (w)
  "Move the current window to the center of the display."
  (interactive "%W")
  (call-hook 'keydrag-before-center-hook (list w))
  ((placement-mode 'centered) w)
  (call-hook 'keydrag-after-center-hook (list w)))

(defun keydrag-keypad-bindings (#!optional keymap prefix slow-prefix)
  "Bind the dragging functions to keypad keys.

KEYMAP is the keymap to bind to keys in, if not supplied `window-keymap' is
used.

PREFIX is the key prefix to use. If not supplied \"A\" (alt) is used.

SLOW-PREFIX is the key prefix to use for slow movement. If not supplied
\"C-A\" (control alt) is used."
  (mapc (lambda (binding)
          (bind-keys (or keymap window-keymap)
                     (format nil "%s-KP_%s" (or prefix "A") (car binding))
                     (intern (format nil "keydrag-%S" (cdr binding)))
                     (format nil "%s-KP_%s" (or slow-prefix "C-A") (car binding))
                     (intern (format nil "keydrag-%S-slowly" (cdr binding)))))
        '(("7" . upleft)
          ("4" . left)
          ("1" . downleft)
          ("2" . down)
          ("3" . downright)
          ("6" . right)
          ("9" . upright)
          ("8" . up)))
  ;; There is no center-slowly, do center on its own.
  (bind-keys (or keymap window-keymap)
             (format nil "%s-KP_5" (or prefix "A"))
             'keydrag-center))

(provide 'keydrag)

;;; keydrag.jl ends here

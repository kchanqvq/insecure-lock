;;; insecure-lock --- Screen lock within Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Qiantan Hong <qhong@alum.mit.edu>
;; Maintainer: Qiantan Hong <qhong@alum.mit.edu>
;; URL: https://github.com/BlueFlo0d/insecure-lock
;; Keywords: unix screensaver security
;; Version: 0.0.0

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides an extensible screen lock framework.
;;
;; It is implemented within Emacs itself rather than interfacing with
;; underlying window system, so it is best used together with EXWM as
;; a screen locker. Otherwise, it can be used as a screen saver.

;;; Code

;;; Core

(defgroup insecure-lock nil "Screen lock within Emacs."
  :prefix "insecure-lock-" :group 'applications)
(defcustom insecure-lock-require-password t
  "If set, intercept input events and require login password to unlock.
Otherwise unlock with any key stroke, acting more like a screen saver."
  :type 'boolean :group 'insecure-lock)

(defvar insecure-lock--saved-local-map nil)
(defvar insecure-lock--saved-global-map nil)
(defvar insecure-lock-map
  (let ((map (make-keymap)))
    (keymap-substitute map 'self-insert-command 'self-insert-command global-map)
    map))
(defun insecure-lock-lock-keys ()
  "Start intercepting input events."
  (when (or insecure-lock--saved-global-map insecure-lock--saved-local-map)
    (error "Already locked keys"))
  (setq insecure-lock--saved-global-map (current-global-map)
        insecure-lock--saved-local-map (current-local-map))
  (use-global-map insecure-lock-map)
  (use-local-map (make-keymap)))
(defun insecure-lock-unlock-keys ()
  "Stop intercepting input events."
  (use-global-map insecure-lock--saved-global-map)
  (use-local-map insecure-lock--saved-local-map)
  (setq insecure-lock--saved-global-map nil
        insecure-lock--saved-local-map nil))

(defvar insecure-lock-mode-hook '(insecure-lock-blank-screen))
(define-minor-mode insecure-lock-mode
  "Global minor mode for screen lock."
  :global t)

(defvar insecure-lock-last-incorrect-attempts 0)
(defun insecure-lock-enter ()
  "Toggle on screen lock."
  (interactive)
  (setq insecure-lock-update-functions nil)
  (insecure-lock-mode)
  (when insecure-lock-update-functions
    (insecure-lock-run-update-timer))
  (if insecure-lock-require-password
      (progn
        (insecure-lock-lock-keys)
        (setq insecure-lock-last-incorrect-attempts 0)
        (while
            (not (= (with-temp-buffer
                      (ignore-error 'quit (insert (read-passwd "Password: ")))
                      (call-process-region (point-min) (point-min)
                                           "sudo" nil nil nil "-S" "-k" "true"))
                    0))
          (cl-incf insecure-lock-last-incorrect-attempts))
        (insecure-lock-unlock-keys)
        (message "%s incorrect attempts" insecure-lock-last-incorrect-attempts))
    (read-key))
  (insecure-lock-stop-update-timer)
  (insecure-lock-mode -1))

(defvar insecure-lock-update-timer nil)
(defvar insecure-lock-update-functions nil)
(defcustom insecure-lock-update-timer-interval 1
  "Interval to run `insecure-lock-update-functions'."
  :type 'number :group 'insecure-lock)
(defun insecure-lock-run-update-timer ()
  (when insecure-lock-update-timer
    (cancel-timer insecure-lock-update-timer))
  (setq insecure-lock-update-timer
        (run-at-time t insecure-lock-update-timer-interval
                     '(lambda () (run-hooks 'insecure-lock-update-functions)))))
(defun insecure-lock-stop-update-timer ()
  (when insecure-lock-update-timer
    (cancel-timer insecure-lock-update-timer)
    (setq insecure-lock-update-timer nil)))

(defvar insecure-lock-idle-timer nil)
(defun insecure-lock-run-idle (seconds)
  "Start idle timer to lock screen after SECONDS.

If SECONDS is nil or non-positive, disable idle timer."
  (interactive (list (read-number "Lock screen after idle seconds, enter 0 to disable: " 300)))
  (when insecure-lock-idle-timer
    (cancel-timer insecure-lock-idle-timer)
    (setq insecure-lock-idle-timer nil))
  (when (and seconds (> seconds 0))
    (setq insecure-lock-idle-timer
          (run-with-idle-timer seconds t 'insecure-lock-enter))))

;;; Screen Lock Modules

(defvar insecure-lock--saved-window-configuration nil)
(defun insecure-lock-blank-screen ()
  "`insecure-lock' module that blanks screen.

Display a blank buffer without modeline in place of any
displaying buffers/windows."
  (if insecure-lock-mode
      (progn
        (when insecure-lock--saved-window-configuration (error "Already blanked screen"))
        (setq insecure-lock--saved-window-configuration (current-window-configuration))
        (with-current-buffer (get-buffer-create " *Insecure Lock Blank Screen*")
          (setq-local mode-line-format nil cursor-type nil)
          (dolist (frame (frame-list))
            (display-buffer-full-frame (current-buffer) nil))))
    (set-window-configuration insecure-lock--saved-window-configuration)
    (setq insecure-lock--saved-window-configuration nil)))

(eval-after-load 'redacted
  (defun insecure-lock-redact ()
    "`insecure-lock' module that redacts buffers."
    (if insecure-lock-mode
        (dolist (frame (frame-list))
          (dolist (window (window-list frame))
            (with-current-buffer (window-buffer window)
              (redacted-mode))))
      (dolist (frame (frame-list))
        (dolist (window (window-list frame))
          (with-current-buffer (window-buffer window)
            (redacted-mode -1)))))))

(eval-after-load 'posframe
  (progn
    (defvar insecure-lock-posframe-parameters
      '(:poshandler posframe-poshandler-frame-center :internal-border-width 3))
    (defun insecure-lock-posframe-default-update-function ()
      (with-current-buffer " *Insecure Lock Screensaver*"
        (delete-region (point-min) (point-max))
        (let ((line1 (propertize (concat " " (format-time-string "%I:%M:%S %p") " ")
                                 'face '(:height 10.0)))
              (line2 (propertize (format-time-string "%a %m/%d/%Y")
                                 'face '(:height 5.0))))
          (insert line1 "\n"
                  (propertize " " 'display
                              `(space :width (,(/ (- (shr-string-pixel-width line1)
                                                     (shr-string-pixel-width line2))
                                                  2))))
                  line2))))
    (defvar insecure-lock-posframe-update-function 'insecure-lock-posframe-default-update-function)
    (defun insecure-lock-posframe ()
      "`insecure-lock' module that display a posframe."
      (if insecure-lock-mode
          (let ((buffer (get-buffer-create " *Insecure Lock Screensaver*")))
            (add-hook 'insecure-lock-update-functions insecure-lock-posframe-update-function)
            (funcall insecure-lock-posframe-update-function)
            (apply #'posframe-show buffer insecure-lock-posframe-parameters))
        (posframe-delete " *Insecure Lock Screensaver*")))))

(provide 'insecure-lock)
;;; insecure-lock.el ends here

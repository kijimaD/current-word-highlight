;;; current-word-highlight-mode.el --- Highlight the current word minor mode

;; Copyright (C) 2021 Kijima Daigo
;; Created date 2021-02-10 00:30 +0900

;; Author: Kijima Daigo <norimaking777@gmail.com>
;; Version: 1.0.1
;; Package-Version: 20210210.0030
;; Package-Commit: bceb7ce86bd6154ecd9ae9fbdb42e1d3c16bd84b
;; Keywords: highlight face convenience word
;; URL: https://github.com/kijimaD/current-word-highlight-mode

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Based on idle-highlight-mode by Phil Hagelberg, Cornelius Mika

;; M-x current-word-highlight-mode or M-x global-current-word-highlight-mode

;; Enabling it in a hook is recommended.  But you don't want it enabled
;; for all buffers, just programming ones.
;; Example:
;; (add-hook 'prog-mode-hook 'current-word-highlight-mode)

;;; Code:

(defgroup current-word-highlight nil
  "Highlight other occurrences of the word at point."
  :group 'current-word-highlight)

(defface current-word-highlight-face
  '((t (:foreground "black" :background "DeepSkyBlue")))
  "Face for main highlight."
  :group 'current-word-highlight)

(defface current-word-highlight-sub-face
  '((t (:foreground "black" :background "HotPink")))
  "Face for sub highlight."
  :group 'current-word-highlight)

(defcustom current-word-highlight-time 0.4
  "Time after which to highlight the word at point."
  :group 'current-word-highlight
  :type 'float)

(defvar current-word-highlight-global-timer nil
  "Timer to trigger highlighting.")

(defvar current-word-highlight-mode nil
  "Dummy for suppress bytecompiler warning.")

(defvar current-word-highlight-overlay-list nil)
(make-variable-buffer-local 'current-word-highlight-overlay-list)

(defun current-word-highlight-mode-maybe ()
  "Fire up `current-word-highlight-mode' if not minibuffer."
  (if (and (not (minibufferp (current-buffer))))
      (current-word-highlight-mode t)))

(defun current-word-highlight-get-current-points ()
  "Get current word beg and end.  If cursor is not on word, get next word beg and end."
  (save-excursion
      (forward-word)
      (current-word-highlight-get-before-points)))

(defun current-word-highlight-get-before-points ()
  "Get before word beg and end.  This function is used when cursor being not on word."
  (save-excursion
    (backward-word)
    (let* ((beg (point))
           '(forward-word)
           (end (point)))
      (list beg end))))

(defun current-word-highlight-light-up (beg end face)
  "Highlight when a cursor is on a word."
  (let* ((overlay (make-overlay beg end nil nil t)))
    (overlay-put overlay 'priority 1001) ; Display word-highlight before auto-highlight-symbol-mode. AHS's priority is 1000.
    (overlay-put overlay 'face face)
    (push overlay current-word-highlight-overlay-list)))

(defun current-word-highlight-unhighlight ()
  "Delete old highlight."
  (mapc 'delete-overlay current-word-highlight-overlay-list)
  (remove-hook 'pre-command-hook #'current-word-highlight-unhighlight))

(defun current-word-highlight-word-at-point ()
  "Highlight the word under the point.  If the point is not on a word, highlight the around word."
  (interactive)
  (current-word-highlight-unhighlight)
  (if current-word-highlight-mode
      (let* ((list (current-word-highlight-get-current-points))
             (beg (nth 0 list))
             (end (nth 1 list)))
        (cond ((and (<= beg (point)) (<= (point) end))
               (current-word-highlight-light-up beg end 'current-word-highlight-face))
              (t (let* ((before-list (current-word-highlight-get-before-points))
                        (before-beg (nth 0 before-list))
                        (before-end (nth 1 before-list)))
                   (current-word-highlight-light-up beg end 'current-word-highlight-sub-face)
                   (current-word-highlight-light-up before-beg before-end 'current-word-highlight-sub-face))))
        (add-hook 'pre-command-hook #'current-word-highlight-unhighlight))))

;;;###autoload
(define-globalized-minor-mode global-current-word-highlight-mode
  current-word-highlight-mode current-word-highlight-mode-maybe
  :group 'current-word-highlight)

;;;###autoload
(define-minor-mode current-word-highlight-mode
  "Current-Word-Highlight Minor Mode"
  :group 'current-word-highlight
  (if current-word-highlight-mode
      (progn (unless current-word-highlight-global-timer
               (setq current-word-highlight-global-timer
                     (run-with-idle-timer current-word-highlight-time
                                          :repeat 'current-word-highlight-word-at-point))))
    (current-word-highlight-unhighlight)))

(provide 'current-word-highlight-mode)
;;; current-word-highlight-mode.el ends here

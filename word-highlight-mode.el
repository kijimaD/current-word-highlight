;;; idle-highlight-mode.el --- highlight the word the point is on

;; Copyright (C) 2008-2011 Phil Hagelberg, Cornelius Mika

;; Author: Phil Hagelberg, Cornelius Mika
;; URL: http://www.emacswiki.org/cgi-bin/wiki/IdleHighlight
;; Package-Version: 20120920.1648
;; Package-Commit: c466f2a9e291f9da1167dc879577b2e1a7880482
;; Version: 1.1.3
;; Created: 2008-05-13
;; Keywords: convenience
;; EmacsWiki: IdleHighlight

;; This file is NOT part of GNU Emacs.

;;; License:

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

;; Based on some snippets by fledermaus from the #emacs channel.

;; M-x idle-highlight-mode sets an idle timer that highlights all
;; occurences in the buffer of the word under the point.

;; Enabling it in a hook is recommended. But you don't want it enabled
;; for all buffers, just programming ones.
;;
;; Example:
;;
;; (defun my-coding-hook ()
;;   (make-local-variable 'column-number-mode)
;;   (column-number-mode t)
;;   (if window-system (hl-line-mode t))
;;   (idle-highlight-mode t))
;;
;; (add-hook 'emacs-lisp-mode-hook 'my-coding-hook)
;; (add-hook 'ruby-mode-hook 'my-coding-hook)
;; (add-hook 'js2-mode-hook 'my-coding-hook)

;;; Code:

(defgroup idle-highlight nil
 "Highlight other occurrences of the word at point."
 :group 'idle-highlight)

(defface idle-highlight-face
  '((t (:foreground "black" :background "DarkOrange3")))
  "Face for bold text."
  :group 'idle-highlight)

(defcustom idle-highlight-exceptions '("end")
  "List of words to be excepted from highlighting."
  :group 'idle-highlight
  :type '(repeat string))

(defcustom idle-highlight-idle-time 0.1
  "Time after which to highlight the word at point."
  :group 'idle-highlight
  :type 'float)

(defvar idle-highlight-regexp nil
 "Buffer-local regexp to be idle-highlighted.")

(defvar idle-highlight-global-timer nil
 "Timer to trigger highlighting.")

(defvar current-word-overlay nil)
(make-variable-buffer-local 'current-word-overlay)

(defun highlight-current-word (beg end)
  (let* ((overlay (make-overlay beg end nil nil t)))
    (overlay-put overlay 'priority 1001) ; auto-highlight-symbol.elより前に表示させたいため。ahsのpriorityは1000なのでそれより大きくする必要がある。
    (overlay-put overlay 'face 'idle-highlight-face)
    (setq current-word-overlay overlay)))

(defun unhighlight-current-word ()
    (if current-word-overlay
        (delete-overlay current-word-overlay)))

(defun idle-highlight-word-at-point ()
  "Highlight the word under the point."
  (interactive)
  (if idle-highlight-mode
      (save-excursion
        (forward-char)
        (backward-word)
        (let* ((start (point))
               (end (+ (point) (length (current-word nil t)))))
          (unhighlight-current-word)
          (highlight-current-word start end)))))

;;;###autoload
(define-minor-mode idle-highlight-mode
  "Idle-Highlight Minor Mode"
  :group 'idle-highlight
  (if idle-highlight-mode
      (progn (unless idle-highlight-global-timer
               (setq idle-highlight-global-timer
                     (run-with-idle-timer idle-highlight-idle-time
                                          :repeat 'idle-highlight-word-at-point)))
             (set (make-local-variable 'idle-highlight-regexp) nil))
    (idle-highlight-unhighlight)))

(provide 'idle-highlight-mode)
;;; idle-highlight-mode.el ends here

;;; elfeed-goodies-split-pane.el --- Elfeed goodies: split pane support
;;
;; Copyright (c) 2015 Gergely Nagy
;;
;; Author: Gergely Nagy
;; URL: https://github.com/algernon/elfeed-goodies
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License: GPLv3+
;;
;;; Code:

(require 'elfeed-goodies)
(require 'popwin)

(defcustom elfeed-goodies/entry-pane-position 'right
  "Position of the popup entry pane."
  :group 'elfeed-goodies
  :type '(choice (const left) (const right) (const top) (const bottom)))

(defcustom elfeed-goodies/entry-pane-size 0.75
  "Size (width or height, depending on position) of the popup entry pane."
  :group 'elfeed-goodies
  :type 'number)

(defun elfeed-goodies/switch-pane (buff)
  "Display BUFF in a popup window."
  (popwin:popup-buffer buff
                       :position elfeed-goodies/entry-pane-position
                       :width (when (member elfeed-goodies/entry-pane-position '(left right))
                                elfeed-goodies/entry-pane-size)
                       :height (when (member elfeed-goodies/entry-pane-position '(top bottom))
                                 elfeed-goodies/entry-pane-size)
                       :stick t
                       :dedicated t))

(defun elfeed-goodies/delete-pane ()
  "Delete the *elfeed-entry* split pane."
  (interactive)
  (let* ((buff (get-buffer "*elfeed-entry*"))
         (window (get-buffer-window buff)))
    (kill-buffer buff)
    (delete-window window)))

(provide 'elfeed-goodies-split-pane)

;;; elfeed-goodies-split-pane.el ends here

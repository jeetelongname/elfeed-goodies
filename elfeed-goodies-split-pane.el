;;; elfeed-goodies-split-pane.el --- Elfeed goodies: split pane support
;;
;; Copyright (c) 2015, 2016 Gergely Nagy
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

(defcustom elfeed-goodies/switch-to-entry t
  "Whether to switch to the *elfeed-entry* buffer when using a split pane, or not."
  :group 'elfeed-goodies
  :type 'boolean)

(defun elfeed-goodies/switch-pane (buff)
  "Display BUFF in a popup window."
  (popwin:popup-buffer buff
                       :position elfeed-goodies/entry-pane-position
                       :width elfeed-goodies/entry-pane-size
                       :height elfeed-goodies/entry-pane-size
                       :stick t
                       :dedicated t
                       :noselect (not elfeed-goodies/switch-to-entry)))

(defun elfeed-goodies/delete-pane ()
  "Delete the *elfeed-entry* split pane."
  (interactive)
  (let* ((buff (get-buffer "*elfeed-entry*"))
         (window (get-buffer-window buff)))
    (kill-buffer buff)
    (delete-window window)))

(defun elfeed-goodies/split-search-show-entry (entry)
  "Display the currently selected item in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (elfeed-show-entry entry)))

(defun elfeed-entry-buffer ()
  (get-buffer-create "*elfeed-entry*"))

(defun elfeed-goodies/split-show-next ()
  "Show the next item in the elfeed-search buffer."
  (interactive)
  (with-current-buffer (elfeed-entry-buffer)
    (condition-case nil (funcall elfeed-show-entry-delete) (error nil)))
  (with-current-buffer (elfeed-search-buffer)
    (forward-line)
    (call-interactively #'elfeed-goodies/split-search-show-entry)))

(defun elfeed-goodies/split-show-prev ()
  "Show the previous item in the elfeed-search buffer."
  (interactive)
  (with-current-buffer (elfeed-entry-buffer)
    (condition-case nil (funcall elfeed-show-entry-delete) (error nil)))
  (with-current-buffer (elfeed-search-buffer)
    (forward-line -1)
    (call-interactively #'elfeed-goodies/split-search-show-entry)))

(provide 'elfeed-goodies-split-pane)

;;; elfeed-goodies-split-pane.el ends here

;;; elfeed-goodies.el --- Elfeed goodies
;;
;; Copyright (c) 2015 Gergely Nagy
;;
;; Author: Gergely Nagy
;; URL: https://github.com/algernon/elfeed-goodies
;; Package-Requires: ((popwin "1.0.0") (powerline "2.2") (elfeed "20151201.1742"))
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; Various bits and pieces to enhance the Elfeed user experience. Includes an
;; adaptive, powerline-based header for the `*elfeed-search*' buffer with a
;; matching entry format; and a split-pane setup.
;;
;;; License: GPLv3+

;;; Code:

(provide 'elfeed-goodies)

(require 'elfeed-goodies-search-powerline)
(require 'elfeed-goodies-split-pane)

(defgroup elfeed-goodies nil
  "Customisation group for `elfeed-goodies'."
  :group 'comm)

;;;###autoload
(defun elfeed-goodies/setup ()
  "Setup Elfeed with extras:

* Adaptive header bar and entries.
* Header bar using powerline.
* Split pane view via popwin."
  (interactive)
  (setq elfeed-search-header-function #'elfeed-goodies/search-header-draw
        elfeed-search-print-entry-function #'elfeed-goodies/entry-line-draw
        elfeed-show-entry-switch #'elfeed-goodies/switch-pane
        elfeed-show-entry-delete #'elfeed-goodies/delete-pane))

;;; elfeed-goodies.el ends here

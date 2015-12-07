;;; elfeed-goodies-logging.el --- Logging support for Elfeed goodies
;;
;; Copyright (c) 2015 Gergely Nagy
;;
;; Author: Gergely Nagy
;; URL: https://github.com/algernon/elfeed-goodies
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License: GPLv3+

;;; Code:

(require 'elfeed-goodies)
(require 'popwin)

(defcustom elfeed-goodies/log-window-position 'bottom
  "Position of the log window."
  :group 'elfeed-goodies
  :type '(choice (const left) (const right) (const top) (const bottom)))

(defcustom elfeed-goodies/log-window-size 0.25
  "Size of the log window."
  :group 'elfeed-goodies
  :type 'number)

;;;###autoload
(defun elfeed-goodies/toggle-logs ()
  "Toggle the display of Elfeed logs in a popup window."
  (interactive)

  (let* ((log-buffer (get-buffer "*elfeed-log*"))
         (log-window (get-buffer-window log-buffer)))
    (if log-window
        (delete-window log-window)
      (progn
        (with-current-buffer log-buffer
          (goto-char (point-max)))
        (popwin:popup-buffer (get-buffer "*elfeed-log*")
                             :position elfeed-goodies/log-window-position
                             :height elfeed-goodies/log-window-size
                             :width elfeed-goodies/log-window-size
                             :stick t
                             :noselect t
                             :dedicated t)))))

(provide 'elfeed-goodies-logging)

;;; elfeed-goodies-logging.el ends here

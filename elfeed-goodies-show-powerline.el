;;; elfeed-goodies-show-powerline.el --- Elfeed goodies: powerlineized show-entry
;;
;; Copyright (c) 2015 Gergely Nagy
;;
;; Author: Gergely Nagy
;; URL: https://github.com/algernon/elfeed-goodies
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; Extends Elfeed with a powerlineized, header-bar enabled entry buffer.
;;
;;; License: GPLv3+
;;
;;; Code:

(require 'elfeed)
(require 'elfeed-goodies)
(require 'powerline)
(require 'message)

(defun elfeed-goodies/entry-header-line ()
  (let* ((title (elfeed-entry-title elfeed-show-entry))
         (tags (elfeed-entry-tags elfeed-show-entry))
         (tags-str (mapconcat #'symbol-name tags ", "))
         (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (feed-title (elfeed-feed-title feed))

         (separator-left (intern (format "powerline-%s-%s"
                                         "arrow-fade"
                                         (car powerline-default-separator-dir))))
         (separator-right (intern (format "powerline-%s-%s"
                                          "arrow-fade"
                                          (cdr powerline-default-separator-dir))))
         (lhs (list
               (powerline-raw (concat " " (propertize tags-str 'face 'elfeed-search-tag-face) " ") 'powerline-active2 'r)
               (funcall separator-left 'powerline-active2 'powerline-active1)
               (powerline-raw (concat " " (propertize title 'face 'message-header-subject) " ") 'powerline-active1 'l)
               (funcall separator-left 'powerline-active1 'mode-line)))
         (rhs (list
               (funcall separator-right 'mode-line 'powerline-active1)
               (powerline-raw (concat " " (propertize feed-title 'face 'elfeed-search-feed-face) " ") 'powerline-active1)
               (funcall separator-right 'powerline-active1 'powerline-active2)
               (powerline-raw (format-time-string "%Y-%m-%d %H:%M:%S %z " date) 'powerline-active2 'l))))
    (concat
     (powerline-render lhs)
     (powerline-fill 'mode-line (powerline-width rhs))
     (powerline-render rhs))))

(defun elfeed-goodies/show-refresh--plain ()
  (interactive)
  (let* ((inhibit-read-only t)
         (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
         (type (elfeed-entry-content-type elfeed-show-entry))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
    (erase-buffer)
    (if content
        (if (eq type 'html)
            (elfeed-insert-html content base)
          (insert content))
      (insert (propertize "(empty)\n" 'face 'italic)))
    (goto-char (point-min))))

(defun elfeed-goodies/show-mode-setup ()
  (setq header-line-format '(:eval (elfeed-goodies/entry-header-line))))

(provide 'elfeed-goodies-show-powerline)

;;; elfeed-goodies-show-powerline.el ends here

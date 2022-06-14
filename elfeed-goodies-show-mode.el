;;; elfeed-goodies-show-mode.el --- Elfeed goodies for elfeed-show-mode
;;
;; Copyright (c) 2015 Gergely Nagy
;;
;; Author: Gergely Nagy
;; URL: https://github.com/algernon/elfeed-goodies
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License: GPLv3+
;;; Commentary:
;; Contains code for improved elfeed show mode.
;; - How to draw its header lines.
;; - Link hint functions to jump to links in entry.
;; - Setup function for the buffer.
;;
;;; Code:

(require 'elfeed)
(require 'elfeed-goodies)
(require 'powerline)
(require 'cl-lib)
(require 'link-hint)

(defcustom elfeed-goodies/show-mode-padding 0
  "Padding on the side of the `*elfeed-entry*' buffer, in characters."
  :group 'elfeed-goodies
  :type 'integer)

(defun elfeed-goodies/entry-header-line ()
  "Generate elfeed goodies header line.
Return a string containing powerline symbols"
  (let* ((title (elfeed-entry-title elfeed-show-entry))
         (title-faces (elfeed-search--faces (elfeed-entry-tags elfeed-show-entry)))
         (tags (elfeed-entry-tags elfeed-show-entry))
         (tags-str (mapconcat #'symbol-name tags ", "))
         (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (entry-author (elfeed-meta elfeed-show-entry :author))
         (feed-title (if entry-author
                         (concat entry-author " (" (elfeed-feed-title feed) ")")
                       (elfeed-feed-title feed)))

         (separator-left (intern (format "powerline-%s-%s"
                                         elfeed-goodies/powerline-default-separator
                                         (car powerline-default-separator-dir))))
         (separator-right (intern (format "powerline-%s-%s"
                                          elfeed-goodies/powerline-default-separator
                                          (cdr powerline-default-separator-dir))))
         (lhs (list
               (powerline-raw (concat " " (propertize tags-str 'face 'elfeed-search-tag-face) " ") 'powerline-active2 'r)
               (funcall separator-left 'powerline-active2 'powerline-active1)
               (powerline-raw (concat " " (propertize title 'face title-faces) " ") 'powerline-active1 'l)
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
  "Insert Content into Entry show buffer.
Show (empty) if there is no content."
  (interactive)
  (let* ((inhibit-read-only t)
         (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
         (type (elfeed-entry-content-type elfeed-show-entry))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
    (erase-buffer)
    (insert "\n")
    (if content
        (if (eq type 'html)
            (elfeed-insert-html content base)
          (insert content))
      (insert (propertize "(empty)\n" 'face 'italic)))
    (goto-char (point-min))))

(defun elfeed-goodies/show-link-hint ()
  "Select a link to visit with link-hint.
Wrapper around link-hint-open-link."
  (interactive)
  (link-hint-open-link))

(define-obsolete-function-alias 'elfeed-goodies/show-ace-link 'elfeed-goodies/show-link-hint "7f0ef62")

(defun elfeed-goodies/show-mode-setup ()
  "Setup function providing defaults for show mode buffer."
  (setq header-line-format '(:eval (elfeed-goodies/entry-header-line))
        left-margin-width elfeed-goodies/show-mode-padding
        right-margin-width elfeed-goodies/show-mode-padding)
  (define-key elfeed-show-mode-map (kbd "M-v") #'elfeed-goodies/show-link-hint))

(provide 'elfeed-goodies-show-mode)

;;; elfeed-goodies-show-mode.el ends here

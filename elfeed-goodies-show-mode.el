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
;;
;;; Code:

(require 'elfeed)
(require 'elfeed-goodies)
(require 'powerline)
(require 'noflet)
(require 'ace-jump-mode)

(defcustom elfeed-goodies/show-mode-padding 0
  "Padding on the side of the `*elfeed-entry*' buffer, in characters."
  :group 'elfeed-goodies
  :type 'integer)

(defun elfeed-goodies/entry-header-line ()
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

(defun elfeed-goodies/show-ace-link ()
  "Select a link to visit with ace-jump."
  (interactive)
  (noflet ((ace-jump-search-candidate (str va-list)
                                      (let ((skip (text-property-any (point-min) (point-max)
                                                                     'help-echo nil))
                                            candidates)
                                        (save-excursion
                                          (while (setq skip (text-property-not-all skip (point-max)
                                                                                   'help-echo nil))
                                            (goto-char skip)
                                            (push (make-aj-position
                                                   :offset (1- skip)
                                                   :visual-area (car va-list))
                                                  candidates)
                                            (setq skip (text-property-any (point) (point-max)
                                                                          'help-echo nil))))
                                        (nreverse candidates))))
    (setq ace-jump-mode-end-hook
          (list `(lambda ()
                   (forward-char 1)
                   (shr-browse-url))))
    (ace-jump-do "foo")))

(defun elfeed-goodies/show-mode-setup ()
  (setq header-line-format '(:eval (elfeed-goodies/entry-header-line))
        left-margin-width elfeed-goodies/show-mode-padding
        right-margin-width elfeed-goodies/show-mode-padding)
  (define-key elfeed-show-mode-map (kbd "M-v") #'elfeed-goodies/show-ace-link))

(provide 'elfeed-goodies-show-mode)

;;; elfeed-goodies-show-mode.el ends here

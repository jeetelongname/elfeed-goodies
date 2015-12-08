;;; elfeed-goodies-search-mode.el --- Elfeed goodies for the search buffer
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
(require 'cl-lib)

(defcustom elfeed-goodies/feed-source-column-width 16
  "Width of the feed source column."
  :group 'elfeed-goodies
  :type 'integer)

(defcustom elfeed-goodies/tag-column-width 24
  "Width of the tags column."
  :group 'elfeed-goodies
  :type 'integer)

(defcustom elfeed-goodies/wide-threshold 0.5
  "Minimum width of the window (percent of the frame) to start using the wide layout from."
  :group 'elfeed-goodies
  :type 'float)

(defun -pad-string-to (str width)
  "Pad `STR' to `WIDTH' characters."
  (format (format "%s%%%ds" str width) ""))

(defun -elfeed/feed-stats ()
  "Collect some Elfeed feed statistics.

Returns a list: the unread count, entry count, and feed count."
  (if (and elfeed-search-filter-active elfeed-search-filter-overflowing)
      (list 0 0 0)
    (cl-loop with feeds = (make-hash-table :test 'equal)
             for entry in elfeed-search-entries
             for feed = (elfeed-entry-feed entry)
             for url = (elfeed-feed-url feed)
             count entry into entry-count
             count (elfeed-tagged-p 'unread entry) into unread-count
             do (puthash url t feeds)
             finally
             (cl-return
              (list unread-count entry-count (hash-table-count feeds))))))

(defun -elfeed/queue-stats ()
  "Collect some stats about the queue.

Returns a list consisting of the feed count, the remaining feeds,
and the length of the active queue."
  (list (hash-table-count elfeed-db-feeds)
        (length url-queue)
        (cl-count-if #'url-queue-buffer url-queue)))

(defun search-header/rhs (separator-left separator-right search-filter stats update)
  (list
   (funcall separator-right 'mode-line 'powerline-active1)
   (powerline-raw (concat " " search-filter) 'powerline-active1 'r)
   (funcall separator-right 'powerline-active1 'powerline-active2)
   (cl-destructuring-bind (unread entry-count feed-count) stats
     (let ((content (format " %d/%d:%d " unread entry-count feed-count)))
       (when url-queue
         (cl-destructuring-bind (total-feeds queue-length in-progress) (-elfeed/queue-stats)
           (setf content (concat content (format " (* %.0f%%%%)"
                                                 (* (/ (- total-feeds (+ queue-length
                                                                         in-progress))
                                                       total-feeds 1.0) 100))))))
       (propertize content
                   'face 'powerline-active2)))
   (funcall separator-right 'powerline-active2 'powerline-active1)
   (powerline-raw (concat " " update) 'powerline-active1 'r)))

(defun search-header/draw-wide (separator-left separator-right search-filter stats db-time)
  (let* ((update (format-time-string "%Y-%m-%d %H:%M:%S %z" db-time))
         (lhs (list
               (powerline-raw (-pad-string-to "Feed" (- elfeed-goodies/feed-source-column-width 4)) 'powerline-active1 'l)
               (funcall separator-left 'powerline-active1 'powerline-active2)
               (powerline-raw (-pad-string-to "Tags" (- elfeed-goodies/tag-column-width 6)) 'powerline-active2 'l)
               (funcall separator-left 'powerline-active2 'mode-line)
               (powerline-raw "Subject" 'mode-line 'l)))
         (rhs (search-header/rhs separator-left separator-right search-filter stats update)))

    (concat (powerline-render lhs)
            (powerline-fill 'mode-line (powerline-width rhs))
            (powerline-render rhs))))

(defun search-header/draw-tight (separator-left separator-right search-filter stats db-time)
  (let* ((update (format-time-string "%H:%M:%S" db-time))
         (lhs (list
               (powerline-raw "Subject" 'mode-line 'l)))
         (rhs (search-header/rhs separator-left separator-right search-filter stats update)))
    (concat (powerline-render lhs)
            (powerline-fill 'mode-line (powerline-width rhs))
            (powerline-render rhs))))

(defun elfeed-goodies/search-header-draw ()
  "Returns the string to be used as the Elfeed header."
  (if (zerop (elfeed-db-last-update))
      (elfeed-search--intro-header)
    (let* ((separator-left (intern (format "powerline-%s-%s"
                                           elfeed-goodies/powerline-default-separator
                                           (car powerline-default-separator-dir))))
           (separator-right (intern (format "powerline-%s-%s"
                                            elfeed-goodies/powerline-default-separator
                                            (cdr powerline-default-separator-dir))))
           (db-time (seconds-to-time (elfeed-db-last-update)))
           (stats (-elfeed/feed-stats))
           (search-filter (cond
                           (elfeed-search-filter-active
                            "")
                           (elfeed-search-filter
                            elfeed-search-filter)
                           (""))))
      (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
          (search-header/draw-wide separator-left separator-right search-filter stats db-time)
        (search-header/draw-tight separator-left separator-right search-filter stats db-time)))))

(defun elfeed-goodies/entry-line-draw (entry)
  "Print ENTRY to the buffer."

  (let* ((title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (concat "[" (mapconcat 'identity tags ",") "]"))
         (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                         elfeed-goodies/tag-column-width 4))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               title-width)
                        :left))
         (tag-column (elfeed-format-column
                      tags-str (elfeed-clamp (length tags-str)
                                             elfeed-goodies/tag-column-width
                                             elfeed-goodies/tag-column-width)
                      :left))
         (feed-column (elfeed-format-column
                       feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                elfeed-goodies/feed-source-column-width
                                                elfeed-goodies/feed-source-column-width)
                       :left)))

    (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
        (progn
          (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
          (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
          (insert (propertize title 'face title-faces 'kbd-help title)))
      (insert (propertize title 'face title-faces 'kbd-help title)))))

(provide 'elfeed-goodies-search-mode)

;;; elfeed-goodies-search-mode.el ends here

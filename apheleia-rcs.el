;;; apheleia-rcs.el --- Apply RCS patches -*- lexical-binding: t -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A library to apply a RCS patch to an Emacs buffer while minimising the
;; displacement of `point'.

;;; Code:

(require 'apheleia-dp)
(require 'apheleia-log)

(require 'cl-lib)
(require 'subr-x)

(defun apheleia--map-rcs-patch (func)
  "Map over the RCS patch in the current buffer.
For each RCS patch command, FUNC is called with an alist that has
the following keys:

- `command': either `addition' or `deletion'
- `start': line number, an integer
- `lines': number of lines to be inserted or removed
- `text': the string to be inserted, only for `addition'

See <https://tools.ietf.org/doc/tcllib/html/rcs.html#section4>
for documentation on the RCS patch format."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (unless (looking-at "$\\|\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
        (error "Malformed RCS patch: %S" (point)))
      (forward-line)
      (when-let ((command (match-string 1)))
        (let ((start (string-to-number (match-string 2)))
              (lines (string-to-number (match-string 3))))
          (pcase command
            ("a"
             (let ((text-start (point)))
               (forward-line lines)
               (funcall
                func
                `((command . addition)
                  (start . ,start)
                  (lines . ,lines)
                  (text . ,(buffer-substring-no-properties
                            text-start (point)))))))
            ("d"
             (funcall
              func
              `((command . deletion)
                (start . ,start)
                (lines . ,lines))))))))))

(defcustom apheleia-max-alignment-size 400
  "Maximum size for diff regions that will have point aligned.
Apheleia uses a dynamic programming algorithm to determine where
point should be placed within a diff region, but this algorithm
has quadratic runtime so it will lock up Emacs if it is run on a
diff region that is too large. The value of this variable serves
as a limit on the input size to the algorithm; larger diff
regions will still be applied, but Apheleia won't try to move
point correctly."
  :type 'integer
  :group 'apheleia)

(defun apheleia--apply-rcs-patch (content-buffer patch-buffer)
  "Apply RCS patch.
CONTENT-BUFFER contains the text to be patched, and PATCH-BUFFER
contains the patch."
  (apheleia--log
   'rcs "Applying RCS patch from %S to %S" patch-buffer content-buffer)
  (let ((commands nil)
        (pos-list nil)
        (window-line-list nil))
    (with-current-buffer content-buffer
      (push `(:type point :pos ,(point)) pos-list)
      (when (marker-position (mark-marker))
        (push `(:type marker :pos ,(mark-marker)) pos-list))
      (dolist (m mark-ring)
        (when (marker-position m)
          (push `(:type marker :pos ,m) pos-list)))
      (dolist (w (get-buffer-window-list nil nil t))
        (push
         `(:type window-point :pos ,(window-point w) :window ,w) pos-list)
        (push (cons w (count-lines (window-start w) (point)))
              window-line-list)))
    (with-current-buffer patch-buffer
      (apheleia--map-rcs-patch
       (lambda (command)
         (with-current-buffer content-buffer
           ;; Could be optimized significantly by moving only as many
           ;; lines as needed, rather than returning to the beginning
           ;; of the buffer first.
           (save-excursion
             (goto-char (point-min))
             (forward-line (1- (alist-get 'start command)))
             ;; Account for the off-by-one error in the RCS patch spec
             ;; (namely, text is added *after* the line mentioned in
             ;; the patch).
             (when (eq (alist-get 'command command) 'addition)
               (forward-line))
             (push `(marker . ,(point-marker)) command)
             (push command commands)
             ;; If we delete a region just before inserting new text
             ;; at the same place, then it is a replacement. In this
             ;; case, check if the replaced region includes the window
             ;; point for any window currently displaying the content
             ;; buffer. If so, figure out where that window point
             ;; should be moved to, and record the information in an
             ;; additional command.
             ;;
             ;; See <https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Point.html>.
             ;;
             ;; Note that the commands get pushed in reverse order
             ;; because of how linked lists work.
             (let ((deletion (nth 1 commands))
                   (addition (nth 0 commands)))
               (when (and (eq (alist-get 'command deletion) 'deletion)
                          (eq (alist-get 'command addition) 'addition)
                          ;; Again with the weird off-by-one
                          ;; computations. For example, if you replace
                          ;; lines 68 through 71 inclusive, then the
                          ;; deletion is for line 68 and the addition
                          ;; is for line 70. Blame RCS.
                          (= (+ (alist-get 'start deletion)
                                (alist-get 'lines deletion)
                                -1)
                             (alist-get 'start addition)))
                 (let ((text-start (alist-get 'marker deletion)))
                   (forward-line (alist-get 'lines deletion))
                   (let ((text-end (point)))
                     (dolist (pos-spec pos-list)
                       (let ((p (plist-get pos-spec :pos)))
                         ;; Check if the point, or marker, or window
                         ;; point, is within the replaced region.
                         ;; Markers pretend to be numbers, so we can
                         ;; run this in any of the three cases.
                         (when (and (< text-start p)
                                    (< p text-end))
                           (let* ((old-text (buffer-substring-no-properties
                                             text-start text-end))
                                  (new-text (alist-get 'text addition))
                                  (old-relative-point (- p text-start))
                                  (new-relative-point
                                   (if (> (max (length old-text)
                                               (length new-text))
                                          apheleia-max-alignment-size)
                                       old-relative-point
                                     (apheleia--align-point
                                      old-text new-text old-relative-point))))
                             (goto-char text-start)
                             (push
                              `((command . move-cursor)
                                (cursor . ,pos-spec)
                                (offset . ,(- new-relative-point
                                              old-relative-point)))
                              commands))))))))))))))
    (with-current-buffer content-buffer
      ;; We run both `goto-char' and `set-window-point' to offset
      ;; point and window point, don't want to chance that both
      ;; changes will stack on top of each other.
      (let ((orig-point (point)))
        (dolist (command (nreverse commands))
          (pcase (alist-get 'command command)
            (`addition
             (save-excursion
               (goto-char (alist-get 'marker command))
               (insert (alist-get 'text command))))
            (`deletion
             (save-excursion
               (goto-char (alist-get 'marker command))
               (forward-line (alist-get 'lines command))
               (delete-region (alist-get 'marker command) (point))))
            (`move-cursor
             (let ((cursor (alist-get 'cursor command))
                   (offset (alist-get 'offset command)))
               (pcase (plist-get cursor :type)
                 (`point
                  (goto-char
                   (+ orig-point offset)))
                 (`marker
                  (set-marker
                   (plist-get cursor :pos)
                   (+ (plist-get cursor :pos) offset)))
                 (`window-point
                  (set-window-point
                   (plist-get cursor :window)
                   (+ orig-point offset))))))))))
    ;; Restore the scroll position of each window displaying the
    ;; buffer.
    (dolist (entry window-line-list)
      (cl-destructuring-bind (w . old-window-line) entry
        (let ((new-window-line
               (count-lines (window-start w) (point))))
          (with-selected-window w
            ;; Sometimes if the text is less than a buffer long, and
            ;; we do a deletion, it might not be possible to keep the
            ;; vertical position of point the same by scrolling.
            ;; That's okay. We just go as far as we can.
            (ignore-errors
              (scroll-down (- old-window-line new-window-line)))))))))

(provide 'apheleia-rcs)

;;; apheleia-rcs.el ends here

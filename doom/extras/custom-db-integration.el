;;; custom-db-integration.el -*- lexical-binding: t; -*-


;; This file integrates your custom dashboard sections with Doom's dashboard
;; Place this in your doom config directory and load it after doom-dashboard

;; +-------------+
;; |  Variables  |
;; +-------------+
(defvar +my-dashboard-width 80
  "Default width used as a fallback when centering content.")

(defvar +my-dashboard-scratch-pad-file "~/.config/doom/extras/db-scratchpad.org"
  "Path to the file containing the Scratch Pad content.")

;; +-------------+
;; |  Functions  |
;; +-------------+
(defun +my-dashboard--center (s)
  "Return string S centered relative to dashboard width."
  (let* ((target-width +doom-dashboard--width) ;; Use Doom's width variable
         (pad (max 0 (/ (- target-width (length s)) 2))))
    (concat (make-string pad ?\s) s)))

(defun +my-dashboard-pad (s width)
  "Pad string S to WIDTH characters."
  (format (concat "%-" (number-to-string width) "s") s))

(defun +my-dashboard-insert-2cols (left-items right-items left-fn right-fn &optional col-width)
  (let* ((col-width (or col-width
                        (max 1
                             (apply #'max
                                    (mapcar (lambda (x) (string-width (car x)))
                                            (append left-items right-items))))))
         (gap 10) ;; space between columns
         (total-width (+ (* 2 col-width) gap))
         (side-pad (max 0 (/ (- +doom-dashboard--width total-width) 2)))
         (rows (max (length left-items) (length right-items))))
    (dotimes (i rows)
      (insert (make-string side-pad ?\s))
      (let ((left (nth i left-items))
            (right (nth i right-items)))
        (when left
          (funcall left-fn left col-width))
        (insert (make-string gap ?\s))
        (when right
          (funcall right-fn right col-width)))
      (insert "\n"))))

;; +---------------+
;; |  Run Section  |
;; +---------------+
(defvar +my-dashboard-run-left
  '(("librewolf" . "librewolf > /dev/null 2>&1")))

(defvar +my-dashboard-run-right
  '(("Power Off" . "systemctl poweroff")))

(defun +my-dashboard-widget-run ()
  "Doom widget function for Run section."
   (+my-dashboard-insert-2cols
   +my-dashboard-run-left
   +my-dashboard-run-right
   ;; Left column buttons
   (lambda (item width)
     (insert-text-button
      (+my-dashboard-pad (car item) width)
      'action (lambda (_)
                (interactive)
                (start-process-shell-command (car item) nil (cdr item)))
      'follow-link t
      'help-echo (cdr item)))
   ;; Right column buttons
   (lambda (item width)
     (insert-text-button
      (+my-dashboard-pad (car item) width)
      'action (lambda (_)
                (interactive)
                (if (string= (car item) "Power Off")
                    (when (y-or-n-p "Are you sure you want to power off? ")
                      (start-process-shell-command (car item) nil (cdr item)))
                  (start-process-shell-command (car item) nil (cdr item))))
      'follow-link t
      'help-echo (cdr item))))
  (insert "\n"))

;; +-----------------------+
;; |  Quick Links Section  |
;; +-----------------------+
(defvar +my-dashboard-quick-links-left
  '(("title1" . "~/file/path/")
    ("title2" . "~/file/path/")
    ("title3" . "~/file/path")))

(defvar +my-dashboard-quick-links-right
  '(("Doom Configs" . "~/.config/doom/")
    ("Sway Configs" . "~/.config/sway/")
    ("Scratch-pad" . "~/.config/doom/db-scratchpad.org")))

(defun +my-dashboard-widget-quick-links ()
  "Doom widget function for Quick Links section."
  (insert (+my-dashboard--center "Quick Links") "\n")
  (+my-dashboard-insert-2cols
   +my-dashboard-quick-links-left
   +my-dashboard-quick-links-right
   ;; Left column buttons
   (lambda (item width)
     (insert-text-button
      (+my-dashboard-pad (car item) width)
      'action (lambda (_)
                (interactive)
                (find-file (cdr item)))
      'follow-link t
      'help-echo (cdr item)))
   ;; Right column buttons
   (lambda (item width)
     (insert-text-button
      (+my-dashboard-pad (car item) width)
      'action (lambda (_)
                (interactive)
                (find-file (cdr item)))
      'follow-link t
      'help-echo (cdr item))))
  (insert "\n"))

;; +---------------+
;; |  Scratch Pad  |
;; +---------------+
(defun +my-dashboard-widget-scratch-pad ()
  (when (file-exists-p +my-dashboard-scratch-pad-file)
    (let ((lines (with-temp-buffer
                   (insert-file-contents +my-dashboard-scratch-pad-file)
                   (split-string (buffer-string) "\n" t))))
      (dolist (line lines)
        (insert (+my-dashboard--center line) "\n"))
      (insert "\n"))))

;; +-------------------+
;; |  Configuration    |
;; +-------------------+

;; Replace Doom's default widget functions with your custom ones
(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner        ;; Keep Doom's banner
        +my-dashboard-widget-run            ;; Your Run section
;        +my-dashboard-widget-quick-links    ;; Your Quick Links section
        +my-dashboard-widget-scratch-pad    ;; Your Scratch Pad section
        doom-dashboard-widget-loaded        ;; Keep Doom's load time
        doom-dashboard-widget-footer))      ;; Keep Doom's footer

;; Optional: Remove the loaded widget and footer if you don't want them
;; (setq +doom-dashboard-functions
;;       '(doom-dashboard-widget-banner
;;         +my-dashboard-widget-run
;;         +my-dashboard-widget-quick-links
;;         +my-dashboard-widget-scratch-pad))

(provide 'custom-dashboard-integration)
;;; custom-dashboard-integration.el ends here

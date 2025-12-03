;;; custom-db.el -*- lexical-binding: t; -*-

;; +-------------+
;; |  Variables  |
;; +-------------+
(defvar +my-dashboard-buffer-name "*my-dashboard*"
  "Name of the dashboard buffer.")

(defvar +my-dashboard-width 80
  "Default width used as a fallback when centering content.")

(defvar +my-dashboard-scratch-pad-file "~/.config/doom/db-scratchpad.org"
  "Path to the file containing the Scratch Pad content.")

(defvar +my-dashboard--timer nil
  "Internal timer used to debounce dashboard refreshes.")

(define-derived-mode +my-dashboard-mode special-mode "DASHBOARD"
  "Minimal dashboard mode for Emacs dashboard."
  (buffer-disable-undo)
  (setq-local truncate-lines t)
  (setq-local display-line-numbers nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil))

;; +-------------+
;; |  Functions  |
;; +-------------+
(defun +my-dashboard--center (s)
  "Return string S centered relative to the current window width."
  (let* ((width (window-width))
         (target-width (if (and width (> width 0)) width +my-dashboard-width))
         (pad (max 0 (/ (- target-width (length s)) 2))))
    (concat (make-string pad ?\s) s)))

(defun +my-dashboard-pad (s width)
  "Pad string S to WIDTH characters."
  (format (concat "%-" (number-to-string width) "s") s))

(defun +my-dashboard-insert-2cols (left-items right-items left-fn right-fn &optional col-width)
  "Insert LEFT-ITEMS and RIGHT-ITEMS in two centered columns.
LEFT-FN and RIGHT-FN handle insertion for each column.
COL-WIDTH defaults to the longest item label length, up to a minimum of 10."
  (let* ((col-width (or col-width
                        (max 10
                             (apply #'max
                                    (mapcar (lambda (x) (string-width (car x)))
                                            (append left-items right-items))))))
         (gap 10) ;; space between columns
         (win-width (window-width))
         (total-width (+ (* 2 col-width) gap))
         (side-pad (max 0 (/ (- win-width total-width) 2)))
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


;; +------------------+
;; |  Banner Section  |
;; +------------------+
(defun +my-dashboard-insert-banner ()
  "Insert ASCII banner from external file at the top of the dashboard."
  (when (file-exists-p "~/.config/doom/ascii-banner.txt")
    (let ((lines (with-temp-buffer
                   (insert-file-contents "~/.config/doom/ascii-banner.txt")
                   (split-string (buffer-string) "\n" t))))
      (dolist (line lines)
        (insert (+my-dashboard--center line) "\n"))
      (insert "\n"))))

;; +---------------+
;; |  Run Section  |
;; +---------------+
(defvar +my-dashboard-run-left
  '(("librewolf" . "librewolf > /dev/null 2>&1")
    ("option3"   . "echo option3")))

(defvar +my-dashboard-run-right
  '(("Power Off" . "systemctl poweroff")
    ("option4"   . "echo option4")))

(defun +my-dashboard-insert-run ()
  "Insert Run section (2 columns, infinite rows, RET-able)."
  (insert (+my-dashboard--center "Run") "\n")
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
  '(("Book"  . "~/void/p41n/")
    ("To-Do" . "~/void/l0g1c/notepad/TODO.org")
    ("Notes" . "~/void/l0g1c/notepad/notes.org")))

(defvar +my-dashboard-quick-links-right
  '(("Configs"     . "~/.config/")
    ("DB Edit"     . "~/.config/doom/custom-db.el")
    ("Scratch-pad" . "~/.config/doom/db-scratchpad.org")))

(defun +my-dashboard-insert-quick-links ()
  "Insert Quick Links section (2 columns, infinite rows, RET-able)."
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
(defun +my-dashboard-insert-scratch-pad ()
  "Insert Scratch Pad from external file with proper spacing."
  (when (file-exists-p +my-dashboard-scratch-pad-file)
    (insert (+my-dashboard--center "Scratch Pad") "\n")
    (let ((lines (with-temp-buffer
                   (insert-file-contents +my-dashboard-scratch-pad-file)
                   (split-string (buffer-string) "\n" t))))
      (dolist (line lines)
        (insert (+my-dashboard--center line) "\n"))
      (insert "\n"))))

;; +-------------------+
;; |  Build Dashboard  |
;; +-------------------+
(defun +my-dashboard--refresh ()
  "Internal function to rebuild the dashboard buffer."
  (let ((buf (get-buffer-create +my-dashboard-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (+my-dashboard-mode)

        ;; Sections
        (+my-dashboard-insert-banner)
        (goto-char (point-max))
        (+my-dashboard-insert-run)
        (+my-dashboard-insert-quick-links)
        (+my-dashboard-insert-scratch-pad)

        ;; Cursor placement
        (goto-char (point-min))
        (search-forward "librewolf" nil t)
        (backward-word 1)
        (read-only-mode 1)))
    buf))

(defun +my-dashboard--debounced-refresh (&rest _)
  "Debounced refresh to prevent repeated redraws."
  (when +my-dashboard--timer
    (cancel-timer +my-dashboard--timer))
  (setq +my-dashboard--timer
        (run-with-timer 0.15 nil #'+my-dashboard--refresh)))

;; Auto-refresh similar to Doom dashboard
(add-hook 'window-size-change-functions     #'+my-dashboard--debounced-refresh)
(add-hook 'window-configuration-change-hook #'+my-dashboard--debounced-refresh)

;; +-----------+
;; |  Startup  |
;; +-----------+
(defun +my-dashboard-create ()
  "Entry point for dashboard creation."
  (+my-dashboard--refresh))

(setq initial-buffer-choice #'+my-dashboard-create)
(setq doom-fallback-buffer-name +my-dashboard-buffer-name)

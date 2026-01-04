;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;
;;
;;
;; ┌─────────────────────────────┐
;; │ Load custom Elisp dashboard │
;; └─────────────────────────────┘
;;(condition-case err
;;    (load! "extras/custom-db-integration.el")
;;  (error
;;   (message "Main dashboard failed: %s - Loading backup..." (error-message-string err))
;;   (condition-case err2
;;       (load! "extras/custom-db-safe.el")
;;     (error
;;      (message "Backup dashboard also failed: %s" (error-message-string err2))))))


;; ┌───────────────────────────┐
;; │ Load custom ORG dashboard │
;; └───────────────────────────┘


(defvar +my-org-dashboard-buffer "*my-org-dashboard*"
  "Name of the Org dashboard buffer.")

(defvar +my-org-dashboard-file "~/.config/doom/extras/orgboard.org"
  "Path to the Org dashboard file.")

(defun +org-dashboard-poweroff ()
  "Ask for confirmation before powering off."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to power off the PC? ")
    (start-process "shutdown" nil "systemctl" "poweroff")))

(defun +my-org-fold-to-level-1 ()
  "Show only top-level headings and fold the rest."
  (org-fold-show-all)
  (org-map-entries
   (lambda ()
     (when (> (org-outline-level) 1)
       (org-fold-hide-entry)))))

(defun +my-org-dashboard ()
  "Open the Org dashboard in a protected buffer."
  (interactive)
  (let ((buf (get-buffer-create +my-org-dashboard-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents +my-org-dashboard-file)
        (org-mode)
        (+my-org-fold-to-level-1)
        (display-line-numbers-mode 0)
        (setq-local indicate-empty-lines nil)
        (setq-local left-margin-width 4)
        (set-window-buffer nil (current-buffer))
        (read-only-mode 1))
      ;; Prevent killing the buffer
      (setq-local kill-buffer-query-functions
                  (list (lambda () (message "Dashboard buffer cannot be killed") nil))))
    (switch-to-buffer buf)))

(defun +my-org-dashboard-refresh ()
  "Refresh the Org dashboard buffer."
  (interactive)
  (when (get-buffer +my-org-dashboard-buffer)
    (with-current-buffer +my-org-dashboard-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents +my-org-dashboard-file)
        (org-mode)
        (+my-org-fold-to-level-1)
        (display-line-numbers-mode 0)
        (setq-local indicate-empty-lines nil)
        (setq-local left-margin-width 4)
        (set-window-buffer nil (current-buffer))
        (read-only-mode 1)))))

;; Auto-open dashboard at startup
(add-hook 'emacs-startup-hook #'+my-org-dashboard)

;; Make Doom fallback buffer the dashboard
(setq doom-fallback-buffer-name +my-org-dashboard-buffer)

;;
;;
;; ┌───────────┐
;; │ Autostart │
;; └───────────┘
;(load! "autostart.el")
;(add-to-list 'display-buffer-alist
;             '("\\*vterm\\*.*"
;               (display-buffer-in-side-window)
;               (side . bottom)
;               (window-height . 3)
;               (window-parameters (no-delete-other-windows . t))))
;
;; ┌──────────┐
;; │ Packages │
;; └──────────┘
(use-package! uniline
  :commands uniline-mode
  :defer t)
;;
;;

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-griswold-variant 'light) ;; Light mode
;;(setq doom-griswold-variant 'dark)  ;; Dark mode
(setq doom-theme 'doom-griswold)
;;
;; default
;;(setq doom-theme 'doom-one)
;;
;;
;;
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
;; ┌─────────────────────────────────┐
;; │ Custom Emacs Auto-save Variable │
;; └─────────────────────────────────┘
;; Disable auto-save completely
(after! files
  (setq auto-save-default nil           ; Turn off auto-save
        auto-save-visited-mode nil      ; No save-on-visit
        auto-save-list-file-prefix nil  ; No auto-save-list files
        auto-save-interval 0            ; No keystroke-based saves
        auto-save-timeout 0))           ; No time-based saves
;;

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

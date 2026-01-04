;;; doom-griswold-theme.el --- Inspired by HPPD -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: 01/03/26
;; Author: K. Mustard
;; Maintainer:
;; Source: https://github.com/sangria59/LK-9T9/
;;
;;; Commentary:
;;
;; A Doom Emacs theme inspired by the Gruvbox palette, with HPPD in mind.
;; Supports light, dark, and balanced variants via `doom-griswold-variant`.
;;
;; Configuration (set variant before loading the theme):
;;   ;; (setq doom-griswold-variant 'light) ;; Light mode
;;   ;; (setq doom-griswold-variant 'dark)  ;; Dark mode
;;   (setq doom-theme 'doom-griswold)       ;; Balanced

;;; Code:

(require 'doom-themes)

(defgroup doom-griswold-theme nil
  "Options for doom-griswold."
  :group 'doom-themes)

(defcustom doom-griswold-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-griswold-theme
  :type '(choice integer boolean))

(defcustom doom-griswold-variant nil
  "Choose between 'balanced (nil), 'dark, or 'light variants."
  :group 'doom-griswold-theme
  :type '(choice (const :tag "Balanced" nil)
                 (const :tag "Dark" dark)
                 (const :tag "Light" light)))

(def-doom-theme doom-griswold
  "Griswold theme with balanced, dark, and light variants."
  :family 'doom-griswold
  :background-mode (if (eq doom-griswold-variant 'light) 'light 'dark)

  ;; name        gui       256       16
  ((bg         (pcase doom-griswold-variant
                 ('dark  '("#433a30" "#282828" nil))
                 ('light '("#d4b486" "#ffffff" nil))
                 (_      '("#7a614a" "#282828" nil))))
   (bg-alt     (pcase doom-griswold-variant
                 ('dark  '("#1d2021" "#1e1e1e" nil))
                 ('light '("#b79d79" "#d5c4a1" nil))
                 (_      '("#1d2021" "#1e1e1e" nil))))
   (bg-alt2    (pcase doom-griswold-variant
                 ('dark  '("#504945" "#504945" brown))
                 ('light '("#998263" "#e2d9c4" nil))
                 (_      '("#504945" "#504945" brown))))
   (bg-alt3    (pcase doom-griswold-variant
                 ('dark  '("#37302f" "#37302f" "black"))
                 ('light '("#cead80" "#d5c4a1" "black"))
                 (_      '("#37302f" "#37302f" "black"))))
   (fg         (pcase doom-griswold-variant
                 ('dark  '("#dfa86a" "#dfdfdf" nil))
                 ('light '("#5b4c3c" "#5b4c3c" nil))
                 (_      '("#c5b8a8" "#d4c09c" nil))))
   (fg-alt     (pcase doom-griswold-variant
                 ('dark  '("#d5c4a1" "#cccccc" nil))
                 ('light '("#7c6f64" "#6f6f6f" nil))
                 (_      '("#d5c4a1" "#cccccc" nil))))

   (base0      (pcase doom-griswold-variant
                 ('dark  '("#0d1011" "black"   nil))
                 ('light '("#db95b9" "#ffffff" nil))
                 (_      '("#0d1011" "black"   nil))))
   (base1      (pcase doom-griswold-variant
                 ('dark  '("#1d2021" "#1d1d1d" nil))
                 ('light '("#95dbb4" "#f5f5f5" nil))
                 (_      '("#1d2021" "#1d1d1d" nil))))
   (base2      (pcase doom-griswold-variant
                 ('dark  '("#282828" "#282828" nil))
                 ('light '("#282828" "#282828" nil))
                 (_      '("#282828" "#282828" nil))))
   (base3      (pcase doom-griswold-variant
                 ('dark  '("#3c3836" "#383838" nil))
                 ('light '("#d5c4a1" "#d5d5d5" nil))
                 (_      '("#3c3836" "#383838" nil))))
   (base4      (pcase doom-griswold-variant
                 ('dark  '("#665c54" "#5c5c5c" nil))
                 ('light '("#bdae93" "#bdbdbd" nil))
                 (_      '("#665c54" "#5c5c5c" nil))))
   (base5      (pcase doom-griswold-variant
                 ('dark  '("#7c6f64" "#6f6f6f" nil))
                 ('light '("#928374" "#909090" nil))
                 (_      '("#7c6f64" "#6f6f6f" nil))))
   (base6      (pcase doom-griswold-variant
                 ('dark  '("#928374" "#909090" nil))
                 ('light '("#7c6f64" "#6f6f6f" nil))
                 (_      '("#928374" "#909090" nil))))
   (base7      (pcase doom-griswold-variant
                 ('dark  '("#d5c4a1" "#cccccc" nil))
                 ('light '("#3c3836" "#383838" nil))
                 (_      '("#d5c4a1" "#cccccc" nil))))
   (base8      (pcase doom-griswold-variant
                 ('dark  '("#fbf1c7" "#fbfbfb" nil))
                 ('light '("#282828" "#282828" nil))
                 (_      '("#fbf1c7" "#fbfbfb" nil))))


   (white       (pcase doom-griswold-variant
                 ('dark  '("#e0e0e0" "#e0e0e0" nil))
                 ('light '("#e0e0e0" "#e0e0e0" nil))
                 (_      '("#e0e0e0" "#e0e0e0" nil))))

   (grey       (pcase doom-griswold-variant
                 ('dark  '("#928374" "#909090" nil))
                 ('light '("#928374" "#909090" nil))
                 (_      '("#928374" "#909090" nil))))
   (red        (pcase doom-griswold-variant
                 ('dark  '("#fb4934" "#e74c3c" nil))
                 ('light '("#9d0006" "#9d0006" nil))
                 (_      '("#88262A" "#9d0006" nil))))
   (magenta    (pcase doom-griswold-variant
                 ('dark  '("#b16286" "#b16286" nil))
                 ('light '("#883455" "#883455" nil))
                 (_      '("#da5a9a" "#da5a9a" nil))))
   (violet     (pcase doom-griswold-variant
                 ('dark  '("#d3869b" "#d3869b" nil))
                 ('light '("#af5f80" "#af5f80" nil))
                 (_      '("#d3869b" "#d3869b" nil))))
   (orange     (pcase doom-griswold-variant
                 ('dark  '("#fe8019" "#fd971f" nil))
                 ('light '("#cc7d3d" "#dd8842" nil))
                 (_      '("#db9052" "#dd8842" nil))))
   (yellow     (pcase doom-griswold-variant
                 ('dark  '("#fabd2f" "#fabd2f" nil))
                 ('light '("#b57614" "#b57614" nil))
                 (_      '("#ccb759" "#ccb759" nil))))
   (dark-yellow (pcase doom-griswold-variant
                 ('dark  '("#d79921" "#fabd2f" nil))
                 ('light '("#8f5902" "#8f5902" nil))
                 (_      '("#d79921" "#fabd2f" nil))))
   (teal       (pcase doom-griswold-variant
                 ('dark  '("#8ec07c" "#8ec07c" nil))
                 ('light '("#5f8f5f" "#5f8f5f" nil))
                 (_      '("#8ec07c" "#8ec07c" nil))))
   (green      (pcase doom-griswold-variant
                 ('dark  '("#b8bb26" "#b8bb26" nil))
                 ('light '("#6c782e" "#6c782e" nil))
                 (_      '("#6b866c" "#6b866c" nil))))
   (dark-green (pcase doom-griswold-variant
                 ('dark  '("#98971a" "#98971a" nil))
                 ('light '("#4c6112" "#4c6112" nil))
                 (_      '("#98971a" "#98971a" nil))))
   (blue       (pcase doom-griswold-variant
                 ('dark  '("#83a598" "#83a598" nil))
                 ('light '("#45707a" "#45707a" nil))
                 (_      '("#8cb8b0" "#83a598" nil))))
   (dark-blue  (pcase doom-griswold-variant
                 ('dark  '("#458588" "#458588" nil))
                 ('light '("#265f6f" "#265f6f" nil))
                 (_      '("#8c9cc4" "#458588" nil))))
   (cyan       (pcase doom-griswold-variant
                 ('dark  '("#8ec07c" "#8ec07c" nil))
                 ('light '("#0b593e" "#4f9f70" nil))
                 (_      '("#9cc4a8" "#9cc4a8" nil))))
   (dark-cyan  (pcase doom-griswold-variant
                 ('dark  '("#689d6a" "#689d6a" nil))
                 ('light '("#5f8f5f" "#5f8f5f" nil))
                 (_      '("#5e8a6b" "#5e8a6b" nil))))

   ;; face categories
   (highlight       yellow)
   (vertical-bar   bg-alt2)
   (selection      bg-alt2)
   (builtin         orange)
   (comments          grey)
   (doc-comments      grey)
   (constants       violet)
   (functions        green)
   (keywords           red)
   (methods          green)
   (operators           fg)
   (type            yellow)
   (strings          green)
   (variables         blue)
   (numbers         violet)
   (region         bg-alt2)
   (error              red)
   (warning         yellow)
   (success          green)

   (vc-modified    (doom-darken cyan 0.15))
   (vc-added       (doom-darken green 0.15))
   (vc-deleted     (doom-darken red 0.15))

   ;; custom categories
   (-modeline-pad
    (when doom-griswold-padded-modeline
      (if (integerp doom-griswold-padded-modeline)
          doom-griswold-padded-modeline
        4)))
   (modeline-bg bg-alt2)
   (modeline-fg (doom-lighten fg-alt 0.25))
   (modeline-inactive-bg (doom-darken modeline-bg 0.15))
   (modeline-inactive-fg base6)
   (org-quote `(,(doom-lighten (car bg) 0.05) ,(cadr bg))))

  ;; Base theme face overrides
  ((button :foreground cyan :underline t :weight 'bold)
   (cursor :background white :foreground fg-alt)
   (hl-line :background fg-alt :foreground bg-alt)
  ; (font-lock-doc-face :foreground red)
   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :background base2 :foreground yellow)
   (isearch :foreground base0 :background orange)
   (lazy-highlight :background yellow :foreground base0 :distant-foreground base0 :bold bold)
   ((link &override) :foreground violet)
   (minibuffer-prompt :foreground cyan)
   (mode-line
    :background bg-alt3 :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background bg :foreground base4
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-inactive-bg)))

   ;;;; company
   (company-preview-common :foreground cyan)
   (company-tooltip-common :foreground cyan)
   (company-tooltip-common-selection :foreground cyan)
   (company-tooltip-annotation :foreground cyan)
   (company-tooltip-annotation-selection :foreground cyan)
   (company-scrollbar-bg :background base2)
   (company-scrollbar-fg :background cyan)
   (company-tooltip-selection :background bg-alt2)
   (company-tooltip-mouse :background bg-alt2 :foreground nil)
   ;;;; css-mode
   (css-proprietary-property :foreground red)
   ;;;; doom-emacs
   (+workspace-tab-selected-face :background dark-green :foreground base8)
   ;;;; doom-modeline
   (doom-modeline-project-dir :bold t :foreground cyan)
   (doom-modeline-buffer-path :inherit 'bold :foreground green)
   (doom-modeline-buffer-file :inherit 'bold :foreground fg)
   (doom-modeline-buffer-modified :inherit 'bold :foreground yellow)
   (doom-modeline-error :background bg)
   (doom-modeline-buffer-major-mode :foreground green :bold t)
   (doom-modeline-info :bold t :foreground cyan)
   (doom-modeline-bar :background dark-green)
   (doom-modeline-panel :background dark-green :foreground fg)
   ;;;; doom-themes
   (doom-themes-neotree-file-face :foreground fg)
   (doom-themes-neotree-hidden-file-face :foreground (doom-lighten fg-alt 0.25))
   (doom-themes-neotree-media-file-face :foreground (doom-lighten fg-alt 0.25))
   ;;;; emacs-lisp-mode
   (highlight-quoted-symbol :foreground base8)
   ;;;; ediff
   (ediff-fine-diff-A    :background (doom-blend red bg 0.4) :weight 'bold)
   (ediff-current-diff-A :background (doom-blend red bg 0.2))
   ;;;; evil
   (evil-search-highlight-persist-highlight-face :background yellow)
   (evil-ex-substitute-replacement :foreground cyan :strike-through nil :inherit 'evil-ex-substitute-matches)
   ;;;; evil-snipe
   (evil-snipe-first-match-face :foreground base8 :background yellow)
   (evil-snipe-matches-face     :foreground yellow :bold t :underline t)
   ;;;; flycheck
   (flycheck-error   :underline `(:style wave :color ,red) :background base2)
   (flycheck-warning :underline `(:style wave :color ,yellow) :background base2)
   (flycheck-info    :underline `(:style wave :color ,blue) :background base2)
   ;;;; dired
   (dired-directory :foreground cyan)
   (dired-marked :foreground yellow)
   (dired-symlink :foreground cyan)
   (dired-header :foreground cyan)
   ;;;; helm
   (helm-swoop-target-line-face :foreground magenta :inverse-video t)
   ;;;; highlight-thing
   (highlight-thing :background (doom-lighten base2 0.03) :distant-foreground fg-alt)
   (highlight-symbol-face :background (doom-lighten base2 0.03) :distant-foreground fg-alt)
   ;;;; ivy
   (ivy-current-match :background bg-alt2)
   (ivy-subdir :background nil :foreground cyan)
   (ivy-action :background nil :foreground cyan)
   (ivy-grep-line-number :background nil :foreground cyan)
   (ivy-minibuffer-match-face-1 :background nil :foreground yellow)
   (ivy-minibuffer-match-face-2 :background nil :foreground yellow)
   (ivy-minibuffer-match-highlight :foreground cyan)
   (counsel-key-binding :foreground cyan)
   ;;;; ivy-posframe
   (ivy-posframe :background base2)
   (ivy-posframe-border :background base1)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground dark-cyan)
   ;;;; magit
   (magit-section-heading             :foreground cyan :weight 'bold)
   (magit-branch-current              :underline green :inherit 'magit-branch-local)
   (magit-diff-hunk-heading           :background base2 :foreground fg-alt)
   (magit-diff-hunk-heading-highlight :background bg-alt2 :foreground fg)
   (magit-diff-context                :foreground base2 :foreground fg-alt)
   ;;;; markdown-mode
   (markdown-header-face :inherit 'bold :foreground green)
   (markdown-header-delimiter-face :foreground orange)
   (markdown-blockquote-face :inherit 'italic :foreground grey)
   (markdown-list-face :foreground grey)
   (markdown-url-face :foreground violet)
   (markdown-pre-face  :foreground cyan)
   (markdown-link-face :inherit 'underline :foreground grey)
   ((markdown-code-face &override) :background (doom-lighten bg-alt 0.045))
   ;;;; mu4e-view
   (mu4e-header-key-face :foreground red :weight 'bold)
   ;;;; neotree
   (neo-root-dir-face   :foreground cyan)
   (doom-neotree-dir-face :foreground cyan)
   (neo-dir-link-face   :foreground cyan)
   (neo-expand-btn-face :foreground magenta)
   ;;;; outline
   ((outline-1 &override) :foreground violet)
   ((outline-2 &override) :foreground cyan)
   ((outline-3 &override) :foreground green)
   ((outline-4 &override) :foreground (doom-lighten violet 0.2))
   ((outline-5 &override) :foreground (doom-lighten dark-cyan 0.25))
   ((outline-6 &override) :foreground (doom-lighten violet 0.4))
   ((outline-7 &override) :foreground (doom-lighten dark-cyan 0.5))
   ((outline-8 &override) :foreground (doom-lighten violet 0.6))
   ;;;; org
   ((org-code &override) :foreground orange)
   (org-date :foreground green)
   (org-document-info :foreground red)
   (org-document-title :foreground red)
   (org-drawer :foreground (doom-lighten cyan 0.4))
   (org-ellipsis :underline nil :foreground orange)
   (org-formula :foreground green)
   (org-meta-line :foreground grey)
   (org-list-dt :foreground cyan)
   ((org-quote &override) :inherit 'italic :foreground base7 :background org-quote)
   (org-table :foreground cyan)
   (org-tag :foreground (doom-darken grey 0.15) :weight 'normal)
   (org-todo :foreground green :bold 'inherit)
   (org-verbatim :foreground yellow)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground orange)
   (rainbow-delimiters-depth-2-face :foreground magenta)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground blue)
   ;;;; show-paren
   ((show-paren-match &override) :foreground 'unspecified :background base5 :bold t)
   ((show-paren-mismatch &override) :foreground 'unspecified :background red)
   ;;;; swiper
   (swiper-line-face :background bg-alt2)
   ;;;; undo-tree
   (undo-tree-visualizer-active-branch-face :foreground cyan)
   (undo-tree-visualizer-current-face :foreground yellow)
   ;;;; vimish-fold
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background bg-alt2 :weight 'light)
   ((vimish-fold-mouse-face &override) :foreground base8 :background yellow :weight 'light)
   ((vimish-fold-fringe &override) :foreground magenta :background magenta)
   ;;;; web-mode
   (web-mode-html-tag-bracket-face :foreground blue)
   (web-mode-html-tag-face         :foreground cyan)
   (web-mode-html-attr-name-face   :foreground cyan)
   (web-mode-json-key-face         :foreground green)
   (web-mode-json-context-face     :foreground cyan)
   ;;;; which-key
   (which-key-key-face                   :foreground green)
   (which-key-group-description-face     :foreground red)
   (which-key-command-description-face   :foreground blue)
   (which-key-local-map-description-face :foreground orange)))

(provide 'doom-griswold-theme)

;;; doom-griswold-theme.el ends here

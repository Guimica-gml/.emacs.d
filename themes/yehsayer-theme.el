(deftheme yehsayer "It's like naysayer, but yeh.")

(let* ((class '((class color) (min-colors 89)))
       (foreground "#D1B897")
       (background "gray10")
       (cursor "white")
       (border foreground)
       (minibuffer cursor)
       (region "gray20")
       (comment "gray65")
       (string comment)
       (mode-line-foreground background)
       (mode-line-background border)
       (mode-line-foreground-inactive foreground)
       (mode-line-background-inactive background)
       (hl-background region)
       (hl-face-background nil)
       (fringe-background "black")
       (failure "red")
       (line-number-foreground comment)
       (line-number-background "#251c11")
       (line-number-current foreground))
  (setq fci-rule-color comment)
  (custom-theme-set-faces
   'yehsayer

   ;; dired
   `(dired-directory ((t (:foreground ,foreground :weight bold))))

   ;; basic stuff
   `(default ((t (:background ,background :foreground ,foreground))))
   `(cursor ((t (:background ,cursor :inverse-video t))))
   `(vertical-border ((t (:foreground ,border))))

   ;; minibuffer
   `(minibuffer-prompt ((t (:foreground ,minibuffer :weight bold))))

   ;; region
   `(region ((t (:background ,region))))
   `(secondary-selection ((t (:background ,region))))

   ;; faces
   `(font-lock-builtin-face ((t (:foreground ,foreground))))
   `(font-lock-constant-face ((t (:foreground ,foreground))))
   `(font-lock-preprocessor-face ((t (:foreground ,foreground :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,foreground :weight bold))))
   `(font-lock-type-face ((t (:foreground ,foreground))))
   `(font-lock-function-name-face ((t (:foreground ,foreground))))
   `(font-lock-variable-name-face ((t (:foreground ,foreground))))

   `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-doc-face ((t (:foreground ,comment))))
   `(font-lock-string-face ((t (:foreground ,foreground :foreground ,string))))

   ;; rust specific
   `(rust-unsafe ((t (:foreground ,foreground :weight bold))))

   ;; isearch
   `(isearch ((t (:foreground ,foreground :background ,region :weight normal))))
   `(isearch-fail ((t (:foreground ,failure :bold t))))
   `(lazy-highlight
     ((t (:foreground ,foreground :background ,region))))

   ;; ido-mode
   `(ido-subdir ((t (:foreground ,foreground :weight bold))))
   `(ido-only-match ((t (:foreground ,foreground :weight bold))))

   ;; show-paren
   `(show-paren-match ((t (:background ,region))))
   `(show-paren-mismatch ((t (:foreground ,failure :weight bold))))

   ;; mode-line
   `(mode-line
     ((t (:overline nil
          :underline nil
          :foreground ,mode-line-foreground
          :background ,mode-line-background
          :box nil))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-inactive
     ((t (:overline nil
          :underline nil
          :foreground ,mode-line-foreground-inactive
          :background ,mode-line-background-inactive
          :box nil))))

   ;; fringe-mode
   `(fringe ((t (:background ,fringe-background))))

   ;; line numbers
   `(line-number ((t (:background ,line-number-background :foreground ,line-number-foreground))))
   `(line-number-current-line ((t (:background ,line-number-background :foreground ,line-number-current :weight bold))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,hl-background))))
   `(hl-line-face ((t (:background ,hl-face-background))))))

(provide-theme 'yehsayer)

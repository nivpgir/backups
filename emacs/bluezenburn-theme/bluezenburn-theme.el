(deftheme bluezenburn "The Bluezenburn color theme")

(let ((class '((class color) (min-colors 89)))
      ;; Bluezenburn palette
      ;; colors with +x are lighter, colors with -x are darker

      (bluezenburn-fg "#ccdcdc")
      (bluezenburn-fg-1 "#556565")
      (bluezenburn-bg-1 "#2b2b2b")
      (bluezenburn-bg-05 "#383838")
      (bluezenburn-bg "#3f3f3f")
      (bluezenburn-bg+1 "#4f4f4f")
      (bluezenburn-bg+2 "#5f5f5f")
      (bluezenburn-bg+3 "#6f6f6f")
      (bluezenburn-red+1 "#a3a3dc")
      (bluezenburn-red "#9393cc")
      (bluezenburn-red-1 "#8383bc")
      (bluezenburn-red-2 "#7373ac")
      (bluezenburn-red-3 "#63639c")
      (bluezenburn-red-4 "#53538c")
      (bluezenburn-orange "#8fafdf")
      (bluezenburn-yellow "#afdff0")
      (bluezenburn-yellow-1 "#9fcfe0")
      (bluezenburn-yellow-2 "#8fbfd0")
      (bluezenburn-green-1 "#5f7f5f")
      (bluezenburn-green "#7f9f7f")
      (bluezenburn-green+1 "#8fb28f")
      (bluezenburn-green+2 "#9fc59f")
      (bluezenburn-green+3 "#afd8af")
      (bluezenburn-green+4 "#bfebbf")
      (bluezenburn-cyan "#e3e093")
      (bluezenburn-blue+1 "#f3bf94")
      (bluezenburn-blue "#d3d08c")
      (bluezenburn-blue-1 "#bbb87c")
      (bluezenburn-blue-2 "#a3a06c")
      (bluezenburn-blue-3 "#8b885c")
      (bluezenburn-blue-4 "#73704c")
      (bluezenburn-blue-5 "#606036")
      (bluezenburn-magenta "#c38cdc"))

  (custom-theme-set-faces
   'bluezenburn
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,bluezenburn-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,bluezenburn-yellow-2 :underline t :weight normal))))

   ;;; basic coloring
   `(default ((t (:foreground ,bluezenburn-fg))))
    ;;  Comment out the above line, and uncomment the below one
    ;;  if you prefer windowed mode to terminal mode.
    ;; `(default ((t (:foreground ,bluezenburn-fg :background ,bluezenburn-bg))))
   `(cursor ((t (:foreground ,bluezenburn-fg :background "white"))))
   `(escape-glyph-face ((t (:foreground ,bluezenburn-red))))
   `(fringe ((t (:foreground ,bluezenburn-fg :background ,bluezenburn-bg+1))))
   `(header-line ((t (:foreground ,bluezenburn-yellow
                                  :background ,bluezenburn-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,bluezenburn-bg-05))))

   ;;; compilation
   `(compilation-column-face ((t (:foreground ,bluezenburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,bluezenburn-green))))
   `(compilation-error-face ((t (:foreground ,bluezenburn-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,bluezenburn-fg))))
   `(compilation-info-face ((t (:foreground ,bluezenburn-blue))))
   `(compilation-info ((t (:foreground ,bluezenburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,bluezenburn-green))))
   `(compilation-line-face ((t (:foreground ,bluezenburn-yellow))))
   `(compilation-line-number ((t (:foreground ,bluezenburn-yellow))))
   `(compilation-message-face ((t (:foreground ,bluezenburn-blue))))
   `(compilation-warning-face ((t (:foreground ,bluezenburn-yellow-1 :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((t (:foreground ,bluezenburn-fg))))
   `(grep-error-face ((t (:foreground ,bluezenburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,bluezenburn-blue))))
   `(grep-match-face ((t (:foreground ,bluezenburn-orange :weight bold))))
   `(match ((t (:background ,bluezenburn-bg-1 :foreground ,bluezenburn-orange :weight bold))))

   ;; faces used by isearch
   `(isearch ((t (:foreground ,bluezenburn-yellow :background ,bluezenburn-bg-1))))
   `(isearch-fail ((t (:foreground ,bluezenburn-fg :background ,bluezenburn-red-4))))
   `(lazy-highlight ((t (:foreground ,bluezenburn-yellow :background ,bluezenburn-bg+2))))

   `(menu ((t (:foreground ,bluezenburn-fg :background ,bluezenburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,bluezenburn-yellow))))
   `(mode-line
     ((,class (:foreground ,bluezenburn-green+1
                           :background ,bluezenburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,bluezenburn-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,bluezenburn-green-1
                      :background ,bluezenburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,bluezenburn-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,bluezenburn-bg+2))))
   `(trailing-whitespace ((t (:background ,bluezenburn-red))))
   `(vertical-border ((t (:foreground ,bluezenburn-fg))))

   ;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,bluezenburn-cyan))))
   `(font-lock-comment-face ((t (:foreground ,bluezenburn-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,bluezenburn-green))))
   `(font-lock-constant-face ((t (:foreground ,bluezenburn-green+4))))
   `(font-lock-doc-face ((t (:foreground ,bluezenburn-green+1))))
   `(font-lock-doc-string-face ((t (:foreground ,bluezenburn-blue-2))))
   `(font-lock-function-name-face ((t (:foreground ,bluezenburn-blue))))
   `(font-lock-keyword-face ((t (:foreground ,bluezenburn-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,bluezenburn-fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,bluezenburn-blue+1))))
   `(font-lock-string-face ((t (:foreground ,bluezenburn-red))))
   `(font-lock-type-face ((t (:foreground ,bluezenburn-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,bluezenburn-orange))))
   `(font-lock-warning-face ((t (:foreground ,bluezenburn-yellow-1 :weight bold :underline t))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))

   ;;; newsticker
   `(newsticker-date-face ((t (:foreground ,bluezenburn-fg))))
   `(newsticker-default-face ((t (:foreground ,bluezenburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,bluezenburn-green+3))))
   `(newsticker-extra-face ((t (:foreground ,bluezenburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,bluezenburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,bluezenburn-green))))
   `(newsticker-new-item-face ((t (:foreground ,bluezenburn-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,bluezenburn-red))))
   `(newsticker-old-item-face ((t (:foreground ,bluezenburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,bluezenburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,bluezenburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,bluezenburn-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,bluezenburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,bluezenburn-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,bluezenburn-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,bluezenburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:foreground ,bluezenburn-yellow))))

   ;;; external

   ;; full-ack
   `(ack-separator ((t (:foreground ,bluezenburn-fg))))
   `(ack-file ((t (:foreground ,bluezenburn-blue))))
   `(ack-line ((t (:foreground ,bluezenburn-yellow))))
   `(ack-match ((t (:foreground ,bluezenburn-orange :background ,bluezenburn-bg-1 :weight bold))))

   ;; auctex
   `(font-latex-bold ((t (:inherit bold))))
   `(font-latex-warning ((t (:inherit font-lock-warning))))
   `(font-latex-sedate ((t (:foreground ,bluezenburn-yellow :weight bold ))))
   `(font-latex-title-4 ((t (:inherit variable-pitch :weight bold))))

   ;; auto-complete
   `(ac-candidate-face ((t (:background ,bluezenburn-bg+3 :foreground "black"))))
   `(ac-selection-face ((t (:background ,bluezenburn-blue-4 :foreground ,bluezenburn-fg))))
   `(popup-tip-face ((t (:background ,bluezenburn-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((t (:background ,bluezenburn-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,bluezenburn-bg-1))))
   `(popup-isearch-match ((t (:background ,bluezenburn-bg :foreground ,bluezenburn-fg))))

   ;; diff
   `(diff-added ((,class (:foreground ,bluezenburn-green+4))
                 (t (:foreground ,bluezenburn-green-1))))
   `(diff-changed ((t (:foreground ,bluezenburn-yellow))))
   `(diff-removed ((,class (:foreground ,bluezenburn-red))
                   (t (:foreground ,bluezenburn-red-3))))
   `(diff-header ((,class (:background ,bluezenburn-bg+2))
                  (t (:background ,bluezenburn-fg :foreground ,bluezenburn-bg))))
   `(diff-file-header
     ((,class (:background ,bluezenburn-bg+2 :foreground ,bluezenburn-fg :bold t))
      (t (:background ,bluezenburn-fg :foreground ,bluezenburn-bg :bold t))))

   ;; ert
   `(ert-test-result-expected ((t (:foreground ,bluezenburn-green+4 :background ,bluezenburn-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,bluezenburn-red :background ,bluezenburn-bg))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,bluezenburn-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,bluezenburn-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   `(eshell-ls-directory ((t (:foreground ,bluezenburn-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,bluezenburn-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,bluezenburn-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   `(eshell-ls-product ((t (:inherit font-lock-doc))))
   `(eshell-ls-special ((t (:foreground ,bluezenburn-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,bluezenburn-cyan :weight bold))))

   ;; flymake
   `(flymake-errline ((t (:foreground ,bluezenburn-red-1 :weight bold :underline t))))
   `(flymake-warnline ((t (:foreground ,bluezenburn-yellow-1 :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((t (:foreground ,bluezenburn-yellow-1 :weight bold :underline t))))
   `(flyspell-incorrect ((t (:foreground ,bluezenburn-red-1 :weight bold :underline t))))

   ;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,bluezenburn-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
   `(erc-default-face ((t (:foreground ,bluezenburn-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,bluezenburn-yellow))))
   `(erc-keyword-face ((t (:foreground ,bluezenburn-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,bluezenburn-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,bluezenburn-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,bluezenburn-green))))
   `(erc-pal-face ((t (:foreground ,bluezenburn-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,bluezenburn-orange :background ,bluezenburn-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,bluezenburn-green+1))))
   `(erc-underline-face ((t (:underline t))))

   ;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,bluezenburn-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,bluezenburn-blue))))
   `(gnus-summary-high-read ((t (:foreground ,bluezenburn-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,bluezenburn-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,bluezenburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,bluezenburn-blue))))
   `(gnus-summary-low-read ((t (:foreground ,bluezenburn-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,bluezenburn-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,bluezenburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,bluezenburn-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,bluezenburn-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,bluezenburn-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,bluezenburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,bluezenburn-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,bluezenburn-blue))))
   `(gnus-cite-10 ((t (:foreground ,bluezenburn-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,bluezenburn-yellow))))
   `(gnus-cite-2 ((t (:foreground ,bluezenburn-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,bluezenburn-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,bluezenburn-green+2))))
   `(gnus-cite-5 ((t (:foreground ,bluezenburn-green+1))))
   `(gnus-cite-6 ((t (:foreground ,bluezenburn-green))))
   `(gnus-cite-7 ((t (:foreground ,bluezenburn-red))))
   `(gnus-cite-8 ((t (:foreground ,bluezenburn-red-1))))
   `(gnus-cite-9 ((t (:foreground ,bluezenburn-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,bluezenburn-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,bluezenburn-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,bluezenburn-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,bluezenburn-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,bluezenburn-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,bluezenburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,bluezenburn-bg+2))))
   `(gnus-signature ((t (:foreground ,bluezenburn-yellow))))
   `(gnus-x ((t (:background ,bluezenburn-fg :foreground ,bluezenburn-bg))))

   ;; helm
   `(helm-header
     ((t (:foreground ,bluezenburn-green
                      :background ,bluezenburn-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,bluezenburn-yellow
                      :background ,bluezenburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,bluezenburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,bluezenburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,bluezenburn-bg :background ,bluezenburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,bluezenburn-green+4 :background ,bluezenburn-bg-1))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,bluezenburn-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,bluezenburn-bg-05)) ; old emacsen
              (t :weight bold)))

   ;; hl-sexp
   `(hl-sexp-face ((,class (:background ,bluezenburn-bg+1))
                   (t :weight bold)))

   ;; ido-mode
   `(ido-first-match ((t (:foreground ,bluezenburn-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,bluezenburn-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,bluezenburn-yellow))))

   ;; js2-mode
   `(js2-warning-face ((t (:underline ,bluezenburn-orange))))
   `(js2-error-face ((t (:foreground ,bluezenburn-red :weight bold))))
   `(js2-jsdoc-tag-face ((t (:foreground ,bluezenburn-green-1))))
   `(js2-jsdoc-type-face ((t (:foreground ,bluezenburn-green+2))))
   `(js2-jsdoc-value-face ((t (:foreground ,bluezenburn-green+3))))
   `(js2-function-param-face ((t (:foreground, bluezenburn-green+3))))
   `(js2-external-variable-face ((t (:foreground ,bluezenburn-orange))))

   ;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,bluezenburn-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,bluezenburn-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,bluezenburn-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,bluezenburn-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,bluezenburn-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,bluezenburn-red+1))))
   `(jabber-activity-face((t (:foreground ,bluezenburn-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,bluezenburn-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((t (:foreground ,bluezenburn-green+2 :background ,bluezenburn-bg))))

   ;; magit
   `(magit-section-title ((t (:foreground ,bluezenburn-yellow :weight bold))))
   `(magit-branch ((t (:foreground ,bluezenburn-orange :weight bold))))
   `(magit-item-highlight ((t (:background ,bluezenburn-bg+1))))

   ;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment))))
   `(message-header-name ((t (:foreground ,bluezenburn-green+1))))
   `(message-header-other ((t (:foreground ,bluezenburn-green))))
   `(message-header-to ((t (:foreground ,bluezenburn-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,bluezenburn-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,bluezenburn-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,bluezenburn-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,bluezenburn-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,bluezenburn-green))))
   `(message-mml ((t (:foreground ,bluezenburn-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((t (:foreground ,bluezenburn-orange))))
   `(mew-face-header-from ((t (:foreground ,bluezenburn-yellow))))
   `(mew-face-header-date ((t (:foreground ,bluezenburn-green))))
   `(mew-face-header-to ((t (:foreground ,bluezenburn-red))))
   `(mew-face-header-key ((t (:foreground ,bluezenburn-green))))
   `(mew-face-header-private ((t (:foreground ,bluezenburn-green))))
   `(mew-face-header-important ((t (:foreground ,bluezenburn-blue))))
   `(mew-face-header-marginal ((t (:foreground ,bluezenburn-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,bluezenburn-red))))
   `(mew-face-header-xmew ((t (:foreground ,bluezenburn-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,bluezenburn-red))))
   `(mew-face-body-url ((t (:foreground ,bluezenburn-orange))))
   `(mew-face-body-comment ((t (:foreground ,bluezenburn-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,bluezenburn-green))))
   `(mew-face-body-cite2 ((t (:foreground ,bluezenburn-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,bluezenburn-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,bluezenburn-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,bluezenburn-red))))
   `(mew-face-mark-review ((t (:foreground ,bluezenburn-blue))))
   `(mew-face-mark-escape ((t (:foreground ,bluezenburn-green))))
   `(mew-face-mark-delete ((t (:foreground ,bluezenburn-red))))
   `(mew-face-mark-unlink ((t (:foreground ,bluezenburn-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,bluezenburn-green))))
   `(mew-face-mark-unread ((t (:foreground ,bluezenburn-red-2))))
   `(mew-face-eof-message ((t (:foreground ,bluezenburn-green))))
   `(mew-face-eof-part ((t (:foreground ,bluezenburn-yellow))))

   ;; mic-paren
   `(paren-face-match ((t (:foreground ,bluezenburn-cyan :background ,bluezenburn-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,bluezenburn-bg :background ,bluezenburn-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,bluezenburn-bg :background ,bluezenburn-red :weight bold))))

   ;; nav
   `(nav-face-heading ((t (:foreground ,bluezenburn-yellow))))
   `(nav-face-button-num ((t (:foreground ,bluezenburn-cyan))))
   `(nav-face-dir ((t (:foreground ,bluezenburn-green))))
   `(nav-face-hdir ((t (:foreground ,bluezenburn-red))))
   `(nav-face-file ((t (:foreground ,bluezenburn-fg))))
   `(nav-face-hfile ((t (:foreground ,bluezenburn-red-4))))

   ;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,bluezenburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,bluezenburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,bluezenburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,bluezenburn-bg+1))))

   ;; org-mode
   `(org-agenda-date-today
     ((t (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,bluezenburn-fg :weight bold))))
   `(org-checkbox ((t (:background ,bluezenburn-bg+2 :foreground "white"
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,bluezenburn-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,bluezenburn-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,bluezenburn-green+3))))
   `(org-formula ((t (:foreground ,bluezenburn-yellow-2))))
   `(org-headline-done ((t (:foreground ,bluezenburn-green+3))))
   `(org-hide ((t (:foreground ,bluezenburn-bg-1))))
   `(org-level-1 ((t (:foreground ,bluezenburn-orange))))
   `(org-level-2 ((t (:foreground ,bluezenburn-green+1))))
   `(org-level-3 ((t (:foreground ,bluezenburn-blue-1))))
   `(org-level-4 ((t (:foreground ,bluezenburn-yellow-2))))
   `(org-level-5 ((t (:foreground ,bluezenburn-cyan))))
   `(org-level-6 ((t (:foreground ,bluezenburn-green-1))))
   `(org-level-7 ((t (:foreground ,bluezenburn-red-4))))
   `(org-level-8 ((t (:foreground ,bluezenburn-blue-4))))
   `(org-link ((t (:foreground ,bluezenburn-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,bluezenburn-green+4))))
   `(org-scheduled-previously ((t (:foreground ,bluezenburn-red-4))))
   `(org-scheduled-today ((t (:foreground ,bluezenburn-blue+1))))
   `(org-special-keyword ((t (:foreground ,bluezenburn-fg-1 :weight normal))))
   `(org-table ((t (:foreground ,bluezenburn-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,bluezenburn-orange))))
   `(org-todo ((t (:bold t :foreground ,bluezenburn-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,bluezenburn-red :weight bold :underline nil))))
   `(org-column ((t (:background ,bluezenburn-bg-1))))
   `(org-column-title ((t (:background ,bluezenburn-bg-1 :underline t :weight bold))))

   ;; outline
   `(outline-8 ((t (:inherit default))))
   `(outline-7 ((t (:inherit outline-8 :height 1.0))))
   `(outline-6 ((t (:inherit outline-7 :height 1.0))))
   `(outline-5 ((t (:inherit outline-6 :height 1.0))))
   `(outline-4 ((t (:inherit outline-5 :height 1.0))))
   `(outline-3 ((t (:inherit outline-4 :height 1.0))))
   `(outline-2 ((t (:inherit outline-3 :height 1.0))))
   `(outline-1 ((t (:inherit outline-2 :height 1.0))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,bluezenburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,bluezenburn-green+2))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,bluezenburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,bluezenburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,bluezenburn-green-1))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,bluezenburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,bluezenburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,bluezenburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,bluezenburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,bluezenburn-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,bluezenburn-green))))
   `( rainbow-delimiters-depth-12-face ((t (:foreground ,bluezenburn-blue-5))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,bluezenburn-green))))
   `(rpm-spec-doc-face ((t (:foreground ,bluezenburn-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,bluezenburn-red))))
   `(rpm-spec-macro-face ((t (:foreground ,bluezenburn-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,bluezenburn-red))))
   `(rpm-spec-package-face ((t (:foreground ,bluezenburn-red))))
   `(rpm-spec-section-face ((t (:foreground ,bluezenburn-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,bluezenburn-blue))))
   `(rpm-spec-var-face ((t (:foreground ,bluezenburn-red))))

   ;; rst-mode
   `(rst-level-1-face ((t (:foreground ,bluezenburn-orange))))
   `(rst-level-2-face ((t (:foreground ,bluezenburn-green+1))))
   `(rst-level-3-face ((t (:foreground ,bluezenburn-blue-1))))
   `(rst-level-4-face ((t (:foreground ,bluezenburn-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,bluezenburn-cyan))))
   `(rst-level-6-face ((t (:foreground ,bluezenburn-green-1))))

   ;; show-paren
   `(show-paren-mismatch ((t (:foreground ,bluezenburn-red-3 :background ,bluezenburn-bg :weight bold))))
   `(show-paren-match ((t (:foreground ,bluezenburn-blue-1 :background ,bluezenburn-bg :weight bold))))

   ;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))

   ;; SLIME
   `(slime-repl-inputed-output-face ((t (:foreground ,bluezenburn-red))))

   ;; tabbar
   `(tabbar-button ((t (:foreground ,bluezenburn-fg
                                    :background ,bluezenburn-bg))))
   `(tabbar-selected ((t (:foreground ,bluezenburn-fg
                                      :background ,bluezenburn-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,bluezenburn-fg
                                        :background ,bluezenburn-bg+1
                                        :box (:line-width -1 :style released-button)))))

   ;; volatile-highlights
   `(vhl/default-face ((t (:background ,bluezenburn-bg+1))))

   ;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,bluezenburn-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,bluezenburn-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,bluezenburn-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,bluezenburn-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,bluezenburn-green+2 :background ,bluezenburn-bg))))
   `(w3m-lnum-match ((t (:background ,bluezenburn-bg-1
                                     :foreground ,bluezenburn-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,bluezenburn-yellow))))

   ;; whitespace-mode
   `(whitespace-space ((t (:background ,bluezenburn-bg :foreground ,bluezenburn-bg+1))))
   `(whitespace-hspace ((t (:background ,bluezenburn-bg :foreground ,bluezenburn-bg+1))))
   `(whitespace-tab ((t (:background ,bluezenburn-bg :foreground ,bluezenburn-red))))
   `(whitespace-newline ((t (:foreground ,bluezenburn-bg+1))))
   `(whitespace-trailing ((t (:foreground ,bluezenburn-red :background ,bluezenburn-bg))))
   `(whitespace-line ((t (:background ,bluezenburn-bg-05 :foreground ,bluezenburn-magenta))))
   `(whitespace-space-before-tab ((t (:background ,bluezenburn-orange :foreground ,bluezenburn-orange))))
   `(whitespace-indentation ((t (:background ,bluezenburn-yellow :foreground ,bluezenburn-red))))
   `(whitespace-empty ((t (:background ,bluezenburn-yellow :foreground ,bluezenburn-red))))
   `(whitespace-space-after-tab ((t (:background ,bluezenburn-yellow :foreground ,bluezenburn-red))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,bluezenburn-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,bluezenburn-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,bluezenburn-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,bluezenburn-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,bluezenburn-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,bluezenburn-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,bluezenburn-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,bluezenburn-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,bluezenburn-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,bluezenburn-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,bluezenburn-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,bluezenburn-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,bluezenburn-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,bluezenburn-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,bluezenburn-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,bluezenburn-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,bluezenburn-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,bluezenburn-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,bluezenburn-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,bluezenburn-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,bluezenburn-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,bluezenburn-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,bluezenburn-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,bluezenburn-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,bluezenburn-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))

   ;; which-func-mode
   `(which-func ((t (:foreground ,bluezenburn-green+4))))

   ;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,bluezenburn-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,bluezenburn-bg-1 :foreground ,bluezenburn-bg-1))))
   )

  ;;; custom theme variables
  (custom-theme-set-variables
   'bluezenburn
   `(ansi-color-names-vector [,bluezenburn-bg ,bluezenburn-red ,bluezenburn-green ,bluezenburn-yellow
                                          ,bluezenburn-blue ,bluezenburn-magenta ,bluezenburn-cyan ,bluezenburn-fg])

   ;; fill-column-indicator
   `(fci-rule-color ,bluezenburn-bg-05))

  ;;; colors for the ansi-term
  (eval-after-load 'term
    `(setq ansi-term-color-vector
           (vector 'unspecified ,bluezenburn-bg ,bluezenburn-red ,bluezenburn-green ,bluezenburn-yellow
                   ,bluezenburn-blue ,bluezenburn-magenta ,bluezenburn-cyan ,bluezenburn-fg))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'bluezenburn)

;;; bluezenburn-theme.el ends here.
(deftheme assem "The Assem color theme")
(let ((class '((class color) (min-colors 89)))
      ;; Assem palette
      ;; colors with +x are lighter, colors with -x are darker
      (assem-fg "#dcdccc")
      ;(assem-fg "#5090ff")
      ;(assem-fg "#BCC7BCC7BCC7")
      (assem-fg-1 "#656555")
      (assem-bg-3 "#000000")
      (assem-bg-2 "#1b1b1b")
      (assem-bg-1 "#2b2b2b")
      (assem-bg-05 "#383838")
      (assem-bg "#473A473A473A")
      (assem-bg+1 "#4f4f4f")
      (assem-bg+2 "#5f5f5f")
      (assem-bg+3 "#6f6f6f")
      (assem-red+1 "#dca3a3")
      (assem-red "#cc9393")
      (assem-red-1 "#bc8383")
      (assem-red-2 "#ac7373")
      (assem-red-3 "#9c6363")
      (assem-red-4 "#8c5353")
      (assem-black "#000000")
      (assem-white "#ffffff")
      (assem-orange "#dfaf8f")
      (assem-yellow "#f0dfaf")
      (assem-yellow-1 "#e0cf9f")
      (assem-yellow-2 "#d0bf8f")
      (assem-green-1 "#5f7f5f")
      (assem-green "#767D9D3F26C2")
      (assem-green+1 "#8fb28f")
      (assem-green+2 "#9fc59f")
      (assem-green+3 "#afd8af")
      (assem-green+4 "#bfebbf")
      (assem-lightgreen "LightGreen")
      (assem-cyan "#0000AC4AAC4A")
      (assem-cyan+1 "#0000C29DC29D")
      (assem-cyan+2 "#0000DBD5DBD5")
      (assem-cyan-1 "#000093F493F4")
      (assem-lightblue "LightBlue1")
      (assem-magic "LightCyan1")
      (assem-blue+1 "#94bff3")
      (assem-blue "#8cd0d3")
      (assem-blue-1 "#7cb8bb")
      (assem-blue-2 "#6ca0a3")
      (assem-blue-3 "#5c888b")
      (assem-blue-4 "#4c7073")
      (assem-blue-5 "#366060")
      (assem-magenta "#dc8cc3"))
  (custom-theme-set-faces
   'assem
   `(button ((t (:background ,assem-bg-1 :underline t))))
   `(link ((,class (:foreground ,assem-yellow :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,assem-yellow-2 :underline t :weight normal))))

   ;;; basic coloring
   `(default ((,class (:foreground ,assem-fg :background ,assem-bg-1))))
   `(cursor ((,class (:foreground ,assem-fg))))
   `(escape-glyph-face ((,class (:foreground ,assem-red))))
   `(fringe ((,class (:foreground ,assem-fg :background ,assem-bg+1))))
   `(header-line ((,class 
		   (:foreground ,assem-yellow
				:background ,assem-bg-1
				:box 
				(:line-width -1 :style released-button)))))
   `(highlight ((,class (:background ,assem-blue-4))))
   
   ;;; compilation
   `(compilation-column-face ((,class (:foreground ,assem-yellow))))
   `(compilation-enter-directory-face ((,class (:foreground ,assem-green))))
   `(compilation-error-face ((,class 
                              (:foreground 
                               ,assem-red+1
                               :weight bold 
                               :underline t))))
   `(compilation-face ((,class (:foreground ,assem-fg))))
   `(compilation-info-face ((,class (:foreground ,assem-blue))))
   `(compilation-info ((,class (:foreground ,assem-green+4 :underline t))))
   `(compilation-leave-directory-face ((,class (:foreground ,assem-green))))
   `(compilation-line-face ((,class (:foreground ,assem-yellow))))
   `(compilation-line-number ((,class (:foreground ,assem-yellow))))
   `(compilation-message-face ((,class (:foreground ,assem-blue))))
   `(compilation-warning-face ((,class (:foreground ,assem-yellow-1 :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((,class (:foreground ,assem-fg))))
   `(grep-error-face ((,class (:foreground ,assem-red-1 :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,assem-blue))))
   `(grep-match-face ((,class (:foreground ,assem-orange :weight bold))))
   `(match ((,class (:background ,assem-bg-1 :foreground ,assem-orange :weight bold))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,assem-yellow :background ,assem-bg-1))))
   `(isearch-fail ((,class (:foreground ,assem-fg :background ,assem-red-4))))
   `(lazy-highlight ((,class (:foreground ,assem-yellow :background ,assem-bg+2))))

   `(menu ((,class (:foreground ,assem-fg :background ,assem-bg))))
   `(minibuffer-prompt ((,class (:foreground ,assem-yellow))))
   `(mode-line
     ((,class (:foreground ,assem-lightgreen
                           :background ,assem-bg-2
                           :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((,class (:foreground ,assem-yellow :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,assem-green-1
                           :background ,assem-bg+1
                           :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,assem-bg+3))))
   `(secondary-selection ((,class (:background ,assem-bg+2))))
   `(trailing-whitespace ((,class (:background ,assem-red))))
   `(vertical-border ((,class (:foreground ,assem-fg))))

   ;;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,assem-blue))))
   `(font-lock-comment-face ((,class (:foreground ,assem-cyan+2))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,assem-cyan+2))))
   `(font-lock-constant-face ((,class (:foreground ,assem-green+4))))
   `(font-lock-doc-face ((,class (:foreground ,assem-green+1))))
   `(font-lock-doc-string-face ((,class (:foreground ,assem-blue+1))))
   `(font-lock-function-name-face ((,class (:foreground ,assem-blue))))
   `(font-lock-keyword-face ((,class (:foreground ,assem-cyan+2 :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,assem-fg))))
   `(font-lock-preprocessor-face ((,class (:foreground ,assem-cyan :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,assem-orange :weight bold))))
   `(font-lock-type-face ((,class (:foreground ,assem-blue))))
   `(font-lock-variable-name-face ((,class (:foreground ,assem-orange))))
   `(font-lock-warning-face ((,class (:foreground ,assem-yellow-1 :weight bold :underline t))))

   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   ;;; newsticker
   `(newsticker-date-face ((,class (:foreground ,assem-fg))))
   `(newsticker-default-face ((,class (:foreground ,assem-fg))))
   `(newsticker-enclosure-face ((,class (:foreground ,assem-green+3))))
   `(newsticker-extra-face ((,class (:foreground ,assem-bg+2 :height 0.8))))
   `(newsticker-feed-face ((,class (:foreground ,assem-fg))))
   `(newsticker-immortal-item-face ((,class (:foreground ,assem-green))))
   `(newsticker-new-item-face ((,class (:foreground ,assem-blue))))
   `(newsticker-obsolete-item-face ((,class (:foreground ,assem-red))))
   `(newsticker-old-item-face ((,class (:foreground ,assem-bg+3))))
   `(newsticker-statistics-face ((,class (:foreground ,assem-fg))))
   `(newsticker-treeview-face ((,class (:foreground ,assem-fg))))
   `(newsticker-treeview-immortal-face ((,class (:foreground ,assem-green))))
   `(newsticker-treeview-listwindow-face ((,class (:foreground ,assem-fg))))
   `(newsticker-treeview-new-face ((,class (:foreground ,assem-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((,class (:foreground ,assem-red))))
   `(newsticker-treeview-old-face ((,class (:foreground ,assem-bg+3))))
   `(newsticker-treeview-selection-face ((,class (:foreground ,assem-yellow))))

   ;;; external

   ;; full-ack
   `(ack-separator ((,class (:foreground ,assem-fg))))
   `(ack-file ((,class (:foreground ,assem-blue))))
   `(ack-line ((,class (:foreground ,assem-yellow))))
   `(ack-match ((,class (:foreground ,assem-orange :background ,assem-bg-1 :weigth bold))))

   ;;; ace-jump
   `(ace-jump-face-background ((,class (:foreground 
                                        ,assem-fg-1
                                        :background
                                        ,assem-bg-1))))
   `(ace-jump-face-foreground ((,class (:foreground 
                                        ,assem-cyan+2
                                        :background
                                        ,assem-bg-1
                                        :weight
                                        extra-bold
                                        ;; :height
                                        ;; 1.2
                                        ;; :width
                                        ;; ultra-expanded
                                        ;; :overline
                                        ;; ,assem-magenta
                                        ;; :underline
                                        ;; ,assem-magenta
                                        ;;:box
                                        ;; (:line-width
                                        ;;  1
                                        ;;  :color
                                        ;;  ,assem-magenta
                                        ;;  :style
                                        ;;  released-button)
                                        ))))

   ;; auctex
   `(font-latex-bold ((,class (:inherit bold))))
   `(font-latex-warning ((,class (:inherit font-lock-warning))))
   `(font-latex-sedate ((,class (:foreground ,assem-yellow :weight bold ))))
   `(font-latex-title-4 ((,class (:inherit variable-pitch :weight bold))))

   ;; auto-complete
   `(ac-candidate-face ((,class (:background ,assem-bg+3 :foreground "black"))))
   `(ac-selection-face ((,class (:background ,assem-blue-4 :foreground ,assem-fg))))
   `(popup-tip-face ((,class (:background ,assem-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,assem-blue-5))))
   `(popup-scroll-bar-background-face ((,class (:background ,assem-bg-1))))
   `(popup-isearch-match ((,class (:background ,assem-bg :foreground ,assem-fg))))

   ;; diff
   `(diff-added ((,class (:foreground ,assem-green+4))))
   `(diff-changed ((,class (:foreground ,assem-yellow))))
   `(diff-removed ((,class (:foreground ,assem-red))))
   `(diff-header ((,class (:background ,assem-bg+2))))
   `(diff-file-header
     ((,class (:background ,assem-bg+2 :foreground ,assem-fg :bold t))))

   ;; ert
   `(ert-test-result-expected ((,class (:foreground ,assem-green+4 :background ,assem-bg))))
   `(ert-test-result-unexpected ((,class (:foreground ,assem-red :background ,assem-bg))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,assem-yellow :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,assem-red-1 :weight bold))))
   `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
   `(eshell-ls-directory ((,class (:foreground ,assem-blue+1 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,assem-red+1 :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,assem-fg))))
   `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
   `(eshell-ls-product ((,class (:inherit font-lock-doc))))
   `(eshell-ls-special ((,class (:foreground ,assem-yellow :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,assem-cyan :weight bold))))

   ;; flymake
   `(flymake-errline ((,class (:foreground ,assem-red-1 :weight bold :underline t))))
   `(flymake-warnline ((,class (:foreground ,assem-yellow-1 :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((,class (:foreground ,assem-yellow-1 :weight bold :underline t))))
   `(flyspell-incorrect ((,class (:foreground ,assem-red-1 :weight bold :underline t))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,assem-blue :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,assem-fg))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,assem-yellow))))
   `(erc-keyword-face ((,class (:foreground ,assem-blue :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,assem-cyan :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,assem-cyan+2 :weigth bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,assem-cyan+1))))
   `(erc-pal-face ((,class (:foreground ,assem-orange :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,assem-blue+1 :background ,assem-bg :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,assem-lightgreen))))
   `(erc-underline-face ((t (:underline t))))

   ;; gnus
   `(gnus-group-mail-1 ((,class (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((,class (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((,class (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((,class (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((,class (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((,class (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((,class (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((,class (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((,class (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((,class (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((,class (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((,class (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((,class (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((,class (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((,class (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((,class (:inherit message-header-other))))
   `(gnus-header-from ((,class (:inherit message-header-from))))
   `(gnus-header-name ((,class (:inherit message-header-name))))
   `(gnus-header-newsgroups ((,class (:inherit message-header-other))))
   `(gnus-header-subject ((,class (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((,class (:foreground ,assem-orange))))
   `(gnus-summary-high-ancient ((,class (:foreground ,assem-blue))))
   `(gnus-summary-high-read ((,class (:foreground ,assem-green :weight bold))))
   `(gnus-summary-high-ticked ((,class (:foreground ,assem-orange :weight bold))))
   `(gnus-summary-high-unread ((,class (:foreground ,assem-fg :weight bold))))
   `(gnus-summary-low-ancient ((,class (:foreground ,assem-blue))))
   `(gnus-summary-low-read ((t (:foreground ,assem-green))))
   `(gnus-summary-low-ticked ((,class (:foreground ,assem-orange :weight bold))))
   `(gnus-summary-low-unread ((,class (:foreground ,assem-fg))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,assem-blue))))
   `(gnus-summary-normal-read ((,class (:foreground ,assem-green))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,assem-orange :weight bold))))
   `(gnus-summary-normal-unread ((,class (:foreground ,assem-fg))))
   `(gnus-summary-selected ((,class (:foreground ,assem-yellow :weight bold))))
   `(gnus-cite-1 ((,class (:foreground ,assem-blue))))
   `(gnus-cite-10 ((,class (:foreground ,assem-yellow-1))))
   `(gnus-cite-11 ((,class (:foreground ,assem-yellow))))
   `(gnus-cite-2 ((,class (:foreground ,assem-blue-1))))
   `(gnus-cite-3 ((,class (:foreground ,assem-blue-2))))
   `(gnus-cite-4 ((,class (:foreground ,assem-green+2))))
   `(gnus-cite-5 ((,class (:foreground ,assem-green+1))))
   `(gnus-cite-6 ((,class (:foreground ,assem-green))))
   `(gnus-cite-7 ((,class (:foreground ,assem-red))))
   `(gnus-cite-8 ((,class (:foreground ,assem-red-1))))
   `(gnus-cite-9 ((,class (:foreground ,assem-red-2))))
   `(gnus-group-news-1-empty ((,class (:foreground ,assem-yellow))))
   `(gnus-group-news-2-empty ((,class (:foreground ,assem-green+3))))
   `(gnus-group-news-3-empty ((,class (:foreground ,assem-green+1))))
   `(gnus-group-news-4-empty ((,class (:foreground ,assem-blue-2))))
   `(gnus-group-news-5-empty ((,class (:foreground ,assem-blue-3))))
   `(gnus-group-news-6-empty ((,class (:foreground ,assem-bg+2))))
   `(gnus-group-news-low-empty ((,class (:foreground ,assem-bg+2))))
   `(gnus-signature ((,class (:foreground ,assem-yellow))))
   `(gnus-x ((,class (:background ,assem-fg :foreground ,assem-bg))))

   ;; helm
   `(helm-header
     ((,class (:foreground ,assem-green
                           :background ,assem-bg
                           :underline nil
                           :box nil))))
   `(helm-source-header
     ((,class (:foreground ,assem-yellow
                           :background ,assem-bg-1
                           :underline nil
                           :weight bold
                           :box (:line-width -1 :style released-button)))))
   `(helm-selection ((,class (:background ,assem-bg+1 :underline nil))))
   `(helm-selection-line ((,class (:background ,assem-bg+1))))
   `(helm-visible-mark ((,class (:foreground ,assem-bg :background ,assem-yellow-2))))
   `(helm-candidate-number ((,class (:foreground ,assem-green+4 :background ,assem-bg-1))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,assem-bg-1))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,assem-orange :weight bold))))
   `(ido-only-match ((,class 
                      (:foreground 
                       ,assem-orange 
                       :background
                       ,assem-bg-2
                       :weight bold))))
   `(ido-subdir ((,class (:foreground ,assem-yellow))))

   ;; js2-mode
   `(js2-warning-face ((,class (:underline ,assem-orange))))
   `(js2-error-face ((,class (:foreground ,assem-red :weight bold))))
   `(js2-jsdoc-tag-face ((,class (:foreground ,assem-green-1))))
   `(js2-jsdoc-type-face ((,class (:foreground ,assem-green+2))))
   `(js2-jsdoc-value-face ((,class (:foreground ,assem-green+3))))
   `(js2-function-param-face ((,class (:foreground, assem-green+3))))
   `(js2-external-variable-face ((,class (:foreground ,assem-orange))))

   ;; jabber-mode
   `(jabber-roster-user-away ((,class (:foreground ,assem-green+2))))
   `(jabber-roster-user-online ((,class (:foreground ,assem-blue-1))))
   `(jabber-roster-user-dnd ((,class (:foreground ,assem-red+1))))
   `(jabber-rare-time-face ((,class (:foreground ,assem-green+1))))
   `(jabber-chat-prompt-local ((,class (:foreground ,assem-blue-1))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,assem-red+1))))
   `(jabber-activity-face((,class (:foreground ,assem-red+1))))
   `(jabber-activity-personal-face ((,class (:foreground ,assem-blue+1))))
   `(jabber-title-small ((,class (:height 1.1 :weight bold))))
   `(jabber-title-medium ((,class (:height 1.2 :weight bold))))
   `(jabber-title-large ((,class (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((,class (:foreground ,assem-green+2 :background ,assem-bg))))

   ;; magit
   `(magit-section-title ((,class (:foreground ,assem-yellow :weight bold))))
   `(magit-branch ((,class (:foreground ,assem-orange :weight bold))))
   `(magit-item-highlight ((,class (:background ,assem-bg+1))))

   ;; message-mode
   `(message-cited-text ((,class (:inherit font-lock-comment))))
   `(message-header-name ((,class (:foreground ,assem-green+1))))
   `(message-header-other ((,class (:foreground ,assem-green))))
   `(message-header-to ((,class (:foreground ,assem-yellow :weight bold))))
   `(message-header-from ((,class (:foreground ,assem-yellow :weight bold))))
   `(message-header-cc ((,class (:foreground ,assem-yellow :weight bold))))
   `(message-header-newsgroups ((,class (:foreground ,assem-yellow :weight bold))))
   `(message-header-subject ((,class (:foreground ,assem-orange :weight bold))))
   `(message-header-xheader ((,class (:foreground ,assem-green))))
   `(message-mml ((,class (:foreground ,assem-yellow :weight bold))))
   `(message-separator ((,class (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((,class (:foreground ,assem-orange))))
   `(mew-face-header-from ((,class (:foreground ,assem-yellow))))
   `(mew-face-header-date ((,class (:foreground ,assem-green))))
   `(mew-face-header-to ((,class (:foreground ,assem-red))))
   `(mew-face-header-key ((,class (:foreground ,assem-green))))
   `(mew-face-header-private ((,class (:foreground ,assem-green))))
   `(mew-face-header-important ((,class (:foreground ,assem-blue))))
   `(mew-face-header-marginal ((,class (:foreground ,assem-fg :weight bold))))
   `(mew-face-header-warning ((,class (:foreground ,assem-red))))
   `(mew-face-header-xmew ((,class (:foreground ,assem-green))))
   `(mew-face-header-xmew-bad ((,class (:foreground ,assem-red))))
   `(mew-face-body-url ((,class (:foreground ,assem-orange))))
   `(mew-face-body-comment ((,class (:foreground ,assem-fg :slant italic))))
   `(mew-face-body-cite1 ((,class (:foreground ,assem-green))))
   `(mew-face-body-cite2 ((,class (:foreground ,assem-blue))))
   `(mew-face-body-cite3 ((,class (:foreground ,assem-orange))))
   `(mew-face-body-cite4 ((,class (:foreground ,assem-yellow))))
   `(mew-face-body-cite5 ((,class (:foreground ,assem-red))))
   `(mew-face-mark-review ((,class (:foreground ,assem-blue))))
   `(mew-face-mark-escape ((,class (:foreground ,assem-green))))
   `(mew-face-mark-delete ((,class (:foreground ,assem-red))))
   `(mew-face-mark-unlink ((,class (:foreground ,assem-yellow))))
   `(mew-face-mark-refile ((,class (:foreground ,assem-green))))
   `(mew-face-mark-unread ((,class (:foreground ,assem-red-2))))
   `(mew-face-eof-message ((,class (:foreground ,assem-green))))
   `(mew-face-eof-part ((,class (:foreground ,assem-yellow))))

   ;; mic-paren
   `(paren-face-match ((,class (:foreground ,assem-cyan :background ,assem-bg :weight bold))))
   `(paren-face-mismatch ((,class (:foreground ,assem-bg :background ,assem-magenta :weight bold))))
   `(paren-face-no-match ((,class (:foreground ,assem-bg :background ,assem-red :weight bold))))

   ;; nav
   `(nav-face-heading ((,class (:foreground ,assem-yellow))))
   `(nav-face-button-num ((,class (:foreground ,assem-cyan))))
   `(nav-face-dir ((,class (:foreground ,assem-green))))
   `(nav-face-hdir ((,class (:foreground ,assem-red))))
   `(nav-face-file ((,class (:foreground ,assem-fg))))
   `(nav-face-hfile ((,class (:foreground ,assem-red-4))))

   ;; mumamo
   `(mumamo-background-chunk-major ((,class (:background nil))))
   `(mumamo-background-chunk-submode1 ((,class (:background ,assem-bg-1))))
   `(mumamo-background-chunk-submode2 ((,class (:background ,assem-bg+2))))
   `(mumamo-background-chunk-submode3 ((,class (:background ,assem-bg+3))))
   `(mumamo-background-chunk-submode4 ((,class (:background ,assem-bg+1))))

   ;; org-mode
   `(org-agenda-date-today
     ((,class (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:foreground ,assem-fg :weight bold))))
   `(org-checkbox ((,class (:background ,assem-bg+2 :foreground "white"
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,assem-blue :underline t))))
   `(org-deadline-announce ((,class (:foreground ,assem-red-1))))
   `(org-done ((,class (:bold t :weight bold :foreground ,assem-green+3))))
   `(org-formula ((,class (:foreground ,assem-yellow-2))))
   `(org-headline-done ((,class (:foreground ,assem-green+3))))
   `(org-hide ((,class (:foreground ,assem-bg-1))))
   `(org-level-1 ((,class (:foreground ,assem-cyan))))
   `(org-level-2 ((,class (:foreground ,assem-blue-1))))
   `(org-level-3 ((,class (:foreground ,assem-blue-3))))
   `(org-level-4 ((,class (:foreground ,assem-cyan+2))))
   `(org-level-5 ((,class (:foreground ,assem-blue+1))))
   `(org-level-6 ((,class (:foreground ,assem-green-1))))
   `(org-level-7 ((,class (:foreground ,assem-red-4))))
   `(org-level-8 ((,class (:foreground ,assem-blue-4))))
   `(org-link ((,class (:foreground ,assem-yellow-2 :underline t))))
   `(org-scheduled ((,class (:foreground ,assem-green+4))))
   `(org-scheduled-previously ((,class (:foreground ,assem-red-4))))
   `(org-scheduled-today ((,class (:foreground ,assem-blue+1))))
   `(org-special-keyword ((,class (:foreground 
                                   ,assem-lightgreen 
                                   :weight 
                                   bold))))
   `(org-table ((,class (:foreground ,assem-green+2))))
   `(org-tag ((,class (:bold t :weight bold))))
   `(org-time-grid ((,class (:foreground ,assem-orange))))
   `(org-todo ((,class (:bold t :foreground ,assem-red :weight bold))))
   `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   `(org-warning ((,class (:bold t :foreground ,assem-red :weight bold :underline nil))))
   `(org-column ((,class (:background ,assem-bg-1))))
   `(org-column-title ((,class (:background ,assem-bg-1 :underline t :weight bold))))

   ;; outline
   `(outline-8 ((,class (:inherit default))))
   `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
   `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
   `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
   `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
   `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
   `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
   `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,assem-cyan))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,assem-yellow))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,assem-blue+1))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,assem-red+1))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,assem-orange))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,assem-blue-1))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,assem-green+4))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,assem-red-3))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,assem-yellow-2))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,assem-green+2))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,assem-blue+1))))
   `(rainbow-delimiters-depth-12-face ((,class (:foreground ,assem-red-4))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((,class (:foreground ,assem-green))))
   `(rpm-spec-doc-face ((,class (:foreground ,assem-green))))
   `(rpm-spec-ghost-face ((,class (:foreground ,assem-red))))
   `(rpm-spec-macro-face ((,class (:foreground ,assem-yellow))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,assem-red))))
   `(rpm-spec-package-face ((,class (:foreground ,assem-red))))
   `(rpm-spec-section-face ((,class (:foreground ,assem-yellow))))
   `(rpm-spec-tag-face ((,class (:foreground ,assem-blue))))
   `(rpm-spec-var-face ((,class (:foreground ,assem-red))))

   ;; rst-mode
   `(rst-level-1-face ((,class (:foreground ,assem-orange))))
   `(rst-level-2-face ((,class (:foreground ,assem-green+1))))
   `(rst-level-3-face ((,class (:foreground ,assem-blue-1))))
   `(rst-level-4-face ((,class (:foreground ,assem-yellow-2))))
   `(rst-level-5-face ((,class (:foreground ,assem-cyan))))
   `(rst-level-6-face ((,class (:foreground ,assem-green-1))))

   ;; show-paren
   `(show-paren-mismatch ((,class (:foreground ,assem-red-3 :background ,assem-bg :weight bold))))
   `(show-paren-match ((,class (:foreground ,assem-blue-1 :background ,assem-bg :weight bold))))

   ;; SLIME
   `(slime-repl-inputed-output-face ((,class (:foreground ,assem-red))))

   ;; volatile-highlights
   `(vhl/default-face ((,class (:background ,assem-bg+1))))

   ;; whitespace-mode
   `(whitespace-space ((,class (:background ,assem-bg :foreground ,assem-bg+1))))
   `(whitespace-hspace ((,class (:background ,assem-bg :foreground ,assem-bg+1))))
   `(whitespace-tab ((,class (:background ,assem-bg :foreground ,assem-red))))
   `(whitespace-newline ((,class (:foreground ,assem-bg+1))))
   `(whitespace-trailing ((,class (:foreground ,assem-red :background ,assem-bg))))
   `(whitespace-line ((,class (:background ,assem-bg-05 :foreground ,assem-magenta))))
   `(whitespace-space-before-tab ((,class (:background ,assem-orange :foreground ,assem-orange))))
   `(whitespace-indentation ((,class (:background ,assem-yellow :foreground ,assem-red))))
   `(whitespace-empty ((,class (:background ,assem-yellow :foreground ,assem-red))))
   `(whitespace-space-after-tab ((,class (:background ,assem-yellow :foreground ,assem-red))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((,class (:foreground ,assem-red-2))))
   `(wl-highlight-folder-many-face ((,class (:foreground ,assem-red-1))))
   `(wl-highlight-folder-path-face ((,class (:foreground ,assem-orange))))
   `(wl-highlight-folder-unread-face ((,class (:foreground ,assem-blue))))
   `(wl-highlight-folder-zero-face ((,class (:foreground ,assem-fg))))
   `(wl-highlight-folder-unknown-face ((,class (:foreground ,assem-blue))))
   `(wl-highlight-message-citation-header ((,class (:foreground ,assem-red-1))))
   `(wl-highlight-message-cited-text-1 ((,class (:foreground ,assem-red))))
   `(wl-highlight-message-cited-text-2 ((,class (:foreground ,assem-green+2))))
   `(wl-highlight-message-cited-text-3 ((,class (:foreground ,assem-blue))))
   `(wl-highlight-message-cited-text-4 ((,class (:foreground ,assem-blue+1))))
   `(wl-highlight-message-header-contents-face ((,class (:foreground ,assem-green))))
   `(wl-highlight-message-headers-face ((,class (:foreground ,assem-red+1))))
   `(wl-highlight-message-important-header-contents ((,class (:foreground ,assem-green+2))))
   `(wl-highlight-message-header-contents ((,class (:foreground ,assem-green+1))))
   `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,assem-green+2))))
   `(wl-highlight-message-signature ((,class (:foreground ,assem-green))))
   `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,assem-fg))))
   `(wl-highlight-summary-answered-face ((,class (:foreground ,assem-blue))))
   `(wl-highlight-summary-disposed-face ((,class (:foreground ,assem-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((,class (:foreground ,assem-blue))))
   `(wl-highlight-summary-normal-face ((,class (:foreground ,assem-fg))))
   `(wl-highlight-summary-thread-top-face ((,class (:foreground ,assem-yellow))))
   `(wl-highlight-thread-indent-face ((,class (:foreground ,assem-magenta))))
   `(wl-highlight-summary-refiled-face ((,class (:foreground ,assem-fg))))
   `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

   ;; which-func-mode
   `(which-func ((,class (:foreground ,assem-green+4))))

   ;; yascroll
   `(yascroll:thumb-text-area ((,class (:background ,assem-bg-1))))
   `(yascroll:thumb-fringe ((,class (:background ,assem-bg-1 :foreground ,assem-bg-1))))
   )

  ;;; custom theme variables
  (custom-theme-set-variables
   'assem
   `(ansi-color-names-vector [,assem-bg ,assem-red ,assem-green ,assem-yellow
                                          ,assem-blue ,assem-magenta ,assem-cyan ,assem-fg])

   ;; fill-column-indicator
   `(fci-rule-color ,assem-bg-05))

  ;;; colors for the ansi-term
  (eval-after-load 'term
    `(setq ansi-term-color-vector
         (vector 'unspecified ,assem-bg ,assem-red ,assem-green ,assem-yellow
		   ,assem-blue ,assem-magenta ,assem-cyan ,assem-fg))))

(provide-theme 'assem)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; anti-zenburn-theme.el --- Low contrast Zenburn-derived theme

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>
;; Version: 1.6

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Credits:

;; Derived from Bozhidar Batsov's port of the Zenburn theme
;; located here http://github.com/bbatsov/zenburn-emacs

;;; Code:

(deftheme anti-zenburn "Reversed Zenburn color theme.")

(let ((class '((class color) (min-colors 89)))
      ;; Zenburn palette
      ;; colors with -x are lighter, colors with +x are darker
      (zenburn-fg "#232333")
      (zenburn-fg-1 "#9a9aaa")
      (zenburn-bg-1 "#d4d4d4")
      (zenburn-bg-05 "#c7c7c7")
      (zenburn-bg "#c0c0c0")
      (zenburn-bg+1 "#b0b0b0")
      (zenburn-bg+2 "#a0a0a0")
      (zenburn-bg+3 "#909090")
      (zenburn-blue+1 "#235c5c")
      (zenburn-blue "#336c6c")
      (zenburn-blue-1 "#437c7c")
      (zenburn-blue-2 "#538c8c")
      (zenburn-blue-3 "#639c9c")
      (zenburn-blue-4 "#73acac")
      (zenburn-light-blue "#205070")
      (zenburn-dark-blue "#0f2050")
      (zenburn-dark-blue-1 "#1f3060")
      (zenburn-dark-blue-2 "#2f4070")
      (zenburn-violet-1 "#a080a0")
      (zenburn-violet "#806080")
      (zenburn-violet+1 "#704d70")
      (zenburn-violet+2 "#603a60")
      (zenburn-violet+3 "#502750")
      (zenburn-violet+4 "#401440")
      (zenburn-bordeaux "#6c1f1c")
      (zenburn-brown+1 "#6b400c")
      (zenburn-brown "#732f2c")
      (zenburn-brown-1 "#834744")
      (zenburn-brown-2 "#935f5c")
      (zenburn-brown-3 "#a37774")
      (zenburn-brown-4 "#b38f8c")
      (zenburn-brown-5 "#c99f9f")
      (zenburn-green "#23733c"))
  (custom-theme-set-faces
   'anti-zenburn
   '(button ((t (:underline t))))
   `(link ((,class (:foreground ,zenburn-dark-blue :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,zenburn-dark-blue-2 :underline t :weight normal))))

   ;;; basic coloring
   `(default ((,class (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(cursor ((,class (:foreground ,zenburn-fg))))
   `(escape-glyph-face ((,class (:foreground ,zenburn-blue))))
   `(fringe ((,class (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
   `(header-line ((,class (:foreground ,zenburn-dark-blue
				       :background ,zenburn-bg-1
				       :box (:line-width -1 :style released-button)))))
   `(highlight ((,class (:background ,zenburn-bg-05))
		(t (:background ,zenburn-blue-4))))

   ;;; compilation
   `(compilation-column-face ((,class (:foreground ,zenburn-dark-blue))))
   `(compilation-enter-directory-face ((,class (:foreground ,zenburn-violet))))
   `(compilation-error-face ((,class (:foreground ,zenburn-blue-1 :weight bold :underline t))))
   `(compilation-face ((,class (:foreground ,zenburn-fg))))
   `(compilation-info-face ((,class (:foreground ,zenburn-brown))))
   `(compilation-info ((,class (:foreground ,zenburn-violet+4 :underline t))))
   `(compilation-leave-directory-face ((,class (:foreground ,zenburn-violet))))
   `(compilation-line-face ((,class (:foreground ,zenburn-dark-blue))))
   `(compilation-line-number ((,class (:foreground ,zenburn-dark-blue))))
   `(compilation-message-face ((,class (:foreground ,zenburn-brown))))
   `(compilation-warning-face ((,class (:foreground ,zenburn-dark-blue-1 :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((,class (:foreground ,zenburn-fg))))
   `(grep-error-face ((,class (:foreground ,zenburn-blue-1 :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,zenburn-brown))))
   `(grep-match-face ((,class (:foreground ,zenburn-light-blue :weight bold))))
   `(match ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-light-blue :weight bold))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,zenburn-dark-blue :background ,zenburn-bg-1))))
   `(isearch-fail ((,class (:foreground ,zenburn-fg :background ,zenburn-blue-4))))
   `(lazy-highlight ((,class (:foreground ,zenburn-dark-blue :background ,zenburn-bg+2))))

   `(menu ((,class (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(minibuffer-prompt ((,class (:foreground ,zenburn-dark-blue))))
   `(mode-line
     ((,class (:foreground ,zenburn-violet+1
			   :background ,zenburn-bg-1
			   :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((,class (:foreground ,zenburn-dark-blue :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,zenburn-violet-1
			   :background ,zenburn-bg-05
			   :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,zenburn-bg-1))))
   `(secondary-selection ((,class (:background ,zenburn-bg+2))))
   `(trailing-whitespace ((,class (:background ,zenburn-blue))))
   `(vertical-border ((,class (:foreground ,zenburn-fg))))

   ;;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,zenburn-bordeaux))))
   `(font-lock-comment-face ((,class (:foreground ,zenburn-violet))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,zenburn-violet))))
   `(font-lock-constant-face ((,class (:foreground ,zenburn-violet+4))))
   `(font-lock-doc-face ((,class (:foreground ,zenburn-violet+1))))
   `(font-lock-doc-string-face ((,class (:foreground ,zenburn-brown-2))))
   `(font-lock-function-name-face ((,class (:foreground ,zenburn-brown))))
   `(font-lock-keyword-face ((,class (:foreground ,zenburn-dark-blue :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,zenburn-fg))))
   `(font-lock-preprocessor-face ((,class (:foreground ,zenburn-brown+1))))
   `(font-lock-string-face ((,class (:foreground ,zenburn-blue))))
   `(font-lock-type-face ((,class (:foreground ,zenburn-brown-1))))
   `(font-lock-variable-name-face ((,class (:foreground ,zenburn-light-blue))))
   `(font-lock-warning-face ((,class (:foreground ,zenburn-dark-blue-1 :weight bold :underline t))))

   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   ;;; newsticker
   `(newsticker-date-face ((,class (:foreground ,zenburn-fg))))
   `(newsticker-default-face ((,class (:foreground ,zenburn-fg))))
   `(newsticker-enclosure-face ((,class (:foreground ,zenburn-violet+3))))
   `(newsticker-extra-face ((,class (:foreground ,zenburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((,class (:foreground ,zenburn-fg))))
   `(newsticker-immortal-item-face ((,class (:foreground ,zenburn-violet))))
   `(newsticker-new-item-face ((,class (:foreground ,zenburn-brown))))
   `(newsticker-obsolete-item-face ((,class (:foreground ,zenburn-blue))))
   `(newsticker-old-item-face ((,class (:foreground ,zenburn-bg+3))))
   `(newsticker-statistics-face ((,class (:foreground ,zenburn-fg))))
   `(newsticker-treeview-face ((,class (:foreground ,zenburn-fg))))
   `(newsticker-treeview-immortal-face ((,class (:foreground ,zenburn-violet))))
   `(newsticker-treeview-listwindow-face ((,class (:foreground ,zenburn-fg))))
   `(newsticker-treeview-new-face ((,class (:foreground ,zenburn-brown :weight bold))))
   `(newsticker-treeview-obsolete-face ((,class (:foreground ,zenburn-blue))))
   `(newsticker-treeview-old-face ((,class (:foreground ,zenburn-bg+3))))
   `(newsticker-treeview-selection-face ((,class (:foreground ,zenburn-dark-blue))))

   ;;; external

   ;; full-ack
   `(ack-separator ((,class (:foreground ,zenburn-fg))))
   `(ack-file ((,class (:foreground ,zenburn-brown))))
   `(ack-line ((,class (:foreground ,zenburn-dark-blue))))
   `(ack-match ((,class (:foreground ,zenburn-light-blue :background ,zenburn-bg-1 :weigth bold))))

   ;; auctex
   `(font-latex-bold ((,class (:inherit bold))))
   `(font-latex-warning ((,class (:inherit font-lock-warning))))
   `(font-latex-sedate ((,class (:foreground ,zenburn-dark-blue :weight bold ))))
   `(font-latex-title-4 ((,class (:inherit variable-pitch :weight bold))))

   ;; auto-complete
   `(ac-candidate-face ((,class (:background ,zenburn-bg+3 :foreground "white"))))
   `(ac-selection-face ((,class (:background ,zenburn-brown-4 :foreground ,zenburn-fg))))
   `(popup-tip-face ((,class (:background ,zenburn-dark-blue-2 :foreground "white"))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,zenburn-brown-5))))
   `(popup-scroll-bar-background-face ((,class (:background ,zenburn-bg-1))))
   `(popup-isearch-match ((,class (:background ,zenburn-bg :foreground ,zenburn-fg))))

   ;; diff
   `(diff-added ((,class (:foreground ,zenburn-violet+4))))
   `(diff-changed ((,class (:foreground ,zenburn-dark-blue))))
   `(diff-removed ((,class (:foreground ,zenburn-blue))))
   `(diff-header ((,class (:background ,zenburn-bg+2))))
   `(diff-file-header
     ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg :bold t))))

   ;; ert
   `(ert-test-result-expected ((,class (:foreground ,zenburn-violet+4 :background ,zenburn-bg))))
   `(ert-test-result-unexpected ((,class (:foreground ,zenburn-blue :background ,zenburn-bg))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,zenburn-dark-blue :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,zenburn-blue-1 :weight bold))))
   `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
   `(eshell-ls-directory ((,class (:foreground ,zenburn-brown+1 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,zenburn-blue+1 :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,zenburn-fg))))
   `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
   `(eshell-ls-product ((,class (:inherit font-lock-doc))))
   `(eshell-ls-special ((,class (:foreground ,zenburn-dark-blue :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,zenburn-bordeaux :weight bold))))

   ;; flymake
   `(flymake-errline ((,class (:foreground ,zenburn-blue-1 :weight bold :underline t))))
   `(flymake-warnline ((,class (:foreground ,zenburn-dark-blue-1 :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((,class (:foreground ,zenburn-dark-blue-1 :weight bold :underline t))))
   `(flyspell-incorrect ((,class (:foreground ,zenburn-blue-1 :weight bold :underline t))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,zenburn-brown :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,zenburn-fg))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,zenburn-dark-blue))))
   `(erc-keyword-face ((,class (:foreground ,zenburn-brown :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,zenburn-dark-blue :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,zenburn-blue :weigth bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,zenburn-violet))))
   `(erc-pal-face ((,class (:foreground ,zenburn-light-blue :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,zenburn-light-blue :background ,zenburn-bg :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,zenburn-violet+1))))
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
   `(gnus-summary-cancelled ((,class (:foreground ,zenburn-light-blue))))
   `(gnus-summary-high-ancient ((,class (:foreground ,zenburn-brown))))
   `(gnus-summary-high-read ((,class (:foreground ,zenburn-violet :weight bold))))
   `(gnus-summary-high-ticked ((,class (:foreground ,zenburn-light-blue :weight bold))))
   `(gnus-summary-high-unread ((,class (:foreground ,zenburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((,class (:foreground ,zenburn-brown))))
   `(gnus-summary-low-read ((t (:foreground ,zenburn-violet))))
   `(gnus-summary-low-ticked ((,class (:foreground ,zenburn-light-blue :weight bold))))
   `(gnus-summary-low-unread ((,class (:foreground ,zenburn-fg))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,zenburn-brown))))
   `(gnus-summary-normal-read ((,class (:foreground ,zenburn-violet))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,zenburn-light-blue :weight bold))))
   `(gnus-summary-normal-unread ((,class (:foreground ,zenburn-fg))))
   `(gnus-summary-selected ((,class (:foreground ,zenburn-dark-blue :weight bold))))
   `(gnus-cite-1 ((,class (:foreground ,zenburn-brown))))
   `(gnus-cite-10 ((,class (:foreground ,zenburn-dark-blue-1))))
   `(gnus-cite-11 ((,class (:foreground ,zenburn-dark-blue))))
   `(gnus-cite-2 ((,class (:foreground ,zenburn-brown-1))))
   `(gnus-cite-3 ((,class (:foreground ,zenburn-brown-2))))
   `(gnus-cite-4 ((,class (:foreground ,zenburn-violet+2))))
   `(gnus-cite-5 ((,class (:foreground ,zenburn-violet+1))))
   `(gnus-cite-6 ((,class (:foreground ,zenburn-violet))))
   `(gnus-cite-7 ((,class (:foreground ,zenburn-blue))))
   `(gnus-cite-8 ((,class (:foreground ,zenburn-blue-1))))
   `(gnus-cite-9 ((,class (:foreground ,zenburn-blue-2))))
   `(gnus-group-news-1-empty ((,class (:foreground ,zenburn-dark-blue))))
   `(gnus-group-news-2-empty ((,class (:foreground ,zenburn-violet+3))))
   `(gnus-group-news-3-empty ((,class (:foreground ,zenburn-violet+1))))
   `(gnus-group-news-4-empty ((,class (:foreground ,zenburn-brown-2))))
   `(gnus-group-news-5-empty ((,class (:foreground ,zenburn-brown-3))))
   `(gnus-group-news-6-empty ((,class (:foreground ,zenburn-bg+2))))
   `(gnus-group-news-low-empty ((,class (:foreground ,zenburn-bg+2))))
   `(gnus-signature ((,class (:foreground ,zenburn-dark-blue))))
   `(gnus-x ((,class (:background ,zenburn-fg :foreground ,zenburn-bg))))

   ;; helm
   `(helm-header
     ((,class (:foreground ,zenburn-violet
			   :background ,zenburn-bg
			   :underline nil
			   :box nil))))
   `(helm-source-header
     ((,class (:foreground ,zenburn-dark-blue
			   :background ,zenburn-bg-1
			   :underline nil
			   :weight bold
			   :box (:line-width -1 :style released-button)))))
   `(helm-selection ((,class (:background ,zenburn-bg+1 :underline nil))))
   `(helm-selection-line ((,class (:background ,zenburn-bg+1))))
   `(helm-visible-mark ((,class (:foreground ,zenburn-bg :background ,zenburn-dark-blue-2))))
   `(helm-candidate-number ((,class (:foreground ,zenburn-violet+4 :background ,zenburn-bg-1))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,zenburn-bg-1))
		   (t (:background ,zenburn-bg-1))))

   ;; hl-sexp
   `(hl-sexp-face ((,class (:background ,zenburn-bg+1))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,zenburn-dark-blue :weight bold))))
   `(ido-only-match ((,class (:foreground ,zenburn-light-blue :weight bold))))
   `(ido-subdir ((,class (:foreground ,zenburn-dark-blue))))

   ;; js2-mode
   `(js2-warning-face ((,class (:underline ,zenburn-light-blue))))
   `(js2-error-face ((,class (:foreground ,zenburn-blue :weight bold))))
   `(js2-jsdoc-tag-face ((,class (:foreground ,zenburn-violet-1))))
   `(js2-jsdoc-type-face ((,class (:foreground ,zenburn-violet+2))))
   `(js2-jsdoc-value-face ((,class (:foreground ,zenburn-violet+3))))
   `(js2-function-param-face ((,class (:foreground, zenburn-violet+3))))
   `(js2-external-variable-face ((,class (:foreground ,zenburn-light-blue))))

   ;; jabber-mode
   `(jabber-roster-user-away ((,class (:foreground ,zenburn-violet+2))))
   `(jabber-roster-user-online ((,class (:foreground ,zenburn-brown-1))))
   `(jabber-roster-user-dnd ((,class (:foreground ,zenburn-blue+1))))
   `(jabber-rare-time-face ((,class (:foreground ,zenburn-violet+1))))
   `(jabber-chat-prompt-local ((,class (:foreground ,zenburn-brown-1))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,zenburn-blue+1))))
   `(jabber-activity-face((,class (:foreground ,zenburn-blue+1))))
   `(jabber-activity-personal-face ((,class (:foreground ,zenburn-brown+1))))
   `(jabber-title-small ((,class (:height 1.1 :weight bold))))
   `(jabber-title-medium ((,class (:height 1.2 :weight bold))))
   `(jabber-title-large ((,class (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((,class (:foreground ,zenburn-violet+2 :background ,zenburn-bg))))

   ;; magit
   `(magit-section-title ((,class (:foreground ,zenburn-dark-blue :weight bold))))
   `(magit-branch ((,class (:foreground ,zenburn-light-blue :weight bold))))
   `(magit-item-highlight ((,class (:background ,zenburn-bg+1))))

   ;; message-mode
   `(message-cited-text ((,class (:inherit font-lock-comment))))
   `(message-header-name ((,class (:foreground ,zenburn-violet+1))))
   `(message-header-other ((,class (:foreground ,zenburn-violet))))
   `(message-header-to ((,class (:foreground ,zenburn-dark-blue :weight bold))))
   `(message-header-from ((,class (:foreground ,zenburn-dark-blue :weight bold))))
   `(message-header-cc ((,class (:foreground ,zenburn-dark-blue :weight bold))))
   `(message-header-newsgroups ((,class (:foreground ,zenburn-dark-blue :weight bold))))
   `(message-header-subject ((,class (:foreground ,zenburn-light-blue :weight bold))))
   `(message-header-xheader ((,class (:foreground ,zenburn-violet))))
   `(message-mml ((,class (:foreground ,zenburn-dark-blue :weight bold))))
   `(message-separator ((,class (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((,class (:foreground ,zenburn-light-blue))))
   `(mew-face-header-from ((,class (:foreground ,zenburn-dark-blue))))
   `(mew-face-header-date ((,class (:foreground ,zenburn-violet))))
   `(mew-face-header-to ((,class (:foreground ,zenburn-blue))))
   `(mew-face-header-key ((,class (:foreground ,zenburn-violet))))
   `(mew-face-header-private ((,class (:foreground ,zenburn-violet))))
   `(mew-face-header-important ((,class (:foreground ,zenburn-brown))))
   `(mew-face-header-marginal ((,class (:foreground ,zenburn-fg :weight bold))))
   `(mew-face-header-warning ((,class (:foreground ,zenburn-blue))))
   `(mew-face-header-xmew ((,class (:foreground ,zenburn-violet))))
   `(mew-face-header-xmew-bad ((,class (:foreground ,zenburn-blue))))
   `(mew-face-body-url ((,class (:foreground ,zenburn-light-blue))))
   `(mew-face-body-comment ((,class (:foreground ,zenburn-fg :slant italic))))
   `(mew-face-body-cite1 ((,class (:foreground ,zenburn-violet))))
   `(mew-face-body-cite2 ((,class (:foreground ,zenburn-brown))))
   `(mew-face-body-cite3 ((,class (:foreground ,zenburn-light-blue))))
   `(mew-face-body-cite4 ((,class (:foreground ,zenburn-dark-blue))))
   `(mew-face-body-cite5 ((,class (:foreground ,zenburn-blue))))
   `(mew-face-mark-review ((,class (:foreground ,zenburn-brown))))
   `(mew-face-mark-escape ((,class (:foreground ,zenburn-violet))))
   `(mew-face-mark-delete ((,class (:foreground ,zenburn-blue))))
   `(mew-face-mark-unlink ((,class (:foreground ,zenburn-dark-blue))))
   `(mew-face-mark-refile ((,class (:foreground ,zenburn-violet))))
   `(mew-face-mark-unread ((,class (:foreground ,zenburn-blue-2))))
   `(mew-face-eof-message ((,class (:foreground ,zenburn-violet))))
   `(mew-face-eof-part ((,class (:foreground ,zenburn-dark-blue))))

   ;; mic-paren
   `(paren-face-match ((,class (:foreground ,zenburn-bordeaux :background ,zenburn-bg :weight bold))))
   `(paren-face-mismatch ((,class (:foreground ,zenburn-bg :background ,zenburn-green :weight bold))))
   `(paren-face-no-match ((,class (:foreground ,zenburn-bg :background ,zenburn-blue :weight bold))))

   ;; nav
   `(nav-face-heading ((,class (:foreground ,zenburn-dark-blue))))
   `(nav-face-button-num ((,class (:foreground ,zenburn-bordeaux))))
   `(nav-face-dir ((,class (:foreground ,zenburn-violet))))
   `(nav-face-hdir ((,class (:foreground ,zenburn-blue))))
   `(nav-face-file ((,class (:foreground ,zenburn-fg))))
   `(nav-face-hfile ((,class (:foreground ,zenburn-blue-4))))

   ;; mumamo
   `(mumamo-background-chunk-major ((,class (:background nil))))
   `(mumamo-background-chunk-submode1 ((,class (:background ,zenburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((,class (:background ,zenburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((,class (:background ,zenburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((,class (:background ,zenburn-bg+1))))

   ;; org-mode
   `(org-agenda-date-today
     ((,class (:foreground "black" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:foreground ,zenburn-fg :weight bold))))
   `(org-checkbox ((,class (:background ,zenburn-bg+2 :foreground "black"
					:box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,zenburn-brown :underline t))))
   `(org-deadline-announce ((,class (:foreground ,zenburn-blue-1))))
   `(org-done ((,class (:bold t :weight bold :foreground ,zenburn-violet+3))))
   `(org-formula ((,class (:foreground ,zenburn-dark-blue-2))))
   `(org-headline-done ((,class (:foreground ,zenburn-violet+3))))
   `(org-hide ((,class (:foreground ,zenburn-bg-1))))
   `(org-level-1 ((,class (:foreground ,zenburn-light-blue))))
   `(org-level-2 ((,class (:foreground ,zenburn-violet+1))))
   `(org-level-3 ((,class (:foreground ,zenburn-brown-1))))
   `(org-level-4 ((,class (:foreground ,zenburn-dark-blue-2))))
   `(org-level-5 ((,class (:foreground ,zenburn-bordeaux))))
   `(org-level-6 ((,class (:foreground ,zenburn-violet-1))))
   `(org-level-7 ((,class (:foreground ,zenburn-blue-4))))
   `(org-level-8 ((,class (:foreground ,zenburn-brown-4))))
   `(org-link ((,class (:foreground ,zenburn-dark-blue-2 :underline t))))
   `(org-scheduled ((,class (:foreground ,zenburn-violet+4))))
   `(org-scheduled-previously ((,class (:foreground ,zenburn-blue-4))))
   `(org-scheduled-today ((,class (:foreground ,zenburn-brown+1))))
   `(org-special-keyword ((,class (:foreground ,zenburn-dark-blue-1))))
   `(org-table ((,class (:foreground ,zenburn-violet+2))))
   `(org-tag ((,class (:bold t :weight bold))))
   `(org-time-grid ((,class (:foreground ,zenburn-light-blue))))
   `(org-todo ((,class (:bold t :foreground ,zenburn-blue :weight bold))))
   `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   `(org-warning ((,class (:bold t :foreground ,zenburn-blue :weight bold :underline nil))))
   `(org-column ((,class (:background ,zenburn-bg-1))))
   `(org-column-title ((,class (:background ,zenburn-bg-1 :underline t :weight bold))))

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
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,zenburn-fg))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,zenburn-violet+2))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,zenburn-dark-blue-2))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,zenburn-bordeaux))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,zenburn-violet-1))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,zenburn-brown+1))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,zenburn-dark-blue-1))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,zenburn-violet+1))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,zenburn-brown-2))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,zenburn-light-blue))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,zenburn-violet))))
   `( rainbow-delimiters-depth-12-face ((,class (:foreground ,zenburn-brown-5))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((,class (:foreground ,zenburn-violet))))
   `(rpm-spec-doc-face ((,class (:foreground ,zenburn-violet))))
   `(rpm-spec-ghost-face ((,class (:foreground ,zenburn-blue))))
   `(rpm-spec-macro-face ((,class (:foreground ,zenburn-dark-blue))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,zenburn-blue))))
   `(rpm-spec-package-face ((,class (:foreground ,zenburn-blue))))
   `(rpm-spec-section-face ((,class (:foreground ,zenburn-dark-blue))))
   `(rpm-spec-tag-face ((,class (:foreground ,zenburn-brown))))
   `(rpm-spec-var-face ((,class (:foreground ,zenburn-blue))))

   ;; rst-mode
   `(rst-level-1-face ((,class (:foreground ,zenburn-light-blue))))
   `(rst-level-2-face ((,class (:foreground ,zenburn-violet+1))))
   `(rst-level-3-face ((,class (:foreground ,zenburn-brown-1))))
   `(rst-level-4-face ((,class (:foreground ,zenburn-dark-blue-2))))
   `(rst-level-5-face ((,class (:foreground ,zenburn-bordeaux))))
   `(rst-level-6-face ((,class (:foreground ,zenburn-violet-1))))

   ;; show-paren
   `(show-paren-mismatch ((,class (:foreground ,zenburn-blue-3 :background ,zenburn-bg :weight bold))))
   `(show-paren-match ((,class (:foreground ,zenburn-brown-1 :background ,zenburn-bg :weight bold))))

   ;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))

   ;; SLIME
   `(slime-repl-inputed-output-face ((,class (:foreground ,zenburn-blue))))

   ;; tabbar
   `(tabbar-button ((,class (:foreground ,zenburn-fg
					 :background ,zenburn-bg))))
   `(tabbar-selected ((,class (:foreground ,zenburn-fg
					   :background ,zenburn-bg
					   :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((,class (:foreground ,zenburn-fg
					     :background ,zenburn-bg+1
					     :box (:line-width -1 :style released-button)))))

   ;; volatile-highlights
   `(vhl/default-face ((,class (:background ,zenburn-bg+1))))

   ;; emacs-w3m
   `(w3m-anchor ((,class (:foreground ,zenburn-dark-blue :underline t
				      :weight bold))))
   `(w3m-arrived-anchor ((,class (:foreground ,zenburn-dark-blue-2
					      :underline t :weight normal))))
   `(w3m-form ((,class (:foreground ,zenburn-blue-1 :underline t))))
   `(w3m-header-line-location-title ((,class (:foreground ,zenburn-dark-blue
							  :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((,class (:foreground ,zenburn-violet+2 :background ,zenburn-bg))))
   `(w3m-lnum-match ((,class (:background ,zenburn-bg-1
					  :foreground ,zenburn-light-blue
					  :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((,class (:foreground ,zenburn-dark-blue))))

   ;; whitespace-mode
   `(whitespace-space ((,class (:background ,zenburn-bg :foreground ,zenburn-bg+1))))
   `(whitespace-hspace ((,class (:background ,zenburn-bg :foreground ,zenburn-bg+1))))
   `(whitespace-tab ((,class (:background ,zenburn-bg :foreground ,zenburn-blue))))
   `(whitespace-newline ((,class (:foreground ,zenburn-bg+1))))
   `(whitespace-trailing ((,class (:foreground ,zenburn-blue :background ,zenburn-bg))))
   `(whitespace-line ((,class (:background ,zenburn-bg-05 :foreground ,zenburn-green))))
   `(whitespace-space-before-tab ((,class (:background ,zenburn-light-blue :foreground ,zenburn-light-blue))))
   `(whitespace-indentation ((,class (:background ,zenburn-dark-blue :foreground ,zenburn-blue))))
   `(whitespace-empty ((,class (:background ,zenburn-dark-blue :foreground ,zenburn-blue))))
   `(whitespace-space-after-tab ((,class (:background ,zenburn-dark-blue :foreground ,zenburn-blue))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((,class (:foreground ,zenburn-blue-2))))
   `(wl-highlight-folder-many-face ((,class (:foreground ,zenburn-blue-1))))
   `(wl-highlight-folder-path-face ((,class (:foreground ,zenburn-light-blue))))
   `(wl-highlight-folder-unread-face ((,class (:foreground ,zenburn-brown))))
   `(wl-highlight-folder-zero-face ((,class (:foreground ,zenburn-fg))))
   `(wl-highlight-folder-unknown-face ((,class (:foreground ,zenburn-brown))))
   `(wl-highlight-message-citation-header ((,class (:foreground ,zenburn-blue-1))))
   `(wl-highlight-message-cited-text-1 ((,class (:foreground ,zenburn-blue))))
   `(wl-highlight-message-cited-text-2 ((,class (:foreground ,zenburn-violet+2))))
   `(wl-highlight-message-cited-text-3 ((,class (:foreground ,zenburn-brown))))
   `(wl-highlight-message-cited-text-4 ((,class (:foreground ,zenburn-brown+1))))
   `(wl-highlight-message-header-contents-face ((,class (:foreground ,zenburn-violet))))
   `(wl-highlight-message-headers-face ((,class (:foreground ,zenburn-blue+1))))
   `(wl-highlight-message-important-header-contents ((,class (:foreground ,zenburn-violet+2))))
   `(wl-highlight-message-header-contents ((,class (:foreground ,zenburn-violet+1))))
   `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,zenburn-violet+2))))
   `(wl-highlight-message-signature ((,class (:foreground ,zenburn-violet))))
   `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,zenburn-fg))))
   `(wl-highlight-summary-answered-face ((,class (:foreground ,zenburn-brown))))
   `(wl-highlight-summary-disposed-face ((,class (:foreground ,zenburn-fg
							      :slant italic))))
   `(wl-highlight-summary-new-face ((,class (:foreground ,zenburn-brown))))
   `(wl-highlight-summary-normal-face ((,class (:foreground ,zenburn-fg))))
   `(wl-highlight-summary-thread-top-face ((,class (:foreground ,zenburn-dark-blue))))
   `(wl-highlight-thread-indent-face ((,class (:foreground ,zenburn-green))))
   `(wl-highlight-summary-refiled-face ((,class (:foreground ,zenburn-fg))))
   `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

   ;; which-func-mode
   `(which-func ((,class (:foreground ,zenburn-violet+4))))

   ;; yascroll
   `(yascroll:thumb-text-area ((,class (:background ,zenburn-bg-1))))
   `(yascroll:thumb-fringe ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-bg-1))))
   )

  ;;; custom theme variables
  (custom-theme-set-variables
   'anti-zenburn
   `(ansi-color-names-vector [,zenburn-bg ,zenburn-blue ,zenburn-violet
			      ,zenburn-dark-blue ,zenburn-brown
			      ,zenburn-green ,zenburn-bordeaux
			      ,zenburn-fg])

   ;; fill-column-indicator
   `(fci-rule-color ,zenburn-bg-05))

  ;;; colors for the ansi-term
  (eval-after-load 'term
    `(setq ansi-term-color-vector
	   (vector 'unspecified ,zenburn-bg ,zenburn-blue
		   ,zenburn-violet ,zenburn-dark-blue ,zenburn-brown
		   ,zenburn-green ,zenburn-bordeaux ,zenburn-fg))))

(provide-theme 'anti-zenburn)

;;; anti-zenburn-theme.el ends here

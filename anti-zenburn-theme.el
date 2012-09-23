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
      ;; Zenburn palette reversed
      ;; colors with -x are lighter, colors with +x are darker
      (azenburn-fg "#232333")
      (azenburn-fg-1 "#9a9aaa")
      (azenburn-bg-1 "#d4d4d4")
      (azenburn-bg-05 "#c7c7c7")
      (azenburn-bg "#c0c0c0")
      (azenburn-bg+1 "#b0b0b0")
      (azenburn-bg+2 "#a0a0a0")
      (azenburn-bg+3 "#909090")
      (azenburn-blue+1 "#235c5c")
      (azenburn-blue "#336c6c")
      (azenburn-blue-1 "#437c7c")
      (azenburn-blue-2 "#538c8c")
      (azenburn-blue-3 "#639c9c")
      (azenburn-blue-4 "#73acac")
      (azenburn-light-blue "#205070")
      (azenburn-dark-blue "#0f2050")
      (azenburn-dark-blue-1 "#1f3060")
      (azenburn-dark-blue-2 "#2f4070")
      (azenburn-violet-1 "#a080a0")
      (azenburn-violet "#806080")
      (azenburn-violet+1 "#704d70")
      (azenburn-violet+2 "#603a60")
      (azenburn-violet+3 "#502750")
      (azenburn-violet+4 "#401440")
      (azenburn-bordeaux "#6c1f1c")
      (azenburn-brown+1 "#6b400c")
      (azenburn-brown "#732f2c")
      (azenburn-brown-1 "#834744")
      (azenburn-brown-2 "#935f5c")
      (azenburn-brown-3 "#a37774")
      (azenburn-brown-4 "#b38f8c")
      (azenburn-brown-5 "#c99f9f")
      (azenburn-green "#23733c"))
  (custom-theme-set-faces
   'anti-zenburn
   '(button ((t (:underline t))))
   `(link ((,class (:foreground ,azenburn-dark-blue :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,azenburn-dark-blue-2 :underline t :weight normal))))

   ;;; basic coloring
   `(default ((,class (:foreground ,azenburn-fg :background ,azenburn-bg))))
   `(cursor ((,class (:foreground ,azenburn-fg))))
   `(escape-glyph-face ((,class (:foreground ,azenburn-blue))))
   `(fringe ((,class (:foreground ,azenburn-fg :background ,azenburn-bg+1))))
   `(header-line ((,class (:foreground ,azenburn-dark-blue
				       :background ,azenburn-bg-1
				       :box (:line-width -1 :style released-button)))))
   `(highlight ((,class (:background ,azenburn-bg-05))
		(t (:background ,azenburn-blue-4))))

   ;;; compilation
   `(compilation-column-face ((,class (:foreground ,azenburn-dark-blue))))
   `(compilation-enter-directory-face ((,class (:foreground ,azenburn-violet))))
   `(compilation-error-face ((,class (:foreground ,azenburn-blue-1 :weight bold :underline t))))
   `(compilation-face ((,class (:foreground ,azenburn-fg))))
   `(compilation-info-face ((,class (:foreground ,azenburn-brown))))
   `(compilation-info ((,class (:foreground ,azenburn-violet+4 :underline t))))
   `(compilation-leave-directory-face ((,class (:foreground ,azenburn-violet))))
   `(compilation-line-face ((,class (:foreground ,azenburn-dark-blue))))
   `(compilation-line-number ((,class (:foreground ,azenburn-dark-blue))))
   `(compilation-message-face ((,class (:foreground ,azenburn-brown))))
   `(compilation-warning-face ((,class (:foreground ,azenburn-dark-blue-1 :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((,class (:foreground ,azenburn-fg))))
   `(grep-error-face ((,class (:foreground ,azenburn-blue-1 :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,azenburn-brown))))
   `(grep-match-face ((,class (:foreground ,azenburn-light-blue :weight bold))))
   `(match ((,class (:background ,azenburn-bg-1 :foreground ,azenburn-light-blue :weight bold))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,azenburn-dark-blue :background ,azenburn-bg-1))))
   `(isearch-fail ((,class (:foreground ,azenburn-fg :background ,azenburn-blue-4))))
   `(lazy-highlight ((,class (:foreground ,azenburn-dark-blue :background ,azenburn-bg+2))))

   `(menu ((,class (:foreground ,azenburn-fg :background ,azenburn-bg))))
   `(minibuffer-prompt ((,class (:foreground ,azenburn-dark-blue))))
   `(mode-line
     ((,class (:foreground ,azenburn-violet+1
			   :background ,azenburn-bg-1
			   :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((,class (:foreground ,azenburn-dark-blue :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,azenburn-violet-1
			   :background ,azenburn-bg-05
			   :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,azenburn-bg-1))))
   `(secondary-selection ((,class (:background ,azenburn-bg+2))))
   `(trailing-whitespace ((,class (:background ,azenburn-blue))))
   `(vertical-border ((,class (:foreground ,azenburn-fg))))

   ;;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,azenburn-bordeaux))))
   `(font-lock-comment-face ((,class (:foreground ,azenburn-violet))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,azenburn-violet))))
   `(font-lock-constant-face ((,class (:foreground ,azenburn-violet+4))))
   `(font-lock-doc-face ((,class (:foreground ,azenburn-violet+1))))
   `(font-lock-doc-string-face ((,class (:foreground ,azenburn-brown-2))))
   `(font-lock-function-name-face ((,class (:foreground ,azenburn-brown))))
   `(font-lock-keyword-face ((,class (:foreground ,azenburn-dark-blue :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,azenburn-fg))))
   `(font-lock-preprocessor-face ((,class (:foreground ,azenburn-brown+1))))
   `(font-lock-string-face ((,class (:foreground ,azenburn-blue))))
   `(font-lock-type-face ((,class (:foreground ,azenburn-brown-1))))
   `(font-lock-variable-name-face ((,class (:foreground ,azenburn-light-blue))))
   `(font-lock-warning-face ((,class (:foreground ,azenburn-dark-blue-1 :weight bold :underline t))))

   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   ;;; newsticker
   `(newsticker-date-face ((,class (:foreground ,azenburn-fg))))
   `(newsticker-default-face ((,class (:foreground ,azenburn-fg))))
   `(newsticker-enclosure-face ((,class (:foreground ,azenburn-violet+3))))
   `(newsticker-extra-face ((,class (:foreground ,azenburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((,class (:foreground ,azenburn-fg))))
   `(newsticker-immortal-item-face ((,class (:foreground ,azenburn-violet))))
   `(newsticker-new-item-face ((,class (:foreground ,azenburn-brown))))
   `(newsticker-obsolete-item-face ((,class (:foreground ,azenburn-blue))))
   `(newsticker-old-item-face ((,class (:foreground ,azenburn-bg+3))))
   `(newsticker-statistics-face ((,class (:foreground ,azenburn-fg))))
   `(newsticker-treeview-face ((,class (:foreground ,azenburn-fg))))
   `(newsticker-treeview-immortal-face ((,class (:foreground ,azenburn-violet))))
   `(newsticker-treeview-listwindow-face ((,class (:foreground ,azenburn-fg))))
   `(newsticker-treeview-new-face ((,class (:foreground ,azenburn-brown :weight bold))))
   `(newsticker-treeview-obsolete-face ((,class (:foreground ,azenburn-blue))))
   `(newsticker-treeview-old-face ((,class (:foreground ,azenburn-bg+3))))
   `(newsticker-treeview-selection-face ((,class (:foreground ,azenburn-dark-blue))))

   ;;; external

   ;; full-ack
   `(ack-separator ((,class (:foreground ,azenburn-fg))))
   `(ack-file ((,class (:foreground ,azenburn-brown))))
   `(ack-line ((,class (:foreground ,azenburn-dark-blue))))
   `(ack-match ((,class (:foreground ,azenburn-light-blue :background ,azenburn-bg-1 :weigth bold))))

   ;; auctex
   `(font-latex-bold ((,class (:inherit bold))))
   `(font-latex-warning ((,class (:inherit font-lock-warning))))
   `(font-latex-sedate ((,class (:foreground ,azenburn-dark-blue :weight bold ))))
   `(font-latex-title-4 ((,class (:inherit variable-pitch :weight bold))))

   ;; auto-complete
   `(ac-candidate-face ((,class (:background ,azenburn-bg+3 :foreground "white"))))
   `(ac-selection-face ((,class (:background ,azenburn-brown-4 :foreground ,azenburn-fg))))
   `(popup-tip-face ((,class (:background ,azenburn-dark-blue-2 :foreground "white"))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,azenburn-brown-5))))
   `(popup-scroll-bar-background-face ((,class (:background ,azenburn-bg-1))))
   `(popup-isearch-match ((,class (:background ,azenburn-bg :foreground ,azenburn-fg))))

   ;; diff
   `(diff-added ((,class (:foreground ,azenburn-violet+4))))
   `(diff-changed ((,class (:foreground ,azenburn-dark-blue))))
   `(diff-removed ((,class (:foreground ,azenburn-blue))))
   `(diff-header ((,class (:background ,azenburn-bg+2))))
   `(diff-file-header
     ((,class (:background ,azenburn-bg+2 :foreground ,azenburn-fg :bold t))))

   ;; ert
   `(ert-test-result-expected ((,class (:foreground ,azenburn-violet+4 :background ,azenburn-bg))))
   `(ert-test-result-unexpected ((,class (:foreground ,azenburn-blue :background ,azenburn-bg))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,azenburn-dark-blue :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,azenburn-blue-1 :weight bold))))
   `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
   `(eshell-ls-directory ((,class (:foreground ,azenburn-brown+1 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,azenburn-blue+1 :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,azenburn-fg))))
   `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
   `(eshell-ls-product ((,class (:inherit font-lock-doc))))
   `(eshell-ls-special ((,class (:foreground ,azenburn-dark-blue :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,azenburn-bordeaux :weight bold))))

   ;; flymake
   `(flymake-errline ((,class (:foreground ,azenburn-blue-1 :weight bold :underline t))))
   `(flymake-warnline ((,class (:foreground ,azenburn-dark-blue-1 :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((,class (:foreground ,azenburn-dark-blue-1 :weight bold :underline t))))
   `(flyspell-incorrect ((,class (:foreground ,azenburn-blue-1 :weight bold :underline t))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,azenburn-brown :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,azenburn-fg))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,azenburn-dark-blue))))
   `(erc-keyword-face ((,class (:foreground ,azenburn-brown :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,azenburn-dark-blue :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,azenburn-blue :weigth bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,azenburn-violet))))
   `(erc-pal-face ((,class (:foreground ,azenburn-light-blue :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,azenburn-light-blue :background ,azenburn-bg :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,azenburn-violet+1))))
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
   `(gnus-summary-cancelled ((,class (:foreground ,azenburn-light-blue))))
   `(gnus-summary-high-ancient ((,class (:foreground ,azenburn-brown))))
   `(gnus-summary-high-read ((,class (:foreground ,azenburn-violet :weight bold))))
   `(gnus-summary-high-ticked ((,class (:foreground ,azenburn-light-blue :weight bold))))
   `(gnus-summary-high-unread ((,class (:foreground ,azenburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((,class (:foreground ,azenburn-brown))))
   `(gnus-summary-low-read ((t (:foreground ,azenburn-violet))))
   `(gnus-summary-low-ticked ((,class (:foreground ,azenburn-light-blue :weight bold))))
   `(gnus-summary-low-unread ((,class (:foreground ,azenburn-fg))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,azenburn-brown))))
   `(gnus-summary-normal-read ((,class (:foreground ,azenburn-violet))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,azenburn-light-blue :weight bold))))
   `(gnus-summary-normal-unread ((,class (:foreground ,azenburn-fg))))
   `(gnus-summary-selected ((,class (:foreground ,azenburn-dark-blue :weight bold))))
   `(gnus-cite-1 ((,class (:foreground ,azenburn-brown))))
   `(gnus-cite-10 ((,class (:foreground ,azenburn-dark-blue-1))))
   `(gnus-cite-11 ((,class (:foreground ,azenburn-dark-blue))))
   `(gnus-cite-2 ((,class (:foreground ,azenburn-brown-1))))
   `(gnus-cite-3 ((,class (:foreground ,azenburn-brown-2))))
   `(gnus-cite-4 ((,class (:foreground ,azenburn-violet+2))))
   `(gnus-cite-5 ((,class (:foreground ,azenburn-violet+1))))
   `(gnus-cite-6 ((,class (:foreground ,azenburn-violet))))
   `(gnus-cite-7 ((,class (:foreground ,azenburn-blue))))
   `(gnus-cite-8 ((,class (:foreground ,azenburn-blue-1))))
   `(gnus-cite-9 ((,class (:foreground ,azenburn-blue-2))))
   `(gnus-group-news-1-empty ((,class (:foreground ,azenburn-dark-blue))))
   `(gnus-group-news-2-empty ((,class (:foreground ,azenburn-violet+3))))
   `(gnus-group-news-3-empty ((,class (:foreground ,azenburn-violet+1))))
   `(gnus-group-news-4-empty ((,class (:foreground ,azenburn-brown-2))))
   `(gnus-group-news-5-empty ((,class (:foreground ,azenburn-brown-3))))
   `(gnus-group-news-6-empty ((,class (:foreground ,azenburn-bg+2))))
   `(gnus-group-news-low-empty ((,class (:foreground ,azenburn-bg+2))))
   `(gnus-signature ((,class (:foreground ,azenburn-dark-blue))))
   `(gnus-x ((,class (:background ,azenburn-fg :foreground ,azenburn-bg))))

   ;; helm
   `(helm-header
     ((,class (:foreground ,azenburn-violet
			   :background ,azenburn-bg
			   :underline nil
			   :box nil))))
   `(helm-source-header
     ((,class (:foreground ,azenburn-dark-blue
			   :background ,azenburn-bg-1
			   :underline nil
			   :weight bold
			   :box (:line-width -1 :style released-button)))))
   `(helm-selection ((,class (:background ,azenburn-bg+1 :underline nil))))
   `(helm-selection-line ((,class (:background ,azenburn-bg+1))))
   `(helm-visible-mark ((,class (:foreground ,azenburn-bg :background ,azenburn-dark-blue-2))))
   `(helm-candidate-number ((,class (:foreground ,azenburn-violet+4 :background ,azenburn-bg-1))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,azenburn-bg-1))
		   (t (:background ,azenburn-bg-1))))

   ;; hl-sexp
   `(hl-sexp-face ((,class (:background ,azenburn-bg+1))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,azenburn-dark-blue :weight bold))))
   `(ido-only-match ((,class (:foreground ,azenburn-light-blue :weight bold))))
   `(ido-subdir ((,class (:foreground ,azenburn-dark-blue))))

   ;; js2-mode
   `(js2-warning-face ((,class (:underline ,azenburn-light-blue))))
   `(js2-error-face ((,class (:foreground ,azenburn-blue :weight bold))))
   `(js2-jsdoc-tag-face ((,class (:foreground ,azenburn-violet-1))))
   `(js2-jsdoc-type-face ((,class (:foreground ,azenburn-violet+2))))
   `(js2-jsdoc-value-face ((,class (:foreground ,azenburn-violet+3))))
   `(js2-function-param-face ((,class (:foreground, azenburn-violet+3))))
   `(js2-external-variable-face ((,class (:foreground ,azenburn-light-blue))))

   ;; jabber-mode
   `(jabber-roster-user-away ((,class (:foreground ,azenburn-violet+2))))
   `(jabber-roster-user-online ((,class (:foreground ,azenburn-brown-1))))
   `(jabber-roster-user-dnd ((,class (:foreground ,azenburn-blue+1))))
   `(jabber-rare-time-face ((,class (:foreground ,azenburn-violet+1))))
   `(jabber-chat-prompt-local ((,class (:foreground ,azenburn-brown-1))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,azenburn-blue+1))))
   `(jabber-activity-face((,class (:foreground ,azenburn-blue+1))))
   `(jabber-activity-personal-face ((,class (:foreground ,azenburn-brown+1))))
   `(jabber-title-small ((,class (:height 1.1 :weight bold))))
   `(jabber-title-medium ((,class (:height 1.2 :weight bold))))
   `(jabber-title-large ((,class (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((,class (:foreground ,azenburn-violet+2 :background ,azenburn-bg))))

   ;; magit
   `(magit-section-title ((,class (:foreground ,azenburn-dark-blue :weight bold))))
   `(magit-branch ((,class (:foreground ,azenburn-light-blue :weight bold))))
   `(magit-item-highlight ((,class (:background ,azenburn-bg+1))))

   ;; message-mode
   `(message-cited-text ((,class (:inherit font-lock-comment))))
   `(message-header-name ((,class (:foreground ,azenburn-violet+1))))
   `(message-header-other ((,class (:foreground ,azenburn-violet))))
   `(message-header-to ((,class (:foreground ,azenburn-dark-blue :weight bold))))
   `(message-header-from ((,class (:foreground ,azenburn-dark-blue :weight bold))))
   `(message-header-cc ((,class (:foreground ,azenburn-dark-blue :weight bold))))
   `(message-header-newsgroups ((,class (:foreground ,azenburn-dark-blue :weight bold))))
   `(message-header-subject ((,class (:foreground ,azenburn-light-blue :weight bold))))
   `(message-header-xheader ((,class (:foreground ,azenburn-violet))))
   `(message-mml ((,class (:foreground ,azenburn-dark-blue :weight bold))))
   `(message-separator ((,class (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((,class (:foreground ,azenburn-light-blue))))
   `(mew-face-header-from ((,class (:foreground ,azenburn-dark-blue))))
   `(mew-face-header-date ((,class (:foreground ,azenburn-violet))))
   `(mew-face-header-to ((,class (:foreground ,azenburn-blue))))
   `(mew-face-header-key ((,class (:foreground ,azenburn-violet))))
   `(mew-face-header-private ((,class (:foreground ,azenburn-violet))))
   `(mew-face-header-important ((,class (:foreground ,azenburn-brown))))
   `(mew-face-header-marginal ((,class (:foreground ,azenburn-fg :weight bold))))
   `(mew-face-header-warning ((,class (:foreground ,azenburn-blue))))
   `(mew-face-header-xmew ((,class (:foreground ,azenburn-violet))))
   `(mew-face-header-xmew-bad ((,class (:foreground ,azenburn-blue))))
   `(mew-face-body-url ((,class (:foreground ,azenburn-light-blue))))
   `(mew-face-body-comment ((,class (:foreground ,azenburn-fg :slant italic))))
   `(mew-face-body-cite1 ((,class (:foreground ,azenburn-violet))))
   `(mew-face-body-cite2 ((,class (:foreground ,azenburn-brown))))
   `(mew-face-body-cite3 ((,class (:foreground ,azenburn-light-blue))))
   `(mew-face-body-cite4 ((,class (:foreground ,azenburn-dark-blue))))
   `(mew-face-body-cite5 ((,class (:foreground ,azenburn-blue))))
   `(mew-face-mark-review ((,class (:foreground ,azenburn-brown))))
   `(mew-face-mark-escape ((,class (:foreground ,azenburn-violet))))
   `(mew-face-mark-delete ((,class (:foreground ,azenburn-blue))))
   `(mew-face-mark-unlink ((,class (:foreground ,azenburn-dark-blue))))
   `(mew-face-mark-refile ((,class (:foreground ,azenburn-violet))))
   `(mew-face-mark-unread ((,class (:foreground ,azenburn-blue-2))))
   `(mew-face-eof-message ((,class (:foreground ,azenburn-violet))))
   `(mew-face-eof-part ((,class (:foreground ,azenburn-dark-blue))))

   ;; mic-paren
   `(paren-face-match ((,class (:foreground ,azenburn-bordeaux :background ,azenburn-bg :weight bold))))
   `(paren-face-mismatch ((,class (:foreground ,azenburn-bg :background ,azenburn-green :weight bold))))
   `(paren-face-no-match ((,class (:foreground ,azenburn-bg :background ,azenburn-blue :weight bold))))

   ;; nav
   `(nav-face-heading ((,class (:foreground ,azenburn-dark-blue))))
   `(nav-face-button-num ((,class (:foreground ,azenburn-bordeaux))))
   `(nav-face-dir ((,class (:foreground ,azenburn-violet))))
   `(nav-face-hdir ((,class (:foreground ,azenburn-blue))))
   `(nav-face-file ((,class (:foreground ,azenburn-fg))))
   `(nav-face-hfile ((,class (:foreground ,azenburn-blue-4))))

   ;; mumamo
   `(mumamo-background-chunk-major ((,class (:background nil))))
   `(mumamo-background-chunk-submode1 ((,class (:background ,azenburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((,class (:background ,azenburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((,class (:background ,azenburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((,class (:background ,azenburn-bg+1))))

   ;; org-mode
   `(org-agenda-date-today
     ((,class (:foreground "black" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:foreground ,azenburn-fg :weight bold))))
   `(org-checkbox ((,class (:background ,azenburn-bg+2 :foreground "black"
					:box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,azenburn-brown :underline t))))
   `(org-deadline-announce ((,class (:foreground ,azenburn-blue-1))))
   `(org-done ((,class (:bold t :weight bold :foreground ,azenburn-violet+3))))
   `(org-formula ((,class (:foreground ,azenburn-dark-blue-2))))
   `(org-headline-done ((,class (:foreground ,azenburn-violet+3))))
   `(org-hide ((,class (:foreground ,azenburn-bg-1))))
   `(org-level-1 ((,class (:foreground ,azenburn-light-blue))))
   `(org-level-2 ((,class (:foreground ,azenburn-violet+1))))
   `(org-level-3 ((,class (:foreground ,azenburn-brown-1))))
   `(org-level-4 ((,class (:foreground ,azenburn-dark-blue-2))))
   `(org-level-5 ((,class (:foreground ,azenburn-bordeaux))))
   `(org-level-6 ((,class (:foreground ,azenburn-violet-1))))
   `(org-level-7 ((,class (:foreground ,azenburn-blue-4))))
   `(org-level-8 ((,class (:foreground ,azenburn-brown-4))))
   `(org-link ((,class (:foreground ,azenburn-dark-blue-2 :underline t))))
   `(org-scheduled ((,class (:foreground ,azenburn-violet+4))))
   `(org-scheduled-previously ((,class (:foreground ,azenburn-blue-4))))
   `(org-scheduled-today ((,class (:foreground ,azenburn-brown+1))))
   `(org-special-keyword ((,class (:foreground ,azenburn-dark-blue-1))))
   `(org-table ((,class (:foreground ,azenburn-violet+2))))
   `(org-tag ((,class (:bold t :weight bold))))
   `(org-time-grid ((,class (:foreground ,azenburn-light-blue))))
   `(org-todo ((,class (:bold t :foreground ,azenburn-blue :weight bold))))
   `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   `(org-warning ((,class (:bold t :foreground ,azenburn-blue :weight bold :underline nil))))
   `(org-column ((,class (:background ,azenburn-bg-1))))
   `(org-column-title ((,class (:background ,azenburn-bg-1 :underline t :weight bold))))

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
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,azenburn-fg))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,azenburn-violet+2))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,azenburn-dark-blue-2))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,azenburn-bordeaux))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,azenburn-violet-1))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,azenburn-brown+1))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,azenburn-dark-blue-1))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,azenburn-violet+1))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,azenburn-brown-2))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,azenburn-light-blue))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,azenburn-violet))))
   `( rainbow-delimiters-depth-12-face ((,class (:foreground ,azenburn-brown-5))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((,class (:foreground ,azenburn-violet))))
   `(rpm-spec-doc-face ((,class (:foreground ,azenburn-violet))))
   `(rpm-spec-ghost-face ((,class (:foreground ,azenburn-blue))))
   `(rpm-spec-macro-face ((,class (:foreground ,azenburn-dark-blue))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,azenburn-blue))))
   `(rpm-spec-package-face ((,class (:foreground ,azenburn-blue))))
   `(rpm-spec-section-face ((,class (:foreground ,azenburn-dark-blue))))
   `(rpm-spec-tag-face ((,class (:foreground ,azenburn-brown))))
   `(rpm-spec-var-face ((,class (:foreground ,azenburn-blue))))

   ;; rst-mode
   `(rst-level-1-face ((,class (:foreground ,azenburn-light-blue))))
   `(rst-level-2-face ((,class (:foreground ,azenburn-violet+1))))
   `(rst-level-3-face ((,class (:foreground ,azenburn-brown-1))))
   `(rst-level-4-face ((,class (:foreground ,azenburn-dark-blue-2))))
   `(rst-level-5-face ((,class (:foreground ,azenburn-bordeaux))))
   `(rst-level-6-face ((,class (:foreground ,azenburn-violet-1))))

   ;; show-paren
   `(show-paren-mismatch ((,class (:foreground ,azenburn-blue-3 :background ,azenburn-bg :weight bold))))
   `(show-paren-match ((,class (:foreground ,azenburn-brown-1 :background ,azenburn-bg :weight bold))))

   ;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))

   ;; SLIME
   `(slime-repl-inputed-output-face ((,class (:foreground ,azenburn-blue))))

   ;; tabbar
   `(tabbar-button ((,class (:foreground ,azenburn-fg
					 :background ,azenburn-bg))))
   `(tabbar-selected ((,class (:foreground ,azenburn-fg
					   :background ,azenburn-bg
					   :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((,class (:foreground ,azenburn-fg
					     :background ,azenburn-bg+1
					     :box (:line-width -1 :style released-button)))))

   ;; volatile-highlights
   `(vhl/default-face ((,class (:background ,azenburn-bg+1))))

   ;; emacs-w3m
   `(w3m-anchor ((,class (:foreground ,azenburn-dark-blue :underline t
				      :weight bold))))
   `(w3m-arrived-anchor ((,class (:foreground ,azenburn-dark-blue-2
					      :underline t :weight normal))))
   `(w3m-form ((,class (:foreground ,azenburn-blue-1 :underline t))))
   `(w3m-header-line-location-title ((,class (:foreground ,azenburn-dark-blue
							  :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((,class (:foreground ,azenburn-violet+2 :background ,azenburn-bg))))
   `(w3m-lnum-match ((,class (:background ,azenburn-bg-1
					  :foreground ,azenburn-light-blue
					  :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((,class (:foreground ,azenburn-dark-blue))))

   ;; whitespace-mode
   `(whitespace-space ((,class (:background ,azenburn-bg :foreground ,azenburn-bg+1))))
   `(whitespace-hspace ((,class (:background ,azenburn-bg :foreground ,azenburn-bg+1))))
   `(whitespace-tab ((,class (:background ,azenburn-bg :foreground ,azenburn-blue))))
   `(whitespace-newline ((,class (:foreground ,azenburn-bg+1))))
   `(whitespace-trailing ((,class (:foreground ,azenburn-blue :background ,azenburn-bg))))
   `(whitespace-line ((,class (:background ,azenburn-bg-05 :foreground ,azenburn-green))))
   `(whitespace-space-before-tab ((,class (:background ,azenburn-light-blue :foreground ,azenburn-light-blue))))
   `(whitespace-indentation ((,class (:background ,azenburn-dark-blue :foreground ,azenburn-blue))))
   `(whitespace-empty ((,class (:background ,azenburn-dark-blue :foreground ,azenburn-blue))))
   `(whitespace-space-after-tab ((,class (:background ,azenburn-dark-blue :foreground ,azenburn-blue))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((,class (:foreground ,azenburn-blue-2))))
   `(wl-highlight-folder-many-face ((,class (:foreground ,azenburn-blue-1))))
   `(wl-highlight-folder-path-face ((,class (:foreground ,azenburn-light-blue))))
   `(wl-highlight-folder-unread-face ((,class (:foreground ,azenburn-brown))))
   `(wl-highlight-folder-zero-face ((,class (:foreground ,azenburn-fg))))
   `(wl-highlight-folder-unknown-face ((,class (:foreground ,azenburn-brown))))
   `(wl-highlight-message-citation-header ((,class (:foreground ,azenburn-blue-1))))
   `(wl-highlight-message-cited-text-1 ((,class (:foreground ,azenburn-blue))))
   `(wl-highlight-message-cited-text-2 ((,class (:foreground ,azenburn-violet+2))))
   `(wl-highlight-message-cited-text-3 ((,class (:foreground ,azenburn-brown))))
   `(wl-highlight-message-cited-text-4 ((,class (:foreground ,azenburn-brown+1))))
   `(wl-highlight-message-header-contents-face ((,class (:foreground ,azenburn-violet))))
   `(wl-highlight-message-headers-face ((,class (:foreground ,azenburn-blue+1))))
   `(wl-highlight-message-important-header-contents ((,class (:foreground ,azenburn-violet+2))))
   `(wl-highlight-message-header-contents ((,class (:foreground ,azenburn-violet+1))))
   `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,azenburn-violet+2))))
   `(wl-highlight-message-signature ((,class (:foreground ,azenburn-violet))))
   `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,azenburn-fg))))
   `(wl-highlight-summary-answered-face ((,class (:foreground ,azenburn-brown))))
   `(wl-highlight-summary-disposed-face ((,class (:foreground ,azenburn-fg
							      :slant italic))))
   `(wl-highlight-summary-new-face ((,class (:foreground ,azenburn-brown))))
   `(wl-highlight-summary-normal-face ((,class (:foreground ,azenburn-fg))))
   `(wl-highlight-summary-thread-top-face ((,class (:foreground ,azenburn-dark-blue))))
   `(wl-highlight-thread-indent-face ((,class (:foreground ,azenburn-green))))
   `(wl-highlight-summary-refiled-face ((,class (:foreground ,azenburn-fg))))
   `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

   ;; which-func-mode
   `(which-func ((,class (:foreground ,azenburn-violet+4))))

   ;; yascroll
   `(yascroll:thumb-text-area ((,class (:background ,azenburn-bg-1))))
   `(yascroll:thumb-fringe ((,class (:background ,azenburn-bg-1 :foreground ,azenburn-bg-1))))
   )

  ;;; custom theme variables
  (custom-theme-set-variables
   'anti-zenburn
   `(ansi-color-names-vector [,azenburn-bg ,azenburn-blue ,azenburn-violet
			      ,azenburn-dark-blue ,azenburn-brown
			      ,azenburn-green ,azenburn-bordeaux
			      ,azenburn-fg])

   ;; fill-column-indicator
   `(fci-rule-color ,azenburn-bg-05))

  ;;; colors for the ansi-term
  (eval-after-load 'term
    `(setq ansi-term-color-vector
	   (vector 'unspecified ,azenburn-bg ,azenburn-blue
		   ,azenburn-violet ,azenburn-dark-blue ,azenburn-brown
		   ,azenburn-green ,azenburn-bordeaux ,azenburn-fg))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory
		(file-name-directory load-file-name))))

(provide-theme 'anti-zenburn)

;;; anti-zenburn-theme.el ends here

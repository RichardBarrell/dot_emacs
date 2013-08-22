;; (find-file "~/.plan")
;; (org-mode)

(setq load-path (cons "/home/richardb/stuff/share/emacs/site-lisp" load-path))
;(load-file "/home/richardb/stuff/ProofGeneral/generic/proof-site.el")
;(setq load-path (cons "/usr/share/emacs23/site-list/php-elisp" load-path)
(setq load-path (cons "/home/richardb/stuff/emacs" load-path))
;(defun my-custom-proof-keys () (interactive)(local-set-key (kbd "C-M-RET") 'proof-assert-until-point-interactive))
;(add-hook 'proof-mode-hook 'my-custom-proof-keys)

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(autoload 'php-mode "php-mode.el" "Php mode." t)

(require 'cl)
(require 'tls)
(require 'lorem-ipsum)
(require 'misc)
(require 'whitespace)
(require 'flymake)
(require 'notify)
(require 'css-mode)
(require 'cycle-buffer)
(require 'ssh)
(require 'grep)
(require 'rainbow-mode)
(require 'undo-tree)
(global-undo-tree-mode)

;; Temporarily highlights things like just-yanked text.
(require 'volatile-highlights)
(volatile-highlights-mode 1)

(require 'python)
(defun silent-python-check ()
  "Like python-check, but doesn't bug me to confirm the command each time."
  (interactive)
  (python-check (concat python-check-command " " (buffer-file-name))))

(add-hook 'python-mode-hook
  '(lambda() (local-set-key "\C-c\C-w" 'silent-python-check)))

(defun nop ()
  "nothing"
  (interactive))
(global-set-key "\C-x\C-g" 'nop)

;; ; 't -> copy/paste with primary x11 copy buffer
(setq x-select-enable-primary 't)

;; https://github.com/capitaomorte/yasnippet
;; (setq yas/root-directory "~/stuff/emacs/yasnippets")
;; (setq yas/trigger-key [\C-,])
;; (yas/initialize)
;; (yas/load-directory yas/root-directory)

;; Show path info in otherwise identical filenames
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; remember last place in each file
(setq-default save-place t)
(require 'saveplace)

; http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(defun jao-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display 
   (if selective-display nil (or column 1))))

(global-set-key [?\C-.] 'jao-toggle-selective-display)

(require 'revbufs)
(global-set-key "\C-x\C-a" 'revbufs)

; I never hit C-z on purpose.
(global-unset-key "\C-z")

(require 'magit)
(global-set-key "\C-xg" 'magit-status)

(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . mercury-mode))
(add-to-list 'auto-mode-alist '("/*.\.php[345]?$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))

(require 'conf-mode)
(add-to-list 'auto-mode-alist '("\\.vcl$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . conf-mode))

(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;(require 'haxe-mode)
;(add-to-list 'auto-mode-alist '("\\.hx$" . haxe-mode))

(require 'ruby-mode)
(require 'inf-ruby)
(require 'ri)

(setq ruby-program-name "/usr/bin/irb1.9.1 --inf-ruby-mode")

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
				     interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
      '(lambda ()
         (inf-ruby-keys)
))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(fset 'yes-or-no-p 'y-or-n-p)
(setq require-final-newline t)

(ispell-minor-mode t)

; (load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")

;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq haskell-program-name "/usr/bin/ghci")

(setq-default indent-tabs-mode nil)

(add-hook 'rst-mode-hook
  '(lambda ()
	 (setq indent-tabs-mode nil)))

(show-paren-mode t)
(setq make-backup-files nil)

(require 'fliptext)
(require 'zenburn)
(eval-after-load "zenburn" '(zenburn))

(defun zap-spaces ()
  (interactive)
  (while (member (char-after) '(32 9)) (delete-char 1))
)

(defun paws-a-tick ()
  (interactive)
  (sleep-for 0.25))
(global-set-key [?\C-'] 'paws-a-tick)

(defun center-next-line ()
  (interactive)
  (progn (next-line) (recenter)))
(defun center-previous-line ()
  (interactive)
  (progn (previous-line) (recenter)))

(defun flyspell-buffer-or-region (start end)
  (interactive "r")
  (if mark-active (flyspell-region start end) (flyspell-buffer)))

(global-set-key "\M-n" 'center-next-line)
(global-set-key "\M-p" 'center-previous-line)

(defun backward-kill-more-word ()
  (interactive)
  (let ((beg (point)))
	(backward-char)
	(while (not (member (char-before) '(32 9 10))) (backward-char))
	(kill-region beg (point))))

(defun kill-completions ()
  (interactive)
  (kill-buffer "*Completions*"))
; Most controversial keybinding thus far:
(global-set-key "\C-x\C-c" 'kill-completions)

; http://steve.yegge.googlepages.com/effective-emacs :)
(global-set-key "\C-w"     'backward-kill-more-word)
; C-x 1 deletes windows only vertically, so as not to mess up nice horz splits
(global-set-key "\C-x1"    'delete-other-windows-vertically)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key [?\C-,] 'sort-lines)
(global-set-key "\C-ca"    'align-regexp)
(global-set-key "\C-cr"    'query-replace-regexp)
(global-set-key "\C-cf"    'flyspell-buffer-or-region)
(global-set-key "\M-/"     'hippie-expand)
(global-set-key [?\C-u]    'backward-kill-line)
(global-set-key [?\C--]    'ucs-insert)
(global-set-key [?\C-<]    'next-buffer)
(global-set-key [?\C->]    'previous-buffer)
(global-set-key [?\M-z]    'zap-up-to-char)
(global-set-key [?\C-+]    'whitespace-mode)
(global-set-key "\C-x\M-r" 'rename-buffer)
(global-set-key "\C-_"     'cycle-buffer-permissive)
(global-set-key [?\C-~]    'compare-windows)
(global-set-key [?\C-z ?\C-k] 'zap-spaces)
(global-set-key "\M-+"     'describe-char)
(global-set-key "\C-ck"    'delete-trailing-whitespace)
(global-set-key [?\C-z ?\C-z] 'woman)

(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)

(defun edit-my-dot-emacs ()
  (interactive)
  (find-file "/home/richardb/stuff/emacs/dot_emacs.el"))
(global-set-key [?\C-\:] 'edit-my-dot-emacs)

; [?\C-#] == [67108899]
; [?\C-|] == [67108988]
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key [67108899] 'kill-current-buffer)

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key "\M-Y" 'yank-pop-forwards) ; M-Y (Meta-Shift-Y)

(defun echo-buffer-name ()
  (interactive)
  (message "%s" (buffer-file-name)))
(global-set-key "\C-x\M-f" 'echo-buffer-name)

(defun which-shell-command (command-name)
  (interactive "MCommand-name? ")
  (shell-command (concat "which " command-name) t))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(line-number-mode t)
(column-number-mode t)

; unlike keyboard-translate, the key translation map is global
; http://groups.google.com/group/gnu.emacs.help/browse_thread/thread/dc158fda36b7dad6
(define-key key-translation-map [?\C-h] [?\C-?])

; was using espresso-mode for javascript
;(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
;(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
;(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
; js2-mode's indentation is just too fucking braindead
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 4)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map [(meta control \;)] 
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
    (js2-highlight-vars-mode))
  (message "Scary JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
;(add-hook 'js2-mode-hook 'js2-custom-setup)
;(defun js2-custom-setup ()
;  (moz-minor-mode 1))

(add-to-list 'auto-mode-alist '("\\.pt$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.zpt$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.zcml$" . xml-mode))

(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

(setq browse-url-generic-program (executable-find "firefox")
      browse-url-browser-function 'browse-url-generic)

(defun tf-toggle-tab-width-setting ()
    "Toggle setting tab widths between 4 and 8"
    (interactive)
    (setq tab-width (if (= tab-width 4) 8 (/ tab-width 2)))
    (setq c-basic-offset tab-width)
    (message "Set tab-width and c-basic-offset to %d." tab-width)
    (redraw-display))

(setq sgml-basic-offset 2)
(global-set-key (kbd "M-#") 'tf-toggle-tab-width-setting)

(defun reverse-other-window (&optional arg)
  "Act like other-window with negated argument. Without argument,
act like (other-window -1)."
  (interactive "p")
  (if arg
	  (other-window (- arg))
	  (other-window -1)))

; control = to switch windows
(global-set-key [?\C-=] 'other-window)
; c-tab and c-s-tab to move around windows
(global-set-key (kbd "<C-S-iso-lefttab>") 'reverse-other-window)
(global-set-key (kbd "<C-tab>") 'other-window)
; control-u acts like it does in bash

(defun backward-kill-line (&optional arg)
  "Nuke from current position back to the start of the line."
  (interactive "p")
  (if arg
    (kill-line (- 1 arg))
    (kill-line 0)))

(defun named-shell (n)
  "Create a shell, already renamed."
  (interactive "MName: ")
  (shell)
  (rename-buffer n))

(defun string/ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

(defun string/starts-with (s arg)
  "returns non-nil if string S starts with ARG.  Else nil."
  (cond ((>= (length s) (length arg))
         (string-equal (substring s 0 (length arg)) arg))
        (t nil)))

(defun named-shell-in-directory (dirname)
  "Open a shell in a directory."
  (interactive "DDirectory: ")
  (let* ((sdname (if (string/ends-with dirname "/") dirname (concat dirname "/")))
         (newbuf (generate-new-buffer sdname)))
    (switch-to-buffer newbuf)
    (cd sdname)
    (shell newbuf)))

(global-set-key [?\C-x ?!] 'named-shell)
(global-set-key [?\C-x ?\"] 'named-shell-in-directory)

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(setq inhibit-startup-message t)

(add-hook 'sgml-mode-hook (lambda ()
			    (setq Lorem-ipsum-paragraph-separator "<br /><br />\n"
				  Lorem-ipsum-sentence-separator "&nbsp;&nbsp;"
				  Lorem-ipsum-list-beginning "<ul>\n"
				  Lorem-ipsum-list-bullet "<li>"
				  Lorem-ipsum-list-item-end "</li>\n"
				  Lorem-ipsum-list-end "</ul>\n")))

(add-hook 'c-mode-hook (lambda () (setq c-basic-offset-4)))

(ispell-minor-mode t)
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;http://younix.us/cgit/cgit.cgi/dhd.git/tree/hbase/.emacs
; sprunge.us owns
;; (defun sprunge (prefix)
;;   "Posts the current buffer to sprunge, and shows the resulting URL in a new buffer"
;;   (interactive "P")
;;   (let ((filename "/tmp/sprunge-post"))
;;     (if prefix (write-file filename) (write-region (region-beginning) (region-end) filename)) ; if invoked with the universal argument / prefix, upload the whole file, else upload just the region
;;     (insert (shell-command-to-string (concat "curl -s -F 'sprunge=<" filename "' http://sprunge.us")))
;;     (delete-char -1))) ; Newline after URL

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ansi-color-for-comint-mode t)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "/usr/bin/chromium-browser" t)
 '(compilation-ask-about-save nil)
 '(explicit-shell-file-name "/bin/bash")
 '(indent-tabs-mode nil)
 '(js2-auto-indent-p t)
 '(js2-bounce-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-enter-indents-newline nil)
 '(js2-highlight-level 3)
 '(js2-indent-on-enter-key nil)
 '(js2-mode-indent-ignore-first-tab t)
 '(js2-rebind-eol-bol-keys nil)
 '(nxml-child-indent 4)
 '(nxml-default-buffer-file-coding-system (quote utf-8))
 '(py-pychecker-command "/home/richardb/bin/pychecker")
 '(py-pychecker-command-args (quote ("")))
 '(python-check-command "/home/richardb/bin/pychecker")
 '(scroll-conservatively 15)
 '(tab-width 4)
 '(term-scroll-to-bottom-on-output nil)
 '(visible-bell t))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

(put 'scroll-left 'disabled nil)
(set-scroll-bar-mode 'right)

(defun alist-insert (alist key value)
  "Returns a new alist with (key,value) inserted into it."
  (let ((newlist nil) (found nil) k v)
	(dolist (e alist)
	  (setq k (car e))
	  (setq v (cdr e))
	  (if (equal k key)
		  (progn (unless found (setq newlist (cons (cons k value) newlist)))
				 (setq found t))
		  (setq newlist (cons (cons k v) newlist))))
	(unless found
	  (setq newlist (cons (cons key value) newlist)))
	(reverse newlist)))

(setq default-frame-alist
	  (alist-insert default-frame-alist 'font-backend "xft"))
;(setq default-frame-alist
;	  (alist-insert default-frame-alist 'font "monofur-12"))
;; (setq default-frame-alist
;; 	  (alist-insert default-frame-alist 'font "Droid Sans Mono Slashed-10"))
(setq default-frame-alist
	  (alist-insert default-frame-alist 'font "Source Code Pro-10:regular"))
;(setq default-frame-alist
;	  (alist-insert default-frame-alist 'font "DejaVu Sans Mono-10"))

; (set-face-font 'default "-*-tamzen-medium-*-*-*-15-*-*-*-*-*-*-*")

;(set-face-attribute 'default nil :font "Source Code Pro-10:regular")

;; On load, zenburn keeps overwriting my custom faces.
;; zenburn is loaded asynchronously for some reason.
;; TODO, figure out how to have it not do that.
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "adobe" :family "Source Code Pro"))))
 '(rst-level-1-face ((t (:background "grey5"))) t)
 '(rst-level-2-face ((t (:background "grey10"))) t)
 '(rst-level-3-face ((t (:background "grey15"))) t)
 '(rst-level-4-face ((t (:background "grey20"))) t)
 '(rst-level-5-face ((t (:background "grey25"))) t)
 '(rst-level-6-face ((t (:background "grey30"))) t))

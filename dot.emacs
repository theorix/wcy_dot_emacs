;(setq wcy-leader-key-mode nil)
;(add-to-list 'compilation-search-path "~/d/working/ejabberd")
;(setq compile-command "cd ~/d/working/ejabberd; make -k")
;;(setq compile-command "cd ~/d/working/ejabberd; ./rebar compile")
;;(setq default-directory "/home/zou/d/working/ejabberd")

(when (display-graphic-p)
  (setq fonts
        (cond ((eq system-type 'darwin)     '("Monaco"     "STHeiti"))
              ((eq system-type 'gnu/linux)  '("Menlo"     "WenQuanYi Zen Hei"))
              ((eq system-type 'windows-nt) '("Consolas"  "Microsoft Yahei"))))

  (setq face-font-rescale-alist '(("STHeiti" . 1.2) ("Microsoft Yahei" . 1.2) ("WenQuanYi Zen Hei" . 1.2)))
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 14))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family (car (cdr fonts))))))

;;(add-to-list 'exec-path "/usr/local/Cellar/erlang/R17.5/bin")
;(add-to-list 'exec-path "/usr/local/Cellar/erlang/R18.0/bin")
(add-to-list 'exec-path "/usr/lib/erlang/bin")
;;(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/bin")
(setenv "PATH" (mapconcat 'identity exec-path ":"))
(setenv "MY_EMACS_HOME" (or (getenv "MY_EMACS_HOME")
                            (concat (getenv "HOME")  "/d/working/wcy_dot_emacs")))
(load (expand-file-name
       "dot.emacs"
       (concat (getenv "MY_EMACS_HOME") "/my.config")))

(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((allout-layout . t))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))
;; Local Variables:
;; mode:emacs-lisp
;; coding: undecided-unix
;; End:
(set-default 'cursor-type 'box)
(put 'upcase-region 'disabled nil)

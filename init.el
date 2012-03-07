(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings
                                  starter-kit-js starter-kit-ruby zenburn-theme
                                  yasnippet-bundle ac-slime)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; personal configuration
(setq user-mail-address "vinod@kurup.com")
;; set calendar's location (for sunrise sunset)
(setq calendar-latitude 35.9162)
(setq calendar-longitude -79.0999)
(setq calendar-location-name "Chapel Hill, NC")
(setq-default kill-whole-line t)        ; ctrl-k kills whole line if at col 0
(menu-bar-mode)

;; ledger
;; make cleared items green, uncleared pink
(add-hook 'ledger-mode-hook 
          (lambda ()
            (highlight-lines-matching-regexp "^..\\(..\\)?/..?/..?[        ]+[^\\*]" (quote hi-pink))
            (highlight-lines-matching-regexp "^..\\(..\\)?/..?/..?[        ]+\\*" (quote hi-green))))

(defun ledger ()
  "Open my ledger file and go to today"
  (interactive)
  (find-file "~/Dropbox/ledger.dat")
  (ledger-find-slot (current-time)))

(defun vk-copy-ledger-entry-to-bottom ()
  "Copy the current transaction to the bottom of the ledger"
  (interactive)
  (re-search-backward "^[12][09]")
  (let ((beg (point)))
    (forward-char)
    (re-search-forward "^[12][09]")
    (beginning-of-line)
    (copy-region-as-kill beg (point))
    (goto-char (point-max))
    (yank '(non nil list))
    (forward-word)
    (forward-char)))

(require 'org-install)
(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "todo.org"))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map [f8] (lambda () (interactive) (org-capture nil "t")))
;;(define-key global-map [f9] 'remember-region)
(setq org-log-done t)
(setq org-agenda-show-log t)
(setq org-return-follows-link t)
(setq org-startup-indented t)
(setq org-agenda-start-on-weekday nil) ; show agenda starting today
(setq org-use-speed-commands t)
(setq org-archive-location (concat org-directory "archive/%s_archive::"))

(defun gtd ()
  "Open my todo list"
  (interactive)
  (find-file (concat org-directory "gtd.org")))

;; Delete old backup versions silently
(setq delete-old-versions t)
;; move to trash instead of Delete
(setq delete-by-moving-to-trash t)

(load-theme 'zenburn t)

(global-set-key [(control x) (control r)] 'esk-sudo-edit)

(fset 'vk-process-movie-list
      [?\C-a down ?\C-s ?2 ?0 ?1 ?1 left left left left ?\C-  ?\C-s ?  ?\C-s left ?\M-w right ?\C-y ?- left left left backspace ?- left left left backspace ?- right right right right right right ?\C-  ?\C-e ?\C-w ?. ?a ?v ?i left left left left ?\C-x ?o ?m ?p ?l ?a ?y ?e ?r ?  ?\C-y return ?\C-x ?o])

(defun vk-slugify (title)
  "Convert a normal Title string to something that can be used in a blog slug."
  (replace-regexp-in-string "[\\. ]+" "-"
                            (replace-regexp-in-string "'" ""
                                                      (downcase title))))

(defun vk-blogpost (title)
  "Create a new blog post."
  (interactive "sPost Title: ")
  (let ((slug (vk-slugify title)))
    (find-file (concat "~/web/kurup.org/source/_posts/"
                       (format-time-string "%Y-%m-%d")
                       "-" slug ".markdown"))
    (insert "---\n")
    (insert "layout: post\n")
    (insert "date: " (format-time-string "%Y/%m/%d %H:%M:%S") "\n")
    (insert "title: " title "\n")
    (insert "comments: true\n")
    (insert "categories: \n")
    (insert "---\n\n")))

;;; org-velocity usage:
(require 'org-velocity)
(setq org-velocity-bucket (expand-file-name "bucket.org" org-directory))
(global-set-key (kbd "C-c v") 'org-velocity-read)

;; edit server for edit-in-emacs chrome extension
(require 'edit-server)
(edit-server-start)

;; twitter mode
;; http://www.emacswiki.org/emacs/TwitteringMode
(require 'twittering-mode)
(setq twittering-use-master-password t)

;; Missing from ruby-mode.el, see https://groups.google.com/group/emacs-on-rails/msg/565fba8263233c28
(defun ruby-insert-end () 
  "Insert \"end\" at point and reindent current line." 
  (interactive) 
  (insert "end") 
  (ruby-indent-line t) 
  (end-of-line)) 

(add-hook 'ruby-mode-hook
          (lambda ()
            (require 'ruby-electric)
            (ruby-electric-mode t)))

;; autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vinod/ac-dict")
(ac-config-default)
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "~/src/android-sdk-linux_x86")
 '(browse-url-browser-function (quote browse-url-chromium))
 '(browse-url-generic-program "chromium-browser")
 '(inferior-lisp-program "lein repl")
 '(nxml-bind-meta-tab-to-complete-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files (quote ("~/org/gtd.org")))
 '(org-capture-templates (quote (("j" "Journal Entry" entry (file "~/org/notes.org") "* %T %?") ("t" "Create Task" entry (file+headline "~/Dropbox/org/gtd.org" "Inbox") "* TODO %?"))))
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-velocity)))
 '(org-refile-targets (quote ((org-agenda-files :level . 1))))
 '(org-velocity-allow-regexps t)
 '(org-velocity-always-use-bucket t)
 '(org-velocity-bucket "/home/vinod/Dropbox/org/bucket.org")
 '(org-velocity-create-method (quote capture))
 '(org-velocity-max-depth 2)
 '(org-velocity-search-method (quote phrase))
 '(temporary-file-directory (concat user-emacs-directory "tmp"))
 '(weblogger-config-alist (quote (("default" "http://www.blogger.com/api" "vvkurup@gmail.com" "" "6482582243742832795")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-hide ((default (:foreground "grey25")) (nil nil))))


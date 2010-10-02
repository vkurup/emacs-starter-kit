(column-number-mode t)                  ; display the column number on modeline
(setq-default kill-whole-line t)        ; ctrl-k kills whole line if at col 0

; ledger
; make cleared items green, uncleared pink
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
  (re-search-backward "^20")
  (let ((beg (point)))
    (forward-char)
    (re-search-forward "^20")
    (beginning-of-line)
    (copy-region-as-kill beg (point))
    (goto-char (point-max))
    (yank '(non nil list))
    (forward-word)
    (forward-char)))

(setq user-mail-address "vinod@kurup.com")

; remember mode
(org-remember-insinuate)
(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "gtd.org"))
(setq org-log-done t)
(setq org-agenda-show-log t)
(setq org-return-follows-link t)
(define-key global-map [f8] 'remember)
(define-key global-map [f9] 'remember-region)
(global-set-key "\C-cb" 'org-iswitchb)

(defun gtd ()
  "Open my todo list"
  (interactive)
  (find-file (concat org-directory "gtd.org")))

(setq org-remember-templates
      '(("Tasks" ?t "* TODO %?\n  %i\n Added: %U\n %a" org-default-notes-file "Inbox")
        ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/org/journal.org" "Entries")
        ("MorningPages" ?m "* %U %?\n\n  %i\n  %a" "~/org/journal.org" "MorningPages")
        ("Idea" ?i "* %^{Title}\n  %i\n  %a" "~/org/journal.org" "New Ideas")))

(setq org-agenda-custom-commands
      '(("p" tags "PROJECT-MAYBE-DONE" nil)
        ("m" tags "PROJECT&MAYBE" nil)
       ))

;; Delete old backup versions silently
(setq delete-old-versions t)

(ido-everywhere t)

;; set calendar's location (for sunrise sunset)
(setq calendar-latitude 35.9162)
(setq calendar-longitude -79.0999)
(setq calendar-location-name "Chapel Hill, NC")

; maximize on startup
(setq initial-frame-alist
      `((left . 0) (top . 0)
        (width . 80) (height . 65)))

(color-theme-zenburn)

;; move to trash instead of Delete
(setq delete-by-moving-to-trash t)

(require 'android-mode)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "~/src/android-sdk-linux_86")
 '(inferior-lisp-program "lein repl")
 '(org-agenda-files (quote ("~/Dropbox/org/gtd.org"))))

;; open files as root
;; from http://www.emacswiki.org/cgi-bin/wiki/TrampMode
(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."
  
  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
         ;; use a separate history list for "root" files.
         (file-name-history find-file-root-history)
         (name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p name)
                     (tramp-dissect-file-name name)))
         path dir file)
    
    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-path tramp)
            dir (file-name-directory path)))
    
    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

(global-set-key [(control x) (control r)] 'find-file-root)

(fset 'vk-process-movie-list
   [?\C-a down ?\C-s ?2 ?0 ?1 ?0 left left left left ?\C-  ?\C-s ?  ?\C-s left ?\M-w right ?\C-y ?- left left left backspace ?- left left left backspace ?- right right right right right right ?\C-  ?\C-e ?\C-w ?. ?a ?v ?i left left left left ?\C-x ?o ?m ?p ?l ?a ?y ?e ?r ?  ?\C-y return ?\C-x ?o])


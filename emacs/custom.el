;; -------------------- admin stuff
;; check os we are running on
(defvar gnulinux-p (string-match "gnu/linux" (symbol-name system-type)))
(defvar macosx-p (string-match "darwin" (symbol-name system-type)))
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))

;; enable server mode for shell client
(server-start)

;; load latest org
(add-to-list 'load-path "~/src/org-mode/lisp")

;; add helm
(add-to-list 'load-path "~/src/helm")
(require 'helm-config)

;; -------------------- mac stuff
;; use cmd as meta on mac
(if macosx-p
  (progn
    (setq mac-option-key-is-meta nil)
    (setq mac-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)))

;; path and exec-path variable with macports
(if macosx-p
  (progn
    (setenv "PATH" (concat "/opt/local/sbin:/opt/local/bin:" (getenv "PATH")))
    (setq exec-path (append '("/opt/local/sbin") '("/opt/local/bin") exec-path))))

;; -------------------- plugins
;; undo tree
(add-to-list 'load-path "~/src/undo-tree")
(require 'undo-tree)
(global-undo-tree-mode)

;; velocity
(add-to-list 'load-path "~/src/org-mode/contrib/lisp" t)
(require 'org-velocity)
(global-set-key (kbd "C-c v") 'org-velocity-read)
(setq org-velocity-always-use-bucket t)
(setq org-velocity-bucket "~/src/org/global.org")
(setq org-velocity-search-method (quote any))

;; velocity
(require 'ox-confluence)

;; babel
(setq org-confirm-babel-evaluate nil)

;; active babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (sh . t)
   (python . t)
   ))

;; evil
(add-to-list 'load-path "~/src/evil")
(require 'evil)
(evil-mode 1)

;; taskjuggler
(require 'ox-taskjuggler)

;; map vim move keys for evil org-mode
(mapcar (lambda (state)
  (evil-declare-key state org-mode-map
    (kbd "M-l") 'org-metaright
    (kbd "M-h") 'org-metaleft
    (kbd "M-k") 'org-metaup
    (kbd "M-j") 'org-metadown
    (kbd "M-L") 'org-shiftmetaright
    (kbd "M-H") 'org-shiftmetaleft
    (kbd "M-K") 'org-shiftmetaup
    (kbd "M-J") 'org-shiftmetadown))
  '(normal insert))

;; disable flyspell mode by default
(setq prelude-flyspell nil)
(setq whitespace-line-column 200)

;; -------------------- org general setup
;; open .org and .org_archive files with org-mode
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

;; useful key mappings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; define default drawers
(setq org-drawers (quote ("properties" "clocktable" "logbook" "clock")))

;; hide leading stars
(setq org-hide-leading-stars 'hidestars)

;; use ido for target completion
(setq org-completion-use-ido t)

;; -------------------- org agenda
;; set agenda file
(setq org-agenda-files (quote
 ("~/src/org/global.org"
  "~/src/org/emenda.org"
  "~/src/org/customers.org"
  "~/src/org/private.org"
  "~/src/org/knowhow.org"
  )))


;; highlight current time in agenda
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1 )))

;; display priorities
(setq org-agenda-fontify-priorities
   (quote ((65 (:foreground "Red")) (66 (:foreground "Blue")) (67 (:foreground "Darkgreen")))))

;; mark weekend
(setq org-agenda-date-weekend (quote (:foreground "Yellow" :weight bold)))

;; hide tasks already done
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

;; hide items tagged with someday
(setq org-agenda-filter-preset '("-someday"))

;; own agenda views
(setq org-agenda-custom-commands
  (quote (
   ("s" "someday" tags "someday" ((org-agenda-filter-preset
           '("+someday"))(org-agenda-todo-ignore-with-date nil)))
   ("z" "admin" todo "admin")
   ("f" "flagged with agenda"
       ((tags "admin")
        (tags "flagged")
        (agenda ""))))))

;; -------------------- org todos
;; set keywords
(setq org-todo-keywords
 '((sequence "todo(t)" "started(s!)" "waiting(w@/!)" "appt(a)" "proj(p)"
             "delegated(g@/!)" "|" "done(d!)" "admin(z)" "canceled(c@)" "archive(r)")))

;; change colors
(setq org-todo-keyword-faces
      '(("todo"  . (:foreground "#b70101" :weight bold))
        ("started"  . (:foreground "#b70101" :weight bold))
        ("appt"  . (:foreground "sienna" :weight bold))
        ("proj"  . (:foreground "blue" :weight bold))
        ("admin"  . (:foreground "orange" :weight bold))
        ("waiting"  . (:foreground "orange" :weight bold))
        ("done"  . (:foreground "forestgreen" :weight bold))
        ("delegated"  . (:foreground "forestgreen" :weight bold))
        ("canceled"  . shadow)))

;; fast todos selection
(setq org-use-fast-todo-selection t)

;; add timestamp when done
(setq org-log-done 'time)

;; use own drawer
(setq org-log-into-drawer t)

;; -------------------- org refile
;; refile targets
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

;; targets start with the file name
(setq org-refile-use-outline-path (quote file))

;; targets complete in steps
(setq org-outline-path-complete-in-steps t)

;; -------------------- org timer
;; save clock in drawer
(setq org-clock-into-drawer "clock")

;; sesume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)

;; sesume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; don't clock out when moving task to a done state
(setq org-clock-out-when-done nil)

;; persist clock
(setq org-clock-persist t)

;; disable auto clock resolution
(setq org-clock-auto-clock-resolution nil)

;; -------------------- org capture
;; setup org-capture
(setq org-default-notes-file (concat org-directory "~/src/org/global.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
 '(("t" "task" entry (file+headline "~/src/org/global.org" "inbox")
      "* todo %?")
   ("c" "task with context" entry (file+headline "~/src/org/global.org" "inbox")
      "* todo %?\n  %i\n  %a")
   ("i" "immediate admin task" entry (file+headline "~/src/org/global.org" "inbox")
          "* admin %?" :clock-in t :clock-keep t)
   ("z" "admin task" entry (file+headline "~/src/org/global.org" "inbox")
      "* admin %?\n  %i\n  %a" :clock-in t :clock-resume t)
   ("j" "journal" entry (file+datetree "~/src/org/global.org")
      "* %?\nentered on %U\n  %i\n  %a")))

;; -------------------- org column view
;; setup default format
(setq org-columns-default-format "%70item(task) %10effort(effort){:} %10clocksum")

;; setup default estimates
(setq org-global-properties (quote (("effort_all" . "0:30 1:00 2:00 4:00 8:00"))))

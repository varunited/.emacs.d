;;; org-config.el --- Org mode and related things    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: data, convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is going to contain everything related to org-mode and related
;; minor modes.

;;; Code:

(use-package org
  :pin org
  :ensure t
  :defer 10
  :bind (("C-c l" . org-store-link)
         :map ctl-quote-map
         ("C-n" . take-notes)
         :map org-mode-map
         ("M-q" . org-fill-paragraph))
  :init
  (setq org-directory (or (getenv "ORG_DIRECTORY")
                          "~/miscellany/personal/org/")
        org-archive-location (format "%s::datetree/* Archived Tasks"
                                     (expand-file-name "_archives/archive.org"
                                                       org-directory)))
  :config
  (setq org-cycle-separator-lines 0
        org-cycle-include-plain-lists 'integrate
        org-catch-invisible-edits 'error
        org-blank-before-new-entry
        '((heading . nil) (plain-list-item . nil))
        org-image-actual-width nil

        org-startup-with-latex-preview nil
        org-preview-latex-image-directory
        (expand-file-name "tmp/ltximg/" user-emacs-directory)

        ;; Speed commands let you use single keys once you are on the
        ;; first of a heading. n/p to go back and forth in the list of
        ;; headlines. For adding new commands see
        ;; `org-speed-commands-user'.
        org-use-speed-commands t

        ;; Keep text indent but always show leading stars
        org-startup-indented t
        org-indent-mode-turns-on-hiding-stars nil

        ;; Keep state change notes into LOGBOOK drawer
        org-log-into-drawer t)

  (setq org-todo-keywords
        ;; state logging for org-habit (! => with timestamp) (@ => timestamp
        ;; + note)
        '((sequence "TODO" "NEXT" "|" "DONE(d!)" )
          (sequence "BLOCKED(b@)" "|" "CANCELLED(c@)")
          (sequence "TRACKING(r)" "TRACKED(g@)"))
        org-todo-keyword-faces
        '(("TODO"      (:foreground "red" :weight bold))
          ("NEXT"      (:foreground "OrangeRed" :weight bold))
          ("BLOCKED"   (:foreground "orange" :weight bold))
          ("DONE"      (:foreground "forest green" :weight bold))
          ("CANCELLED" (:foreground "forest green" :weight bold))
          ("TRACKING"  (:foreground "light green" :weight bold))
          ("TRACKED"   (:foreground "forest green" :weight bold))))

  (setq org-todo-state-tags-triggers
        ;; Remove :someday: tag when tasks are done.
        '((done ("someday"))))

  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        ;; refiling across multiple levels
        org-refile-targets `((nil . (:maxlevel . 8))
                             ("main.org" . (:tag . "category"))
                             ("capture.org" . (:maxlevel . 3))
                             ("someday.org" . (:maxlevel . 2)))
        org-refile-allow-creating-parent-nodes t)

  (add-to-list 'org-modules 'org-habit)
  (add-hook 'org-mode-hook (lambda ()
                             (auto-fill-mode +1)
                             (setq mode-name "Org")
                             (local-set-key [remap goto-address-at-point]
                                            #'org-open-at-point)))
  (plist-put org-format-latex-options
             :foreground "Black")
  (plist-put org-format-latex-options
             :background "White")
  (plist-put org-format-latex-options
             :scale 1.3)

  ;; Add a new structure template
  (push '("t" "#+TITLE: ?") org-structure-template-alist))


(use-package org-plus-contrib
  :ensure t
  :pin org)

(use-package org-agenda
  :bind (("C-c o" . jump-to-org-agenda)
         ("C-c a" . org-agenda)
         :map org-agenda-mode-map
         ("C-o" . org-agenda-list)
         ("C-S-o" . custom-agenda-view)
         ("x" . org-agenda-quit)
         ("C-c C-r" . org-agenda-refile)
         ("C-c C-c" . org-review-captures)
         ("C-c C-n" . take-notes)
         ("C-c C-f" . org-agenda-find-file)
         ("C-c C-s" . org-schedule-and-todo)
         ("C-c C-d" . org-deadline-and-todo))
  :preface
  (defun org-agenda-find-file ()
    "Open one of files in `org-agenda-files'."
    (interactive)
    (let ((f (org-completing-read "File: " org-agenda-files)))
      (find-file f)))

  (defun org-deadline-and-todo ()
    (interactive)
    (org-agenda-todo "TODO")
    (call-interactively #'org-agenda-deadline))

  (defun org-schedule-and-todo ()
    (interactive)
    (org-agenda-todo "TODO")
    (call-interactively #'org-agenda-schedule))

  (defun jump-to-org-agenda ()
    "Jump to the agenda buffer.
     Credits: John Wigley."
    (interactive)
    (when (not (minibufferp))
      (let ((buf (get-buffer "*Org Agenda*"))
            wind)
        (if buf
            (if (setq wind (get-buffer-window buf))
                (select-window wind)
              (if (called-interactively-p 'interactive)
                  (progn
                    (select-window (display-buffer buf t t))
                    (org-fit-window-to-buffer)
                    (org-agenda-redo))
                (with-selected-window (display-buffer buf)
                  (org-fit-window-to-buffer)
                  (org-agenda-redo))))
          (custom-agenda-view)))))

  (defun org-review-captures ()
    "Review all captured tasks."
    (interactive)
    (find-file (expand-file-name "capture.org"
                                 org-directory)))

  (defun custom-agenda-view ()
    "Show my custom agenda view."
    (interactive)
    (org-agenda nil "i")
    (org-fit-window-to-buffer)
    (org-agenda-redo))

  (defun org-backlog-compare-fn (a b &optional some-tag)
    "Compare to entries in Backlog initially.
     Tasks with tag :someday are low priority."
    (let* ((tag (or some-tag "someday"))
           (a-pos (get-text-property 0 'org-marker a))
           (b-pos (get-text-property 0 'org-marker b))
           (a-has-tag (when-let ((a-tags-string (org-entry-get a-pos "TAGS")))
                        (string-match-p (regexp-quote tag) a-tags-string)))
           (b-has-tag (when-let ((b-tags-string (org-entry-get b-pos "TAGS")))
                        (string-match-p (regexp-quote tag)
                                        (org-entry-get b-pos "TAGS")))))
      (cond
       ((and a-has-tag (not b-has-tag)) -1)
       ((and b-has-tag (not a-has-tag)) +1))))

  :init
  (setq org-agenda-cmp-user-defined #'org-backlog-compare-fn
        org-complete-tags-always-offer-all-agenda-tags t)

  ;; Show org-agenda after some idle-time
  (add-hook 'emacs-startup-hook
            (lambda () (run-with-idle-timer 300 t 'jump-to-org-agenda)))

  (defvar org-agenda-known-files
    (list "journal.org" "main.org" "remember.org" "habits.org" "work.org")
    "Files recognized as agenda files.")

  (setq org-agenda-files
        (mapcar (lambda (f)
                  (expand-file-name f org-directory))
                (seq-filter (lambda (f)
                              (member f org-agenda-known-files))
                            (let ((default-directory org-directory))
                              (file-expand-wildcards "*.org"))))
        org-agenda-text-search-extra-files
        (append (file-expand-wildcards (expand-file-name "_archives/*.org"
                                                         org-directory))
                (file-expand-wildcards (expand-file-name "notes/*.org"
                                                         org-directory))
                (list (expand-file-name "capture.org"
                                        org-directory))))
  (setq org-agenda-span 1
        org-agenda-restore-windows-after-quit t

        org-agenda-span 7
        org-agenda-show-all-dates nil
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-deadline-if-done t
        org-deadline-warning-days 3

        org-agenda-tags-column 90
        org-agenda-show-inherited-tags nil
        org-agenda-compact-blocks nil
        org-agenda-block-separator "╭──────────────"
        org-agenda-clockreport-parameter-plist
        '(:link t :max-level 4 :fileskip0 t :compact t :narrow 80)

        ;; Keep all tasks in same state grouped together and then sort
        ;; among them according to priority.  The order of tasks in
        ;; agenda represents my ability to act on them.
        org-agenda-sorting-strategy
        '((agenda habit-down time-up todo-state-down priority-down category-keep)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep))

        ;; Custom agenda vews
        org-agenda-custom-commands
        '(("i" "My Agenda"
           ((agenda "TODO"
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'todo
                                                 '("ONGOING"
                                                   "NEXT"
                                                   "BLOCKED")))))
            (todo "NEXT"
                  ((org-agenda-overriding-header "  Next Tasks:")))
            (todo "BLOCKED"
                  ((org-agenda-overriding-header "  Blocked Tasks:")))
            (todo "TODO"
                  ;; Tasks for which I haven't thought about at all.q
                  ((org-agenda-overriding-header "  Backlog:")
                   (org-agenda-todo-ignore-scheduled 'all)
                   (org-agenda-todo-ignore-deadlines 'all)
                   (org-agenda-todo-ignore-timestamp 'all)
                   (org-agenda-sorting-strategy '(user-defined-down
                                                  priority-down
                                                  category-keep))))))))

  ;; Split org-agenda vertically | @TODO: Find a better way.
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (when (window-at-side-p (selected-window) 'top)
                (toggle-window-split))
              ;; Also add appointments from org-agenda entries.
              (quietly (org-agenda-to-appt))
              (setq mode-name "Org Agenda")
              (local-set-key (kbd "C-c P") 'org-pomodoro)
              (local-set-key [remap goto-address-at-point] #'org-agenda-open-link)
              (hl-line-mode 1))))

(use-package org-cliplink :ensure t)
(use-package org-capture
  :bind (("C-c c" . org-capture))
  :config
  (setq org-capture-templates
        `(("i" "TODO" entry (file+headline "capture.org" "Tasks")
           ,(concat
             "* TODO %?                                           %^G\n\n"
             "%(let ((link (plist-get org-store-link-plist :annotation)))"
             "     (if (string-empty-p link) "
             "         link "
             "       (concat \"Here: \" link))) "))
          ;; Keeping the type to plain prevents `org-capture' from doing fancy
          ;; newline insertions.
          ("l" "Articles" plain (file+regexp "habits.org"
                                             "^.*Articles to read.*")
           "- [ ] %(org-cliplink-capture) %?")
          ("j" "Journal" entry (file+olp+datetree "journal.org")
           ,(concat "* %? %^G           \n\n"
                    "╭──────────────      \n"
                    " Entered on %U       \n"
                    " Was in: [[%F][%f]]  \n"
                    " ──────────────      \n"
                    " %i                  \n"
                    "╰──────────────        "))
          ("n" "Notes" entry (file+headline "notes.org" "Notes")
           "* %?\n ")
          ("h" "Habit" entry (file+headline "habits.org"  "Habits")
           ,(concat "* TODO %?\n"
                    "  SCHEDULED: <%(read-date \"%Y-%m-%d\") "
                    ".+%^{Repeat every (add + for non-backlog tasks)|1d|1w|1m|}> \n"
                    "  :PROPERTIES:       \n"
                    "  :STYLE:    habit   \n"
                    "  :END:              \n"))
          ("b" "Birthday" plain (file+headline "remember.org" "Birthdays")
           "\%\\%(org-anniversary %(read-date)) %?")
          ("a" "Anniversary" plain (file+headline "remember.org" "Anniversaries")
           "\%\\%(org-anniversary %(read-date)) %?"))))

(use-package org-colview
  :doc "Column view shows `org-mode' entries in a table."
  :after org
  :init
  (setq org-columns-default-format
        "%60ITEM(Task) %10EFFORT(Estimated Effort) {:} %10CLOCKSUM(Time Spent)"))

(use-package ob-ipython
  :doc "Org-babel with IPython."
  :if (executable-find "jupyter")
  :ensure t
  :after org
  :config
  (setq ob-ipython-resources-dir
        (expand-file-name "data/obipy-resources" org-directory)))

(use-package ob-elixir :ensure t :defer t)

(use-package ob
  :after org
  :init
  (setq org-babel-python-command "ipython"
        org-confirm-babel-evaluate nil)

  (add-hook 'org-babel-after-execute-hook
            #'org-display-inline-images
            'append)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               `((emacs-lisp . t)
                                 (ipython    . ,(executable-find "jupyter"))
                                 (R          . t)
                                 (ruby       . t)
                                 (gnuplot    . t)
                                 (clojure    . t)
                                 (C          . t)
                                 (elixir     . t))))

(use-package org-habit
  :after org-agenda
  :init
  (setq org-habit-graph-column 80
        org-habit-show-done-always-green t))

(use-package org-clock
  :after org-agenda
  :init
  (setq org-clock-idle-time 5)

  :config
  ;; Let's see when they remove `org-combine-plists' if ever.
  (setq org-clocktable-defaults (org-combine-plists org-clocktable-defaults
                                                    (list :stepskip0 t
                                                          :fileskip0 t))
        org-clock-heading-function
        (lambda ()
          (s-truncate 30 (nth 4 (org-heading-components))))))

(use-package org-attach
  :after org
  :preface
  (defun org-att-complete-link (&optional arg)
    "Completion dispatcher for att: links (rewritten from org-attach-open)"
    (let* ((attach-dir (expand-file-name org-attach-directory org-directory))
	       (file-paths (directory-files-recursively attach-dir ".*"))
           (completions (mapcar (lambda (f)
                                  (cons (file-name-base f) f))
                                file-paths))
	       (file-name (completing-read "att: " completions nil t))
	       (path (assoc-default file-name completions)))
      (concat "att:" path)))
  :config
  (add-to-list 'org-link-abbrev-alist '("att" . org-attach-expand-link))
  (org-link-set-parameters "att" :complete 'org-att-complete-link))

(use-package org-pomodoro
  :ensure t
  :after org-agenda
  :bind (:map org-mode-map
              ("C-c p" . org-pomodoro))
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t
        org-pomodoro-length 30
        org-pomodoro-long-break-length 20
        org-pomodoro-short-break-length 10)

  (let ((default-directory (expand-file-name "audio/"
                                             emacs-assets-directory)))
    (setq org-pomodoro-finished-sound
          (expand-file-name "quite-impressed.wav")
          org-pomodoro-short-break-sound
          (expand-file-name "decay.wav")
          org-pomodoro-long-break-sound
          (expand-file-name "quite-impressed.wav"))))

(use-package org-edna
  :doc
  "Enables defining workflows with conditions that must be met
   before a task is marked done and actions that get triggered
   once a task is marked done."
  :ensure t
  :defer t)

(provide 'org-config)
;;; org-config.el ends here

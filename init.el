;;; init.el --- varunited's Emacs configuration -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2018  Varun Sharma

;; Author: Varun Sharma <varunited@gmail.com>
;; Keywords: Emacs

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

;;; Commentary:

;;  My Emacs configuration.

;;; Code:

;;; The Epoch
(defconst emacs-start-time (current-time))

;;; Avoid garbage collection during Emacs startup.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda ()
                             ;; Make it 20MB post Emacs initialization.
                             (setq gc-cons-threshold (* 20 1024 1024))
                             (run-with-idle-timer 10 t #'garbage-collect)))

;; Try to make `.emacs.d` relocatable
(setq user-emacs-directory
      (file-name-directory (or load-file-name
                               "~/.emacs.d/init.el")))

;;; Add some directories to PATH for Emacs
(progn (mapc (lambda (dir)
               (let ((dir-full-path (expand-file-name dir)))
                 (unless (member dir-full-path exec-path)
                   (add-to-list 'exec-path dir-full-path t))))
             '("~/.cargo/bin"
               "~/.local/bin/"
               "~/dotfiles/local/bin/"
               "~/.cabal/bin/"))
       (setenv "PATH" (mapconcat #'identity exec-path ":")))

;;; PACKAGE ARCHIVES
;;  ─────────────────────────────────────────────────────────────────
(require 'package)

(eval-and-compile
  (let* ((no-ssl (or (and (memq system-type '(windows-nt ms-dos))
                          (not (gnutls-available-p)))
                     ;; For some reason Travis-CI isn't able to connect to
                     ;; Melpa using TLS.
                     (getenv "CI")))
         (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
    (add-to-list 'package-archives (cons "melpa" url) t)
    (add-to-list 'package-archives (cons "org"  "https://orgmode.org/elpa/") t))
  (setq package-enable-at-startup nil
	    package-user-dir (expand-file-name "packages/elpa/"
                                           user-emacs-directory))
  (package-initialize))

;;; USE-PACKAGE
;; ──────────────────────────────────────────────────────────────────
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "packages/lisp/use-package/"
					                        user-emacs-directory))
  (require 'use-package))

(setq use-package-verbose t)
(use-package diminish :ensure t)

(eval-and-compile
  (add-to-list 'use-package-keywords :doc t)
  (defun use-package-handler/:doc (name-symbol _keyword _docstring rest state)
    "An identity handler for :doc.
Currently, the value for this keyword is being ignore.  In the
future, I might want to add its value to name-symbol's
documentation string.

Argument NAME-SYMBOL is the first argument to `use-package' in a declaration.
Argument KEYWORD here is simply :doc.
Argument DOCSTRING is the value supplied for :doc keyword.
Argument REST is the list of rest of the  keywords.
Argument STATE is maintained by `use-package' as it processes symbols."
    (let ((body (use-package-process-keywords name-symbol rest state)))
      body)))

;;; Byte-compilation
(setq load-prefer-newer t)
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-save-mode))

;; LIBRARIES
;;  ─────────────────────────────────────────────────────────────────
(use-package ht        :defer t :ensure t)
(use-package fsm       :defer t :ensure t)
(use-package websocket :defer t :ensure t)
(use-package memoize   :defer t :ensure t)
(use-package s         :defer t :ensure t)
(use-package dash      :defer t :ensure t)
(use-package request   :defer t :ensure t)
(use-package htmlize   :defer t :ensure t)
(use-package mustache  :defer t :ensure t)
(use-package anaphora  :defer t :ensure t)

(use-package alert
  :defer t
  :ensure t
  :config
  (setq alert-default-style 'libnotify))

(use-package async
  :defer t
  :ensure t
  :config
  (use-package async-bytecomp
    :config
    (setq async-byte-compile-log-file
          (expand-file-name "tmp/async-bytecomp.log" user-emacs-directory))))

(use-package levenshtein
  :defines levenshtein-sort
  :defer t
  :load-path "lib/")

(use-package frecency
  :doc
  "Record scores for times based on frequency and recency.
   https://slack.engineering/a-faster-smarter-quick-switcher-77cbc193cb60"
  :defer t
  :ensure t)

;;
;;  ─────────────────────────────────────────────────────────────────
(use-package defs
  :doc "Var and function definitions that must be loaded before
  everything else."
  :demand t
  :load-path "lib/"
  :pin manual
  :preface
  (defun seq-map-indexed (function sequence)
    "Return the result of applying FUNCTION to each element of SEQUENCE.
      Unlike `seq-map', FUNCTION takes two arguments: the element of
      the sequence, and its index within the sequence."
    (let ((index 0))
      (seq-map (lambda (elt)
                 (prog1
                     (funcall function elt index)
                   (setq index (1+ index))))
               sequence)))

  (defmacro setd (&rest var-dirs)
    "Set vars to dirs in VAR-DIRS relative to `user-emacs-directory'.

     The reason for writing this macro is to avoid calling
     `expand-file-name' all the time and to avoid missing calling the
     same.  This is important for keeping the directory paths portable
     as Windows and Linux have different path styles."
    `(let ((default-directory ,user-emacs-directory))
       ,(cons 'setq (seq-map-indexed (lambda (x sym-index)
                                       (if (= (mod sym-index 2) 0)
                                           x
                                         (list 'expand-file-name x)))
                                     var-dirs))))

  :init
  (bind-keys* :prefix "C-."
              :prefix-map ctl-period-map)
  (bind-keys* :prefix "C-'"
              :prefix-map ctl-quote-map)

  :bind (("C-c m" . switch-to-minibuffer)
         ("C-c 0" . quick-switch-themes)
         ("<print>" . snap-it)
         :map ctl-quote-map
         ("c e" . vicarie/eval-print-last-sexp)
         ("c =" . vicarie/eval-replace-last-sexp)
         ("c c" . center-text-for-reading)
         ("c r" . rename-file-and-buffer)
         :map ctl-period-map
         ("u" . underline-text)
         ("d" . duplicate-current-line)
         ("s" . surround-symbol-with)))

(use-package no-littering
  :ensure t
  :config
  ;; Keep all misc state in $(user-emacs-direcotry)/tmp/
  (make-directory (expand-file-name "tmp/" user-emacs-directory) t)

  ;; Make tmp/ default. Paths are overridden if the files are important.
  (setd no-littering-etc-directory "tmp/"
        no-littering-var-directory "tmp/"))

(use-package "startup"
  :preface
  (defun emacs-init-end ()
    "Function to be called at the end of Emacs init process."
    (run-with-timer 1
                    nil
                    (lambda ()
                      ;; Emacs init took (actual-emacs-up-time, uptime-felt-like)
                      (let ((msg (format "Emacs init took (%s, %.2f seconds)."
                                         (emacs-init-time)
                                         (1- (time-to-seconds
                                              (time-subtract (current-time)
                                                             emacs-start-time))))))
                        (message msg)
                        (alert msg)))))
  :hook (after-init . emacs-init-end)
  :init
  ;; ----------------------------------------
  ;; Thanks to
  ;; https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; So that man pages are rendered properly irrespective of LC_* variable
  ;; values.
  (setq locale-coding-system 'utf-8)

  (setq buffer-file-coding-system 'utf-8)

  (setq large-file-warning-threshold
        (* 30 1024 1024))

  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  ;; ----------------------------------------

  (setd tutorial-directory "tmp/tutorial/")

  ;; Buffer contents auto-saved post initial file contents
  (setq auto-save-list-file-prefix
        (expand-file-name "tmp/autosaves/" user-emacs-directory))

  ;; Backups of file before current changes
  (setq backup-directory-alist
        `((".*" . ,(expand-file-name "tmp/backups/"
                                     user-emacs-directory)))
        backup-by-copying t
        version-control t
        delete-old-versions t
        kept-new-versions 5
        kept-old-versions 5)

  (setq initial-major-mode 'fundamental-mode))

(use-package custom
  :doc "Custom configuration and personal information."
  :init
  (defvar secrets-file (expand-file-name "secrets.el"
                                         emacs-assets-directory))
  (setd custom-file "custom.el")

  ;; My custom file usually doesn't contain settings that would be visible to
  ;; me. I have some variables setup though that help me reduce disk reads
  ;; while setting up fonts.
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; I can load the secrets file asynchronously because I use for things
  ;; that I don't need immediately after starting Emacs.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (file-exists-p secrets-file)
                (load secrets-file))))
  ;; Misc
  (setq-default tab-width 2)
  (setq-default fill-column 79)

  ;; Enable some disabled commands
  (mapc (lambda (c) (put c 'disabled nil))
        '(narrow-to-region
          upcase-region
          downcase-region
          capitalize-region
          erase-buffer
          set-goal-column
          list-timers))

  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package saved-macros
  :doc
  "It's important to demand this package to be loaded so as to
  make all saved macros available."
  :load-path "lib/"
  :demand t
  :bind ("C-x C-k m" . saved-macros-save-macro))

(use-package wtf
  :load-path "packages/lisp/"
  :commands wtf-is)

(use-package emlib
  :load-path "packages/rest/emlib/"
  :defer t)

(use-package livemacs
  :defer t
  :commands livemacs-begin
  :load-path "packages/rest/livemacs/")

;;; Utilities
;; ──────────────────────────────────────────────────────────────────
(use-package restclient
  :doc "Restclient comes in handy a lot of times."
  :defer t
  :commands restclient
  :ensure t)

(use-package prodigy
  :defer t
  :commands prodigy
  :ensure t
  :config
  (load-file (expand-file-name "etc/prodigy-service-defs.el"
                               user-emacs-directory)))

(use-package pcre2el
  :doc "I intend to read the code carefully someday."
  :ensure t
  :bind ("C-' c /" . pcre->elisp)
  :preface
  (defun pcre->elisp (beg end)
    "Replace PCRE regex in region (BEG END) with its elisp equivalent."
    (interactive "r")
    (let ((pcre-regex (buffer-substring-no-properties beg end)))
      (delete-region beg end)
      (insert (pcre-to-elisp pcre-regex)))))

;;; Thanks to https://github.com/Wilfred
(use-package suggest :defer t :commands suggest :ensure t)
(use-package emr
  :doc "Keeping it here so that I can look at it some time in future"
  :disabled t
  :ensure t)

(use-package elisp-refs :ensure t :defer t)
(use-package helpful
  :ensure t
  :load-path "packages/lisp/helpful/"
  :bind (("C-h v" . helpful-variable)
         ("C-h f" . helpful-function)
         ("C-h k" . helpful-key)
         ("C-h o" . helpful-symbol)
         :map helpful-mode-map
         ("<tab>" . forward-button)
         ("<backtab>" . backward-button)))

(use-package page-break-lines :disabled t :ensure t)

(use-package appearance
  :doc "`use-package' doesn't throw an error for non-existent packages"
  :after custom
  :load-path "themes/"
  :defines quick-switch-themes
  :preface
  (defun font-availablep (font)
    "Return true if FONT is available on system.
	 This is written to avoid calling `find-font' repeatedly."
    (let ((favailablep (intern (concat font "-availablep"))))
      (if (boundp favailablep)
          (symbol-value favailablep)
        (customize-save-variable favailablep
                                 (not (null (find-font (font-spec :name font))))))))

  :init
  (add-to-list 'custom-theme-load-path
               (expand-file-name "themes/"
                                 user-emacs-directory))
  (load-theme 'tangotango)

  ;; Setup my favorite fonts [if available]
  (if (font-availablep "Symbola")
      (set-fontset-font "fontset-default" nil
                        (font-spec :name "Symbola" :size 15)
                        nil 'append)
    (message "You do not have Symbola font installed."))

  ;; Font for reading news
  (cond
   ((font-availablep "Carlito")
    ;; It would have been great if I could set the background to white
    ;; while reading anything other than code. Emacs doesn't support
    ;; buffer-local themes and doing this would require nasty tricks
    ;; with hooks.
    (set-face-attribute 'variable-pitch nil
                        :family "Carlito"
                        :height 130
                        :weight 'normal
                        :width 'ultraexpanded)))

  (setq inhibit-splash-screen t)

  ;; Make the window simpler.
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; 4 pixels wide fringes
  (fringe-mode 4)

  ;; Maximize emacs on startup
  (when (window-system)
    (add-to-list 'default-frame-alist
                 '(fullscreen . maximized))))

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'dark
        sml/no-confirm-load-theme t)
  (sml/setup))

;; KEY BINDINGS
;; ──────────────────────────────────────────────────────────────────
(bind-keys* ("C-<return>" . other-window*))

(bind-keys :map ctl-quote-map
           ("C-'" . imenu))

(bind-keys :map ctl-period-map
           ("C-o" . goto-address-at-point)
           ("C-c" . out-or-onto-calc)
           ("C-a" . align-regexp))

;; ──────────────────────────────────────────────────────────────────
(use-package minibuffer
  :config
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode +1))

(use-package time
  :preface
  (defun display-current-time ()
    "Show current time in ECHO area."
    (interactive)
    (message "\n\n%76s\n\n"
             (propertize (format-time-string "%H:%M %b %d")
                         'face '(
                                 :foreground "DeepSkyBlue"
                                 :background "Black"
                                 :height 2.0
                                 ))))
  :bind ("C-. C-t" . display-current-time)
  :init
  (do-when-idle #'display-time-mode (lambda () (display-time-mode -1)) 10)

  :config
  (setq display-time-format " %a %h_%d %H:%M "))

(use-package battery
  :defer 5
  :config
  (setq battery-mode-line-format " %p%% %L")
  (display-battery-mode +1))

(use-package calendar
  :defer t
  :commands calendar
  :bind (:map ctl-quote-map
              ("C-t" . calendar))
  :init
  (setq diary-file (expand-file-name "diary" emacs-assets-directory)
        ;; Weeks start on Monday.
        calendar-week-start-day 1))

(use-package timeclock
  :preface
  (defvar timeclock-idle-minutes 10
    "Minutes of inactivity that is allowed.")

  (defvar timeclock-clock-out-timer nil
    "Idle timer that clocks out.")
  (defvar timeclock-clock-in-timer nil
    "Timer that throttles clock ins.")

  (defun timeclock-clock-in* ()
    "Clock in to or change project to the current project."
    (ignore-errors
      ;; Clock in only if the current change is for the currently active
      ;; window's buffer.
      (when (and (eq (get-buffer-window (current-buffer))
                     (selected-window))
                 ;; Ignore changes to `timeclock-file'.
                 (not (equal buffer-file-name timeclock-file)))
        (let ((project (or (projectile-project-name)
                           (buffer-file-name)
                           (buffer-name)))
              (inhibit-message t))
          (if (and (timeclock-currently-in-p)
                   (not (equal project (caddr timeclock-last-event))))
              (progn
                (timeclock-out)
                (when timeclock-clock-in-timer
                  (cancel-timer timeclock-clock-in-timer))
                (setq timeclock-clock-in-timer
                      (run-with-timer 10
                                      nil
                                      (lambda ()
                                        (when (not (timeclock-currently-in-p))
                                          (timeclock-in nil project))))))
            (timeclock-in nil project)))
        ;; Clock out when idle for `timeclock-idle-minutes' minutes.
        (when timeclock-clock-out-timer
          (cancel-timer timeclock-clock-out-timer))
        (setq timeclock-clock-out-timer
              (run-with-idle-timer (* timeclock-idle-minutes 60)
                                   nil
                                   (lambda ()
                                     (when (timeclock-currently-in-p)
                                       (timeclock-out))))))))

  :config
  (setq timeclock-workday (or (getenv "TIMECLOCK_WORKDAY")
                              (* 4 60 60))
        timeclock-use-display-time nil
        timeclock-file (expand-file-name "timelog"
                                         emacs-assets-directory))

  ;; Start clocking in only after `timeclock' has been loaded
  (hook-into-modes #'timeclock-clock-in*
                   'after-save
                   'first-change
                   'auto-save)
  (timeclock-mode-line-display +1))

(use-package appt
  :defer t
  :preface
  (defun show-appt-notifications (disp-fn &rest args)
    "Show notifications for an appointment using default alert style."
    (alert (format "In %s minutes: %s" (first args) (third args)) :title "Reminder")
    (apply disp-fn args))

  :init
  (eval-after-load "org-mode"
    '(appt-activate +1))
  (add-hook 'org-agenda-finalize-hook
            (lambda ()
              (quietly (org-agenda-to-appt))))

  :config
  (setq appt-audible t
        appt-display-duration 15)

  (advice-add 'appt-disp-window :around #'show-appt-notifications))

(use-package browse-url
  :defer t
  :doc "Make chromium the default browser if it is installed."
  :init
  (cond
   ((executable-find "firefox")
    (setq browse-url-browser-function 'browse-url-firefox))
   ((executable-find "chromium")
    (setq browse-url-browser-function 'browse-url-chromium))))

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-delay 0)
  (show-paren-mode 1))

(use-package rainbow-mode        :ensure t :defer t)
(use-package rainbow-identifiers :ensure t :defer t)

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode +1)
  (keyfreq-autosave-mode +1))

(use-package fancy-narrow
  :doc
  "Narrow to region while dimming the rest of the screen.  I
  have disabled this minor-mode as this is a global minor mode
  and it doesn't work with `org-mode'."
  :disabled t
  :diminish fancy-narrow-mode
  :init (fancy-narrow-mode 1))

(use-package select
  :init
  (setq select-enable-clipboard t))

(use-package linum
  :defer t
  :commands linum-mode
  :init
  (setq linum-format "%2d│"))

(use-package linum-relative
  :ensure t
  :bind ("M-g l" . jump-to-relative-line)
  :preface
  (defun jump-to-relative-line (arg)
    "Jump to a relative line number."
    (interactive "P")
    (linum-relative-mode +1)
    (let ((relative-line-number (read-number "Line: ")))
      (forward-line (if arg (- relative-line-number) relative-line-number)))
    (linum-relative-mode -1))

  :init
  (setq linum-relative-current-symbol ""))

(use-package uniquify
  :doc "Unique buffer names"
  :diminish t
  :init
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator " • "))

;;; TEXT-EDITING, FOLDING and NAVIGATION
;; ─────────────────────────────────────────────────────────────────
(use-package elec-pair
  :config
  ;; This minor-mode function enables the minor mode only when the optional
  ;; argument is either omitted or nil.
  (electric-pair-mode))

(use-package wgrep :defer t :ensure t)

(use-package region-bindings-mode
  :ensure t
  :config
  (region-bindings-mode-enable))

(use-package hydra :ensure t :commands defhydra :demand t)

(use-package simple
  :doc "The great simple.el"
  :demand t
  :bind (("M-q" . fill-or-unfill)
         ("M-[" . backward-kill-word))
  :hook ((prog-mode hledger-mode)  . comment-auto-fill)
  :preface
  (defun auto-fill-current-line ()
    "Break current line with `do-auto-fill'."
    (interactive)
    (do-auto-fill))

  (defun unfill-paragraph ()
    "Unfill paragraph removing hard newlines.
     From Emacs Wiki."
    (interactive)
    (let* ((inhibit-read-only t)
           (fill-column (point-max)))
      (fill-paragraph nil)))

  (def-lineup fill-or-unfill
    "Sequence filling/unfilling with the same biding."
    #'auto-fill-current-line
    #'fill-paragraph
    #'unfill-paragraph)

  (defun comment-auto-fill ()
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode +1))

  (defun just-one-below ()
    "Join following line with this line."
    (interactive)
    (delete-indentation +1))

  :init
  ;; Multiple-cursors changes transient-mark-mode to (only only .. t),
  ;; if shift-select-mode is enabled.
  (setq shift-select-mode nil)

  ;; Make text copied/cut from outside Emacs part of Emacs kill-ring on first
  ;; kill inside Emacs.
  (setq save-interprogram-paste-before-kill t
        kill-do-not-save-duplicates t)

  (setq async-shell-command-buffer 'new-buffer
        set-mark-command-repeat-pop t
        column-number-mode t)

  (defhydra hydra-space (global-map "C-.")
    "edit-space> "
    ("C-u   " delete-indentation  "just-one-^")
    ("C-v   " just-one-below      "just-one-v")
    ("M-u   " kill-whitespace     "kill-all")
    ("C-SPC " cycle-spacing       "cycle")
    ("q     " nil                 "quit"))

  :diminish auto-fill-function)

(use-package crux
  :ensure t
  :bind (("C-k"           . crux-smart-kill-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("C-M-<return>"  . crux-switch-to-previous-buffer))
  :hook (after-init . crux-reopen-as-root-mode))

(use-package misc
  :doc "Where simple ends, maybe misc.el begins"
  :bind (("M-z" . zap-up-to-char)
         ("M-Z" . copy-from-above-command)))

(use-package picture
  :bind (:map ctl-quote-map
              ("C-p" . picture-mode)))

(use-package savehist
  :demand t
  :config
  (setq savehist-file (expand-file-name "tmp/savehist.el"
                                        user-emacs-directory)
        savehist-additional-variables '(kill-ring limit-usage))
  (savehist-mode +1))

(use-package footnote
  :doc
  "For footnotes. On a side note [maybe footnote], I notices
  that with a keymap bound to a key sequence, it doesn't work the
  first time you press the key sequence. @TODO"
  :config
  :bind (:map ctl-quote-map
              ("f a" . Footnote-add-footnote)
              ("f d" . Footnote-delete-footnote)
              ("f g" . Footnote-goto-footnote)))

(use-package multiple-cursors
  :doc "A minor mode for editing with multiple cursors."
  :ensure t
  :preface

  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c >" . mc/mark-all-like-this)

   :map region-bindings-mode-map
   ("C-x $" . mc-hide-unmatched-lines-mode)
   ("C-x !" . mc/mark-all-like-this)
   ("M-k" . mc/mmlte--up)
   ("M-j" . mc/mmlte--down)
   ("M-h" . mc/mmlte--left)
   ("M-l" . mc/mmlte--right)

   :map ctl-quote-map
   ("m" . mc/mark-more-like-this-extended)
   ("M" . mc/edit-lines))

  :init
  (setq mc/list-file (expand-file-name "tmp/mc-lists.el"
                                       user-emacs-directory))
  (setq mc/always-run-for-all t))


(use-package expand-region
  :doc "A minor mode for expanding region by semantic units."
  :ensure t
  :bind ("C-. @" . er/expand-region)
  :init
  (delete-selection-mode 1))

(use-package change-inner
  :ensure t
  :preface
  (defvar change-hash
    (let ((h (make-hash-table)))
      (mapc (lambda (pair) (puthash (car pair) (cdr pair) h))
            '(("(" . ")") ("[" . "]") ("\"" . "\"") ("'" . "'")))
      h))
  (defun change-wrapper ()
    "Change wrapper char from A to B in text around point."
    (interactive)
    (require 'change-inner)
    (let* ((here (point))
           (_ (change-outer* nil nil))
           (c (read-char "Substitue: "))
           (s (pop kill-ring))
           (slen (length s)))
      (aset s 0 c)
      (aset s (1- slen) (gethash c change-hash))
      (insert s)
      (goto-char here)))

  :bind* (("M-i" . change-inner)))

(use-package hideshow
  :doc
  "A collection of packages to fold code better. This is far from
  begin good enough. I intend to improve this in the future. I
  don't really need code folding more than what is already
  provided with `selective-display' anyways."
  :defer t
  :diminish hs-minor-mode
  :commands hs-toggle-hiding
  :hook (emacs-lisp-mode . hs-minor-mode)
  :config
  (use-package origami
    :disabled t
    :ensure t
    :diminish origami-mode)

  (use-package vimish-fold
    :ensure t
    :diminish vimish-fold-mode
    :init
    (setq vimish-fold-dir (expand-file-name "tmp/vimish-fold"
                                            user-emacs-directory))))

(use-package boxquote
  :doc "For nice boxes for quoting text."
  :bind (:map ctl-quote-map
              ("f q" . boxquote-region))
  :ensure t)

(use-package highlight-symbol
  :ensure t
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev))
  :preface
  (defun highlight--current-symbol (&rest _)
    "Pulse highlight symbol at point."
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (pulse-momentary-highlight-region (car bounds) (cdr bounds))))

  :init
  (advice-add 'highlight-symbol-jump :after #'highlight--current-symbol))

(use-package markdown-mode :defer t :ensure t)
(use-package csv-mode      :defer t :ensure t)
(use-package auctex        :defer t :ensure t)

(use-package writeroom-mode
  :disabled t
  :doc
  "Distraction free setup for reading.  This would replace my
  naive `center-text-for-reading'."
  :ensure t
  :hook nov-mode)

(use-package olivetti
  :doc "Similar to `writeroom-mode' but a little less hard-core."
  :ensure t
  :hook (nov-mode . olivetti-mode))

(use-package nov
  :ensure t
  :defer t
  :mode ("\\.epub" . nov-mode)
  :config
  (setd nov-save-place-file "tmp/nov-places"))

(use-package artbollocks-mode
  :ensure t
  :defer t
  :hook ((org-mode))
  :diminish artbollocks-mode
  :init
  (use-package writegood-mode
    :ensure t
    :defer t
    :diminish writegood-mode
    :hook ((org-mode)))

  :config
  (unbind-key (kbd "C-c [") artbollocks-mode-keymap)
  (unbind-key (kbd "C-c ]") artbollocks-mode-keymap)
  (unbind-key (kbd "C-c \\") artbollocks-mode-keymap)
  (unbind-key (kbd "C-c /") artbollocks-mode-keymap)
  (unbind-key (kbd "C-c =") artbollocks-mode-keymap))

;;; EMACS Itself
;; ─────────────────────────────────────────────────────────────────
(use-package discover
  :doc "Discovering more about Emacs"
  :ensure t
  :config
  (global-discover-mode +1))

(use-package which-key
  :doc "Get quick emacs key binding suggestions"
  :ensure t
  :demand 2
  :diminish which-key-mode
  :config
  (setq which-key-max-description-length nil)
  (which-key-mode +1))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-project-root projectile-project-name)
  :bind (("C-c p p" . projectile-switch-project)
         ("C-c p f" . projectile-find-file))
  :init
  (setd projectile-known-projects-file "tmp/projectile-projects.eld"
        projectile-cache-file "tmp/projectile.cache")
  (setq projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s ")
  (setq projectile-completion-system 'ivy)

  :config
  (mapc (lambda (d) (push d projectile-globally-ignored-directories))
        '("vendor/bundle"))
  (mapc (lambda (f) (push f projectile-globally-ignored-files))
        '("GTAGS" "GRTAGS" "GPATH"))

  (projectile-mode +1))

;;; SESSIONS and BOOKMARKS
;; ──────────────────────────────────────────────────────────────────
(use-package bookmark
  :ensure t
  :defer 5
  :bind (("C-z" . save-or-reset-window-config))
  :preface
  (defvar save-or-rest-window-config nil
    "Variable storing the last window configuration.
     It would be more useful to have a ring of window configurations.")

  (defvar save-or-reset-window-config-saved-hook nil
    "Function to called after saving window configuration.")

  (defun save-or-reset-window-config (arg)
    "Save window configuration if not saved already, else restore it.
     Uses register: + for saving window configuration.
     With a prefix ARG, unconditionally reset and save new config."
    (interactive "P")
    (let ((current-window-config (current-window-configuration)))
      (cond
       ((or arg (not save-or-rest-window-config))
        (setq save-or-rest-window-config
              current-window-config)
        (message "Save window configuration")
        (run-hooks 'save-or-reset-window-config-saved-hook))

       (t
        (set-window-configuration save-or-rest-window-config)
        (setq save-or-rest-window-config nil)
        (message "Restored window configuration")))))

  :init
  (add-hook 'save-or-reset-window-config-saved-hook
            (lambda ()
              (qs-refresh-scratch-buffer t)
              (delete-other-windows)))
  :config
  (setq bookmark-save-flag 1))

(use-package saveplace
  :init
  (save-place-mode +1)
  :config
  (setq save-place-file
        (locate-user-emacs-file "tmp/saved-places")))

;;; WORKSPACES
;; ――――――――――――――――――――――――――――――――――――――――
(use-package eyebrowse
  :doc
  "Using eyebrowse-mode to have workspace-likes in Emacs
   On Non-Linux laptops, I should bind \"C-c C-w\" to \"Super\" so that
   it feels liks XMonad inside Emacs."
  :ensure t
  :preface
  (defun open-a-shell ()
    (interactive)
    (shell (generate-new-buffer "*shell*")))

  :bind (:map eyebrowse-mode-map
              ("C-c w RET" . open-a-shell)
              ("C-c w t"   . toggle-window-split)
              ("C-c w s"   . split-and-shell)
              ("C-c w S"   . shell)
              ("C-c w r"   . window-configuration-to-register))
  :config
  (setq eyebrowse-mode-line-style t
        eyebrowse-wrap-around t
        eyebrowse-mode-line-right-delimiter "] ")

  :init
  ;; Need this in :init because it is used in defining the mode-map
  (setq eyebrowse-keymap-prefix "w")

  (eyebrowse-mode +1)

  ;; Create three window configurations on Emacs startup.
  (add-hook 'emacs-startup-hook (lambda ()
                                  (dotimes (_ 2)
                                    (eyebrowse-create-window-config)
                                    ;; Just to be Number 1!
                                    (eyebrowse-last-window-config))))

  (defhydra hydra-eyebrowse (eyebrowse-mode-map "C-c w"
                                                :timeout 1.0)
    "eyebrowse"
    ("<" eyebrowse-prev-window-config "prev")
    (">" eyebrowse-next-window-config "next")
    ("TAB" eyebrowse-last-window-config "last")
    ("n" eyebrowse-create-window-config "create" :exit t)
    ("q" nil "quit"))

  (gen-prefixed rotate-windows)
  (defhydra hydra-rotate-windows (eyebrowse-mode-map "C-c w")
    "rotate-widows> "
    ("k" rotate-windows/prefixed "⤿")
    ("j" rotate-windows "↷")))

(use-package winner
  :bind (("C-c w /"   . winner-undo)
         ("C-c w \\"  . winner-redo))
  :config
  (winner-mode +1))


;;; NAVIGATION
;; ――――――――――――――――――――――――――――――――――――――――
(use-package isearch
  :doc
  "Search for the string in the active region, if there is
   any. For some reason advising `isearch-mode' doesn't work.
   With this C-o in `isearch-mode' is impotent at the moment."

  :preface
  (defun isearch-exit-other-end ()
    "Exit isearch, at the opposite end of the string."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end))

  (defun isearch-yank-selection ()
    "Put selection from buffer into search string."
    (interactive)
    (when (region-active-p)
      (deactivate-mark))
    (isearch-yank-internal 'mark))

  (defun isearch-backward-other-window ()
    (interactive)
    (split-window-vertically)
    (call-interactively 'isearch-backward))

  (defun isearch-forward-other-window ()
    (interactive)
    (split-window-vertically)
    (call-interactively 'isearch-forward))

  :bind (("C-M-r" . isearch-backward-other-window)
         ("C-M-s" . isearch-forward-other-window))

  :config
  (advice-add 'isearch-mode
              :after (lambda (&rest _rest)
                       (when (region-active-p)
                         (isearch-yank-selection))))
  (defhydra hydra-isearch (:color pink)
    "isearch> "
    ("C-<return>" isearch-exit-other-end    "other end" :exit t)
    ("C-="        isearch-toggle-case-fold  "toggle case")
    ("C-t"        isearch-toggle-regexp     "toggle regexp")
    ("C-^"        isearch-edit-string       "edit string")
    ("C-i"        isearch-complete          "complete"))
  (bind-key "C-o" #'hydra-isearch/body isearch-mode-map))

(use-package avy
  :ensure t
  :bind (("M-g w" . avy-goto-word-1)
         :map ctl-period-map
         ("C-." . avy-goto-char-timer)))

(use-package "window"
  :init
  (setq display-buffer-alist
        '((".*\\*Async Shell Command\\*.**" . (display-buffer-no-window . nil))))

  ;; I think I will mostly want the pointer to go to the end with M-r
  ;; And then I would do a M-l to re-center it. Since both of them use
  ;; `recenter-positions'. I am using advices.
  (setq recenter-positions '(bottom top middle))
  (advice-add 'recenter-top-bottom
              :around (lambda (f &rest args)
                        (let ((recenter-positions '(middle top bottom)))
                          (apply f args))))

  :config
  (defhydra hydra-enlarge-window (global-map "C-x"
                                             :timeout 3.0)
    "window>"
    ("^" enlarge-window "enlarge-vertically")
    ("}" enlarge-window-horizontally "enlarge-horizontally")
    ("{" shrink-window-horizontally "shrink-horizontally")))


(use-package ace-window
  :doc
  "This should come after `window's use-package
   declaration. Otherwise, `window' would overwrite the binding for \\[ace-window]]."
  :ensure t
  :doc "Use `ace-window' instead of `other-window'."
  :bind ("C-x o" . ace-window))

;; ――――――――――――――――――――――――――――――――――――――――
(use-package recentf
  :defer t
  :config
  (setq recentf-auto-cleanup 'never
        recentf-keep '(file-remote-p file-readable-p)
        recentf-max-saved-items 1000
        recentf-save-file (expand-file-name "tmp/recentf"
                                            user-emacs-directory))
  (recentf-mode +1))


;; ――――――――――――――――――――――――――――――――――――――――
(use-package flyspell
  :diminish flyspell-mode
  :preface
  (defun enable-flyspell ()
    "Unbind C-. from `flyspell-mode-map'."
    (flyspell-mode 1)
    (unbind-key "C-." flyspell-mode-map))
  :hook ((markdown-mode
          latex-mode
          org-mode
          message-mode
          hledger-mode) . enable-flyspell))

(use-package ispell
  :bind (:map ctl-period-map
              ("f" . ispell-word))
  :config
  (setq ispell-personal-dictionary personal-dictionary-file))

;; ──────────────────────────────────────────────────────────────────
(use-package pcache
  :defer t
  :ensure t
  :init
  (setd pcache-directory "tmp/pcache/"))

(use-package popup :defer t :ensure t)

(use-package quoted-scratch
  :doc "A beautiful quote for your scracth buffer."
  :defer t
  :load-path "packages/rest/quoted-scratch/"
  :commands qs-refresh-quote-when-idle
  :bind (:map ctl-quote-map
              ("c q" . qs-add-new-quote))
  :init
  (setq initial-scratch-message "")

  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-with-timer 1 nil 'qs-refresh-scratch-buffer)
              (qs-refresh-quote-when-idle))))

(use-package wordly
  :doc "Lookup word definition and synonyms."
  :load-path "packages/rest/wordly/"
  :defer t
  :pin manual
  :bind (:map ctl-quote-map
              ("c d" . wordly-define-word-at-point)
              ("c s" . wordly-show-synonyms-for-word-at-point)))

(use-package define-word
  :ensure t
  :bind (:map ctl-quote-map
              ("c D" . define-word)))


(use-package mylife-mode
  :doc "A major for miscellaneous things."
  :disabled t
  :after (pcache popup)
  :load-path "packages/rest/mylife-mode/")

(use-package wolfram
  :doc "The app id should already be defined in `my-wolfram-alpha-app-id'."
  :ensure t
  :bind (:map ctl-quote-map
              ("c a" . wolfram-alpha))
  :defer t
  :init
  (and (boundp 'my-wolfram-alpha-app-id)
       (setq wolfram-alpha-app-id my-wolfram-alpha-app-id)))

;;; Completion at Point
;; ――――――――――――――――――――――――――――――――――――――――
(use-package company
  :doc "I don't like auto-completion at all. However, I have
  realized that sometimes, it can come in really handy,
  especially while programming in a dynamic language that
  encourages very long identifier names."
  :ensure t
  :demand t
  :diminish company-mode
  :bind (
         :map global-map
         ("C-. TAB" . company-complete)
         :map company-active-map
         ("<tab>" . company-complete-common-or-cycle)
         )
  :config
  (bind-keys :map company-active-map
             ("C-n" . company-select-next-or-abort)
             ("C-p" . company-select-previous-or-abort))

  (global-company-mode +1))

(use-package company-statistics
  :doc "Sort `company-mode' completions by frequency."
  :ensure t
  :hook (after-init . company-statistics-mode)
  :init
  (setd company-statistics-file "tmp/company-statistics-cache.el"))

;; ──────────────────────────────────────────────────────────────────
(use-package beginend
  :ensure t
  :defer 5
  :diminish beginend-global-mode
  :config
  (beginend-global-mode +1)
  (mapc (lambda (hook-mode-cons)
          (diminish (cdr hook-mode-cons)))
        beginend-modes))

(use-package dired-x
  :after dired
  :preface
  (defun dired-to-first-entry ()
    "Move point to the first interesting entry in a dired buffer."
    (interactive)
    (goto-char (point-min))
    (dired-next-dirline 2)
    (forward-line))

  (defun dired-to-last-entry ()
    "Move point to the last interesting entry in a dired buffer."
    (interactive)
    (goto-char (point-max))
    (dired-previous-line 1))

  (defun dired-up-repeatedly ()
    "Go up repeatedly in a Dired buffer."
    (interactive)
    (repeat-command 'dired-up-directory #'message))

  (defun dired-go-home ()
    "Switch current directory to ~/."
    (interactive)
    (dired-jump nil (expand-file-name "~/")))

  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("M-<"   . dired-to-first-entry)
         ("M->"   . dired-to-last-entry)
         ("z"     . kill-buffer-delete-window)
         ("j"     . ido-find-file)
         ("~"     . dired-go-home)
         ("C-c u" . dired-up-repeatedly))
  :hook ((dired-after-readin . dired-hide-details-mode)
         (dired-mode         . hl-line-mode))
  :init
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-hide-details-hide-information-lines nil)

  :config
  (setq dired-auto-revert-buffer t)

  (use-package dired-narrow
    :ensure t
    :bind (:map dired-mode-map
                ("/" . dired-narrow)))

  (use-package dired-subtree
    :ensure t
    :bind (:map dired-mode-map
                ("I" . dired-subtree-toggle))))

(use-package all-the-icons-dired
  :doc
  "Fonts can be installed by calling `all-the-icons-install-fonts'."
  :diminish 'all-the-icons-dired-mode
  :disabled t
  :ensure t
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))


;;; SNIPPETS and ABBREVS
;; ――――――――――――――――――――――――――――――――――――――――
(use-package yasnippet
  :defer 5
  :ensure t
  :diminish yas-minor-mode
  :bind (("C-c & C-h" . yas-describe-tables)
         :map yas-keymap
         ("<tab>" . my-yas-next-field-or-maybe-expand))
  :preface
  (defun my-yas-next-field-or-maybe-expand ()
    "When in company, stay in company."
    (interactive)
    (if (company-tooltip-visible-p)
        (company-complete-common-or-cycle)
      (yas-next-field-or-maybe-expand)))

  :config
  (setq yas-key-syntaxes '(yas-try-key-from-whitespace))

  ;; Keep personal snippets away from existing default snippets.
  (push (expand-file-name "snippets/" user-emacs-directory)
        yas-snippet-dirs)
  (yas-global-mode +1))

(use-package auto-yasnippet
  :after yasnippet
  :doc
  "Anonymous yas snippets on the go."
  :ensure t
  :bind (:map ctl-quote-map
              ("a c" . aya-create)
              ("a e" . aya-expand)
              ("a p" . aya-persist-snippet)))

(use-package abbrev
  :diminish abbrev-mode
  :init
  (setq-default abbrev-mode t
                save-abbrevs nil
                abbrev-file-name (expand-file-name "lib/abbrev_defs"
                                                   user-emacs-directory))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;;; Personal Finance
;; ――――――――――――――――――――――――――――――――――――
(use-package hledger-mode
  :pin manual
  :after htmlize
  :load-path "packages/rest/hledger-mode/"
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :commands hledger-enable-reporting
  :preface
  (defun hledger/next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))

  (defface hledger-warning-face
    '((((background dark))
       :background "Red" :foreground "White")
      (((background light))
       :background "Red" :foreground "White")
      (t :inverse-video t))
    "Face for warning"
    :group 'hledger)

  (defun hledger/prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry))

  :bind (("C-c j" . hledger-run-command)
         :map hledger-mode-map
         ("C-c e" . hledger-jentry)
         ("M-p" . hledger/prev-entry)
         ("M-n" . hledger/next-entry))

  :hook ((hledger-view-mode . hl-line-mode)
         (hledger-view-mode . center-text-for-reading))

  :config
  (setq hledger-jfile
        (expand-file-name "~/miscellany/personal/finance/accounting.journal"))

  ;; Expanded account balances in the overall monthly report are
  ;; mostly noise for me and do not convey any meaningful information.
  (setq hledger-show-expanded-report nil)

  (add-hook 'hledger-view-mode-hook
            (lambda ()
              (run-with-timer 1
                              nil
                              (lambda ()
                                (when (equal hledger-last-run-command
                                             "balancesheet")
                                  ;; highlight frequently changing accounts
                                  (highlight-regexp "^.*\\(savings\\|cash\\).*$")
                                  (highlight-regexp "^.*credit-card.*$"
                                                    'hledger-warning-face))))))

  (add-hook 'hledger-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'hledger-company))))

(use-package hledger-input
  :pin manual
  :load-path "packages/rest/hledger-mode/"
  :bind (("C-c e" . hledger-capture)
         :map hledger-input-mode-map
         ("C-c C-b" . popup-balance-at-point))
  :preface
  (defun popup-balance-at-point ()
    "Show balance for account at point in a popup."
    (interactive)
    (if-let ((account (thing-at-point 'hledger-account)))
        (message (hledger-shell-command-to-string (format " balance -N %s "
                                                          account)))
      (message "No account at point")))
  :hook ((hledger-input-post-commit . hledger-show-new-balances)
         (hledger-input-mode        . auto-fill-mode))
  :config
  (setq hledger-input-buffer-height 20)
  (add-hook 'hledger-input-mode-hook
            (lambda ()
              (make-local-variable 'company-idle-delay)
              (setq-local company-idle-delay 0.1))))

;;; Programming in general
;; ――――――――――――――――――――――――――――――――――――
(use-package semantic
  :defer 10
  :hook ((c-mode c++-mode java-mode) . semantic-mode))

(use-package semanticdb
  :defer 10
  :init
  (setd semanticdb-default-save-directory "tmp/semanticdb/"))

(use-package realgud
  :doc "`gud' with bells and whistles."
  :defer t
  :ensure t)

(use-package "indent"
  :init
  (setq-default indent-tabs-mode nil
                tab-always-indent 'complete))

(use-package xref
  :doc "Find definitions like the coolest kid."
  :ensure t
  :bind (("M-." . xref-find-definitions)
         ("M-," . xref-pop-marker-stack)))

(use-package ggtags
  :ensure t
  :bind (:map ctl-period-map
              ("M-." . generate-gtags))
  :hook ((c-mode c++-mode erlang-mode java-mode) . ggtags-mode)
  :preface
  (defun generate-gtags ()
    "Generate gtags for current project."
    (interactive)
    (when-let ((default-directory (projectile-project-root)))
      (shell-command "gtags")
      (message "Generated gtags in %s" default-directory)))
  :config
  (unbind-key "M-<" ggtags-navigation-map))

(use-package smart-jump
  :ensure t
  :config
  ;; Adds modes that do not provide jump to definition functionality
  ;; themselves.
  (setq smart-jump-default-mode-list '(web-mode))
  (smart-jump-setup-default-registers))

(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :hook ((prog-mode  . flyspell-prog-mode)
         (prog-mode  . subword-mode)
         (after-init . global-flycheck-mode))
  :init
  (use-package subword
    :diminish subword-mode)

  (setq flycheck-global-modes '(clojure-mode
                                elixir-mode
                                emacs-lisp-mode
                                python-mode
                                haskell-mode
                                ruby-mode
                                go-mode
                                c-mode))
  (add-hook 'flyspell-prog-mode-hook
            (lambda ()
              ;; C-. is my personal prefix. I just don't like it being
              ;; stolen.
              (unbind-key "C-." flyspell-mode-map))))

(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish aggressive-indent-mode
  :hook ((emacs-lisp-mode clojure-mode) . aggressive-indent-mode))

(use-package password-cache
  :init
  (setq password-cache t
        password-cache-expiry 3600))

(use-package comint
  :defer t
  :config
  (setq comint-scroll-show-maximum-output nil))

(use-package eshell
  :defer t
  :preface
  (defmacro with-face (str &rest properties)
    "Propertize string STR with PROPERTIES."
    `(propertize ,str 'face (list ,@properties)))

  :init
  ;; Set up visual comamnds and sub-commands
  (add-hook 'eshell-mode-hook
            (lambda ()
              ;; For eshell/sudo as it as password caching
              (require 'em-tramp)
              (require 'em-term)
              (add-to-list 'eshell-visual-commands "vim")
              (add-to-list 'eshell-visual-subcommands
                           '("git" "commit" "log" "diff" "grep"))))
  :config
  (setq eshell-aliases-file (expand-file-name "etc/eshell-aliases"
                                              user-emacs-directory)
        eshell-hist-ignoredups t
        eshell-cmpl-cycle-completions nil
        eshell-highlight-prompt nil)

  ;; Do not scroll automatically.
  (setq eshell-scroll-show-maximum-output nil)

  (setq eshell-prefer-lisp-functions t
        eshell-prefer-lisp-variables t)

  (setq eshell-prompt-regexp "^[^#λ\n]* [#λ] "
        eshell-prompt-function
        (lambda ()
          (concat
           (with-face "(" :foreground "white")
           (with-face (file-name-nondirectory (eshell/pwd)) :foreground "tomato")
           (with-face ")" :foreground "white")
           (with-face (if (= (user-uid) 0) " #" " λ") :foreground "pale green" :bold)
           (with-face " " :foreground "white")))))

(use-package eval-in-repl :ensure t :defer t)

;;; HASKELL-MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package haskell-mode
  :ensure t
  :defer t
  :hook ((haskell-mode . haskell-doc-mode)
         (haskell-mode . haskell-indentation-mode)))

(use-package intero
  :ensure t
  :defer t
  :disabled t
  :hook (haskell-mode . intero-mode))

;;; GRAPHICS
;; ──────────────────────────────────────────────────────────────────
(use-package gnuplot-mode :defer t :ensure t)

;;; Notes, Journal and Task Manager
;;  ─────────────────────────────────────────────────────────────────
(use-package deft
  :ensure t
  :bind (:map ctl-quote-map
              ("C-d" . deft))
  :config
  (setq deft-directory (expand-file-name "notes/" org-directory)
        deft-default-extension "org"
        deft-auto-save-interval 600))

(use-package org-config
  :defer 2
  :disabled t
  :commands jump-to-org-agenda
  :doc
  "My org-mode configuration outgrew and hence I had to move it
   to a separate file."
  :load-path "etc/")

;; W3M
;; ──────────────────────────────────────────────────────────────────
(use-package w3m
  :doc "W3M is better than EWW but load only w3m is installed."
  :defer t
  :ensure t
  :if (or (executable-find "w3m") (not (message "Install w3m!")))
  :hook (w3m-mode . variable-pitch-mode)
  :init
  (setq w3m-fill-column 80
        w3m-confirm-leaving-secure-page nil
        w3m-default-display-inline-images t))

(use-package shr
  :doc "The HTML parser that is used at multiple places in Emacs"
  :defer t
  :init
  (setq shr-width 80))

;;; Ivy and Friends
;; ──────────────────────────────────────────────────────────────────
(use-package smex
  :doc "Used by `ivy-M-x' for sorting based on frequency + recency."
  :ensure t
  :config
  (setd smex-save-file "tmp/smex-items"))

(use-package ivy
  :demand t
  :ensure t
  :diminish (ivy-mode . #(" 🙒" 0 2      ; How I want it to be shown.
                          (face          ; But this doesn't work.
                           (:foreground "green"))))
  :preface
  (defun ivy--sort-by-len (name candidates)
    "Sort CANDIDATES based on similarity of their length with NAME."
    (let ((name-len (length name))
          (candidates-count (length candidates)))
      (if (< 500 candidates-count)
          candidates
        (sort candidates
              (lambda (a b)
                (< (abs (- name-len (length a)))
                   (abs (- name-len (length b)))))))))

  (defun ignore-dired-buffers (b)
    (when (get-buffer b)
      (with-current-buffer b
        (eq major-mode 'dired-mode))))

  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("M-H"   . ivy-resume))

  :config
  (bind-keys :map ivy-minibuffer-map
             ("C-r" . ivy-previous-line-or-history)
             ("M-r" . ivy-reverse-i-search))

  (setq ivy-initial-inputs-alist nil
        ivy-format-function #'ivy-format-function-arrow
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))
        ivy-use-virtual-buffers t
        ;; ivy provides better completion compared to default, company mode
        ;; doesn't work nicely with `completion-in-region-function'.
        ivy-do-completion-in-region t)

  (setq ivy-ignore-buffers `("\\` "
                             "\\`\\*"
                             "\\`\\*git-monitor:"
                             "\\`\\*magit-process:"
                             "\\`\\*magit:"
                             "\\`\\*helpful:"
                             "\\.elc$"
                             "\\`\\.newsrc-dribble\\'"
                             "\\`\\.newsrc.eld\\'"
                             ignore-dired-buffers))

  ;; Add custom sorting functions individually instead of changing the
  ;; defaults.
  (mapc (lambda (x)
          (push x ivy-sort-matches-functions-alist))
        '((helpful-function . ivy--sort-by-len)
          (helpful-variable . ivy--sort-by-len)))

  ;; Disable sorting on input collection
  (mapc (lambda (cmd)
          (push (list cmd) ivy-sort-functions-alist))
        '(hledger-run-command))

  (ivy-mode +1)

  (use-package ivy-hydra
    :ensure t
    :bind (:map ivy-minibuffer-map
                ("M-o" . ivy-dispatching-done-hydra)))

  (use-package ivy-rich
    :ensure t
    :config
    (ivy-set-display-transformer 'ivy-switch-buffer
                                 'ivy-rich-switch-buffer-transformer)
    (setq ivy-virtual-abbreviate 'full
          ivy-rich-switch-buffer-align-virtual-buffer t
          ivy-rich-path-style 'abbrev))

  (use-package swiper
    :ensure t
    :bind (("C-. C-s" . swiper*))
    :commands swiper-from-isearch
    :preface
    (defun swiper* ()
      (interactive)
      (swiper (and (region-active-p)
                   (progn (deactivate-mark)
                          (buffer-substring-no-properties (mark)
                                                          (point))))))
    :init
    (bind-key "C-." #'swiper-from-isearch isearch-mode-map)
    :config
    (bind-keys :map swiper-map
               ("M-%" . swiper-query-replace)
               ("C-w" . ivy-yank-word)
               ("M-h" . swiper-avy)))

  (use-package counsel
    :ensure t
    :commands counsel-ag
    :bind (("M-x"       . counsel-M-x)
           ("C-x 8 RET" . counsel-unicode-char)
           ("C-c r"     . counsel-recentf)
           ("C-. C-r"   . counsel-project-ag)
           ("C-. C-l"   . counsel-locate)
           ("M-y"       . counsel-yank-pop)

           :map minibuffer-local-map
           ("M-r" . counsel-minibuffer-history)

           :map ctl-quote-map
           ("c p" . counsel-linux-app))
    :preface
    (defun counsel-project-ag ()
      "Start `counsel-ag' for current project."
      (interactive)
      (counsel-ag (or (and (symbol-at-point)
                           (symbol-name (symbol-at-point)))
                      "")
                  (projectile-project-root)))

    :config
    (setq counsel-yank-pop-separator
          (format "\n%s\n" (make-string 60 ?┅)))

    (setq counsel-find-file-ignore-regexp
          (concat "\\(\\`\\.[^.]\\|"
                  (regexp-opt completion-ignored-extensions)
                  "\\)"))

    ;; `org-mode-map' and `org-agenda-mode-map' might not be loaded yet.
    (eval-after-load "org"
      '(bind-keys :map org-mode-map
                  ("C-c C-j" . counsel-org-goto)
                  ("C-c C-q" . counsel-org-tag)))

    (eval-after-load "org-agenda"
      '(bind-keys :map org-agenda-mode-map
                  (":" . counsel-org-tag-agenda)))

    (use-package counsel-bbdb
      :ensure t
      :after bbdb
      :config
      ;; This will feel like a hack until I get used to this being here or I
      ;; forget about its existence.
      (setf (symbol-function 'bbdb-complete-mail)
            #'counsel-bbdb-complete-mail))))

;;; JAVA
(use-package java-mode
  :defer t
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              (electric-pair-mode +1)
              (meghanada-mode +1)
              (setq c-basic-offset 4
                    tab-width 4
                    c-block-comment-prefix "* "))))

(use-package javadoc-lookup
  :bind ("C-h j" . javadoc-lookup)
  :ensure t)

(use-package meghanada
  :after java-mode
  :ensure t
  :init
  (setd meghanada-server-install-dir
        "tmp/meghanada")
  :config
  (unbind-key "M-." meghanada-mode-map)
  (unbind-key "M-," meghanada-mode-map))

(use-package scala-mode
  :ensure t
  :pin melpa
  :defer t
  :config
  (use-package sbt-mode
    :ensure t
    :pin melpa)

  (use-package ensime
    :ensure t
    :pin melpa))

;;; C/C++-MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package cc-mode
  :bind (:map c-mode-base-map
              ("<return>" . newline-and-indent)
              ("C-c C-k"  . compile)
              ("C-c C-t"  . c-toggle-comment-style))
  :config
  (setq-default c-block-comment-flag t)

  (use-package cc-config
    :demand t
    :pin manual
    :load-path "etc/"))

;;; RUST
;; ──────────────────────────────────────────────────────────────────
(use-package rust-mode
  :defer t
  :ensure t
  :config
  (use-package cargo
    :ensure t
    :config
    (unless (executable-find "cargo")
      (message "Please install cargo or modify PATH to include it.")))

  (when (executable-find "cargo")
    (mapc (lambda (pkg)
            (unless (executable-find pkg)
              (message "Installing %s..." pkg)
              (shell-command (concat "cargo install " pkg))))
          '("racer" "rustfmt")))

  (use-package racer
    :ensure t
    :config
    (setq racer-rust-src-path
          (expand-file-name "~/code/rust/src/")))

  (use-package flycheck-rust
    :ensure t)

  (add-hook 'rust-mode-hook
            (lambda ()
              (racer-mode +1)
              (eldoc-mode +1)
              (cargo-minor-mode +1)
              (flycheck-rust-setup))))

;;; PYTHON-MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package python-mode
  :defer t
  :config
  (setq python-indent-guess-indent-offset-verbose nil)

  (use-package py-autopep8
    :ensure t
    :hook (elpy-mode . py-autopep8-enable-on-save)
    :init
    (when (not (executable-find "autopep8"))
      (message "AutoPEP8 not installed!")))

  (use-package elpy
    :ensure t
    :defer t
    :config
    (elpy-enable)
    (diminish #'highlight-indentation-mode)
    (if (executable-find "ipython")
        (elpy-use-ipython)
      (message "IPython not found!")))

  (when (not (executable-find "flake8"))
    (message "Please install flake8")))

;;; LISP MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package macrostep :defer t :ensure t)

(use-package iedit
  :ensure t
  :bind ("C-' c C-;" . iedit-mode))

(use-package which-func
  :init
  :hook ((emacs-lisp-mode) . which-function-mode)
  :config
  (setq which-func-modes '(emacs-lisp-mode)
        which-func-unknown ""))

(use-package eldoc
  :diminish eldoc-mode
  :config
  (global-eldoc-mode +1))

(use-package lispy
  :ensure t
  :after emacs-lisp-mode
  :config
  (setq lispy-eval-display-style 'overlay))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :bind (:map paredit-mode-map
              ("M-S" . paredit-splice-sexp)
              ("M-R" . paredit-raise-sexp)
              ("C-M-)" . utils-paredit-slurp-all-the-way-forward)
              ("C-M-(" . utils-paredit-slurp-all-the-way-backward)
              ("C-M-}" . utils-paredit-barf-all-the-way-forward)
              ("C-M-{" . utils-paredit-barf-all-the-way-backward))
  :preface
  (defun utils-paredit-barf-all-the-way-backward ()
    (interactive)
    (paredit-split-sexp)
    (paredit-backward-down)
    (paredit-splice-sexp))

  (defun utils-paredit-barf-all-the-way-forward ()
    (interactive)
    (paredit-split-sexp)
    (paredit-forward-down)
    (paredit-splice-sexp)
    (if (eolp) (delete-horizontal-space)))

  (defun utils-paredit-slurp-all-the-way-backward ()
    (interactive)
    (catch 'done
      (while (not (bobp))
        (save-excursion
          (paredit-backward-up)
          (if (eq (char-before) ?\()
              (throw 'done t)))
        (paredit-backward-slurp-sexp))))

  (defun utils-paredit-slurp-all-the-way-forward ()
    (interactive)
    (catch 'done
      (while (not (eobp))
        (save-excursion
          (paredit-forward-up)
          (if (eq (char-after) ?\))
              (throw 'done t)))
        (paredit-forward-slurp-sexp))))
  :hook ((emacs-lisp-mode
          lisp-mode
          clojure-mode
          cider-repl-mode
          eshell-mode
          eval-expression-minibuffer-setup)
         . enable-paredit-mode)
  :config
  (unbind-key "M-r" paredit-mode-map)
  (unbind-key "M-s" paredit-mode-map))

(use-package smartparens
  :ensure t
  :defer 2
  :bind (:map smartparens-mode-map
              ("M-S" . sp-splice-sexp)
              ("C-M-)" . utils-paredit-slurp-all-the-way-forward)
              ("C-M-(" . utils-paredit-slurp-all-the-way-bacward)
              ("C-M-}" . utils-paredit-barf-all-the-way-forward)
              ("C-M-{" . utils-paredit-barf-all-the-way-bacward)
              ("M-R" . sp-raise-sexp))
  :diminish smartparens-mode
  :config
  (setq sp-highlight-pair-overlay nil)
  (setq sp-ignore-modes-list
        (append sp-ignore-modes-list
                '(emacs-lisp-mode
                  lisp-mode
                  clojure-mode
                  cider-repl-mode
                  eshell-mode
                  eval-expression-minibuffer-setup)))
  (sp-use-paredit-bindings)
  (smartparens-global-strict-mode +1)
  (unbind-key "M-r" smartparens-mode-map)
  (unbind-key "M-s" smartparens-mode-map)
  (unbind-key "C-w" smartparens-strict-mode-map))

(use-package clojure-mode
  :defer t
  :mode "\\.clj\\'"
  :ensure t)

(use-package cider
  :ensure t
  :after (clojure-mode emacs-lisp-mode)
  :hook (clojure-mode . cider-mode)
  :config
  (setq nrepl-log-messages t
        cider-auto-jump-to-error nil
        cider-prompt-for-symbol nil
        cider-repl-history-file (expand-file-name "tmp/cider-repl-history.el"
                                                  user-emacs-directory)

        cider-eldoc-display-for-symbol-at-point nil
        cider-eldoc-display-context-dependent-info t))

(use-package clj-refactor
  :after clojure-mode
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (setq cljr-ignore-analyzer-errors t
        cljr-warn-on-eval nil
        cljr-ignore-paths '()))

(use-package racket-mode
  :defer t
  :ensure t
  :config
  (use-package scribble-mode :ensure t))

(use-package geiser
  :defer t
  :ensure t)

;;; [WO]MAN-MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package man
  :bind (:map ctl-quote-map
              ("C-m" . man))
  :config
  (add-hook 'Man-mode-hook
            (lambda ()
              (set-face-attribute 'Man-overstrike nil
                                  :inherit font-lock-type-face
                                  :bold t)
              (set-face-attribute 'Man-underline nil
                                  :inherit font-lock-keyword-face
                                  :underline t))))

;;; WHITESPACE-MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package whitespace
  :doc "For the 80-column rule."
  :diminish whitespace-mode
  :preface
  (defun cleanup-whitespace ()
    "Remove whitespaces."
    (interactive)
    (whitespace-cleanup)
    (delete-trailing-whitespace))
  :hook (prog-mode . whitespace-mode)
  :init
  (setq whitespace-style '(face lines-tail))

  ;; Cleanup whitespace before saving files
  (add-hook 'before-save-hook
            (lambda ()
              ;; Exclude whitespace-sensitive modes that I know of.
              ;;
              ;; 1. Graphics in `ob-ipython' do not work if we remove trailing
              ;;    newlines. So, this should be excluded for buffers in
              ;;    `image-mode'.
              (when (not (memq major-mode '(markdown-mode
                                            image-mode)))
                (cleanup-whitespace)))))

;;; INTERNET
;; ──────────────────────────────────────────────────────────────────
(use-package webjump
  :disabled t
  :preface
  (defun webjump-with-w3m ()
    "Webjump but with w3m."
    (interactive)
    (let ((browse-url-browser-function 'w3m-browse-url))
      (call-interactively 'webjump))))

(use-package google-this
  :ensure t
  :defer 10
  :config
  (global-set-key (kbd "C-. /") #'google-this-mode-submap))

;;; WEB DEVELOPMENT
;; ──────────────────────────────────────────────────────────────────
(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.phtml\\'"
         "\\.html\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'" )
  :config
  (setq web-mode-auto-close-style 2)

  (add-hook 'web-mode-hook
            (lambda ()
              (setq web-mode-markup-indent-offset 2
                    web-mode-css-indent-offset 2
                    web-mode-code-indent-offset 2)))

  (use-package emmet-mode
    :ensure t
    :hook (web-mode . emmet-mode)))

(use-package skewer-mode
  :disabled t
  :ensure t
  :config
  (add-hook 'html-mode-hook #'skewer-html-mode)
  (add-hook 'js2-mode-hook #'skewer-mode)
  (add-hook 'css-mode-hook #'skewer-css-mode))

(use-package js2-mode    :defer t :ensure t)
(use-package coffee-mode :defer t :ensure t)
(use-package elm-mode    :defer t :ensure t)

;;; ERLANG AND ELIXIR
;; ──────────────────────────────────────────────────────────────────
(use-package erlang :defer t :ensure t)
(use-package elixir-mode
  :ensure t
  :defer t
  :config
  (bind-keys :map elixir-mode-map
             ("C-M-e" . elixir-end-of-defun)
             ("C-M-a" . elixir-beginning-of-defun))

  (use-package flycheck-mix   :ensure t :config (flycheck-mix-setup))
  (use-package flycheck-credo :ensure t :config (flycheck-credo-setup)))

(use-package alchemist
  :ensure t
  :after elixir-mode
  :bind (:map elixir-mode-map
              ("C-x C-e" . alchemist-eval-current-line))
  :preface
  (defun alchemist-help-flash-at-point (&rest _)
    "Flash definition at point."
    (let ((expr (alchemist-scope-expression)))
      (when (not (string-empty-p expr))
        (save-excursion
          (while (not (looking-at expr))
            (backward-char))
          (pulse-momentary-highlight-region (point)
                                            (search-forward expr))))))
  (defun alchemist-next-overlay ()
    "Move to next overlay in displayed documentation."
    (interactive)
    (goto-char (next-overlay-change (apply #'max
                                           (point)
                                           (mapcar #'overlay-end
                                                   (overlays-at (point)))))))

  (defvar alchemist-help-last)
  (defun alchemist-help-last ()
    "Go back to the old help page in `alchemist-help-minor-mode'."
    (interactive)
    (unless (eq last-command 'alchemist-help-last)
      (setq alchemist-help-last alchemist-help-search-history))
    (if (not alchemist-help-last)
        (message "No items left in history now.")
      (message "Searching doc for: %s" (car alchemist-help-last))
      (alchemist-help-lookup-doc (pop alchemist-help-last))))

  :init
  (setq alchemist-key-command-prefix (kbd "C-c ;")
        alchemist-goto-elixir-source-dir (expand-file-name "~/code/elixir/")
        alchemist-goto-erlang-source-dir (expand-file-name "~/code/otp/"))
  (add-hook 'alchemist-help-minor-mode-hook
            (lambda ()
              (bind-keys :map alchemist-help-minor-mode-map
                         ("RET" . alchemist-help-search-at-point)
                         ("l" . alchemist-help-last)
                         ("TAB" . alchemist-next-overlay))))
  (add-hook 'elixir-mode-hook
            #'alchemist-mode)
  (advice-add 'alchemist-help-search-at-point
              :before #'alchemist-help-flash-at-point)

  :config
  (use-package elixir-skels
    :load-path "etc/")

  (require 'eval-in-repl-iex)

  (bind-keys :map elixir-mode-map
             ("C-x M-e" . eir-eval-in-iex)))

;;; RUBY MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package ruby-mode
  :defer t
  :preface
  (defun insert-latest-gemspec (gem)
    "Print the latest version of GEM.
     This function also kills the gemspec so that it can be yanked
     immediately."
    (interactive "sGem: ")
    (message "Talking to RubyGems...")
    (request
     (format "https://rubygems.org/api/v1/versions/%s/latest.json" gem)
     :parser #'json-read
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let* ((full-version-str (assoc-default 'version data))
               (minor-version-str (replace-regexp-in-string "\\.[0-9]+$"
                                                            ""
                                                            full-version-str))
               (gemspec (format "gem '%s', '~> %s', '>= %s'"
                                gem
                                minor-version-str
                                full-version-str)))
          (insert gemspec)
          (message "--> %s" gemspec))))
     :error
     (cl-function
      (lambda (&rest args &key error-thrown &allow-other-keys)
        (message "Failed with : %s" error-thrown)))))

  (defun search-ruby-gems (search-term)
    "Search for GEM at Rubygems."
    (interactive "sSearch: ")
    (message "Talking to RubyGems...")
    (request
     "https://rubygems.org/api/v1/search.json"
     :params `(("query" . ,search-term))
     :parser #'json-read
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let* ((completions-alist
                (mapcar
                 (lambda (gem)
                   (let ((gem-name (assoc-default 'name gem))
                         (gem-info (assoc-default 'info gem)))
                     (cons (format "%s : %s"
                                   gem-name
                                   (truncate-string-to-width gem-info 80 0 nil t))
                           gem-name)))
                 data))
               (chosen-gem (assoc-default (completing-read "Select: "
                                                           completions-alist)
                                          completions-alist)))
          (insert-latest-gemspec chosen-gem))))
     :error (cl-function
             (lambda (&rest args &key error-thrown &allow-other-keys)
               (message "Error: %s" error-thrown)))))
  :bind (:map ruby-mode-map
              ("C-c C-g" . search-ruby-gems)))

(use-package rinari
  :ensure t
  :after ruby-mode
  :bind (:map ruby-mode-map
              ("C-c i" . rinari-insert-erb-skeletion))
  :config
  (global-rinari-mode)
  (add-hook 'ruby-mode-hook 'electric-pair-mode))

(use-package robe
  :ensure t
  :defer t
  :init
  (add-hook 'ruby-mode-hook #'robe-mode))

(use-package haml-mode :ensure t :defer t)
(use-package rvm
  :doc "RVM doesn't work out of the box with eshell."
  :ensure t
  :defer t)

;;; GO MODE
;; ──────────────────────────────────────────────────────────────────
(use-package go-mode
  :ensure t
  :defer t
  :bind (:map go-mode-map
              ("M-." . gdef-jump))
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save)
              (electric-pair-mode 1))))

;;; Emacs Speaks Statistics
;; ──────────────────────────────────────────────────────────────────
(use-package ess :ensure t :defer t)

;;; LUA MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package lua-mode :defer t :ensure t)

;;; YAML
;; ──────────────────────────────────────────────────────────────────
(use-package yaml-mode
  :defer t
  :ensure t
  :mode "\\.yml\\'")

;;; MAGIT AND FRIENDS
;;  ─────────────────────────────────────────────────────────────────
(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode +1))

(use-package magit
  :ensure t
  :defer t
  :preface
  (defun magit-change-repository (directory)
    "Change to git repository in DIRECTORY."
    (interactive "D")
    (let ((default-directory directory))
      (call-interactively #'magit-status)))

  :bind (;; So that indentation is sane
         :map magit-mode-map
              ("C-c C-r" . magit-change-repository)
              :map ctl-period-map
              ("C-m" . magit-status)
              )
  :config
  (setq magit-completing-read-function 'ivy-completing-read)

  (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
  (add-hook 'magit-mode-hook 'magit-auto-revert-mode)

  (mapc (lambda (mode)
          (add-hook mode
                    (lambda ()
                      ;; I want to use C-RET solely for switching buffers.
                      (unbind-key "C-RET" (intern (format "%s-map" mode))))))
        '(magit-mode magit-status-mode)))

(use-package magithub
  :ensure t
  :disabled t
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory
        (expand-file-name "~/code/")))

(use-package git-gutter
  :doc "For me, it's magit's hunks ported to an Emacs buffer."
  :ensure t
  :defer 5
  :diminish git-gutter-mode
  :bind ("C-x C-g" . git-gutter)
  :config
  (defhydra hydra-git-gutter (global-map "C-x g")
    "git-gutter"
    ("n" git-gutter:next-hunk "next")
    ("p" git-gutter:previous-hunk "prev" )
    ("=" git-gutter:popup-hunk "popup")
    ("s" git-gutter:stage-hunk "stage")
    ("r" git-gutter:revert-hunk "revert"))

  (global-git-gutter-mode +1))

(use-package git-timemachine
  :ensure t
  :preface
  (defun git-timemachine-show-diff ()
    "Display diff of changes introduced by current commit."
    (interactive)
    (vc-version-diff (list git-timemachine-file)
                     (car git-timemachine-revision)
                     (car (git-timemachine--next-revision
                           (git-timemachine--revisions)))))
  :config
  (bind-key "D" #'git-timemachine-show-diff git-timemachine-mode-map))

;; TRAMP-MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package tramp
  :defer 10
  :init
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name (expand-file-name "tmp/tramp"
                                                      user-emacs-directory))
  :config
  ;; Make backups for tramp files in their original locations
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))
  ;; Honor remote PATH variable
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


;;; BBDB
;; ──────────────────────────────────────────────────────────────────
(use-package bbdb
  :ensure t
  :after gnus
  :preface
  (defun bbdb/contacts-filter (addr)
    "Filter out ADDR if no need to save in BBDB."
    (cond
     ((null addr) addr)
     ((string-match-p ".*no.*reply.*@.*" addr) nil)
     (t addr)))

  :init
  (setq bbdb-file (expand-file-name "bbdb" emacs-assets-directory))

  :config
  (setq bbdb-silent t)

  (use-package bbdb-mua
    :after gnus
    :defer 10
    :config
    (setq bbdb-mua-auto-update-p 'create
          bbdb-mua-pop-up nil)
    (bbdb-mua-auto-update-init 'gnus 'message))

  (setq bbdb-add-aka t
        bbdb-add-name t
        bbdb-add-mails t
        bbdb-new-mails-primary t
        bbdb-canonicalize-mail-function #'bbdb/contacts-filter
        bbdb-complete-mail-allow-cycling t)

  (add-hook 'bbdb-after-change-hook
            (lambda (&rest _)
              (bbdb-save nil nil))))

;;; NEWS
;; ──────────────────────────────────────────────────────────────────
(use-package youtube-dl
  :load-path "packages/lisp/"
  :defer t
  :config
  (setq youtube-dl-directory "~/Downloads/"))

(use-package elfeed
  :ensure t
  :commands elfeed
  :after (youtube-dl w3m)
  :preface
  (defun elfeed-show-entry-switch (buffer)
    "Function to switch to elfeed show BUFFER."
    (switch-to-buffer buffer)
    (center-text-for-reading))
  :config
  (setq elfeed-show-entry-switch #'elfeed-show-entry-switch)
  (load-file (locate-user-emacs-file "etc/elfeed-config.el")))


;;; EMAIL
;; ──────────────────────────────────────────────────────────────────
(use-package gnus
  :defer t
  :init
  (setd gnus-init-file "etc/gnus-config.el")
  ;; So that I have SMTP settings even if Gnus config hasn't been loaded yet.
  (add-hook 'message-mode-hook
            (lambda () (load-file gnus-init-file)))

  :config
  ;; Check mail not more than once every 4 hour.
  (limit-usage #'gnus (* 4 60))

  (hook-into-modes #'hl-line-mode
                   'gnus-summary-mode
                   'gnus-group-mode))

;;; DOCUMENT VIEWING
;; ──────────────────────────────────────────────────────────────────
(use-package pdf-tools
  :doc "The images produced by this mode are crisper and clearer
  compared to those by `docview-mode'."
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("j" . image-next-line)
              ("k" . image-previous-line)
              ("J" . pdf-view-next-line-or-next-page)
              ("K" . pdf-view-previous-line-or-previous-page)
              ("R" . reopen-pdf-file))
  :hook ((pdf-view-mode . pdf-outline-minor-mode)
         (pdf-view-mode . pdf-links-minor-mode)
         (pdf-view-mode . pdf-view-auto-slice-minor-mode))
  :preface
  (defun reopen-pdf-file ()
    "Reopen PDF file."
    (interactive)
    (let ((file-path (buffer-file-name (current-buffer)))
          (current-page (pdf-view-current-page))
          (interleave-enabledp interleave-pdf-mode))
      (kill-buffer)
      (find-file file-path)
      (pdf-view-goto-page current-page)
      (when interleave-enabledp
        (interleave-pdf-mode +1)))))

(use-package interleave
  :doc "For adding org-mode notes to pages in a PDF file."
  :defer t
  :ensure t
  :preface
  (defvar notif-id nil)
  (defun check-if-pdf-org-pages-are-synced ()
    "Visual indication that pdf page and org file page are out of sync."
    (interactive)
    (when (eq (buffer-name (current-buffer)) interleave-org-buffer)
      (let ((org-page (with-current-buffer interleave-org-buffer
                        (string-to-number
                         (org-entry-get-with-inheritance interleave--page-note-prop))))
            (pdf-page (with-current-buffer interleave-pdf-buffer
                        (pdf-view-current-page))))
        (when (not (= org-page pdf-page))
          (with-current-buffer interleave-org-buffer
            ;; Find a better way to show this.
            (setq notif-id
                  (notifications-notify :title "Org page out of sync!"
                                        :replaces-id notif-id)))))))
  :init
  (add-hook 'interleave-mode-hook
            (lambda ()
              (run-with-idle-timer 0.4 t #'check-if-pdf-org-pages-are-synced))))

;;; MUSIC
;; ──────────────────────────────────────────────────────────────────
(use-package emms-setup
  :ensure emms
  :defer t
  :commands emms
  :bind (:map emms-playlist-mode-map
              ("l" . lyrics/show-lyrics-for-current-track-in-emms )
              ("L" . lyrics/hide-last-lyrics-in-emms))
  :init
  (setd emms-directory "tmp/emms")

  :config
  (emms-standard)
  (emms-default-players)

  (add-hook 'emms-playlist-mode-hook #'hl-line-mode)

  (setq emms-playing-time-display-format "[Playing: %s]")
  (add-hook 'emms-player-started-hook
            (lambda ()
              (emms-mode-line -1)
              (emms-playing-time-enable-display))))


;;; ERC
;;  ─────────────────────────────────────────────────────────────────
(use-package erc-config
  :commands erc-connect
  :load-path "etc/"
  :defer t)

;;; EMACS-SERVER
;;  ─────────────────────────────────────────────────────────────────
(use-package server
  :demand t
  :config
  (unless (server-running-p)
    (server-start))

  (pcase system-type
    (`windows-nt
     ;; Default directory on windows isn't ~/
     (setq default-directory (expand-file-name "~/"))
     (setq interprogram-paste-function 'x-selection-value)
     ;; for some reason, selection highlight isn't turned on by default
     (transient-mark-mode t))
    (`darwin
     ;; Modify the CMD key to be Meta key
     ;; (setq mac-command-modifier 'meta)
     ;; This is very stupid of Apple keyboards
     ;; (setq mac-right-option-modifier 'ctrl)
     ;; I don't need a fn
     ;; (setq mac-function-modifier 'ctrl)
     (when (< emacs-major-version 25)
       (setq visible-bell nil)))
    (`gnu/linux
     ;; Feels like home! We can ignore the worries of the world.
     ;; Fetch and built latest Emacs at midnight on weekends.
     (midnight-mode +1)
     (add-hook 'midnight-hook
               (lambda ()
                 (when (member (format-time-string "%A")
                               '("Sunday" "Saturday"))
                   (message "You might want to rebuild Emacs today.")))))))

;;; ──────────────────────────────────────────────────────────────────

(use-package highlight :ensure t :defer t)
(use-package eval-sexp-fu
  :load-path "packages/lisp/"
  :commands eval-sexp-fu-flash-mode
  :pin manual
  :init
  (hook-into-modes #'eval-sexp-fu-flash-mode
                   'emacs-lisp-mode
                   'clojure-mode))

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :hook (after-init . volatile-highlights-mode))

(use-package es-mode
  :doc "Useful when working with Elasticsearch"
  :defer t
  :ensure t)

(use-package annotate
  :doc
  "For annotating arbitrary files."
  :ensure t
  :defer t)

(use-package tldr
  :load-path "packages/lisp/"
  :preface
  (defun tldr-take-to-eshell ()
    "Move current line to `eshell'."
    (interactive)
    (let ((text (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))
      (unless (buffer-live-p (and (boundp 'eshell-buffer-name)
                                  eshell-buffer-name))
        (split-window-sensibly)
        (eshell))
      (pop-to-buffer eshell-buffer-name)
      (require 's)
      (insert (s-trim text))))

  :bind (:map ctl-period-map
              ("C-w" . tldr))

  :init
  ;; There should be no trailing / here!
  (setd tldr-directory-path "tmp/tldr")
  (add-hook 'tldr-mode-hook #'utils-easy-move-mode)
  (add-hook 'tldr-mode-hook 'hl-line-mode)

  :config
  (bind-keys :map tldr-mode-map
             ("e" . tldr-take-to-eshell)))

(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))

(use-package proof-site
  :load-path "packages/lisp/PG/generic/"
  :commands proofgeneral)

(use-package know-your-http-well
  :ensure t
  :defer t)

(use-package coin-ticker
  :ensure t
  :commands coin-ticker-mode
  :init
  (do-when-idle #'coin-ticker-mode
                (lambda () (coin-ticker-mode -1))
                60)
  :config
  (setq coin-ticker-price-symbol "₹"
        coin-ticker-price-convert "INR"))

;;; BLOGGING
;; ──────────────────────────────────────────────────────────────────
(use-package org-blog
  :load-path "~/blog/src/"
  :bind ("C-c b" . write-post)
  :preface
  (defun write-post ()
    "Write a new post or modify an existing draft."
    (interactive)
    (require 'f)
    (let* ((default-directory "~/blog/src/drafts/")
           (all-posts (f-files "~/blog/src/posts/"
                               (lambda (path)
                                 (s-ends-with-p ".org" path))
                               t))
           (file-path (completing-read "File: " all-posts)))
      ;; If `file-path' doesn't exist yet, this would create it in drafts/
      ;; directory
      (find-file file-path))))

;;; GAMES
;; ──────────────────────────────────────────────────────────────────
(use-package chess :ensure t :defer t)

;;; Custom key-bindings

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;(global-set-key (kbd "C-M-<right>") 'switch-to-next-buffer)
;;(global-set-key (kbd "C-M-<left>") 'switch-to-prev-buffer)

(global-set-key (kbd "M-;") 'comment-or-uncomment-line-or-region)

(setq-default cursor-type 'bar)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position t)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq inhibit-startup-message t
      initial-scratch-message ""
      inhibit-startup-echo-area-message t)

;; Show tilde `~` for empty lines in left fringe
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(set-fringe-bitmap-face 'tilde 'font-lock-comment-face)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(setq-default indicate-empty-lines t)

;; Disables audio bell
(setq ring-bell-function
      (lambda () (message "*provide*")))

(setq mac-command-modifier 'control)

(add-to-list 'exec-path "/usr/local/bin")

(beep 'init)
;;; init.el ends here

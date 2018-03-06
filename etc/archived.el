;;; archived.el --- Packages that I no longer use.   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi;;; Disabled packages <narendraj9@gmail.com>
;; Keywords: lisp

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

;; For configurations that isn't being actively used but might come in handy in
;; the future.

;;; Code:
;;  ──────────────────────────────────────────────────────────────────

(use-package irfc
  :disabled t
  :ensure t
  :mode (".*rfc[0-9]+\\.txt" . irfc-mode)
  :hook (irfc-mode . olivetti-mode)
  :init
  (defun rfc-search ()
    "Search for an RFC and visit it with `irfc-visit'."
    (interactive)
    (let ((rindex (expand-file-name "etc/rfc-index.txt" user-emacs-directory)))

      (ivy-read "RFC search: "
                (s-lines (with-temp-buffer
                           (insert-file-contents rindex)
                           (buffer-string)))
                :require-match t
                :action (lambda (x)
                          (irfc-visit (nth 1 (s-split "\t" x))))))))

(use-package wakatime-mode
  :ensure t
  :disabled t
  :diminish wakatime-mode
  :config
  (global-wakatime-mode +1))

(use-package hyperbole
  :disabled t
  :doc
  "I need to figure out how to use and integrate it in my workflow"
  :bind (("C-h h"  . hyperbole)
         ("M-RET " . hkey-operate))
  :ensure t)

(use-package helm
  :disabled t
  :doc "Helm for everything except file paths. This should be
  executed before `ido` because I want `ido` to overwrite
  whatever helm sets up for file path completion."
  :ensure t
  :defer 2
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-c r" . helm-recentf)

         :map ctl-quote-map
         ("g" . helm-google-suggest)
         ("c f" . helm-sudo-find-file)

         :map ctl-period-map
         ("r" . helm-resume)

         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))

  :preface
  (defun helm-sudo-find-file ()
    "Open file with super user privileges."
    (interactive)
    (let ((file-path (ido-read-file-name "File: " "/")))
      (helm-find-file-as-root file-path)))

  :init
  (setq helm-always-two-windows t
        helm-split-window-in-side-p nil
        helm-split-window-default-side 'below
        helm-adaptive-history-file (expand-file-name "tmp/helm-adaptive-history"
                                                     user-emacs-directory))

  :config
  (helm-mode +1)

  ;; My `read-file-name' should always be IDO's `read-file-name'. That's how I
  ;; like it.
  (setq helm-completing-read-handlers-alist
        '(
          ;; Where helm shines,
          (describe-function . helm-completing-read-symbols)
          (describe-variable . helm-completing-read-symbols)
          (describe-symbol . helm-completing-read-symbols)
          (find-function . helm-completing-read-symbols)
          (debug-on-entry . helm-completing-read-symbols)

          (disassemble . helm-completing-read-symbols)
          (trace-function . helm-completing-read-symbols)
          (trace-function-foreground . helm-completing-read-symbols)
          (trace-function-background . helm-completing-read-symbols)

          ;; Where helm sucks,
          (org-capture . ido)
          (dired-do-rename . ido)
          (dired-do-copy . ido)
          (org-set-tags . nil)
          (org-agenda-set-tags . nil)
          (ffap-alternate-file . ido)
          (tmm-menubar . nil)
          (find-file . ido-find-file)
          (find-file-at-point . nil)
          (ffap . nil)
          (execute-extended-command . nil))))

(use-package helm-grep
  :disabled t
  :preface
  (defun helm-grep-it (arg)
    "Run the_silver_search with `helm-do-grep.

    Argument ARG is the prefix argument which when not supplied
    causes `vc-root-dir' to be the default root.  Otherwise
    current-directory is the default root."
    (interactive "P")
    (if arg
        (call-interactively 'helm-do-grep-ag-with-directory)
      ;; Now let it be a bit more convenient.
      (let* ((default-directory (expand-file-name (or (ignore-errors (vc-root-dir))
                                                      (ignore-errors
                                                        (progn (require 'magit)
                                                               (magit-toplevel)))
                                                      default-directory)))
             (ag-type-list '((emacs-lisp-mode . "--elisp")
                             (clojure-mode . "--clojure")))
             (ag-type (assoc-default major-mode ag-type-list)))
        (helm-grep-ag-1 default-directory (list ag-type)))))

  (defun helm-do-grep-ag-with-directory (d)
    "Do `helm-do-grep-ag' with `default-directory' set to D."
    (interactive "DRoot: ")
    (let ((default-directory d))
      (call-interactively 'helm-do-grep-ag)))

  (defun helm-multi-occur-on-candidates (&optional _candidate)
    "Run `multi-occur' on all marked _CANDIDATE s files.
     Works in *helm* or *helm-grep* buffers."
    (interactive)
    (with-current-buffer (if (eq major-mode 'helm-grep-mode)
                             (current-buffer)
                           helm-buffer)
      (let ((files (mapcar (lambda (c)
                             (car (helm-grep-split-line c)))
                           (helm-marked-candidates))))
        (helm-multi-occur-1 (mapcar #'find-file-noselect
                                    (delete-duplicates files
                                                       :test 'equal))))))

  :bind (:map ctl-period-map
              ("C-r" . helm-grep-it))
  :init
  (setq helm-grep-ag-command            ; useful while searching in dotfiles
        "ag --line-numbers --hidden -u -S --color --nogroup %s %s %s")

  (eval-after-load "helm-grep"
    '(progn
       (cl-defmethod helm-setup-user-source ((source helm-grep-ag-class))
         (setf (slot-value source 'nomark) nil))
       (setq helm-grep-actions
             (append helm-grep-actions
                     '(("Occur" . helm-multi-occur-on-candidates)))))))

(use-package ido-completing-read+
  :disabled t
  :doc "I want ido-completing-read+ for helm."
  :after helm
  :ensure t)

(use-package helm-bbdb
  :disabled t
  :doc "I am not sure if I even use this."
  :after gnus
  :ensure t)

(use-package helm-dash
  :disabled t
  :ensure t
  :bind (:map ctl-quote-map
              ("C-o" . helm-dash-at-point))
  :config
  (setq helm-dash-browser-func 'w3m
        helm-dash-enable-debugging nil)
  (make-variable-buffer-local 'helm-dash-docsets))

(use-package ido
  :disabled t
  :doc
  "I like `ido' for file paths' and buffer completion. Currently,
  the vector syntax [remap ..] is broken and using strings there
  is a workaround:
  https://github.com/jwiegley/use-package/issues/166"

  :preface
  (defvar ido-temp-list nil)
  (defun add-duplicate-buffers-to-ido-uniquely ()
    "Add files with the name base name in `recentf-list'."
    (let ((filename-filepath-pairs
           (mapcar (lambda (f) (cons (file-name-nondirectory f) f))
                   recentf-list))
          (temp-list))
      (dolist (filename-filepath-pair filename-filepath-pairs)
        (let* ((name (car filename-filepath-pair))
               (path (cdr filename-filepath-pair))
               (name-with-path
                (format "%s • %s"
                        name
                        (file-name-base
                         (directory-file-name
                          (file-name-directory path))))))
          ;; Only add an entry if we already do not have a
          ;; real/virtual buffer for it.
          (when (not (or (equal (cdr (assoc name ido-virtual-buffers))
                                path)
                         (and (get-buffer name)
                              (equal (buffer-file-name (get-buffer name))
                                     path))))
            ;; Time to make a different name and add to both lists.
            (push (cons name-with-path path) ido-virtual-buffers)
            (push name-with-path temp-list))))
      (setq ido-temp-list
            (cl-remove-duplicates (nconc ido-temp-list
                                         (nreverse temp-list))
                                  :test 'equal
                                  :from-end t))))

  :init
  (add-hook 'ido-make-buffer-list-hook
            #'add-duplicate-buffers-to-ido-uniquely)

  (setq ido-use-virtual-buffers t
        ido-enable-flex-matching t
        ido-auto-merge-work-directories-length -1
        ido-save-directory-list-file (expand-file-name "tmp/ido.last"
                                                       user-emacs-directory)
        ido-ignore-buffers '("\\` "  "^#.*" ".*freenode\.net.*"
                             ".*irc\.slack\.com.*" "\\*helm.*" ".*\\.org$")
        ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
                          " [Matched]" " [Not readable]" " [Too big]"
                          " [Confirm]"))
  :config
  (ido-mode +1)
  (mapc (lambda (f) (push f ido-ignore-files))
        (list "GPATH" "GTAGS"  "GRTAGS")))


;;; DEFINITIONS
;;; ──────────────────────────────────────────────────────────────────
(defun get-window-position ()
  "Return active window's rough position."
  (require 'winner)
  (let* ((edges (window-edges (selected-window) nil nil t))
         (window-x* (/ (+ (nth 0 edges) (nth 2 edges)) 2.0))
         (window-y* (/ (+ (nth 1 edges) (nth 3 edges)) 2.0))
         (center-x (/ (frame-pixel-width) 2.0))
         (center-y (/ (frame-pixel-height) 2.0))

         (window-x (- window-x* center-x))
         (window-y (- window-y* center-y))

         ;; Center of Emacs frame is the origin. +x-axis goes towards the
         ;; right, +y-axis goes down.
         (window-theta (string-to-number (calc-eval (format "arctan2(%s, %s)"
                                                            window-y
                                                            window-x))))
         (window-count (count-windows)))
    (message "%s Degrees" window-theta)
    (cond
     ((= window-count 1) "One window!")

     ((< window-theta -90 ) "Top Left")

     ((< window-theta 0)   "Top Right")

     ((< window-theta 90) "Bottom Right")

     (t "Bottom Left"))))

;;; ──────────────────────────────────────────────────────────────────

(provide 'archived)
;;; archived.el ends here

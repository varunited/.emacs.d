;;; defs.el --- Utility var/functions for my init.el   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience, local

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

;; This file would contain all the var/fucntion defintions that I
;; originally used to keep in my init.el

;;; Code:

(require 'seq)
(require 's)

(defvar emacs-assets-directory
  (expand-file-name "~/miscellany/assets/")
  "Directory to keep assets that should not be shared.")

(defvar personal-dictionary-file
  (expand-file-name "~/miscellany/assets/personal-dict.en.pws")
  "File to keep words that I think should be a part of my dictionary.")

(defsubst hook-into-modes (func &rest modes)
  "Add FUNC to mode-hooks for MODES.  Credits: John Whigley."
  (dolist (mode modes) (add-hook (intern (format "%s-hook" mode))
                                 func)))

(defsubst make-interactive (f &rest rest)
  "Define F to be an interactive function called with REST args."
  (lambda ()
    (interactive)
    (apply f rest)))

(defun other-window* (arg)
  "Act like `other-window' but change direction when ARG is given."
  (interactive "p")
  (if (= arg 4)
      (other-window -1)
    (other-window +1)))

(defun center-text-for-reading (&optional arg)
  "Setup margins for reading long texts.
If ARG is supplied, reset margins and fringes to zero."
  (interactive "P")
  (set-fringe-style 0)
  ;; Set the margin width to zero first so that the whole window is
  ;; available for text area.
  (set-window-margins (selected-window) 0)
  (let ((margin-width (if arg
                          0
                        (/ (- (+ (window-width)
                                 left-margin-width
                                 right-margin-width)
                              79)
                           2))))
    (setq left-margin-width margin-width)
    ;; `set-window-margings' does a similar thing but those changes do
    ;; not persist across buffer switches.
    (set-window-buffer nil (current-buffer))))

(defun shrink-current-window ()
  "Shrink current window by 5 lines/columns."
  (interactive
   (or (enlarge-window -5)
       (enlarge-window-horizontally -5))))

(defun fill-paragraph-and-move-forward ()
  "Combine `fill-paragraph'and `forward-paragraph'.
I tend to use then together always."
  (interactive)
  (fill-paragraph)
  (forward-paragraph))

(defun slack-connect ()
  "Connect to slack."
  (interactive)
  (require 'erc-services)
  (require 'tls)
  (erc-services-mode +1)
  (if (boundp 'my-slack-vicarie-password)
      (erc-tls :server "vicarie.irc.slack.com"
               :port 6697
               :nick "narendraj9"
               :password my-slack-vicarie-password)
    (message "Error: my-slack-vicarie-password not bound")))


(defun vicarie/eval-last-sexp-and-do (f)
  "Eval the last sexp and call F on its value."
  (let ((standard-output (current-buffer))
        (value (eval-last-sexp nil)))
    (funcall f value)))


(defun vicarie/eval-print-last-sexp ()
    "Evaluate and print the last sexp on the same line."
  (interactive)
  (vicarie/eval-last-sexp-and-do (lambda (value)
                                   (insert (format " (= %s ) " value)))))


(defun vicarie/eval-replace-last-sexp ()
  "Evaluate and replace last sexp with its value."
  (interactive)
  (vicarie/eval-last-sexp-and-do (lambda (value)
                                   (backward-kill-sexp)
                                   (insert (format "%s" value)))))


(defun create-file-for-buffer ()
  "Create a temporary file for the current buffer.
To be used for buffers that don't have an associated file."
  (let* ((temp-file (make-temp-file
                     (replace-regexp-in-string "\*"
                                               ""
                                               (buffer-name))
                     nil
                     ".txt")))
    (write-region (point-min) (point-max) temp-file)
    temp-file))


(defun upload-file (file-path)
  "Upload a file to transfer.sh using curl.
I am thinking that
using curl is more efficient for binary files than using a buffer
and calling `upload-buffer'.
Argument FILE-PATH is the path to file."
  (interactive "fFile: ")
  (kill-new (shell-command-to-string
             (format "%s %s %s%s"
                     "curl -s --upload-file"
                     (shell-quote-argument file-path)
                     "https://transfer.sh/"
                     (shell-quote-argument
                      (file-name-nondirectory file-path)))))
  (message "Link copied to clipboard: %s" (s-trim (current-kill 0))))


(defun upload-buffer ()
  "Upload current buffer to transfer.sh.
This function uses the function `upload-region'."
  (interactive)
  (upload-region (point-min) (point-max)))


(defun upload-region (beg end)
  "Upload the contents of the selected region in current buffer.
It uses transfer.sh Link to the uploaded file is copied to
clipboard.  Creates a temp file if the buffer isn't associted with
a file.
Argument BEG beginning point for region.
Argument END ending point for region."
  (interactive "r")
  (let* ((buf-file-path (buffer-file-name))
         (file-path (or buf-file-path
                        (create-file-for-buffer)))
         (file-name (file-name-nondirectory file-path))
         (upload-url (format "https://transfer.sh/%s"
                             file-name))
         (url-request-method "PUT")
         (url-request-data (buffer-substring-no-properties beg end))
         (url-callback (lambda (_)
                         (search-forward "\n\n")
                         (let ((url-link (buffer-substring (point)
                                                           (point-max))))
                           (kill-new url-link)
                           (message "Link copied to clipboard: %s"
                                    (s-trim url-link))
                           (kill-buffer (current-buffer))))))
    (url-retrieve upload-url url-callback)))


(defun org-late-todo (n)
  "Switch todo assuming an old date [N days ago]."
  (interactive "nDays: ")
  (let* ((delta-time (days-to-time n))
         (now (time-subtract (current-time)
                             delta-time)))
    (letf (((symbol-function 'current-time) (lambda () now)))
      (org-agenda-todo))))


(defun switch-to-minibuffer ()
  "Switch to minibuffer."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))


(defun read-date (&optional format)
  "Get date from the user and return it in the format FORMAT.
If format isn't specified it defaults to `%Y %m %d`"
  (format-time-string (if format format "%Y %m %d")
                      (org-time-string-to-time (org-read-date))))


(defun make-old-content-read-only ()
  "Only allow for appending new content in the buffer."
  (interactive)
  (save-excursion
    (let ((begin (point-min))
          (end (progn
                 (goto-char (point-max))
                 (backward-word)  ; Upto the line containing a word
                 (end-of-line)
                 (point))))
      (add-text-properties begin end
                           '(read-only t rear-nonsticky t front-sticky t)))))


(defun kill-buffer-delete-window ()
  "Kill current buffer and delete its window."
  (interactive)
  (kill-buffer (current-buffer))
  (when (< 1 (count-windows))
    (delete-window)))


(defun inhibit-read-only ()
  "Avoid read-only mode.
Because eshell is silly and into read-only mode on typing over prompt."
  (interactive)
  (setq inhibit-read-only t))


(defun kill-with-linenum (beg end)
  "Kill region (BEG, END) with line numbers."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (skip-chars-backward "\n \t")
    (setq end (point))
    (let* ((chunk (buffer-substring beg end))
           (chunk (concat
                   (format "╭──────── #%-d ─ %s ──\n│ "
                           (line-number-at-pos beg)
                           (or (buffer-file-name) (buffer-name))
                           )
                   (replace-regexp-in-string "\n" "\n│ " chunk)
                   (format "\n╰──────── #%-d ─"
                           (line-number-at-pos end)))))
      (kill-new chunk)))
  (deactivate-mark))


(defun he-toggle ()
  "Toggle highlight expression inside selected parens.
Useful when showing code."
  (interactive)
  (if (equal show-paren-style 'parenthesis)
      (setq show-paren-style 'expression)
    (setq show-paren-style 'parenthesis)))


(defun kill-other-buffers ()
  "Kill all buffers except the current one and the erc buffers."
  (interactive)
  (let ((ignore-buffers (cons (current-buffer)
                              (if (fboundp 'erc-buffer-list)
                                  (erc-buffer-list)
                                nil))))
    (mapc (lambda (buffer)
            (when (not (memq buffer ignore-buffers))
              (kill-buffer buffer)))
          (buffer-list))))


(defun yank-to-x-clipboard (&optional region-beg region-end)
  "Yank selected text in (REGION-BEG, REGION-END) to X clipboard.
Use when on console."
  (interactive "r")
  (shell-command-on-region region-beg region-end "xclip -i -selec clip"))


(defun insert-date-at-point ()
  "Insert current date at the current position of point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))


(defun notify (msg &optional font-size duration)
  "Notify me with a MSG of size FONT-SIZE for DURATION seconds.
Requires that dzen is installed."
  (start-process-shell-command "dzen" nil
                               (format "echo %s | dzen2 -l 200 -fn 'Comic Sans MS:size=%s' -p %s"
                                       (shell-quote-argument msg)
                                       (or font-size 25)
                                       (or duration 5))))


;; Setup an emacs window into 70-30% horizontally.
(fset 'split-thirty-seventy
      "\C-x2\C-u4\C-x^\C-u4\C-x^\C-u4\C-x^")


(defun split-and-shell ()
  "Split the buffer vertically and shart shell in one of the windows."
  (interactive)
  (if-let ((w (get-buffer-window "*eshell*")))
      (select-window w)
    (execute-kbd-macro (symbol-function 'split-thirty-seventy))
    (other-window 1)
    (eshell)))

;;; Defunctional Playground
;;  ─────────────────────────────────────────────────────────────────
(defun take-notes ()
  "Open notes file."
  (interactive)
  "Quick go to the notes file. "
  (let ((default-directory (expand-file-name "notes/" org-directory)))
    (call-interactively #'find-file)))

(defun snap-it-to-file ()
  "Take a screenshot of Emacs and return the file path."
  (make-directory "/tmp/screenshots/" t)
  (let ((default-directory "/tmp/screenshots/"))
    (shell-command-to-string
     "scrot -u -e 'echo -n /tmp/screenshots/$f'")))


(defun snap-it ()
  "Take a screenshot and upload it to transfer.sh."
  (interactive)
  (upload-file (snap-it-to-file)))


(defun go-back-to-intellij ()
  "Change focus to window running android studio."
  (interactive)
  (shell-command "wmctrl -a 'Android Studio'"))


(defun post-to-slack (webhook-url message)
  "Post to the slack WEBHOOK-URL contents of MESSAGE."
  (let ((url-request-method "POST")
        (url-request-data (json-encode `(:text ,message)))
        (url-request-extra-header '(("Content-Type" . "application/json")))
        (url-callback  (lambda (_)
                         (search-forward "\n\n")
                         (let ((info-text (buffer-substring (point)
                                                            (point-max))))
                           (message (format "slack: %s"
                                            (s-trim info-text)))
                           (kill-buffer (current-buffer))))))
    (url-retrieve webhook-url url-callback)))


(defun post-region-to-slack-cooking (beg end)
  "Post region (BEG, END) to one of my slack channels."
  (interactive "r")
  (if (boundp 'my-slack-vicarie-cooking-webhook)
      (post-to-slack my-slack-vicarie-cooking-webhook
                     (buffer-substring beg end))
    (message "`my-slack-vicarie-cooking-webhook` not bound to the webhook url")))


(defun screenshot-frame (window-id)
  "Take a screenshot of 400x200 pixels window with WINDOW-ID.
Taken from Chris Done's config"
  (shell-command-to-string
   (concat "import -window "
           (shell-quote-argument window-id)
           " +repage /tmp/frames/`date +%s`.png"))
  (pulse-momentary-highlight-region (window-start)
                                    (window-end)))


(defun start-recording-window ()
  "Record screenshots of the window and prepare a gif."
  (interactive)
  (message "Click on the window you want to record")
  (if (file-directory-p "/tmp/frames/")
      (delete-directory "/tmp/frames/" t))
  (make-directory "/tmp/frames/" t)
  (blink-cursor-mode -1)
  (let* ((window-id  (s-trim (shell-command-to-string
                              "xwininfo | grep 'Window id' | cut -d ' ' -f 4")))
         ;; Take a screenshot if I am idle for 1 second
         (timer (run-with-idle-timer 0.5
                                     t
                                     `(lambda ()
                                        (screenshot-frame ,window-id)))))
    (message "Started recording... [C-c x : Stop recording]")
    (global-set-key
     (kbd "C-c x")
     `(lambda (delay-between-frames)
        (interactive "nDelay between frames (e.g. 70): ")
        (cancel-timer ,timer)
        (message "Stopped recording!")
        (blink-cursor-mode 1)
        (global-unset-key (kbd "C-c x"))
        (message (shell-command-to-string
                  (format "convert -delay %s /tmp/frames/*.png /tmp/frames/out.gif && %s"
                          delay-between-frames
                          "echo Ouput saved to /tmp/frames/out.gif &")))))))


(defun fetch-parse-and-do (url parser action)
  "Helper function for all network related things.
Fetches the URL.  Changes the current buffer to the response
buffer.  Calls the PARSER on the buffer starting at the beginning
of the HTTP response in the response buffer.  Calls ACTION on the
value returned by the parser function."
  (let ((url-request-method "GET")
        (url-callback `(lambda (status)
                         (if status
                             (message "%s" status)
                           (search-forward "\n\n")
                           (,action (,parser))
                           (kill-buffer (current-buffer))))))
    (url-retrieve url url-callback)))


(defun get-location-coordinates (address)
  "Show and return the latitude and longitude of ADDRESS.
This uses the google maps api as described here:
https://developers.google.com/maps/documentation/geocoding/intro"
  (interactive "sAddress: ")
  (let ((api-url-endpoint
         (url-encode-url
          (format "https://maps.googleapis.com/maps/api/geocode/json?address=%s"
                  address)))
        (get-info-string-from-result
         (lambda (result)
           (let* ((fmt_address (assoc-default 'formatted_address result))
                  (geometry (assoc-default 'geometry result))
                  (location (assoc-default 'location geometry))
                  (latitude (assoc-default 'lat location))
                  (longitude (assoc-default 'lng location)))
             (format "Address: %s \nLatitude: %s\nLongitude: %s\n"
                     fmt_address
                     latitude
                     longitude)))))
    (fetch-parse-and-do api-url-endpoint
                        'json-read
                        (lambda (response)
                          (display-message-or-buffer
                           (mapconcat (lambda (result)
                                        (funcall get-info-string-from-result
                                                 result))
                                      (assoc-default 'results response)
                                      "\n"))))))


(defun toggle-window-split ()
  "Toggle between horizontal and vertical splits.
This has been taken from http://whattheemacsd.com/."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun swap--w1<->w2 (w1 w2)
  "Swap buffers and positions in W1 with those in W2.
Note w1 and w2 are references to window objects.  So, it is
important get the state of those buffers before we change any one
of them."
  (let ((b1 (window-buffer w1))
        (s1 (window-start w1))
        (b2 (window-buffer w2))
        (s2 (window-start w2)))
    (set-window-buffer w1 b2)
    (set-window-start w1 s2)
    (set-window-buffer w2 b1)
    (set-window-start w2 s1)))

(defun rotate-windows (&optional arg)
  "Rotate your windows.
This has been taken from http://whattheemacsd.com/.
If prefix ARG is given, the direction of rotation is reversed.
Default direction is anti-clockwise."
  (interactive "P")
  (let ((current-buffer (current-buffer)))
    (if (not (> (count-windows) 1))
        (message "You can't rotate a single window!")
      (let* ((window-refs (window-list)) ; list starts with current window
             (w1 (car window-refs))
             (w2 (if arg (car (last window-refs)) (cadr window-refs))))
        (swap--w1<->w2 w1 w2)))
    ;; Keep point in the same buffer
    (select-window (get-buffer-window current-buffer))))


(defun surround-symbol-with (c)
  "Surround the symbol at point with string C.
This works with any mode that supports thingatpt.el for a symbol."
  (interactive "cWrapper char: ")
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (s (make-string 1 c)))
    (when bounds
      ;; Insert the second delimiter first to avoid changing the
      ;; starting bound.
      (goto-char (cdr bounds))
      (insert s)
      ;; Now the first delimiter
      (goto-char (car bounds))
      (insert s))))


(defun out-or-onto-calc ()
  "Jump to the calc buffer or the editing buffer."
  (interactive)
  (if (eq major-mode 'calc-mode)
      (calc-other-window)
    (calc)))


(defun duplicate-current-line (prefix)
  "Duplicate current line.
Argument PREFIX decides whether we keep the point on current line
or the duplicated line."
  (interactive "P")
  (let* ((col (current-column))
         (beg (line-beginning-position))
         (end (line-end-position))
         (text (buffer-substring beg end)))
    (save-excursion
      (end-of-line)
      (insert (format "\n%s" text)))
    ;; When the prefix isn't supplied move the point to the next
    ;; line. It is more natural to make a copy of the first line and
    ;; edit the copy below that line.
    (when (not prefix)
      (forward-line)
      (move-to-column col))))


(defun open-woman-page (&optional cmd)
  "Open woman page for CMD  in a split right window."
  (interactive)
  (split-window-sensibly)
  (woman cmd))

(defun move-line-down ()
  "Move the current line down keeping the column."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  "Move the current line up keeping the column."
  (interactive)
  (let ((col (current-column)))
    (forward-line)
    (transpose-lines -1)
    (forward-line -1)
    (move-to-column col)))

(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters."
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))

(defun goto-char-in-line ()
  "Vim-like goto-char in Emacs.
Lands you before the character or after the last character.  Just
keep pressing the character until you get to the point.  I wrote
this and found out that there is `jump-char' which is better than
this."
  (interactive)
  (let* ((goto-char (if (eq last-command this-command)
                        (get this-command 'goto-char)
                      (put this-command 'goto-char (read-char "Goto: ")))))
    (and (looking-at (string goto-char)) (forward-char))
    (if (not (search-forward (string goto-char) (line-end-position) t))
        (message "No matches found.")
      (backward-char)
      (pulse-momentary-highlight-region (point)
                                        (1+ (point)))
      (set-transient-map (let ((map (make-sparse-keymap)))
                           (define-key map (vector goto-char)
                             #'goto-char-in-line)
                           map)))))

(defun underline-text (arg)
  "Insert a line under the current line based on prefix ARG.
It is filled with a default underline character `='.  If point had
been at the end of the line, moves point to the beginning of the
line directly following the underlining.  It does not underline
the line's leading whitespace, trailing whitespace, or comment
symbols.  With prefix `C-u' prompts user for a custom underline
character.  With prefix double prefix, do not underline whitespace
embedded in the line.
© Copyright 2015 Boruch Baum <boruch_baum@gmx.com>, GPL3+ license."
  (interactive "p")
  (let* ((original-point (point))
         (underline-char
          (replace-regexp-in-string "[[:cntrl:][:space:]]" "="
                                    (if (= arg 1)
                                        "="
                                      (char-to-string
                                       (read-char "What character to underline with?")))))
         (original-point-is-eol
          (when (looking-at "$") t))
         (original-point-is-eob
          (= original-point (point-max))))
    (beginning-of-line)
    (unless
        (when (looking-at "[[:space:]]*$")
          (beginning-of-line 0)
          (when (looking-at "[[:space:]]*$")
            (goto-char original-point)
            (message "nothing to do")))
      (insert
       (buffer-substring (line-beginning-position) (line-end-position))
       "\n")
      (save-restriction
        (narrow-to-region
         (progn
           (goto-char (1- (re-search-forward "[^[:space:]]" nil t)))
           (cond
            ((looking-at ";+")   (match-end 0))
            ((looking-at "#+")   (match-end 0))
            ((looking-at "//+")  (match-end 0))
            ((looking-at "/\\*+") (match-end 0))
            (t (point))))
         (1+ (progn
               (goto-char (line-end-position))
               (re-search-backward "[^[:space:]]" nil t))))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (if (= arg 16)
            (while (re-search-forward "[^[:space:]]" nil t)
              (replace-match underline-char nil))
          (re-search-forward "[^[:space:]]" nil t)
          (goto-char (1- (point)))
          (while (re-search-forward "." nil t)
            (replace-match underline-char nil)))
        (widen))
      (if original-point-is-eob
          (goto-char (point-max))
        (if original-point-is-eol
            (goto-char (re-search-forward "^"))
          (goto-char original-point))))))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting.
Credits: Emacs Prelude"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defvar quick-switch-themes
  (let ((themes-list (list 'jazz
                           'vicarie-and-blackboard
                           'tangotango
                           'dracula)))
    (nconc themes-list themes-list))
  "A circular list of themes to keep switching between.
Make sure that the currently enabled theme is at the head of this
list always.

A nil value implies no custom theme should be enabled.")

(defun quick-switch-themes* ()
  "Switch between to commonly used faces in Emacs.
One for writing code and the other for reading articles."
  (interactive)
  (if-let* ((next-theme (cadr quick-switch-themes)))
      (progn (when-let* ((current-theme (car quick-switch-themes)))
               (disable-theme (car quick-switch-themes)))
             (load-theme next-theme t)
             (message "Loaded theme: %s" next-theme))
    ;; Always have the dark mode-line theme
    (mapc #'disable-theme (delq 'smart-mode-line-dark custom-enabled-themes)))
  (setq quick-switch-themes (cdr quick-switch-themes)))

(defun repeat-command (command &optional message-fn)
  "Make a repeatable command out of COMMAND.
Optional argument MESSAGE-FN (defaults to 'ignore) is used to
print information about what repeat is doing."
  (let ((repeat-message-function (or message-fn 'ignore)))
    (setq last-repeatable-command command)
    (repeat nil)))

(defun quick-switch-themes ()
  "A repeatable version of `quick-switch-themes'.
The last key in the key binding can be used for repeating it."
  (interactive)
  (repeat-command 'quick-switch-themes*))

(defmacro def-lineup (command docstring &rest commands)
  "Make a COMMAND with DOCSTRING to run COMMANDS in order on repeated usage.
Currently works for commands that need no interactive input.  It
automatically makes the sequence a circular sequence
logically.  That's because `(car nil)' equals nil.

Argument DOCSTRING would serve as the docstring for COMMAND.

Example usage:
\(def-lineup fill-unfill
            \"Cycle between filling a line, filling a para and unfill.\"
            #'auto-fill-current-line #'fill-paragraph #'unfill-paragraph)

This would define a new command with the name fill-unfill that
would run commands in '(auto-fill-current-line fill-paragraph unfill-paragraph)
one after the other in a circular fashion, when called repeatedly."
  `(defun ,command () ,docstring
          (interactive)
          (let* ((command-sequence
                  (if-let* ((cseq (get this-command 'command-sequence))
                            ((eq last-command this-command)))
                      cseq
                    (list ,@commands)))
                 (next-command (car command-sequence)))
            (message (format "-> %s" next-command))
            (call-interactively next-command)
            (put this-command 'command-sequence (cdr command-sequence)))))

(defun enable-mode-for (mode secs)
  "Enable MODE for SECS seconds and disable it again.
Assume that (MODE +1) enables the mode and (MODE -1) disables
it."
  (funcall mode +1)
  (run-with-timer secs nil (lambda () (funcall mode -1))))

(defmacro gen-prefixed (command)
  "Create command with COMMAND that is called with a raw prefix arg.
Generated command is `command/prefixed'."
  `(defun ,(intern (concat (symbol-name command) "/prefixed")) ()
     (interactive)
     ,(concat "Calls `" (symbol-name command) "' with a prefix.")
     (funcall (function ,command) '(4))))

(defun split-money (amount)
  "Function to split expense of value AMOUNT  between two people.
Since I do not keep an account of change (< ₹ 10), I would add
enough expense for myself so as to make it a multiple of 10.

Return: (list expenses-noted-by-me
              expense-for-me
              expense-for-other-party)

Note: Don't waste your time trying to understanding all this."
  (let* ((change (mod amount 10))
         (expense-noted (+ amount (* (- 10 change) (cl-signum change))))
         (expense-for-other-party (/ amount 2.0)))
    (list expense-noted
          (- expense-noted expense-for-other-party)
          expense-for-other-party)))

(defmacro quietly (&rest forms)
  "Inhibit messages while evaluatings FORMS."
  `(let ((inhibit-message t))
     ,@forms))

(defun rebuild-emacs-on-linux ()
  "Rebuild Emacs from master.
Assumes that binary `emacs` is in PATH and symlinked to the real
binary file."
  (interactive)
  (async-shell-command
   "file `readlink $(which emacs)`;
	if [[ $? -eq 0 ]]; then
		# emacs/src
		cd $(dirname `readlink $(which emacs)`);
		# emacs/
        cd ..;
		git pull;
		while [[ $? -ne 0 ]]; do
			sleep 1m;
			notify-send \"Emacs : git pull failed\. Retrying...\";
			git pull;
		done
		make all;
		notify-send \"Built Emacs $(emacs --version | head -1)!\";
		notify-send  \"$(git log --oneline -1)\" ;
	else
		notify-send --urgency critical \"Something went wrong with building Emacs!\";
    fi"))

(defun utils-easy-move (map)
  "Set key-bindings (in MAP) for easier navigation."
  (interactive)
  (define-key map (kbd "h") 'backward-char)
  (define-key map (kbd "l") 'forward-char)
  (define-key map (kbd "j") 'next-line)
  (define-key map (kbd "k") 'previous-line)
  (define-key map (kbd "n") 'next-line)
  (define-key map (kbd "p") 'previous-line)
  map)

(defun utils-uneasy-move (map)
  "Reset key bindings (in MAP) set by easy-move."
  (interactive)
  (define-key map (kbd "h"))
  (define-key map (kbd "l"))
  (define-key map (kbd "j"))
  (define-key map (kbd "k"))
  (define-key map (kbd "n"))
  (define-key map (kbd "p"))
  map)

;;;###autoload
(define-minor-mode utils-easy-move-mode
  "A mode for bindings in a read only environment. Mimicks vim."
  nil
  "Easy"
  (utils-easy-move (make-sparse-keymap)))

(defun format-alist (x)
  "Format alist X for display."
  (let ((result ""))
    (dolist (kv x)
      (setq result (format "%s\n%s %s %s"
                           result
                           (car kv) "⟶" (cdr kv))))
    result))

(defun get-coin-price (coin-name)
  "Get rate for COIN-NAME.
Uses https://api.coinmarketcap.com/v1/ticker/$COIN-NAME/"
  (interactive "sCoin: ")
  (request (format "https://api.coinmarketcap.com/v1/ticker/%s/" coin-name)
           :parser 'json-read
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (let* ((result (aref data 0))
                              (coin-id (alist-get 'id result))
                              (usd-price (alist-get 'price_usd result)))
                         (display-message-or-buffer
                          (format "%s ($ %s)\n%s"
                                  coin-id
                                  usd-price
                                  (format-alist result))))))
           :error (cl-function
                   (lambda (&key error-thrown &allow-other-keys)
                     (message "Failed with: %s" error-thrown)))))

(defun do-when-idle (f g interval)
  "Call F when idle for INTERVAL seconds and then G when there is activity.
Thanks to Michael Heerdegen <michael_heerdegen@web.de>."
  (letrec ((run-timer-fun (lambda ()
                            (run-with-idle-timer interval
                                                 nil
                                                 (lambda ()
                                                   (funcall f)
                                                   (add-hook 'post-command-hook
                                                             activity-fun)))))
           (activity-fun (lambda ()
                           (remove-hook 'post-command-hook activity-fun)
                           (funcall g)
                           (funcall run-timer-fun))))
    (funcall run-timer-fun)))

(defvar limit-usage (make-hash-table :test 'equal))
(defun limit-usage (command max-minutes)
  "Limit usage of COMMAND to only once in MAX-MINUTES."
  (advice-add command
              :around
              `(lambda (orig-fn &rest args)
                 (let* ((hash-key (intern ,(symbol-name command)))
                        (last-ts (gethash hash-key limit-usage))
                        (gap-in-minutes (and last-ts
                                             (/ (time-to-seconds
                                                 (time-subtract (current-time)
                                                                last-ts))
                                                60))))
                   (if (and gap-in-minutes (< gap-in-minutes ,max-minutes))
                       (message "Last Accessed: %s Time to go: %.2f minutes"
                                (format-time-string "%FT%T%z" last-ts)
                                (- ,max-minutes gap-in-minutes))
                     (apply orig-fn args)
                     (puthash hash-key (current-time) limit-usage))))))

(defun overlays-to-text ()
  "Converts overlays to text.
From: https://www.emacswiki.org/emacs/OverlaysToText"
  (interactive)
  (let ((tb (get-buffer-create "*text*"))
        (s (point-min))
	    (os (overlays-in (point-min) (point-max))))
    (with-current-buffer tb
      (erase-buffer))
    (setq os (sort os (lambda (o1 o2)
			            (< (overlay-start o1)
			               (overlay-start o2)))))
    (mapc (lambda (o)
            (let ((bt (buffer-substring-no-properties s (overlay-start o)))
                  (b (overlay-get o 'before-string))
                  (text (or (overlay-get o 'display)
                            (buffer-substring-no-properties (overlay-start o) (overlay-end o))))
                  (a (overlay-get o 'after-string))
                  (inv (overlay-get o 'invisible)))
              (with-current-buffer tb
                (insert bt)
                (unless inv
                  (when b (insert b))
                  (insert text)
                  (when a (insert a))))
              (setq s (overlay-end o))))
          os)
    (let ((x (buffer-substring-no-properties s (point-max))))
      (with-current-buffer tb
        (insert x)))
    (pop-to-buffer tb)))

(provide 'defs)
;;; defs.el ends here

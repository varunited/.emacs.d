;;; prodigy-service-defs.el --- Services for prodigy.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience

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

;; Definitions of services that can be used by prodigy.el

;;; Code:

(require 'prodigy)

(prodigy-define-service
  :name "blog@localhost"
  :command "python2"
  :args '("-m" "SimpleHTTPServer" "8000")
  :cwd "~/blog/"
  :tags '(file-server)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "comms"
  :command "emacs"
  :args '("--name" "SecondEmacs"
          "--eval" "(progn (gnus) (elfeed) (erc-connect))")
  :cwd "~"
  :tags '(comms)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(provide 'prodigy-service-defs)
;;; prodigy-service-defs.el ends here

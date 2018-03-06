;;; elfeed-config.el --- My elfeed config            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi(require 'cl-lib) <narendraj9@gmail.com>
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

;;

;;; Code:

(require 'elfeed)
(require 'youtube-dl)

(setq-default elfeed-search-filter "-junk @1-week-ago +unread")

;; More keybindings
(define-key elfeed-search-mode-map (kbd "j") #'next-line)
(define-key elfeed-search-mode-map (kbd "k") #'previous-line)
(define-key elfeed-show-mode-map (kbd "j") #'next-line)
(define-key elfeed-show-mode-map (kbd "k") #'previous-line)

(setq elfeed-feeds
      '(("http://cachestocaches.com/feed/" emacs blog)
        ("http://cestlaz.github.io/rss.xml" emacs blog)
        ("https://news.ycombinator.com/rss" hn)
        ("https://www.schneier.com/blog/atom.xml" security)
        ("http://www.allthingsdistributed.com/atom.xml" distributed)
        ("https://martinfowler.com/feed.atom" blog design)
        ("http://backtracks.fm/ycombinator/ycombinator/feed" blog)
        ("http://blog.cryptographyengineering.com/feeds/posts/default" blog)
        ("http://accidental-art.tumblr.com/rss" image math)
        ("http://english.bouletcorp.com/feed/" comic)
        ("http://amitp.blogspot.com/feeds/posts/default" blog dev)
        ("http://bit-player.org/feed" blog math)
        ("http://simblob.blogspot.com/feeds/posts/default" blog dev)
        ("http://blog.carlosgaldino.com/atom.xml" blog dev)
        ("https://utcc.utoronto.ca/~cks/space/blog/?atom" blog dev)
        ("https://blog.coinbase.com/rss/" product bitcoin)
        ("http://www.commitstrip.com/en/feed/" comic dev)
        ("http://www.bitercomics.com/feed/" comic)
        ("http://feeds.feedburner.com/Buttersafe" comic)
        ("http://feeds.feedburner.com/CatVersusHuman" comic)
        ("http://chainsawsuit.com/feed/" comic)
        ("http://feeds.feedburner.com/channelATE" comic)
        ("http://deep-dark-fears.tumblr.com/rss" comic)
        ("https://www.blogger.com/feeds/19727420/posts/default" blog)
        ("https://www.debian.org/security/dsa" debian list security important)
        ("https://www.debian.org/News/news" debian list)
        ("http://dvdp.tumblr.com/rss" image)
        ("https://www.digitalocean.com/blog/feed" blog product)
        ("http://eli.thegreenplace.net/feeds/all.atom.xml" blog dev)
        ("http://emacshorrors.com/feed.atom" blog emacs)
        ("http://feeds.exploringbinary.com/ExploringBinary" blog dev)
        ("http://feeds.feedburner.com/Explosm" comic)
        ("http://www.extrafabulouscomics.com/1/feed" comic)
        ("http://www.exocomics.com/feed" comic)
        ("http://www.tedunangst.com/flak/rss" dev blog)
        ("http://firefly.nu/feeds/all.atom.xml" blog dev)
        ("http://feeds.feedburner.com/Pidjin" comic)
        ("http://www.goneintorapture.com/rss" comic)
        ("http://www.businesscat.happyjar.com/feed/" comic)
        ("http://feeds.feedburner.com/InvisibleBread" comic)
        ("http://blog.ioactive.com/feeds/posts/default" blog security)
        ("http://irreal.org/blog/?feed=rss2" blog)
        ("http://blog.reverberate.org/feeds/posts/default" dev blog)
        ("http://feeds.feedburner.com/lefthandedtoons/awesome" comic)
        ("http://gottwurfelt.wordpress.com/feed/" blog math)
        ("http://feeds.feedburner.com/LoadingArtist" comic)
        ("https://www.masteringemacs.org/feed" blog emacs)
        ("http://steve-yegge.blogspot.com/atom.xml" blog dev)
        ("http://www.mazelog.com/rss" math puzzle)
        ("http://www.mrlovenstein.com/rss.xml" comic)
        ("http://mortoray.com/feed/" blog dev)
        ("http://nedroid.com/feed/" comic)
        ("https://nickdesaulniers.github.io/atom.xml" blog dev)
        ("http://nullprogram.com/feed/" blog dev nullprogram)
        ("https://blogs.msdn.microsoft.com/oldnewthing/feed" blog dev)
        ("http://www.optipess.com/feed/" comic)
        ("http://piecomic.tumblr.com/rss" comic)
        ("http://planet.emacsen.org/atom.xml" emacs planet)
        ("http://possiblywrong.wordpress.com/feed/" blog math puzzle)
        ("http://feeds.wnyc.org/radiolab" audio)
        ("http://www.safelyendangered.com/feed/" comic)
        ("https://www.schneier.com/blog/atom.xml" blog security)
        ("http://www.smbc-comics.com/rss.php" comic)
        ("http://www.howstuffworks.com/podcasts/stuff-you-should-know.rss" audio)
        ("https://github.com/blog/all.atom" blog dev product)
        ("http://blog.plover.com/index.atom" blog dev)
        ("http://use-the-index-luke.com/blog/feed" blog dev databases)
        ("http://slatestarcodex.com/feed/" blog philosophy)
        ("http://www.thingsinsquares.com/feed/" comic)
        ("http://what-if.xkcd.com/feed.atom" blog)
        ("http://www.whompcomic.com/rss.php" comic)
        ("http://xkcd.com/atom.xml" comic)
        ("http://www.reddit.com/r/dailyprogrammer/.rss" subreddit)
        ("http://www.reddit.com/user/JimKB/submitted.rss" comic)
        ("https://www.snellman.net/blog/rss-index.xml" dev networking)
        ("1veritasium" youtube)
        ("http://www.aaronsw.com/2002/feeds/pgessays.rss" blog) ; Paul Graham's essays
        ("UCQvdU25Eqk3YS9-QnILhKKQ" youtube)
        ("https://vicarie.in/archive.xml")
        ("https://apfelmus.nfshost.com/rss.xml" blog haskell)
        ("http://blog.twonegatives.com/rss" blog) ; Two negatives
        ("UCYO_jab_esuFRV4b17AJtAw" youtube)      ; 3Blue1Brown
        ("adric22" youtube)                       ; The 8-bit Guy
        ("whoisjimmy" youtube)                    ; How Ridiculous
        ("UCXNxwOuuR7LT-SkEfOJiwgA" youtube)      ; Long Plays
        ("UCO8DQrSp5yEP937qNqTooOw" youtube)      ; Strange Parts
        ("UCwRqWnW5ZkVaP_lZF7caZ-g" youtube)      ; Retro Game Mechanics
        ("UCsXVk37bltHxD1rDPwtNM8Q" youtube)))    ; Kurzgesagt – In a Nutshell

;; Strike-through all read articles
(set-face-attribute 'elfeed-search-title-face nil :strike-through t)
(set-face-attribute 'elfeed-search-unread-title-face nil :strike-through nil)

;; With 16 connections, my Emacs slows down a bit.
(elfeed-set-max-connections 8)

(provide 'elfeed-config)
;;; elfeed-config.el ends here

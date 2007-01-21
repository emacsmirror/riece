;;; riece-mcat.el --- message catalog for Japanese -*- coding: iso-2022-jp -*-
;; Copyright (C) 2007 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>

;; This file is part of Riece.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; To update riece-mcat-japanese-alist, do `make update-mcat'.

;;; Code:

(defconst riece-mcat-japanese-alist
  '(("%s killed %s" . "%s $B$,(B %s $B$r;&$7$^$7$?(B")
    ("%s has left IRC" . "%s $B$,(B IRC $B$rN%$l$^$7$?(B")
    ("%s has left %s" . "%s $B$,(B %s $B$rN%$l$^$7$?(B")
    ("Topic on %s by %s: %s" . "%s $B$N%H%T%C%/$,(B %s $B$K$h$j@_Dj$5$l$^$7$?(B: %s")
    ("Topic by %s: %s\n" . "%s $B$K$h$k%H%T%C%/@_Dj(B: %s\n")
    ("Mode on %s by %s: %s" . "%s $B$N%b!<%I$,(B %s $B$K$h$j@_Dj$5$l$^$7$?(B: %s")
    ("Mode by %s: %s\n" . "%s $B$K$h$k%b!<%I@_Dj(B: %s\n")
    ("%s invites %s to %s" . "%s $B$,(B %s $B$r(B %s $B$K>7BT$7$F$$$^$9(B")
    ("%s kicked %s out from %s\n" . "%s $B$,(B %s $B$r(B %s $B$+$i=3$j=P$7$^$7$?(B\n")
    ("%s kicked %s out from %s" . "%s $B$,(B %s $B$r(B %s $B$+$i=3$j=P$7$^$7$?(B")
    ("%s (%s) has joined %s\n" . "%s (%s) $B$,(B %s $B$K;22C$7$^$7$?(B")
    ("%s (%s) has joined %s" . "%s (%s) $B$,(B %s $B$K;22C$7$^$7$?(B")
    ("Sending QUIT...done" . "QUIT $B$rAw?.$7$F$$$^$9(B...$B40N;(B")
    ("Sending QUIT..." . "QUIT $B$rAw?.$7$F$$$^$9(B...")
    ("Sending QUIT to \"%s\"...done" . "\"%s\" $B$K(B QUIT $B$rAw?.$7$F$$$^$9(B...$B40N;(B")
    ("Sending QUIT to \"%s\"..." . "\"%s\" $B$K(B QUIT $B$rAw?.$7$F$$$^$9(B...")
    ("Password: Quit" . "$B%Q%9%o!<%I(B: $BCf;_(B")
    ("Password: " . "$B%Q%9%o!<%I(B: ")
    ("Password for %s: Quit" . "%s $B$N%Q%9%o!<%I(B: $BCf;_(B")
    ("Password for %s: " . "%s $B$N%Q%9%o!<%I(B: ")
    ("Logging in to IRC server..." . "IRC $B%5!<%P$K%m%0%$%s$7$F$$$^$9(B...")
    ("Logging in to %s..." . "%s $B$K%m%0%$%s$7$F$$$^$9(B...")
    ("Connecting to IRC server...failed: %S" . "IRC $B%5!<%P$K@\B3$7$F$$$^$9(B...$B<:GT(B: %S")
    ("Connecting to IRC server...done" . "IRC $B%5!<%P$K@\B3$7$F$$$^$9(B...$B40N;(B")
    ("Connecting to IRC server..." . "IRC $B%5!<%P$K@\B3$7$F$$$^$9(B...")
    ("Connecting to %s...failed: %S" . "%s $B$K@\B3$7$F$$$^$9(B...$B<:GT(B: %S")
    ("Connecting to %s...done" . "%s $B$K@\B3$7$F$$$^$9(B...$B40N;(B")
    ("Connecting to %s..." . "%s $B$K@\B3$7$F$$$^$9(B...")
    ("Logging in to IRC server...done" . "IRC $B%5!<%P$K%m%0%$%s$7$F$$$^$9(B...$B40N;(B")
    ("Logging in to %s...done" . "%s $B$K%m%0%$%s$7$F$$$^$9(B...$B40N;(B")
    ("Already registered" . "$BEPO?:Q$_$G$9(B")
    ("Really quit IRC? " . "$BK\Ev$K(B IRC $B$r$d$a$^$9$+(B? ")))

(provide 'riece-mcat-japanese)

;;; riece-mcat-japanese.el ends here

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
  '(("%d users on %s: " . "%d $B?M$,(B %s $B$K$$$^$9(B: ")
    ("%d users: " . "%d $B?M(B: ")
    ("%s (%s) has joined %s" . "%s (%s) $B$,(B %s $B$K;22C$7$^$7$?(B")
    ("%s (%s) has joined %s\n" . "%s (%s) $B$,(B %s $B$K;22C$7$^$7$?(B\n")
    ("%s has left %s" . "%s $B$,(B %s $B$rN%$l$^$7$?(B")
    ("%s has left IRC" . "%s $B$,(B IRC $B$rN%$l$^$7$?(B")
    ("%s invites %s to %s" . "%s $B$,(B %s $B$r(B %s $B$K>7BT$7$F$$$^$9(B")
    ("%s is %s (%s)" . "%s $B$O(B %s (%s)")
    ("%s is %s idle" . "%s $B$O(B %s $B%"%$%I%k>uBV(B")
    ("%s is (%s)" . "%s $B$O(B (%s)")
    ("%s is away: %s" . "%s $B$ON%@JCf(B: %s")
    ("%s is running on %s: %s" . "%s $B$,(B %s $B$GF0$$$F$$$^$9(B: %s")
    ("%s kicked %s out from %s" . "%s $B$,(B %s $B$r(B %s $B$+$i=3$j=P$7$^$7$?(B")
    ("%s kicked %s out from %s\n" . "%s $B$,(B %s $B$r(B %s $B$+$i=3$j=P$7$^$7$?(B\n")
    ("%s killed %s" . "%s $B$,(B %s $B$r;&$7$^$7$?(B")
    ("%s users, topic: %s\n" . "%s $B?M!"%H%T%C%/(B: %s\n")
    ("%s: %s users, topic: %s" . "%s: %s $B?M!"%H%T%C%/(B: %s\n")
    ("Already registered" . "$BEPO?:Q$_$G$9(B")
    ("Away message: " . "$BN%@J$N%a%C%;!<%8(B: ")
    ("Beginning of buffer" . "$B%P%C%U%!$N@hF,$G$9(B")
    ("Can't find completion for \"%s\"" . "\"%s\" $B$KBP$9$kJd40$,8+$D$+$j$^$;$s(B")
    ("Change layout: " . "$BJQ998e$N%l%$%"%&%H(B: ")
    ("Change mode for channel/user: " . "$B%b!<%I$rJQ99$9$k%A%c%s%M%k$^$?$O%f!<%6(B: ")
    ("Close server: " . "$B@\B3$rJD$8$k%5!<%P(B: ")
    ("Command to execute on \"%s\":" . "\"%s\" $B$G<B9T$9$k%3%^%s%I(B: ")
    ("Connecting to %s..." . "%s $B$K@\B3$7$F$$$^$9(B...")
    ("Connecting to %s...done" . "%s $B$K@\B3$7$F$$$^$9(B...$B40N;(B")
    ("Connecting to %s...failed: %S" . "%s $B$K@\B3$7$F$$$^$9(B...$B<:GT(B: %S")
    ("Connecting to IRC server..." . "IRC $B%5!<%P$K@\B3$7$F$$$^$9(B...")
    ("Connecting to IRC server...done" . "IRC $B%5!<%P$K@\B3$7$F$$$^$9(B...$B40N;(B")
    ("Connecting to IRC server...failed: %S" . "IRC $B%5!<%P$K@\B3$7$F$$$^$9(B...$B<:GT(B: %S")
    ("Created on %s\n" . "%s $B$K:n@.$5$l$^$7$?(B\n")
    ("End of buffer" . "$B%P%C%U%!$N=*C<$G$9(B")
    ("Erroneous nickname \"%s\".  Choose a new one: " . "$BIT@5$J%K%C%/%M!<%`(B \"%s\"$B!#?7$7$$%K%C%/%M!<%`(B: ")
    ("Finger user: " . "$B?H85$rD4$Y$k%f!<%6(B: ")
    ("Invite user: " . "$B>7BT$9$k%f!<%6(B: ")
    ("Inviting %s\n" . "%s $B$r>7BT$7$F$$$^$9(B\n")
    ("Inviting %s to %s" . "%s $B$r(B %s $B$K>7BT$7$F$$$^$9(B")
    ("Join channel/user (default %s): " . "$B;22C$9$k%A%c%s%M%k$^$?$O%f!<%6(B ($B4{DjCM(B %s): ")
    ("Join channel/user: " . "$B;22C$9$k%A%c%s%M%k$^$?$O%f!<%6(B: ")
    ("Key for %s: " . "%s $B$N%-!<(B: ")
    ("Key for %s: Quit" . "%s $B$N%-!<(B: $BCf;_(B")
    ("Kick user: " . "$B=3$j=P$9%f!<%6(B: ")
    ("LIST pattern: " . "LIST $B$N%Q%?!<%s(B: ")
    ("Logging in to %s..." . "%s $B$K%m%0%$%s$7$F$$$^$9(B...")
    ("Logging in to %s...done" . "%s $B$K%m%0%$%s$7$F$$$^$9(B...$B40N;(B")
    ("Logging in to IRC server..." . "IRC $B%5!<%P$K%m%0%$%s$7$F$$$^$9(B...")
    ("Logging in to IRC server...done" . "IRC $B%5!<%P$K%m%0%$%s$7$F$$$^$9(B...$B40N;(B")
    ("Message to user: " . "$B%f!<%6$X$N%a%C%;!<%8(B: ")
    ("Message: " . "$B%a%C%;!<%8(B")
    ("Mode (? for help)" . "$B%b!<%I(B ($B%X%k%W$O(B ?)")
    ("Mode by %s: %s\n" . "%s $B$K$h$k%b!<%I@_Dj(B: %s\n")
    ("Mode for %s: %s" . "%s $B$N%b!<%I(B: %s")
    ("Mode on %s by %s: %s" . "%s $B$N%b!<%I$,(B %s $B$K$h$j@_Dj$5$l$^$7$?(B: %s")
    ("Mode: " . "$B%b!<%I(B: ")
    ("NAMES pattern: " . "NAMES $B$N%Q%?!<%s(B: ")
    ("Nickname \"%s\" already in use.  Choose a new one: " . "$B%K%C%/%M!<%`(B \"%s\" $B$O4{$K;HMQ$5$l$F$$$^$9!#?7$7$$%K%C%/%M!<%`(B: ")
    ("No changes made.  Save anyway? " . "$BJQ99$,$"$j$^$;$s$,!"J]B8$7$^$9$+(B? ")
    ("No channel" . "$B%A%c%s%M%k$J$7(B")
    ("No server process" . "$B%5!<%P$N%W%m%;%9$,$"$j$^$;$s(B")
    ("No text to send" . "$BAw?.$9$k%F%-%9%H$,$"$j$^$;$s(B")
    ("None" . "$B$J$7(B")
    ("Online: " . "$B%*%s%i%$%s(B: ")
    ("Open server: " . "$B@\B3$9$k%5!<%P(B: ")
    ("Part from channel/user (default %s): " . "$BN%C&$9$k%A%c%s%M%k$^$?$O%f!<%6(B ($B4{DjCM(B %s): ")
    ("Password for %s: " . "%s $B$N%Q%9%o!<%I(B: ")
    ("Password for %s: Quit" . "%s $B$N%Q%9%o!<%I(B: $BCf;_(B")
    ("Password incorrect from %s." . "%s $B$N%Q%9%o!<%I$,IT@5$G$9!#(B")
    ("Password: " . "$B%Q%9%o!<%I(B: ")
    ("Password: Quit" . "$B%Q%9%o!<%I(B: $BCf;_(B")
    ("Really quit IRC? " . "$BK\Ev$K(B IRC $B$r$d$a$^$9$+(B? ")
    ("Really want to query LIST without argument? " . "$BK\Ev$K0z?t$J$7$N(B LIST $B$rH/9T$7$^$9$+(B? ")
    ("Really want to query NAMES without argument? " . "$BK\Ev$K0z?t$J$7$N(B NAMES $B$rH/9T$7$^$9$+(B? ")
    ("Really want to query WHO without argument? " . "$BK\Ev$K0z?t$J$7$N(B WHO $B$rH/9T$7$^$9$+(B? ")
    ("Recent messages of the day:\n" . "$B:G6a$N%a%C%;!<%8(B ($B:#F|Cf(B):\n")
    ("Recent messages up to %d lines:\n" . "$B:G6a$N%a%C%;!<%8(B (%d $B9T$^$G(B):\n")
    ("Sending QUIT to \"%s\"..." . "\"%s\" $B$K(B QUIT $B$rAw?.$7$F$$$^$9(B...")
    ("Sending QUIT to \"%s\"...done" . "\"%s\" $B$K(B QUIT $B$rAw?.$7$F$$$^$9(B...$B40N;(B")
    ("Sending QUIT..." . "QUIT $B$rAw?.$7$F$$$^$9(B...")
    ("Sending QUIT...done" . "QUIT $B$rAw?.$7$F$$$^$9(B...$B40N;(B")
    ("Server: " . "$B%5!<%P(B")
    ("Set +o for users" . "+o $B$9$k%f!<%6(B")
    ("Set +v for users" . "+v $B$9$k%f!<%6(B")
    ("Set topic: " . "$B?7$7$$%H%T%C%/(B: ")
    ("Switch to channel/user: " . "$B0\F0@h$N%A%c%s%M%k$^$?$O%f!<%6(B: ")
    ("Switch to number: " . "$B0\F0@h$NHV9f(B: ")
    ("Topic by %s: %s\n" . "%s $B$K$h$k%H%T%C%/@_Dj(B: %s\n")
    ("Topic for %s: %s" . "%s $B$N%H%T%C%/(B: ")
    ("Topic on %s by %s: %s" . "%s $B$N%H%T%C%/$,(B %s $B$K$h$j@_Dj$5$l$^$7$?(B: %s")
    ("Topic: " . "$B%H%T%C%/(B: ")
    ("Type \\[describe-mode] for help" . "$B%X%k%W$r8+$k$K$O(B \\[describe-mode]")
    ("Unset +o for users" . "-o $B$9$k%f!<%6(B")
    ("Unset +v for users" . "-v $B$9$k%f!<%6(B")
    ("WHO pattern: " . "WHO $B$N%Q%?!<%s(B: ")
    ("[Available modes: " . "[$B;HMQ2DG=$J%b!<%I(B: ")
    ("days" . "$BF|(B")
    ("hours" . "$B;~4V(B")
    ("minutes" . "$BJ,(B")
    ("on via server %s: %s" . "$B%5!<%P(B %s $B7PM3(B: %s")
    ("seconds" . "$BIC(B")))

(provide 'riece-mcat-japanese)

;;; riece-mcat-japanese.el ends here

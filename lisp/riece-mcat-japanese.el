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

;; To update riece-mcat-japanese-alist, `make update-mcat'.

;;; Code:

(defconst riece-mcat-japanese-alist
  '(("Sending QUIT...done" . "QUIT を送信しています...完了")
    ("Sending QUIT..." . "QUIT を送信しています...")
    ("Sending QUIT to \"%s\"...done" . "\"%s\" に QUIT を送信しています...完了")
    ("Sending QUIT to \"%s\"..." . "\"%s\" に QUIT を送信しています...")
    ("Password: Quit" . "パスワード: 中止")
    ("Password: " . "パスワード: ")
    ("Password for %s: Quit" . "%s のパスワード: 中止")
    ("Password for %s: " . "%s のパスワード: ")
    ("Logging in to IRC server..." . "IRC サーバにログインしています...")
    ("Logging in to %s..." . "%s にログインしています...")
    ("Connecting to IRC server...failed: %S" . "IRC サーバに接続しています...失敗: %S")
    ("Connecting to IRC server...done" . "IRC サーバに接続しています...完了")
    ("Connecting to IRC server..." . "IRC サーバに接続しています...")
    ("Connecting to %s...failed: %S" . "%s に接続しています...失敗: %S")
    ("Connecting to %s...done" . "%s に接続しています...完了")
    ("Connecting to %s..." . "%s に接続しています...")
    ("Logging in to IRC server...done" . "IRC サーバにログインしています...完了")
    ("Logging in to %s...done" . "%s にログインしています...完了")
    ("Already registered" . "登録済みです")
    ("Really quit IRC? " . "本当に IRC をやめますか? ")))

(provide 'riece-mcat-japanese)

;;; riece-mcat-japanese.el ends here

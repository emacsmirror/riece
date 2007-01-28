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
  '(("%s (%s) has joined %s" . "%s (%s) が %s に参加しました")
    ("%s (%s) has joined %s\n" . "%s (%s) が %s に参加しました\n")
    ("%s has left %s" . "%s が %s を離れました")
    ("%s has left IRC" . "%s が IRC を離れました")
    ("%s invites %s to %s" . "%s が %s を %s に招待しています")
    ("%s kicked %s out from %s" . "%s が %s を %s から蹴り出しました")
    ("%s kicked %s out from %s\n" . "%s が %s を %s から蹴り出しました\n")
    ("%s killed %s" . "%s が %s を殺しました")
    ("Already registered" . "登録済みです")
    ("Connecting to %s..." . "%s に接続しています...")
    ("Connecting to %s...done" . "%s に接続しています...完了")
    ("Connecting to %s...failed: %S" . "%s に接続しています...失敗: %S")
    ("Connecting to IRC server..." . "IRC サーバに接続しています...")
    ("Connecting to IRC server...done" . "IRC サーバに接続しています...完了")
    ("Connecting to IRC server...failed: %S" . "IRC サーバに接続しています...失敗: %S")
    ("Created on %s\n" . "%s に作成されました\n")
    ("Erroneous nickname \"%s\".  Choose a new one: " . "謝ったニックネーム \"%s\"。新しいニックネーム: ")
    ("Key for %s: " . "%s のキー: ")
    ("Key for %s: Quit" . "%s のキー: 中止")
    ("Logging in to %s..." . "%s にログインしています...")
    ("Logging in to %s...done" . "%s にログインしています...完了")
    ("Logging in to IRC server..." . "IRC サーバにログインしています...")
    ("Logging in to IRC server...done" . "IRC サーバにログインしています...完了")
    ("Mode by %s: %s\n" . "%s によるモード設定: %s\n")
    ("Mode on %s by %s: %s" . "%s のモードが %s により設定されました: %s")
    ("Nickname \"%s\" already in use.  Choose a new one: " . "ニックネーム \"%s\" は既に使用されています。新しいニックネーム: ")
    ("No channel" . "チャンネルなし")
    ("None" . "なし")
    ("Password for %s: " . "%s のパスワード: ")
    ("Password for %s: Quit" . "%s のパスワード: 中止")
    ("Password incorrect from %s." . "%s のパスワードが不正です。")
    ("Password: " . "パスワード: ")
    ("Password: Quit" . "パスワード: 中止")
    ("Really quit IRC? " . "本当に IRC をやめますか? ")
    ("Recent messages of the day:\n" . "最近のメッセージ(今日中):\n")
    ("Recent messages up to %d lines:\n" . "最近のメッセージ(%d行):\n")
    ("Sending QUIT to \"%s\"..." . "\"%s\" に QUIT を送信しています...")
    ("Sending QUIT to \"%s\"...done" . "\"%s\" に QUIT を送信しています...完了")
    ("Sending QUIT..." . "QUIT を送信しています...")
    ("Sending QUIT...done" . "QUIT を送信しています...完了")
    ("Server: " . "サーバ")
    ("Topic by %s: %s\n" . "%s によるトピック設定: %s\n")
    ("Topic on %s by %s: %s" . "%s のトピックが %s により設定されました: %s")
    ("Type \\[describe-mode] for help" . "ヘルプを見るには \\[describe-mode]")))

(provide 'riece-mcat-japanese)

;;; riece-mcat-japanese.el ends here

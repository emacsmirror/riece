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
  '(("%d users on %s: " . "%d 人が %s にいます: ")
    ("%d users: " . "%d 人: ")
    ("%s (%s) has joined %s" . "%s (%s) が %s に参加しました")
    ("%s (%s) has joined %s\n" . "%s (%s) が %s に参加しました\n")
    ("%s has left %s" . "%s が %s を離れました")
    ("%s has left IRC" . "%s が IRC を離れました")
    ("%s invites %s to %s" . "%s が %s を %s に招待しています")
    ("%s is %s (%s)" . "%s は %s (%s)")
    ("%s is %s idle" . "%s は %s アイドル状態")
    ("%s is (%s)" . "%s は (%s)")
    ("%s is away: %s" . "%s は離席中: %s")
    ("%s is running on %s: %s" . "%s が %s で動いています: %s")
    ("%s kicked %s out from %s" . "%s が %s を %s から蹴り出しました")
    ("%s kicked %s out from %s\n" . "%s が %s を %s から蹴り出しました\n")
    ("%s killed %s" . "%s が %s を殺しました")
    ("%s users, topic: %s\n" . "%s 人、トピック: %s\n")
    ("%s: %s users, topic: %s" . "%s: %s 人、トピック: %s\n")
    ("Already registered" . "登録済みです")
    ("Away message: " . "離席のメッセージ: ")
    ("Beginning of buffer" . "バッファの先頭です")
    ("Can't find completion for \"%s\"" . "\"%s\" に対する補完が見つかりません")
    ("Change layout: " . "変更後のレイアウト: ")
    ("Change mode for channel/user: " . "モードを変更するチャンネルまたはユーザ: ")
    ("Close server: " . "接続を閉じるサーバ: ")
    ("Command to execute on \"%s\":" . "\"%s\" で実行するコマンド: ")
    ("Connecting to %s..." . "%s に接続しています...")
    ("Connecting to %s...done" . "%s に接続しています...完了")
    ("Connecting to %s...failed: %S" . "%s に接続しています...失敗: %S")
    ("Connecting to IRC server..." . "IRC サーバに接続しています...")
    ("Connecting to IRC server...done" . "IRC サーバに接続しています...完了")
    ("Connecting to IRC server...failed: %S" . "IRC サーバに接続しています...失敗: %S")
    ("Created on %s\n" . "%s に作成されました\n")
    ("End of buffer" . "バッファの終端です")
    ("Erroneous nickname \"%s\".  Choose a new one: " . "不正なニックネーム \"%s\"。新しいニックネーム: ")
    ("Finger user: " . "身元を調べるユーザ: ")
    ("Invite user: " . "招待するユーザ: ")
    ("Inviting %s\n" . "%s を招待しています\n")
    ("Inviting %s to %s" . "%s を %s に招待しています")
    ("Join channel/user (default %s): " . "参加するチャンネルまたはユーザ (既定値 %s): ")
    ("Join channel/user: " . "参加するチャンネルまたはユーザ: ")
    ("Key for %s: " . "%s のキー: ")
    ("Key for %s: Quit" . "%s のキー: 中止")
    ("Kick user: " . "蹴り出すユーザ: ")
    ("LIST pattern: " . "LIST のパターン: ")
    ("Logging in to %s..." . "%s にログインしています...")
    ("Logging in to %s...done" . "%s にログインしています...完了")
    ("Logging in to IRC server..." . "IRC サーバにログインしています...")
    ("Logging in to IRC server...done" . "IRC サーバにログインしています...完了")
    ("Message to user: " . "ユーザへのメッセージ: ")
    ("Message: " . "メッセージ")
    ("Mode (? for help)" . "モード (ヘルプは ?)")
    ("Mode by %s: %s\n" . "%s によるモード設定: %s\n")
    ("Mode for %s: %s" . "%s のモード: %s")
    ("Mode on %s by %s: %s" . "%s のモードが %s により設定されました: %s")
    ("Mode: " . "モード: ")
    ("NAMES pattern: " . "NAMES のパターン: ")
    ("Nickname \"%s\" already in use.  Choose a new one: " . "ニックネーム \"%s\" は既に使用されています。新しいニックネーム: ")
    ("No changes made.  Save anyway? " . "変更がありませんが、保存しますか? ")
    ("No channel" . "チャンネルなし")
    ("No server process" . "サーバのプロセスがありません")
    ("No text to send" . "送信するテキストがありません")
    ("None" . "なし")
    ("Online: " . "オンライン: ")
    ("Open server: " . "接続するサーバ: ")
    ("Part from channel/user (default %s): " . "離脱するチャンネルまたはユーザ (既定値 %s): ")
    ("Password for %s: " . "%s のパスワード: ")
    ("Password for %s: Quit" . "%s のパスワード: 中止")
    ("Password incorrect from %s." . "%s のパスワードが不正です。")
    ("Password: " . "パスワード: ")
    ("Password: Quit" . "パスワード: 中止")
    ("Really quit IRC? " . "本当に IRC をやめますか? ")
    ("Really want to query LIST without argument? " . "本当に引数なしの LIST を発行しますか? ")
    ("Really want to query NAMES without argument? " . "本当に引数なしの NAMES を発行しますか? ")
    ("Really want to query WHO without argument? " . "本当に引数なしの WHO を発行しますか? ")
    ("Recent messages of the day:\n" . "最近のメッセージ (今日中):\n")
    ("Recent messages up to %d lines:\n" . "最近のメッセージ (%d 行まで):\n")
    ("Sending QUIT to \"%s\"..." . "\"%s\" に QUIT を送信しています...")
    ("Sending QUIT to \"%s\"...done" . "\"%s\" に QUIT を送信しています...完了")
    ("Sending QUIT..." . "QUIT を送信しています...")
    ("Sending QUIT...done" . "QUIT を送信しています...完了")
    ("Server: " . "サーバ")
    ("Set +o for users" . "+o するユーザ")
    ("Set +v for users" . "+v するユーザ")
    ("Set topic: " . "新しいトピック: ")
    ("Switch to channel/user: " . "移動先のチャンネルまたはユーザ: ")
    ("Switch to number: " . "移動先の番号: ")
    ("Topic by %s: %s\n" . "%s によるトピック設定: %s\n")
    ("Topic for %s: %s" . "%s のトピック: ")
    ("Topic on %s by %s: %s" . "%s のトピックが %s により設定されました: %s")
    ("Topic: " . "トピック: ")
    ("Type \\[describe-mode] for help" . "ヘルプを見るには \\[describe-mode]")
    ("Unset +o for users" . "-o するユーザ")
    ("Unset +v for users" . "-v するユーザ")
    ("WHO pattern: " . "WHO のパターン: ")
    ("[Available modes: " . "[使用可能なモード: ")
    ("days" . "日")
    ("hours" . "時間")
    ("minutes" . "分")
    ("on via server %s: %s" . "サーバ %s 経由: %s")
    ("seconds" . "秒")))

(provide 'riece-mcat-japanese)

;;; riece-mcat-japanese.el ends here

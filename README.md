# Allo Conversation Scraper (ACS)

This application pulls the Allo database from an Android device over adb and then
parses it to generate a conversation log in searchable, single-page, html format.

Currently, this program requires a rooted Android device to pull the message database.

## Build
```
(load "main.lisp")
(sb-ext:save-lisp-and-die "acs" :toplevel #'main :executable t :compression 9)
```

## Todo
* Make sure that the same stickers aren't being pulled multiple times
* Display group chat names when they are set (when listing conversations)
* Add support for backing up other chat applications
* Add support for unrooted Android
* Convert animated webp to gif images (or just cry and use Chromium)
* Merge the `:sticker` and `:image` types. Give `:image` a field to store the file format / mime type.
  conversion can be done after the file type is checked.

## API
* `*REMOTE-DB-PATH*` Stores the location the the database to be pulled on the Android device.


## Reverse Engineering
* Database located at: `/data/data/com.google.android.apps.fireball/databases/fireball.db`
* Media located at: `/sdcard/Allo/`
* Contact information stored in the `fireball_users` table of `fireball.db`
* Conversation information stored in the `conversations` table or `fireball.db`
* All messages stored in the `messages` table of `fireball.db`

## Dependencies
* Common Lisp (SBCL)
* SQL Library (cl-dbi)
* URI Parsing Library (quri)
* Regex (cl-ppcre)
* Webp Conversion (imagemagick)
* Phone Communication (adb)

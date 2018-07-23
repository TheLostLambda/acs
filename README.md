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
* Limit line width to 80 so default terminals fit all the text on screen
* When output directories are entered, ensure they are actual directories
* Convert animated webp to gif images (or just cry and use Chromium)

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

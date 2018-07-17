# Allo Conversation Scraper (ACS)

This application pulls the Allo database from an Android device over adb and then
parses it to generate a conversation log in searchable, single-page, html format.

## Todo
* Support for date ranges in `list-messages`
* Add dynamic text size from the :|text_size| field
* Make timestamp reveal on click not hover
* Fit message background to content
* Make message colors sender dependent
* Update docstrings
* Convert animated webp to gif images

## Reverse Engineering
* Database located at: `/data/data/com.google.android.apps.fireball/databases/fireball.db`
* Media located at: `/sdcard/Allo/`
* Contact information stored in the `fireball_users` table of `fireball.db`
* Conversation information stored in the `conversations` table or `fireball.db`
* All messages stored in the `messages` table of `fireball.db`

## Tech
* Common Lisp (SBCL)
* SQL Library (cl-dbi)
* URI Parsing Library (quri)
# Allo Conversation Scraper (ACS)

This application pulls the Allo database from an Android device over adb and then
parses it to generate a conversation log in searchable, single-page, html format.

## Todo
* Convert animated webp to gif images
* Use relative paths for resource links in the html
* Add support for videos
* Add proper CSS to format messages in an easy to read way (and hide timestamps)
* Filter out empty messages in list-messages
* Update docstrings

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
#!/bin/sh -e

dbfile=${1:-"$HOME/.config/glow/db/contacts.db"}
schemadir=${2:-$(dirname $0)}

# A successful load should not produce any warnings or output.
sqlite3 "$dbfile" < "$schemadir/schema.sql"

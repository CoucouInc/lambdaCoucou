#!/usr/bin/env sh

mkdir -p backups/
for i in $@ ; do
  SOURCE="resources/$i"
  TARGET="backups/$i-`date -I`"
  cp "$SOURCE" "$TARGET";
  gzip "$TARGET";
done

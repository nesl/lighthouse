#!/bin/sh

VERS="0.0.1"
DATE="2007-02-13"

# Leave the desired layout uncommented.
LAYOUT=layout          # Tables based layout.

ASCIIDOC_HTML="asciidoc --unsafe --backend=xhtml11 --attribute icons --attribute iconsdir=./images/icons --attribute=badges --attribute=revision=$VERS  --attribute=date=$DATE"

$ASCIIDOC_HTML --conf-file=${LAYOUT}.conf --attribute iconsdir=./icons --attribute=index-only index.txt
$ASCIIDOC_HTML --conf-file=${LAYOUT}.conf --attribute iconsdir=./icons --attribute=index-only checkpoint.txt


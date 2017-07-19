#!/bin/bash

arch="$1"
version=$(grep "^version" confetti.cabal | sed 's/ //g'  | cut -d ':' -f 2)
file="confetti-$version-$arch.tar.gz"

echo "Creating $archive"

stack install
cp ~/.local/bin/confetti .
strip confetti
tar -czvf "$archive" confetti

echo "Done!"

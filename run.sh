#! /bin/bash

# exit when any command fails
set -e

mv ui/browser/elm-stuff /tmp/elm-stuff
mv ~/.elm /tmp/.elm

# If at first you don't succeed, try, try again
cabal build || cabal build

mv /tmp/elm-stuff ui/browser/elm-stuff
mv /tmp/.elm ~/.elm

cd ui/browser
./elm make src/Index.elm --output elm.wat
wat2wasm elm.wat

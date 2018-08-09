#! /bin/bash

# exit when any command fails
set -e

# If at first you don't succeed, try, try again
cabal build

cd ui/browser
./elm make src/Index.elm --output elm.wat
wat2wasm elm.wat

# Measurement extractor

## Prerequisities

### Debian and Ubuntu users

    apt-get install libghc6-datetime-dev

## Everyone else

First you need to install Cabal either manually or via your package
manager. You are on your own. Then you can install the required
packages via cabal:

    cabal install datetime

TODO you need patched CouchDB

## Compiling

    ghc --make Stats.hs

## Usage

For usage instructions:

    ./Stats --help

To get statistics of measurements newer than August 21th, 2011:

    ./Stats -s 2011-08-21

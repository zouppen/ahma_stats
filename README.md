# Measurement extractor

## Prerequisities

To compile this software, you need Haskell platform and a bunch of
libraries.

### Debian and Ubuntu users

    apt-get install ghc6 libghc6-datetime-dev

### Everyone else

First you need to download GHC from http://hackage.haskell.org/platform/

Then you need to install Cabal either manually or via your package
manager. You are on your own. Then you can install the required
packages via cabal:

    cabal install datetime

### Finally

Finally, make sure you have patched version of CouchDB which supports
authentication. The patch is pending to mainline. Meanwhile you may
grab the sources from https://github.com/zouppen/haskell-couchdb and
follow it's install instructions.

## Compiling

    ghc --make Stats

## Usage

This tool has a nice command line interface To see usage instructions, run:

    ./Stats --help

For example, to get statistics of measurements newer than August 21th,
2011 in your current time zone:

    ./Stats -s 2011-08-21

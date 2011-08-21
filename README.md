# Measurement extractor

This utility extracts measurements from a database which is filled by
*ahmattimittari*, which collects data from sensors connected to an
embedded computer. To help visualizing the measurements, I wrote this
tool.

You can get *ahmattimittari* from https://github.com/zouppen/ahmattimittari .

**NB!** This is no general-purpose data extractor. This project mainly
serves my needs and may be useful to you when programming with CouchDB
and Haskell. I'm not an expert in either. Please send a message to me
if you have any suggestions how to do things more professionally. :-)

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

## CouchDB view

You need a view in CouchDB. See file `couchdb/README.md` for more
information how to do that.

## Compiling

    ghc --make Stats

## Usage

This tool has a nice command line interface To see usage instructions, run:

    ./Stats --help

For example, to get statistics of a single day, we ask measurements
between August 20th midnight and August 21st midnight. The tool is
using your current time zone by default. By running the following in my
environment:

    ./Stats -s 2011-08-20 -e 2011-08-21

I get:

    Time zone is +0300
    Location: box
      minimum: +32.3 °C at 2011-08-20 08:22:04
      maximum: +32.7 °C at 2011-08-20 00:22:05
      average: +32.4 °C
      number of samples: 71
    Location: in
      minimum: +16.6 °C at 2011-08-20 08:22:04
      maximum: +16.9 °C at 2011-08-20 20:42:04
      average: +16.7 °C
      number of samples: 59
    Location: out
      minimum: +11.3 °C at 2011-08-20 06:22:04
      maximum: +16.3 °C at 2011-08-20 16:42:04
      average: +13.2 °C
      number of samples: 62

The number of samples tend to vary because some of the measurements
went wrong. That's quite normal with long cabling and may be fixed in
`ahmattimittari` if needed.

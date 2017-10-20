# SonoCleaner

SonoCleaner is a tool for correcting artifacts in measurements obtained from
[sonomicrometry](https://en.wikipedia.org/wiki/Sonomicrometry). SonoCleaner
corrects artifacts in a more automated manner than existing tools. As a result,
SonoCleaner is more efficient, effective, and consistent.

For more details, see our **paper** (not yet published).

---

## Usage

Please see the **User's guide** for a tutorial on how to use the program, and
the **paper** for a protocol for the correction of artifacts.

---

## Installation

Currently only MacOS and 64-bit Linux (x86\_64) are supported.

This tool is distributed as a binary executable [download-link]. To run the
program, one only needs to download and run the executable for your operating
system, given that all the program's dependencies are installed.

### Requirements

This program depends on the gtk+3 library for its graphical user interface.

#### MacOS

Gtk+3 can be obtained on MacOS through the [Homebrew](https://brew.sh) package
manager.

Homebrew requires the use of a recent version of OS X; at the time of writing
(2017), it requries OS X 10.11 or higher. The installation and use of Homebrew
requires use of the terminal. See their [installation
instructions](https://docs.brew.sh/Installation.html) or their
[documentation](https://docs.brew.sh/) for more information.

To install gtk+3, paste and run the following in the terminal:

> brew install gtk+3 adwaita-icon-theme

#### Linux

The packages providing gtk+3 are listed below for several distributions.

__Ubuntu__, __Debian__: libgtk-3-0

__Fedora__: gtk3

---

## Building from source

The program is written in Haskell and is built with [Stack](https://haskellstack.org).

### Dependencies

The dependencies for building the program are: stack, cairo, gtk3, pango, and
pkg-config.

On Debian linux, these dependencies translate to the following packages:

> haskell-stack libcairo2-dev libgtk-3-dev libpango1.0-dev pkg-config

On MacOS, the following [Homebrew](https://brew.sh) "formulae" suffice:

> haskell-stack gtk+3 adwaita-icon-theme

### Building

Once the depenencies are installed, clone the source repository and change
directory to the SonoCleaner project folder.

> git clone https://github.com/awjchen/SonoCleaner.git SonoCleaner  
> cd SonoCleaner/SonoCleaner

Use Stack to fetch a Haskell compiler and other build tools.

> stack setup  
> stack install gtk2hs-buildtools

Finally, use Stack to build the program itself.

> stack install

The first build of the program will take much longer as Stack downloads the
program's dependencies and builds them from source, but caches them for future
builds.

If successful, these steps will place a SonoCleaner executable in the .local/bin
directory of your home folder.

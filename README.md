[![Build Status](https://travis-ci.org/fedora-haskell/fedora-haskell-tools.png)](https://travis-ci.org/fedora-haskell/fedora-haskell-tools)
[![Hackage](http://img.shields.io/hackage/v/fedora-haskell-tools.png)](http://hackage.haskell.org/package/fedora-haskell-tools)
[![Stackage LTS](http://stackage.org/package/fedora-haskell-tools/badge/lts)](http://stackage.org/lts/package/fedora-haskell-tools)
[![Stackage Nightly](http://stackage.org/package/fedora-haskell-tools/badge/nightly)](http://stackage.org/nightly/package/fedora-haskell-tools)
[![license](https://img.shields.io/badge/license-GPLv3+-brightgreen.svg)](https://www.gnu.org/licenses/gpl.html)

# Fedora Haskell Tools

Tools for Fedora Haskell package maintainence.

## fhpkg
Git clones and pull, etc Fedora Haskell package repos.
Can also generate Hackage distro meta data.
Builds set of packages locally or in Koji or Mock.
(fhbuild has been merged into fhpkg.)

For local builds unbuilt dependent packages are also built recursively.

For updates built in Koji, buildroot overrides are created and waited for.
Chain builds packages with wait-repo.
Also checks that updated dependencies have been built first.

Please be careful when using the build commands:
eg for Rawhide this can easily cause package builds to take place in Koji
when packages haven't been built yet for the latest disttag.

See TODO for more planned features.


## fhbz
Update version update bugs in Bugzilla for Haskell packages.

## fhmock
Tool for standalone mock chroots for building and development.

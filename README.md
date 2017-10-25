[![Build Status](https://travis-ci.org/fedora-haskell/fedora-haskell-tools.png)](https://travis-ci.org/fedora-haskell/fedora-haskell-tools)

# Fedora Haskell Tools

## fhbuild
Builds set of packages locally or in Koji or Mock.

For local builds unbuilt dependent packages are also built recursive.

For updates built in Koji, buildroot overrides are created and waited for.

Please be careful using this: eg for Rawhide this can easily cause
package builds to take place in Koji when packages haven't been built
yet for the latest disttag.

See TODO for more planned features.

## fhpkg
Git clones and pull, etc Fedora Haskell package repos.
Can also generate Hackage distro meta data.

Unless you specify a branch it will use "fedpkg clone -B".
It doesn't do anonymous cloning (-a) yet.

This replaces the scripts:
- https://pagure.io/haskell-sig-old/blob/master/f/bin/haskell-pkgs.sh
- https://pagure.io/haskell-sig-old/blob/master/f/packages/current-packages


fhpkg and fhbuild may eventually be merged.

## fhbz
Update version update bugs in Bugzilla for Haskell packages.

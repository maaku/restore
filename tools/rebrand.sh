# Copyright (c) 2019 The Restore Developers.
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0.  If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#!/bin/bash

set -e

name="restore"
Name="Restore"
user="maaku"
email="mark@friedenbach.org"

if [ -e src/Data/Restore ]; then
    git mv src/Data/Restore src/Data/"${Name}"
    git commit -m "[Branding] Rename files \"Restore\" -> \"${Name}\"."
    for fn in $(git ls-tree -r --name-only HEAD | grep "[bB]inary"); do
        oldfn=$(basename "${fn}")
        newfn=$(basename "${fn}" | sed -e "s:restore:${name}:g" | sed -e "s:Restore:${Name}:g")
        if [ "${oldfn}"x != "${newfn}"x ]; then
            git mv "${fn}" $(dirname "${fn}")/"${newfn}"
        fi
    done
    git commit --amend -m "[Branding] Rename files \"Restore\" -> \"${Name}\"."
fi

git grep -z -l restore | xargs -0 sed -i "s:restore:${name}:g"
git grep -z -l Restore | xargs -0 sed -i "s:Restore:${Name}:g"
git grep -z -l "with${Name}File" | xargs -0 sed -i "s:with${Name}File:withBinaryFile:g"
git grep -z -l "${name}-0" | xargs -0 sed -i "s:${name}-0:binary-0:g"
git grep -z -l "${name} serial" | xargs -0 sed -i "s:${name} serial:binary serial:g"
git grep -z -l "${Name} serial" | xargs -0 sed -i "s:${Name} serial:Binary serial:g"
git grep -z -l "${name} format" | xargs -0 sed -i "s:${name} format:binary format:g"
git grep -z -l "disk as ${name}" | xargs -0 sed -i "s:disk as ${name}:disk as binary:g"
git grep -z -l "New${Name}" | xargs -0 sed -i "s:New${Name}:NewBinary:g"
git grep -z -l "mark@friedenbach.org" | xargs -0 sed -i "s:mark@friedenbach.org:${email}:g"
git grep -z -l "maaku" | xargs -0 sed -i "s:maaku:${user}:g"
git grep -z -l "Kolmodin <${email}>" | xargs -0 sed -i "s:Kolmodin <${email}>:Kolmodin <kolmodin@gmail.com>:g"
git checkout -- LICENSE
git checkout -- changelog.md
git checkout -- benchmarks/Cabal24.hs

git commit -a -m "[Branding] Re-brand \"Restore\" -> \"${Name}\"."

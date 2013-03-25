#!/bin/bash
set -e
error() { echo "$@" >&2; exit 1; }

environment=${1:-Production}
postgres_yml=config/postgresql-$environment.yml
[[ -e $postgres_yml ]] || error $postgres_yml is missing.
echo Building $environment tarball.

tar cvf yesodoro-reboot.tar dist/build/yesodoro-reboot/yesodoro-reboot static/{css,fonts,images,js} config/settings.yml

sed "s/- Staging/- $environment/" config/keter.yaml > tmp/config/keter.yaml
cp -p $postgres_yml tmp/config/postgresql.yml
tar uvf yesodoro-reboot.tar -C tmp config/keter.yaml config/postgresql.yml

gzip <yesodoro-reboot.tar >yesodoro-reboot.keter
rm yesodoro-reboot.tar

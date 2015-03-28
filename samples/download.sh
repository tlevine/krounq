#!/bin/sh
set -e
for instrument in BD CHH CLP CSH HH HT LT MT OHH RD RIM SD; do
  if ! test -f 909-$instrument.zip; then
    wget http://machines.hyperreal.org/manufacturers/Roland/TR-909/samples/909-$instrument.zip
  fi
  unzip 909-$instrument.zip
done

#!/bin/sh
set -e
for instrument in BD CHH CLP CSH HH HT LT MT OHH RD RIM SD; do
  wget http://machines.hyperreal.org/manufacturers/Roland/TR-909/samples/909-$instrument.zip
done

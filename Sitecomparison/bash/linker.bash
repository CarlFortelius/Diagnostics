#!/bin/bash

set -ex
repository=/ec/res4/scratch/fnm/MastVerifData
site=CABA
mods="EC01 FRAR FARO"
newdir=${repository}/${site}_LONG

# editing line
################################################

obsdir=${newdir}/obs/${site}
mkdir -p $obsdir/
cd $obsdir
ln -s   ${repository}/????/???/obs/${site}/Meas* .

for mod in $mods 
do
	moddir=${newdir}/fc/${site}/${mod}
	mkdir -p $moddir
	cd $moddir
	ln -s  ${repository}/????/???/fc/${site}/${mod}/* .
done


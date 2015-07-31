#!/bin/bash

cp archsys.scala debian/usr/local/sbin/archsys.scala

version=`awk '/^Version/ {print $2}' < debian/DEBIAN/control`

pkg=archsys-$version.deb
dpkg -b debian $pkg
echo "Package in ${pkg}"

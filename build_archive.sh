#!/bin/bash

# Usage :
#   bash build_archive.sh
# 
# Build the archive 'Baptiste_Lefebvre.tgz'.

tar czf Baptiste_Lefebvre.tgz README src/*.ml src/*.mli

#!/usr/bin/env Rscript
# Extract Quake archive in WAL (technically 'WAL2') or PAK format to directory.
#
# Dependencies:
#   Requires the 'wal' R package to be installed.
#   Note: To install it, start a recent R version and type: install.packages('wal')
#
# USAGE: ./pakextract.R <archive_file> <output_dir>
#
# EXAMPLE: ./pakextract.R knave.wad ~/mydata/knave_extracted/
#
# Written by Tim Schaefer
# This program is part of 'wal' and covered by its license. See https://github.com/dfsp-spirit/wal for details.

library("wal");
args = commandArgs(trailingOnly = TRUE);

if (length(args) != 2) {
  stop(sprintf("pakextract -- Extract Quake archives in PAL or WAL format.\nUSAGE: pakextract.R <archive_file> <output_dir>\nEXAMPLES: ./pakextract.R knave.wad ~/mydata/knave_extracted/\n           ./pakextract.R pak0.pak ~/mydata/pak0_extracted/"));
} else {
  wal::qarchive.extract(args[1], args[2]);
}

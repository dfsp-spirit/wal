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
  cat(sprintf("=== pakextract -- Extract Quake archives in PAK or WAL format. ===\n"));
  stop(sprintf("USAGE: pakextract.R <archive_file> <output_dir>\nEXAMPLES: ./pakextract.R knave.wad ~/mydata/knave_extracted/\n          ./pakextract.R pak0.pak ."));
} else {
  wal::qarchive.extract(args[1], args[2]);
}

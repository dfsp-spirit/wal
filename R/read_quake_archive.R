
#' @title Extract any of the supported Quake archives.
#'
#' @param filepath character string, path to existing and readable file in PAK or WAD2 format.
#'
#' @param outdir character string, path to an existing and writeable output directory into which to extract the archive.
#'
#' @param do_pre_checks logical, whether to perform extra sanity checks on the other parameters.
#'
#' @param format character string, of one 'auto' to detect from filename, 'QARCHIVE_TYPE_WAD' for WAD2, or 'QARCHIVE_TYPE_PAK' for PACK.
#'
#' @export
qarchive.extract <- function(filepath, outdir, do_pre_checks = TRUE, format = 'auto') {

  if(do_pre_checks) {
    if(! file.exists(filepath)) {
      stop(sprintf("File '%s' does not exist or cannot be read. Please check or fix permissions.\n", filepath));
    }
    if(! dir.exists(outdir)) {
      stop(sprintf("Output directory '%s' does not exist or cannot be read. Please create it or fix permissions.\n", outdir));
    }
  }

  qarchive_type = qarchive.type.from.filename(filepath);
  if(qarchive_type == 'QARCHIVE_TYPE_WAD') {
    wad.extract(filepath, outdir);
  } else if(qarchive_type == 'QARCHIVE_TYPE_PAK') {
    pak.extract(filepath, outdir);
  } else {
    stop("Invalid or unsupported Quake archive type.");
  }
}


#' @title Determine archive type from file name extension.
#'
#' @inheritParams qarchive.extract
#'
#' @return character string, one of 'QARCHIVE_TYPE_WAD' or 'QARCHIVE_TYPE_PAK'.
#'
#' @keywords internal
qarchive.type.from.filename <- function(filepath) {
  if(any(endsWith(filepath, c('.wad', '.WAD')))) {
    return('QARCHIVE_TYPE_WAD');
  } else if(any(endsWith(filepath, c('.pak', '.PAK')))) {
    return('QARCHIVE_TYPE_PAK');
  } else {
    warning(sprintf("Cannot guess archive type from file extension, trying 'PAK'."));
    return('QARCHIVE_TYPE_PAK');
  }
}

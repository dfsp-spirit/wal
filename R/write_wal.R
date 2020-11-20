
## 24 bit JPEG/PNG colors to indexed WAL colors:
# convert RGB colors to LAB space (colorscience::rgb2x or see grDevices::convertColor, https://cran.r-project.org/web/packages/colordistance/vignettes/lab-analyses.html)
# use deltaE metric to compute distances (colorscience::deltaE2000() or spacesXYZ::DeltaE())
# pick color with

## mimap issue:
# find out storage order
# find out width and height in pixels of the 3 smaller mipmap levels (depends on 1st, I guess)

#' @title Write WAL instance to bitmap file in WAL format.
#'
#' @param filepath character string, path to the file including extension
#'
#' @param wal a wal instance. Note that 1 will be substracted from the data when it is written, as indices are stored 0-based in the file.
#'
#' @examples
#' \dontrun{
#'    walf = '~/data/q2_pak0_extracted/textures/e1u2/basic1_7.wal';
#'    wal = read.wal(walf);
#'    writeWAL(tempfile(fileext = ".wal"), wal);
#' }
#'
#' @export
writeWAL <- function(filepath, wal) {

  fh = file(filepath, "wb", blocking = TRUE);
  on.exit({ close(fh) }, add = TRUE);
  endian = "little";

  if(nchar(wal$header$tex_name) > 0L) {
    if(nchar(wal$header$tex_name) > 32L) {
      stop("Max length for tex_name is 32.");
    }
    writeChar(wal$header$tex_name, fh, eos = NULL);
  }
  writeBin(as.raw(rep(0L, (32L - nchar(wal$header$tex_name)))), fh, endian = endian); # fill remaining space up to max 32 bytes with zeroes.

  writeBin(as.integer(wal$header$width), fh, size = 4, endian = endian);
  writeBin(as.integer(wal$header$height), fh, size = 4, endian = endian);
  writeBin(as.integer(wal$header$mip_level_offsets), fh, size = 4, endian = endian); # these are 4 integers.

  if(nchar(wal$header$anim_name) > 0L) {
    if(nchar(wal$header$anim_name) > 32L) {
      stop("Max length for anim_name is 32.");
    }
    writeChar(wal$header$anim_name, fh, eos = NULL);
  }
  writeBin(as.raw(rep(0L, (32L - nchar(wal$header$anim_name)))), fh, endian = endian); # fill remaining space up to max 32 bytes with zeroes.

  writeBin(as.integer(wal$header$flags), fh, size = 4, endian = endian);
  writeBin(as.integer(wal$header$contents), fh, size = 4, endian = endian);
  writeBin(as.integer(wal$header$value), fh, size = 4, endian = endian);

  # write data
  writeBin(as.integer(wal$file_data_all_mipmaps -1L), fh, size = 1, endian = endian);
}

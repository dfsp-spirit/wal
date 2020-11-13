

#' @title Read bitmap file in WAL format.
#'
#' @param filepath character string, path to the file including extension
#'
#' @param hdr logical, whether to return full list with header
#'
#' @param hdr_only logical, whether to read only the header
#'
#' @return array with color data, or wal instance (named list) if `hdr` is `TRUE`
#'
#' @examples
#' \dontrun{
#'    walf = '~/data/q2_pak0_extracted/textures/e1u2/basic1_7.wal';
#'    wal = read.wal(walf);
#'    plot(wal);
#'    # show palette:
#'    plot(1:256, col=rgb(wal$palette, maxColorValue = 255));
#' }
#'
#' @export
read.wal <- function(filepath, hdr = TRUE, hdr_only = FALSE, num_mip_maps = 4L) {
  fh = file(filepath, "rb");
  on.exit({ close(fh) });

  endian = 'little';

  wal = list();
  header = list();

  header$tex_name = readChar(fh, 32L);
  header$width = readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  header$height = readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  header$mip_level_offsets = readBin(fh, integer(), n = num_mip_maps, size = 1, signed = FALSE, endian = endian);
  header$anim_name = readChar(fh, 32L);  # next frame in animation.
  header$flags = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  header$contents = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  header$value = readBin(fh, integer(), n = 1, size = 4, endian = endian);

  header$ident = readBin(fh, integer(), n = 1, size = 1, endian = endian);
  if(header$ident != 10L) {
    stop("File not in PCX format.");
  }

  header$paintbrush_version = readBin(fh, integer(), n = 1, size = 1, endian = endian);
  header$encoding_type = readBin(fh, integer(), n = 1, size = 1, endian = endian); # 0 = none, 1 = runlength enc.
  header$bitpix = readBin(fh, integer(), n = 1, size = 1, endian = endian); # bits per pixel, defines number of possible colors in image. 1 = 2, 2 = 4, 4 = 16, 8 = 256.

  wal = list('header' = header);
  return(wal);
}




#' @title Read bitmap file in WAL format.
#'
#' @param filepath character string, path to the file including extension
#'
#' @param hdr logical, whether to return full list with header
#'
#' @param hdr_only logical, whether to read only the header
#'
#' @param num_mip_maps integer, the number of mipmap levels stored in the file. Leave this alone unless you know what you are doing.
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
  header$width = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  header$height = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  header$mip_level_offsets = readBin(fh, integer(), n = num_mip_maps, size = 4, endian = endian);
  header$anim_name = readChar(fh, 32L);  # Next frame name in animation, if any. Empty string if none, which is the most common case.
  header$flags = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  header$contents = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  header$value = readBin(fh, integer(), n = 1, size = 4, endian = endian);

  if(header$width < 1L | header$height < 1L) {
    warning("File not in WAL format (or invalid zero-length image dimension).");
  }

  # Read data for all mipmaps.
  mip_level0_data_size = header$width * header$height * (256L + 64L + 16L + 4L) %/% 256L;

  wal = list('header' = header);
  return(wal);
}

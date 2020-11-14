

#' @title Read bitmap file in WAL format.
#'
#' @param filepath character string, path to the file including extension
#'
#' @param hdr logical, whether to return full list with header
#'
#' @param hdr_only logical, whether to read only the header
#'
#' @return integer pixel matrix, each pixel value is in range 0-255 and refers to an index in a palette. The palette is NOT included in the file, so you will need to define one or get it from elsewhere to see the final image.
#'
#' @examples
#' \dontrun{
#'    walf = '~/data/q2_pak0_extracted/textures/e1u2/basic1_7.wal';
#'    wal = read.wal(walf);
#'    #plot(imager::as.cimg(wal$image))
#' }
#'
#' @export
read.wal <- function(filepath, hdr = TRUE, hdr_only = FALSE) {
  fh = file(filepath, "rb");
  on.exit({ close(fh) });

  endian = 'little';
  num_mip_maps = 4L;

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

  if(hdr_only) {
    return(header);
  }

  if(header$width < 1L | header$height < 1L) {
    warning("File not in WAL format (or invalid zero-length image dimension).");
  }

  # Read data for all mipmaps.
  mip_level0_data_size = header$width * header$height;

  # sanity check: does the data length of the first mipmap match width x height.
  if(header$mip_level_offsets[2L] - header$mip_level_offsets[1L] != mip_level0_data_size) {
    warning(sprintf("Expected %d pixel values in image based on width %d and height %d, but first mipmap size is %d.\n", mip_level0_data_size, header$width * header$height, (header$mip_level_offsets[2L] - header$mip_level_offsets[1L])));
  }

  seek(fh, where = header$mip_level_offsets[1L], origin = "start");
  raw_data = readBin(fh, integer(), n = mip_level0_data_size, size = 1, signed = FALSE, endian = endian); # vector

  if(length(raw_data) != mip_level0_data_size) {
    warning(sprintf("Expected %d pixel values, but %d read.\n", mip_level0_data_size, length(raw_data)));
  }

  image_data = matrix(data = raw_data, nrow = header$height, ncol = header$width, byrow = TRUE); # reshaped to image matrix

  wal = list('header' = header, 'image' = image_data);

  if(hdr) {
    return(wal);
  } else {
    return(wal$image);
  }
}




# rgb(pal_q2[c(1,3,5,7),]/255)
# pal_q2[c(1,3,5,7),]/255

#' @title Read bitmap file in WAL format.
#'
#' @param filepath character string, path to the file including extension
#'
#' @param hdr logical, whether to return full list with header
#'
#' @param hdr_only logical, whether to read only the header
#'
#' @param apply_palette optional 256 x 3 integer matrix, the palette. Must contain values in range 0..255. Pass `ǸULL` if you do not want to apply any palette. The resulting \code{wal} object will not have an 'image' entry then.
#'
#' @return integer pixel matrix, each pixel value is in range 0-255 and refers to an index in a palette. The palette is NOT included in the file, so you will need to define one or get it from elsewhere to see the final image.
#'
#' @examples
#' \dontrun{
#'    walf = '~/data/q2_pak0_extracted/textures/e1u2/basic1_7.wal';
#'    wal = read.wal(walf);
#'    plot(wal);
#' }
#'
#' @export
read.wal <- function(filepath, hdr = TRUE, hdr_only = FALSE, apply_palette = wal::pal_q2()) {
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

  raw_data = raw_data + 1L; # R uses 1-based indices.

  if(length(raw_data) != mip_level0_data_size) {
    warning(sprintf("Expected %d pixel values, but %d read.\n", mip_level0_data_size, length(raw_data)));
  }

  pixel_cmap_indices = matrix(data = raw_data, nrow = header$height, ncol = header$width, byrow = TRUE); # reshaped to image matrix

  wal = list('header' = header, 'raw_data' = raw_data, 'pixel_cmap_indices' = pixel_cmap_indices);
  class(wal) = c(class(wal), 'wal');

  if(! is.null(apply_palette)) {
    check.palette(apply_palette);
    channel_red = apply_palette[raw_data, 1];
    channel_green = apply_palette[raw_data, 2];
    channel_blue = apply_palette[raw_data, 3];
    wal$image = array( c( channel_red , channel_green, channel_blue ) , dim = c( header$width , header$height , 3));
  } else {
    wal$image = NULL;
  }

  if(hdr) {
    return(wal);
  } else {
    if(is.null(apply_palette)) {
      stop("Cannot return image without palette. Set hdr to TRUE or pass non-NULL apply_palette parameter.");
    }
    return(wal$image);
  }
}


#' @title Check palette, stop on invalid data.
#'
#' @param pal a palette, i.e., a 256 x 3 integer matrix, with values in range 0..255L.
#'
#' @keywords internal
check.palette <- function(pal) {
  if(! is.matrix(pal)) {
    stop("Palette must be a matrix.");
  }
  if( ! all.equal(dim(pal), c(256, 3))) {
    stop("Palette must be a 256 x 3 matrix.");
  }
  if(min(pal) < 0L | max(pal) > 255L) {
    stop("All palette values must be in range 0..255.")
  }
  if(max(pal) <= 1L) {
    warning(sprintf("Palette max value is %d, but the values must be in range 0..255.\n", max(pal)));
  }
  return(invisible(NULL));
}



#' @title S3 plot function for wal image.
#'
#' @param x a wal instance.
#'
#' @param ... extra args, not used.
#'
#' @export
#' @importFrom graphics plot
plot.wal <- function(x, ...) {
  if(requireNamespace('imager', quietly = TRUE)) {
    if(! is.null(x$image)) {
      graphics::plot(imager::as.cimg(array(x$image, dim=c(x$header$width, x$header$height, 1, 3))));
    } else {
      warning("The wal instance contains no final image, did you set a palette? Using grayscale preview palette.");
      apply_palette = cbind(0L:255L, 0L:255L, 0L:255L);
      check.palette(apply_palette);
      raw_data = x$raw_data;
      channel_red = apply_palette[raw_data, 1];
      channel_green = apply_palette[raw_data, 2];
      channel_blue = apply_palette[raw_data, 3];
      img = array( c( channel_red , channel_green, channel_blue ) , dim = c( x$header$width , x$header$height , 3));
      graphics::plot(imager::as.cimg(array(img, dim=c(x$header$width, x$header$height, 1, 3))));
    }
  } else {
    stop("The 'imager' package must be installed to plot PCX images.");
  }
}






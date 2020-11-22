

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
#' @param apply_palette optional 256 x 3 integer matrix, the palette. Must contain values in range 0..255. Pass NULL if you do not want to apply any palette. The resulting \code{wal} object will not have an 'image' entry then.
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
  read_mip_maps = TRUE;

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

  header$mipmaps = list();
  header$mipmaps$mip_level0_data_size = mip_level0_data_size;
  header$mipmaps$mip_level1_data_size = header$mip_level_offsets[3L] - header$mip_level_offsets[2L];
  header$mipmaps$mip_level2_data_size = header$mip_level_offsets[4L] - header$mip_level_offsets[3L];
  seek(fh, where = 0L, origin = "end"); end_pos = seek(fh, where = NA); # Find file size (last position).
  header$mipmaps$mip_level3_data_size = end_pos - header$mip_level_offsets[4L];


  seek(fh, where = header$mip_level_offsets[1L], origin = "start");
  raw_data = readBin(fh, integer(), n = mip_level0_data_size, size = 1, signed = FALSE, endian = endian); # vector
  raw_data = raw_data + 1L; # R uses 1-based indices. Note that raw_data is for first mipmap only.


  if(length(raw_data) != mip_level0_data_size) {
    warning(sprintf("Expected %d pixel values, but %d read.\n", mip_level0_data_size, length(raw_data)));
  }

  pixel_cmap_indices = matrix(data = raw_data, nrow = header$height, ncol = header$width, byrow = TRUE); # reshaped to image matrix

  wal = list('header' = header, 'raw_data' = raw_data, 'pixel_cmap_indices' = pixel_cmap_indices);
  class(wal) = c(class(wal), 'wal');

  if(! is.null(apply_palette)) {
    check.palette(apply_palette);
  }

  wal$header$mipmaps$mip_level0_dim = c(wal$header$width, wal$header$height);

  if(read_mip_maps) {
    seek(fh, where = header$mip_level_offsets[2L], origin = "start");
    wal$raw_data_mip_level1 = readBin(fh, integer(), n = header$mipmaps$mip_level1_data_size, size = 1, signed = FALSE, endian = endian) + 1L;
    wal$image_mip_level1 = apply.palette.to.rawdata(wal$raw_data_mip_level1, apply_palette, header$width / 2L, header$height / 2L);
    wal$header$mipmaps$mip_level1_dim = c(wal$header$width / 2L, wal$header$height / 2L);

    seek(fh, where = header$mip_level_offsets[3L], origin = "start");
    wal$raw_data_mip_level2 = readBin(fh, integer(), n = header$mipmaps$mip_level2_data_size, size = 1, signed = FALSE, endian = endian) + 1L;
    wal$image_mip_level2 = apply.palette.to.rawdata(wal$raw_data_mip_level2, apply_palette, header$width / 4L, header$height / 4L);
    wal$header$mipmaps$mip_level2_dim = c(wal$header$width / 4L, wal$header$height / 4L);

    seek(fh, where = header$mip_level_offsets[4L], origin = "start");
    wal$raw_data_mip_level3 = readBin(fh, integer(), n = header$mipmaps$mip_level3_data_size, size = 1, signed = FALSE, endian = endian) + 1L;
    wal$image_mip_level3 = apply.palette.to.rawdata(wal$raw_data_mip_level3, apply_palette, header$width / 8L, header$height / 8L);
    wal$header$mipmaps$mip_level3_dim = c(wal$header$width / 8L, wal$header$height / 8L);
  }

  wal$image = apply.palette.to.rawdata(raw_data, apply_palette, header$width, header$height);
  wal$file_data_all_mipmaps = c(wal$raw_data, wal$raw_data_mip_level1, wal$raw_data_mip_level2, wal$raw_data_mip_level3);

  if(hdr) {
    return(wal);
  } else {
    if(is.null(apply_palette)) {
      stop("Cannot return image without palette. Set hdr to TRUE or pass non-NULL apply_palette parameter.");
    }
    return(wal$image);
  }
}


#' @title Apply a palette to index data to create a 2D image.
#'
#' @param raw_data integer vector of pixel data, each entry represents an index into the palette.
#'
#' @param apply_palette integer matrix, the palette.
#'
#' @param img_width integer, the width of the image to create.
#'
#' @param img_height integer, the height of the image to create.
#'
#' @keywords internal
apply.palette.to.rawdata <- function(raw_data, apply_palette, img_width, img_height) {
  if(! is.null(apply_palette)) {
    channel_red = apply_palette[raw_data, 1];
    channel_green = apply_palette[raw_data, 2];
    channel_blue = apply_palette[raw_data, 3];
    return(array( c( channel_red , channel_green, channel_blue ) , dim = c( img_width , img_height , 3)));
  } else {
    return(NULL);
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
      warning("The wal instance contains no final image, using grayscale preview palette. Use 'plotwal.mipmap()' to set palette for viewing.");
      apply_palette = cbind(0L:255L, 0L:255L, 0L:255L);
      check.palette(apply_palette);
      img = apply.palette.to.rawdata(x$raw_data, apply_palette, x$header$width , x$header$height);
      graphics::plot(imager::as.cimg(array(img, dim=c(x$header$width, x$header$height, 1, 3))));
    }
  } else {
    stop("The 'imager' package must be installed to plot PCX images.");
  }
}


#' @title Plot a mipmap level from a WAL image.
#'
#' @param wal a WAL image instance, as returned by \code{read.wal}.
#'
#' @param mip_level integer in range 0..3, the mipmap to plot. Level 0 is the original full-size image, the other ones get smaller and smaller (by factor 2 on each dimension, so 1/4th the size of their predecessor).
#'
#' @inheritParams read.wal
#'
#' @export
plotwal.mipmap <- function(wal, mip_level = 0L, apply_palette = wal::pal_q2()) {
  if(mip_level < 0L | mip_level > 3L) {
    stop("Paramter 'mip_level' must be an integer in range 0..3.");
  }

  raw_data = get.wal.mipmap.data(wal, mip_level);
  img_width = get.wal.mipmap.widths(wal$header$width)[mip_level+1L];
  img_height = get.wal.mipmap.heights(wal$header$height)[mip_level+1L];

  if(mip_level == 0L) {
    img = wal$image;
  } else if(mip_level == 1L) {
    img = wal$image_mip_level1;
  } else if(mip_level == 2L) {
    img = wal$image_mip_level2;
  } else { # 3
    img = wal$image_mip_level3;
  }
  img = NULL;

  if(! is.null(img)) {
    graphics::plot(imager::as.cimg(array(img, dim=c(img_width, img_height, 1, 3))));
  } else {
    if(is.null(apply_palette)) {
      warning("The wal instance contains no final image and none supplied in parameter 'apply_palette'. Using grayscale preview palette.");
      apply_palette = cbind(0L:255L, 0L:255L, 0L:255L);
    }
    check.palette(apply_palette);
    img = apply.palette.to.rawdata(raw_data, apply_palette, img_width , img_height);
    graphics::plot(imager::as.cimg(array(img, dim=c(img_width, img_height, 1, 3))));
  }

}

#' @title Retrieve raw data for given mipmap level from WAL instance.
#'
#' @inheritParams plotwal.mipmap
#'
#' @keywords internal
get.wal.mipmap.data <- function(wal, mip_level) {
  mm_offset = get.mipmap.data.offsets(wal$header$width, wal$header$height, start_at = 0L);
  mm_len = get.mipmap.data.lengths(wal$header$width, wal$header$height);
  mm0 = wal$file_data_all_mipmaps[mm_offset[1]:mm_offset[2]];
  mm1 = wal$file_data_all_mipmaps[mm_offset[2]:mm_offset[3]];
  mm2 = wal$file_data_all_mipmaps[mm_offset[3]:mm_offset[4]]
  mm3 = wal$file_data_all_mipmaps[mm_offset[4]:(mm_offset[4]+mm_len[4])];
  if(mip_level == 0L) {
    return(mm0);
  } else if(mip_level == 1L) {
    return(mm1);
  } else if(mip_level == 2L) {
    return(mm2);
  } else if(mip_level == 3L) {
    return(mm3);
  } else {
    stop("Invalid mip_level, must be 0..3.");
  }
}


#' @keywords internal
get.wal.mipmap.widths <- function(width_mm0) {
  return(c(width_mm0, width_mm0/2, width_mm0/4, width_mm0/8));
}

#' @keywords internal
get.wal.mipmap.heights <- function(height_mm0) {
  return(get.wal.mipmap.widths(height_mm0));
}


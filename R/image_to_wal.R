
#' @title Convert image to WAL instance.
#'
#' @description Convert an input RGB image to a WAL instance, re-mapping its colors to the WAL palette in the process and generating the mipmaps.
#'
#' @param in_image numeric matrix with 3 dimensions: widt, height, channels. Values must be in range 0..1. This is the image format returned by \code{jpeg::readJPEG} and \code{png::readPNG}. The image can have arbitrary colors, but the colors in the final WAL image will be limited to the palette. Both the width and height must be multiples of 8. Typical idtech1/2 textures use 32, 64, ..., 512. The reason is the mipmaps.
#'
#' @param apply_palette n x 3 integer matrix, the palette for the WAL image. This is not saved to the wal image, but still required because the colors from the \code{in_image} will be adapted to the palette colors (replaced with the most similar ones). If the palette does not cover the colors in the source image well, the resulting WAL image will look bad (dissimilar to the source image).
#'
#' @inheritParams writeWAL
#'
#' @return wal instance
#'
#' @examples
#' \dontrun{
#'    wal = img.to.wal(jpeg::readJPEG("~/mytex.jpg"));
#' }
#'
#' @export
img.to.wal <- function(in_image, apply_palette = wal::pal_q2(), wal = wal.template()) {
  if(length(dim(in_image)) != 3L) {
    stop("Parameter 'in_image' must have 3 dimensions: image width, height, and channels (R, G, B).");
  }

  check.palette(apply_palette);

  wal$header$width = dim(in_image)[1];
  wal$header$height = dim(in_image)[2];
  num_channels = dim(in_image)[3];

  if(num_channels != 3L) {
    stop("Parameter 'in_image': third dimension must have length 3 (channels R, G, B).");
  }
  if(wal$header$width %% 8 != 0L) {
    stop(sprintf("Input image has invalid width %d, must be a multiple of 8.", wal$header$width));
  }
  if(wal$header$height %% 8 != 0L) {
    stop(sprintf("Input image has invalid height %d, must be a multiple of 8.", wal$header$height));
  }

  in_image = in_image * 255L; # convert data from range 0..1 to 0..255.
  in_image_matrix = matrix(in_image, c((wal$header$width * wal$header$height), 3L));
  palette_col_indices = closest.color.from.palette(in_image_matrix, apply_palette);

  wal$file_data_all_mipmaps = expand.rawdata.to.mipmaps(palette_col_indices, wal$header$width, wal$header$height);
  wal$raw_data = wal$file_data_all_mipmaps[1:(wal$header$width * wal$header$height)];

  wal$header$mip_level_offsets = get.mipmap.data.offsets(wal$header$width, wal$header$height);
  return(wal);
}


#' @title Compute length of mipmaps in bytes from width and height of largest image (mipmap0).
#'
#' @param mm0_width integer, width of mipmap 0
#'
#' @param mm0_height integer, height of mipmap 0
#'
#' @return integer vector of length 4, the lengths.
#'
#' @keywords internal
get.mipmap.data.lengths <- function(mm0_width, mm0_height) {
  m0_l = mm0_width * mm0_height;
  m1_l = (mm0_width/2) * (mm0_height/2);
  m2_l = (mm0_width/4) * (mm0_height/4);
  m3_l = (mm0_width/8) * (mm0_height/8);
  return(c(m0_l, m1_l, m2_l, m3_l));
}


#' @title Get mipmap offsets for WAL header, based on mipmap sizes and start offset.
#'
#' @inheritParams get.mipmap.data.lengths
#'
#' @param start_at integer, the offset at which the data starts in the file. Must be 100L for WAL format.
#'
#' @return integer vector of length 4, the offsets.
#'
#' @keywords internal
get.mipmap.data.offsets <- function(mm0_width, mm0_height, start_at = 100L) {
  mipmaps_lengths = get.mipmap.data.lengths(mm0_width, mm0_height);
  m0_o = start_at;
  m1_o = start_at + mipmaps_lengths[1];
  m2_o = start_at + mipmaps_lengths[1] + mipmaps_lengths[2];
  m3_o = start_at + mipmaps_lengths[1] + mipmaps_lengths[2] + mipmaps_lengths[3];
  return(c(m0_o, m1_o, m2_o, m3_o));
}



#' @title Generate a WAL structure template.
#'
#' @description Generates a WAL instance that can be modified and filled with new data. The template represents a black 32x32 image (if the palette used to display it adhers to the convention that the first color is black). The indices used are 1-based (black is at index 1, not 0).
#'
#' @keywords internal
wal.template <- function() {
  wal = list('header' = list());

  wal$header$tex_name = "e1u1/black";
  wal$header$width = 32L;
  wal$header$height = 32L;

  # the last (4th) offset is the beginning of the data for the last mipmap, the end is the EOF.
  wal$header$mip_level_offsets = c(100L, (100L + 32*32), (100L + 32*32 + 16*16),  (100L + 32*32 + 16*16 + 8*8));
  wal$header$anim_name = "";
  wal$header$flags = 0L;
  wal$header$contents = 0L;
  wal$header$value = 0L;

  num_values = 32*32 + 16*16 + 8*8 + 4*4; # for all mipmaps
  wal$file_data_all_mipmaps = rep(1L, num_values); # the first value in the palette is black.

  wal$raw_data = wal$file_data_all_mipmaps[1:(32*32)];

  class(wal) = c('wal', class(wal));
  return(wal);
}


#' @title Given the pixel data for the largest image, generate the full data for all mipmaps.
#'
#' @param raw_data_mip_level0 integer vector or matrix, the image data for the largest mipmap.
#'
#' @param width integer, width of image for mip level 0
#'
#' @param height integer, width of image for mip level 0
#'
#' @keywords internal
expand.rawdata.to.mipmaps <- function(raw_data_mip_level0, width, height, byrow = TRUE) {
  if(! is.matrix(raw_data_mip_level0)) {
    raw_data_mip_level0 = matrix(raw_data_mip_level0, ncol = width, byrow = byrow);
  }

  mip_level1 = half.image(raw_data_mip_level0, byrow = byrow);
  mip_level2 = half.image(mip_level1, byrow = byrow);
  mip_level3 = half.image(mip_level2, byrow = byrow);
  return(c(as.integer(raw_data_mip_level0), as.integer(mip_level1), as.integer(mip_level2), as.integer(mip_level3)));
}


#' @title Reduce image size by 2 along both axes by dropping pixels.
#'
#' @param image_data integer matrix, a 1-channel image.
#'
#' @keywords internal
half.image <- function(image_data, byrow = TRUE) {
  if(! is.matrix(image_data)) {
    stop("Parameter 'image_data' must be a matrix.");
  }
  mip_data = rep(NA, ((ncol(image_data) / 2L) * (nrow(image_data) / 2L)));

  current_index = 1L;
  for(row_idx in 1:nrow(image_data)) {
    for(col_idx in 1:ncol(image_data)) {
      if(row_idx %% 2 == 0L) {
        if(col_idx %% 2 == 0L) {
          mip_data[current_index] = image_data[row_idx, col_idx];
          current_index = current_index + 1L;
        }
      }
    }
  }

  mip_data = matrix(mip_data, ncol = (ncol(image_data) / 2L), byrow = byrow);
  return(mip_data);
}


#' @title Find closest color from palette for each RGB color.
#'
#' @description Find closest color from a palette for given colors. The similarity method used to define 'closest' is deltaE, and the input RGB colors are transformed to LAB space for the computation, assuming they are given in sRGB space.
#'
#' @param colors_rgb n x 3 integer matrix, the truecolor (arbitrary) input RGB colors for which you want to find the most similar colors included in the fixed palette. Range 0..255.
#'
#' @param fixed_palette_rgb the fixed palette, an n x 3 matrix of integers, representing the fixed palette colors in RGB values in range 0..255.
#'
#' @return vector of n integers, the index of the closest color into the palette for each of the \code{colors_rgb}.
#'
#' @examples
#'     colors_rgb = matrix(c(255, 0, 0, 100, 100, 100, 10, 10, 10, 5, 5, 5),
#'      ncol = 3, byrow = TRUE);
#'     fixed_palette_rgb = matrix(c(255, 0, 0, 255, 5, 0, 11, 11, 11, 0, 0, 0,
#'      255, 255, 255), ncol = 3, byrow = TRUE);
#'     pal_similar_colors = closest.color.from.palette(colors_rgb,
#'      fixed_palette_rgb);
#'
#' @importFrom spacesXYZ DeltaE
#' @importFrom grDevices convertColor
#' @export
closest.color.from.palette <- function(colors_rgb, fixed_palette_rgb) {
  if(max(colors_rgb) <= 1.0) {
    warning("Parameters 'colors_rgb': max data value <= 1.0, is the data in range 0-255?");
  }
  if(max(fixed_palette_rgb) <= 1.0) {
    warning("Parameters 'fixed_palette_rgb': max data value <= 1.0, is the data in range 0-255?");
  }
  colors_lab = grDevices::convertColor(colors_rgb, from="sRGB", to="Lab");
  fixed_palette_lab = grDevices::convertColor(fixed_palette_rgb, from="sRGB", to="Lab");
  result_indices = rep(NA, nrow(colors_rgb));
  for(i in 1:nrow(colors_rgb)) {
    result_indices[i] = which.min(spacesXYZ::DeltaE(colors_lab[i,], fixed_palette_lab));
  }
  return(result_indices);
}

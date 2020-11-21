
#' @title Convert image to WAL instance.
#'
#' @description Convert an input RGB image to a WAL instance, re-mapping its colors to the WAL palette in the process and generating the mipmaps.
#'
#' @param in_image numeric matrix with 3 dimensions: widt, height, channels. Values must be in range 0..1. This is the image format returned by \code{jpeg::readJPEG} and \code{png::readPNG}. The image can have arbitrary colors, but the colors in the final WAL image will be limited to the palette. Both the width and height must be powers of 2 (>= 8) typical idtech1/2 textures use 32, 64, ..., 512. The reason is the mipmaps.
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

  width = dim(in_image)[1];
  height = dim(in_image)[2];
  num_channels = dim(in_image)[3];

  if(num_channels != 3L) {
    stop("Parameter 'in_image': third dimension must have length 3 (channels R, G, B).");
  }
  supported_tex_sizes = c(8L, 16L, 32L, 64L, 128L, 256L, 512L, 1024L, 2048L, 4096L, 8192L);
  if( ! width %in% supported_tex_sizes) {
    stop(sprintf("Input image has invalid width %d, must be a power of 2 (8, 16, 32, 64, ...).", width));
  }
  if( ! height %in% supported_tex_sizes) {
    stop(sprintf("Input image has invalid height %d, must be a power of 2 (8, 16, 32, 64, ...).", height));
  }

  palette_col_indices = closest.color.from.palette(in_image, apply_palette);


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
  colors_lab = grDevices::convertColor(colors_rgb, from="sRGB", to="Lab");
  fixed_palette_lab = grDevices::convertColor(fixed_palette_rgb, from="sRGB", to="Lab");
  result_indices = rep(NA, nrow(colors_rgb));
  for(i in 1:nrow(colors_rgb)) {
    result_indices[i] = which.min(spacesXYZ::DeltaE(colors_lab[i,], fixed_palette_lab));
  }
  return(result_indices);
}

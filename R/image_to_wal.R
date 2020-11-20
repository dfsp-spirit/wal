
#' @title Convert image to WAL instance.
#'
#' @description Convert an input RGB image to a WAL instance, re-mapping its colors to the WAL palette in the process and generating the mipmaps.
#'
#' @param in_image numeric matrix with 3 dimensions: widt, height, channels. Values must be in range 0..1. This is the image format returned by \code{jpeg::readJPEG} and \code{png::readPNG}. The image can have arbitrary colors, but the colors in the final WAL image will be limited to the palette. Both the width and height must be powers of 2 (>= 8) typical idtech1/2 textures use 32, 64, ..., 512. The reason is the mipmaps.
#'
#' @param apply_palette n x 3 integer matrix, the palette for the WAL image. This is not saved to the wal image, but still required because the colors from the \code{in_image} will be adapted to the palette colors (replaced with the most similar ones). If the palette does not cover the colors in the source image well, the resulting WAL image will look bad (dissimilar to the source image).
#'
#' @return wal instance
#'
#' @examples
#' \dontrun{
#'    wal = img.to.wal(jpeg::readJPEG("~/mytex.jpg"));
#' }
#'
img.to.wal <- function(in_image, apply_palette = wal::pal_q2()) {
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
expand.rawdata.to.mipmaps <- function(raw_data_mip_level0, width, height) {
  raw_data_mip_level0 = as.integer(raw_data_mip_level0);
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

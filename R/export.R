
#' @title Export wal instance to JPEG format image file.
#'
#' @param wal a wal instance, as returned by \code{read.wal}
#'
#' @param filepath character string, path to the JPEG file to write, including the file extension.
#'
#' @param ... extra parameters passed to \code{jpeg::writeJPEG}. Can be used to set JPEG quality.
#'
#' @inheritParams read.wal
#'
#' @importFrom jpeg writeJPEG
#' @importFrom freesurferformats rotate3D flip3D
#' @export
wal.export.to.jpeg <- function(wal, filepath, apply_palette = wal::pal_q2(), ...) {
  if(! is.character(filepath)) {
    stop("Parameter 'filepath' must be a character string.");
  }
  if(is.null(apply_palette)) {
    stop("Paramter 'palette' must not be NULL");
  }
  check.palette(apply_palette);

  channel_red = apply_palette[wal$raw_data, 1];
  channel_green = apply_palette[wal$raw_data, 2];
  channel_blue = apply_palette[wal$raw_data, 3];

  jpeg_img = array( (c( channel_red , channel_green, channel_blue )) , dim = c( wal$header$width, wal$header$height, 3)) / 255.;
  jpeg_img = freesurferformats::rotate3D(jpeg_img, 3, 90)
  jpeg_img = freesurferformats::flip3D(jpeg_img, 1, "horizontally")
  jpeg::writeJPEG(jpeg_img, target = filepath, ...);
}

#' @title Export wal instance to PNG format image file.
#'
#' @param wal a wal instance, as returned by \code{read.wal}
#'
#' @param filepath character string, path to the PNG file to write, including the file extension.
#'
#' @param ... extra parameters passed to \code{png::writePNG}.
#'
#' @inheritParams read.wal
#'
#' @importFrom png writePNG
#' @importFrom freesurferformats rotate3D flip3D
#' @export
wal.export.to.png <- function(wal, filepath, apply_palette = wal::pal_q2(), ...) {
  if(! is.character(filepath)) {
    stop("Parameter 'filepath' must be a character string.");
  }
  if(is.null(apply_palette)) {
    stop("Paramter 'palette' must not be NULL");
  }
  check.palette(apply_palette);

  channel_red = apply_palette[wal$raw_data, 1];
  channel_green = apply_palette[wal$raw_data, 2];
  channel_blue = apply_palette[wal$raw_data, 3];

  png_img = array( (c( channel_red , channel_green, channel_blue )) , dim = c( wal$header$width, wal$header$height, 3)) / 255.;
  png_img = freesurferformats::rotate3D(png_img, 3, 90)
  png_img = freesurferformats::flip3D(png_img, 1, "horizontally")
  png::writePNG(png_img, target = filepath, ...);
}


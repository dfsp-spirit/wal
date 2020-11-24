# functions to read WAD files, which are archives holding several other files (think tar).


#' @title Read Quake WAD file.
#'
#' @param filepath character string, path to the file.
#'
#' @return a wad instance, can be used to extract data or list contents.
#'
#' @examples
#' \dontrun{
#'    wadf = '~/knave.wad';
#'    wad = read.wad(wadf);
#'    wad.contents(wad);
#' }
#'
#' @export
read.wad <- function(filepath) {
  fh = file(filepath, "rb");
  on.exit({ close(fh) });

  endian = 'little';

  wad = list('header' = list(), 'wad_dir' = list());

  wad$header$magic = readChar(fh, 4L);

  if(wad$header$magic != "WAD2") {
    stop("Not a supported WAD file: file magic mismatch.");
  }

  wad$header$num_entries = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  wad$header$dir_offset = readBin(fh, integer(), n = 1, size = 4, endian = endian);

  seek(fh, where = wad$header$dir_offset, origin = "start");
  wad$wad_dir$offset = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  wad$wad_dir$dsize = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  wad$wad_dir$size = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  wad$wad_dir$type = readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  wad$wad_dir$compression = readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian);
  wad$wad_dir$dummy = readBin(fh, integer(), n = 1, size = 2, signed = FALSE, endian = endian);
  wad$wad_dir$dir_name = readChar(fh, 16L);

  class(wad) = c(class(wad), "wad");
  return(wad);
}


#' @title Get integers representing WAD dir entry types.
#'
#' @keywords internal
#'
#' @seealso wad_dir.types.string
wad_dir.types.int <- function() {
  return(c(64L, 66L, 68L, 69L));
}

#' @title Get strings describing WAD dir entry types.
#'
#' @keywords internal
#'
#' @seealso wad_dir.types.int
wad_dir.types.string <- function() {
  return(c("color_palette", "pic_status_bar", "texture", "pic_console"));
}


#' @title List WAD file contents.
#'
#' @param wad a wad instance, see \code{read.wad}
#'
#' @return data.frame, the files inside the wad.
wad.contents <- function(wad) {

}

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

  wadentry_offset = c();
  wadentry_dsize = c();
  wadentry_size = c();
  wadentry_type = c();
  wadentry_compression = c();
  wadentry_dummy = c();
  wadentry_dir_name = c();

  for(entry_idx in 1L:wad$header$num_entries) {
    wadentry_offset = c(wadentry_offset, readBin(fh, integer(), n = 1, size = 4, endian = endian));
    wadentry_dsize = c(wadentry_dsize, readBin(fh, integer(), n = 1, size = 4, endian = endian));
    wadentry_size = c(wadentry_size, readBin(fh, integer(), n = 1, size = 4, endian = endian));
    wadentry_type = c(wadentry_type, readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian));
    wadentry_compression = c(wadentry_compression, readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian));
    wadentry_dummy = c(wadentry_dummy, readBin(fh, integer(), n = 1, size = 2, signed = FALSE, endian = endian));
    wadentry_dir_name = c(wadentry_dir_name, readChar(fh, 16L));
  }

  wadentry_type_string = get.wadentry.type.strings(wadentry_type);

  wad_dir_entries = data.frame("offset" = wadentry_offset, "dsize" = wadentry_dsize, "size" = wadentry_size, "type" = wadentry_type, "type_string" = wadentry_type_string, "compression" = wadentry_compression, "dummy" = wadentry_dummy, "dir_name" = wadentry_dir_name);
  wad$dir_entries = wad_dir_entries;

  class(wad) = c(class(wad), "wad");
  return(wad);
}


#' @title Translate wad directory entry types from the integer to the string representation.
#'
#' @param wadentry_type_int integer, WAD entry type code
#'
#' @return type string
#'
#' @keywords internal
get.wadentry.type.strings <- function(wadentry_type_int) {
  wadentry_string = rep("unknown", length(wadentry_type_int));
  cr = 1L;
  for(wti in wad_dir.types.int()) {
    wadentry_string[which(wadentry_type_int == wti)] = wad_dir.types.string()[cr];
    cr = cr + 1L;
  }
  return(wadentry_string);
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
#' @param wad a wad instance, see \code{read.wad}. Alternatively  a character string, which will be interpreted as a filepath to a WAD file that should be loaded.
#'
#' @return data.frame, info on the files inside the wad.
#' @export
wad.contents <- function(wad) {
  if(is.character(wad)) {
    wad = read.wad(wad);
  }
  contents = data.frame("type" = wad$dir_entries$type_string, "name" = wad$dir_entries$dir_name, "size" = wad$dir_entries$size);
  return(contents);
}

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

  wad = list('header' = list());

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

  wad$contents = data.frame("offset" = wadentry_offset, "dsize" = wadentry_dsize, "size" = wadentry_size, "type" = wadentry_type, "type_string" = wadentry_type_string, "compression" = wadentry_compression, "dummy" = wadentry_dummy, "dir_name" = wadentry_dir_name, stringsAsFactors = FALSE);

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


#' @title Read part of binary file and save as new file.
#'
#' @param infile for input file, part of it gets read.
#'
#' @param read_from integer, index at which to start reading, from start of file. Used to \code{seek} to the position.
#'
#' @param read_len integer, the number of bytes to read.
#'
#' @param outfile character string, the output filename.
#'
#' @keywords internal
save.filepart <- function(infile, read_from, read_len, outfile) {
  fh = file(infile, "rb");
  on.exit({ close(fh) });
  endian = "little";

  read_from = as.integer(read_from);
  if(read_from < 0L) { stop("Invalid read_from parameter."); }
  read_len = as.integer(read_len);
  if(read_len < 0L) { stop("Invalid read_len parameter."); }

  #cat(sprintf("Writing %d bytes starting at %d to new file %s.\n", read_len, read_from, outfile))

  seek(fh, where = read_from, origin = "start");
  raw_data = readBin(fh, raw(), n = read_len, size = 1L, endian = endian);
  close(fh);

  if(length(raw_data) != read_len) {
    warning(sprintf("Extracted filepart length mismatch: expected %d, read %d.\n", read_len, length(raw_data)));
  }

  fh_out = file(outfile, "wb");
  on.exit({ close(fh_out) });
  writeBin(raw_data, fh_out);
  flush(fh_out);
}


#' @title S3 print function for WAD
#'
#' @param x wad instance
#'
#' @param ... extra arguments, ignored
#'
#' @export
print.wad <- function(x, ...) {
  num_palettes = length(which(x$contents$type == 64L));
  num_pic_statbar = length(which(x$contents$type == 66L));
  num_tex = length(which(x$contents$type == 68L));
  num_pic_console = length(which(x$contents$type == 69L));
  cat(sprintf("WAD file holding %d palettes, %d statbar pics, %d textures and %d console pics.\n", num_palettes, num_pic_statbar, num_tex, num_pic_console));
}


#' @title Get strings describing WAD dir entry types.
#'
#' @keywords internal
#'
#' @seealso wad_dir.types.int
wad_dir.types.string <- function() {
  return(c("color_palette", "pic_status_bar", "texture", "pic_console"));
}


#' @title Get file extensions for WAD dir entry type strings.
#'
#' @keywords internal
#'
#' @return named list, which maps \code{wad_dir.types.string}s to file extensions. Afaik, there are not standard file extensions for these file types, and I made the ones used here up.
#'
#' @seealso wad_dir.types.string
wad_dir.fileext.mapping <- function() {
  ext_mapping = list("color_palette" = '.qpl', "pic_status_bar" = '.psb', "texture" = '.q1t', "pic_console" = '.pco', "default" = '.qrs');
  return(ext_mapping);
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
  contents = data.frame("type" = wad$contents$type_string, "name" = wad$contents$dir_name, "size" = wad$contents$size, stringsAsFactors = FALSE);
  return(contents);
}


#' @title Extract WAD contents into existing directory.
#'
#' @param wad_filepath character string, path to input WAD file.
#'
#' @param outdir character string, the output directory in which the files should be created. The filenames are derived from the data in the WAD.
#'
#' @param file_ext_mapping named list, with keys corresponding to the type names and values are file extensions, including the dot, to use for them.
#'
#' @note One can read extracted textures with \code{read.quake1miptex()}.
#'
#' @export
wad.extract <- function(wad_filepath, outdir = getwd(), file_ext_mapping = wad_dir.fileext.mapping()) {
  wad = read.wad(wad_filepath);
  if(nrow(wad$contents) > 0L) {
    for(row_idx in 1:nrow(wad$contents)) {
      out_filename_cleaned = wad.texname.clean(wad$contents$dir_name[row_idx]);

      file_ext = file_ext_mapping$default;
      if(wad$contents$type_string[row_idx] %in% file_ext_mapping) {
        file_ext = file_ext_mapping[[wad$contents$type_string[row_idx]]];
      }

      out_filepath = file.path(outdir, paste(out_filename_cleaned, file_ext, sep=""));
      save.filepart(wad_filepath, wad$contents$offset[row_idx], wad$contents$dsize[row_idx], out_filepath);
    }
  } else {
    warning("Empty WAD file.");
  }
}


#' @title Replace special chars in texture names to turn it into a valid filename.
#'
#' @param texnames character string, texture names from a WAD file. The textures may contain the special characters '*' and '+', which are used to indicate sequences (textures that change on an event, like a pressed button turning from red to green) and other things.
#'
#' @return character strings usable as filenames.
#'
#' @keywords internal
wad.texname.clean <- function(texnames) {
  texnames = gsub("\\*", "s__", texnames);
  texnames = gsub("\\+", "p__", texnames);
  return(texnames);
}


#' @title Read a Quake mipmap texture from a WAD2 file.
#'
#' @param filepath character string, path to WAD file.
#'
#' @param at_offset integer, the index in the WAD file where the texture starts.
#'
#' @return a 'qmiptex' instance, its like a wall with shorter name field (16 instead of 32) and some fields (anim_name, flags, contents, value) missing.
#'
#' @examples
#' \dontrun{
#'     qm = read.quake1miptex("~/knave.wad", at_offset = 1317632);
#'     plotwal.mipmap(qm, apply_palette = pal_q1());
#' }
#'
#' @export
read.quake1miptex <- function(filepath, at_offset = 0L) {
  fh = file(filepath, "rb");
  on.exit({ close(fh) });
  endian = "little";

  seek(fh, where = at_offset, origin = "start");
  qtex = list('header' = list());
  class(qtex) = c(class(qtex), 'qmiptex', 'wal');

  num_mip_levels = 4L;

  qtex$header$name = readChar(fh, 16L);
  qtex$header$width = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  qtex$header$height = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  qtex$header$mip_level_offsets = readBin(fh, integer(), n = num_mip_levels, size = 4, endian = endian);

  if(qtex$header$width <= 0L | qtex$header$height <= 0L) {
    stop("Invalid mipmap texture image dimensions");
  }

  data_length_all_mipmaps = sum(get.mipmap.data.lengths(qtex$header$width, qtex$header$height));
  pixel_data = readBin(fh, integer(), n = data_length_all_mipmaps, size = 1L, signed = FALSE, endian = endian);
  qtex$file_data_all_mipmaps = pixel_data + 1L;
  qtex$raw_data = get.wal.mipmap.data(qtex, mip_level = 0L);
  return(qtex);
}

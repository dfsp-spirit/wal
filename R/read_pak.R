# Functions to read PAK archives.

#' @title Read Quake PAK archive.
#'
#' @param filepath character string, path to the file including extension.
#'
#' @return a 'pak' instance.
#'
#' @examples
#' \dontrun{
#'    pakf = '~/.steam/steam/steamapps/common/Quake/Id1/PAK0.PAK';
#'    pak = read.pak(pakf);
#' }
#'
#' @export
read.pak <- function(filepath) {
  fh = file(filepath, "rb");
  on.exit({ close(fh) });

  endian = 'little';
  pak = list('header' = list('derived' = list()));

  pak$header$id = readChar(fh, 4L, useBytes = TRUE);

  if(pak$header$id != "PACK") {
    stop(sprintf("File '%s' not in Quake PACK format.\n", filepath));
  }

  pak$header$ft_offset = readBin(fh, integer(), n = 1L, size = 4L, endian = endian); # file table offset.
  pak$header$ft_size = readBin(fh, integer(), n = 1, size = 4, endian = endian); # size of file table.
  pak$header$derived$num_files = pak$header$ft_size / 64L;

  # read file data.
  seek(fh, where = pak$header$ft_offset, origin = "start");
  entry_file_names = rep(NA, pak$header$derived$num_files);
  entry_offsets = rep(NA, pak$header$derived$num_files);
  entry_sizes = rep(NA, pak$header$derived$num_files);
  for(entry_idx in 1:pak$header$derived$num_files) {
    entry_file_names[[entry_idx]] = readChar(fh, 56, useBytes = TRUE);
    entry_offsets[[entry_idx]] = readBin(fh, integer(), n = 1L, size = 4L, endian = endian); # file data offset.
    entry_sizes[[entry_idx]] = readBin(fh, integer(), n = 1L, size = 4L, endian = endian); # file data size.
  }
  pak$contents = data.frame('name' = entry_file_names, 'offset' = entry_offsets, 'size' = entry_sizes, stringsAsFactors = FALSE);

  class(pak) = c(class(pak), 'pak');
  return(pak);
}


#' @title Extract PAK contents into existing directory.
#'
#' @param pak_filepath character string, path to input PAK file.
#'
#' @param outdir character string, the output directory in which the files should be created. Must be writeable. The sub directories and filenames are derived from the data in the WAD.
#'
#' @note PAK files can contain a directory structure, and new subdirectories will be created under \code{outdir} as needed to preserve it.
#'
#' @export
pak.extract <- function(pak_filepath, outdir = getwd()) {
  if(! dir.exists(outdir)) {
    stop(sprintf("Base output directory '%d' does not exist.\n", outdir));
  }
  pak = read.pak(pak_filepath);
  if(nrow(pak$contents) > 0L) {
    for(row_idx in 1:nrow(pak$contents)) {
      out_filename_with_dir_part = pak$contents$name[row_idx]; # something like 'e1u1/metal2_2'.

      out_subdirs = file.path(outdir, dirname(out_filename_with_dir_part));
      if(! dir.exists(out_subdirs)) {
        dir.create(out_subdirs, recursive = TRUE);
      }

      out_filename = basename(out_filename_with_dir_part);

      out_filepath = file.path(out_subdirs, out_filename);
      save.filepart(pak_filepath, pak$contents$offset[row_idx], pak$contents$size[row_idx], out_filepath);
    }
  } else {
    warning("Empty PAK file.");
  }
}



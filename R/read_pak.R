# Functions to read PAK archives.

#' @title Read Quake PAK archive.
#'
#' @param filepath character string, path to the file including extension.
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
  pak$contents = data.frame('name' = entry_file_names, 'offset' = entry_offsets, 'size' = entry_sizes);

  class(pak) = c(class(pak), 'pak');
  return(pak);
}

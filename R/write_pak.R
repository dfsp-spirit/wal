
#' @title Create a PAK file from the contents of a directory.
#'
#' @param out_pakfile character string, the full path and PAK filename that should be created. If only a filename is given, it will be created in the current working directory.
#'
#' @param contents_dir character string, a directory. The contents, without the directory itself, will become the PAK file contents. Must exist and be readable.
#'
#' @examples
#' \dontrun{
#'     pd = "~/data/q1_pak0_extracted";
#'     pak.create("~/mypak0.pak", pd);
#' }
#'
#' @export
pak.create <- function(out_pakfile, contents_dir) {
  if(!dir.exists(contents_dir)) {
    stop(sprintf("The 'contents_dir' directory '%s' does not exist or cannot be read.\n", contents_dir));
  }

  # Collect data to write.
  pak_contents_filenames = list.files(path = contents_dir, full.names = FALSE, recursive = TRUE);
  num_files = length(pak_contents_filenames);

  pak_contents_offsets = rep(NA, num_files); # offset of file in PAK archive.
  pak_contents_lengths = rep(NA, num_files); # length of file in bytes.

  pak_header_length = 4L + 4L + 4L; # id char + size int + offset int.
  current_offset = pak_header_length + 1L; # start AFTER header.
  if(num_files > 0L) {
    for(file_idx in 1:num_files) {
      fname = pak_contents_filenames[[file_idx]];
      pak_contents_lengths[[file_idx]] = file.size(file.path(contents_dir, fname));
      pak_contents_offsets[[file_idx]] = current_offset; # start offset of file data in PAK.
      current_offset = current_offset + pak_contents_lengths[[file_idx]];
    }
    filetable = data.frame('name' = pak_contents_filenames, 'offset' = pak_contents_offsets, 'size' = pak_contents_lengths, stringsAsFactors = FALSE);

    # Write the actual PAK file.
    pak.write(out_pakfile, contents_dir, filetable);
  } else {
    stop("Empty contents_dir, cannot create PAK file.");
  }
}


#' @title Write a PAK file based on the filetable.
#'
#' @inheritParams pak.create
#'
#' @inheritParams pak.header.create
#'
#' @keywords internal
pak.write <- function(out_pakfile, contents_dir, filetable, filetable_at_end = TRUE) {

  pak = pak.header.create(filetable, filetable_at_end = filetable_at_end);

  # Write data.
  fh = file(out_pakfile, "wb");
  on.exit({ close(fh) });

  endian = 'little';
  writeChar(pak$header$id, fh, useBytes = TRUE, eos = NULL);
  writeBin(pak$header$ft_offset, fh, size = 4L, endian = endian);
  writeBin(pak$header$ft_size, fh, size = 4L, endian = endian);

  # Write filetable before data if requested. This is reflected in the header (ft_offset).
  if(! filetable_at_end) {
    for(file_entry_idx in 1:nrow(filetable)) {
      writeChar(filetable$name[[file_entry_idx]], fh, nchars = 56L, useBytes = TRUE);
      writeBin(filetable$offset[[file_entry_idx]], fh, size = 4L, endian = endian);
      writeBin(filetable$size[[file_entry_idx]], fh, size = 4L, endian = endian);
    }
  }

  # Write data for all files.
  for(file_entry_idx in 1:nrow(filetable)) {
    source_file = file.path(contents_dir, filetable$name[[file_entry_idx]]);
    read_len = filetable$size[[file_entry_idx]];
    if(! file.exists(source_file)) {
      stop(sprintf("Cannot read source file '%s' for PAK.\n", source_file));
    } else {
      fh_source = file(source_file, "rb");
      raw_data = readBin(fh_source, raw(), n = read_len, endian = endian);
      close(fh_source);
      if(length(raw_data) != read_len) {
        stop(sprintf("Could not read %d bytes for file '%s'\n", read_len, source_file));
      }
      writeBin(raw_data, fh, endian = endian);
    }
  }

  # Write filetable after data if requested. This is reflected in the header (ft_offset).
  if(filetable_at_end) {
    for(file_entry_idx in 1:nrow(filetable)) {
      writeChar(filetable$name[[file_entry_idx]], fh, nchars = 56L, useBytes = TRUE);
      writeBin(filetable$offset[[file_entry_idx]], fh, size = 4L, endian = endian);
      writeBin(filetable$size[[file_entry_idx]], fh, size = 4L, endian = endian);
    }
  }
}


#' @title Generate PAK header from filetable.
#'
#' @param filetable data.frame, the PAK contents filetable.
#'
#' @param filetable_at_end logical, whether to place the filetable at the end of the file, after all file data. If FALSE, it will be places after the PAK header, before the file data. Should not matter, leave this alone if in doubt.
#'
#' @return named list, the PAK stub. The only entry is the 'header' list.
#'
#' @keywords internal
pak.header.create <- function(filetable, filetable_at_end = TRUE) {
  pak = list('header' = list());
  pak$header$id = "PACK";
  pak$header$ft_size = nrow(filetable) * 64L;

  pak_header_size = 4L + 4L + 4L; # id + size + offset.
  total_file_size = sum(filetable$size); # length of file data.

  if(filetable_at_end) {
    pak$header$ft_offset = pak_header_size + total_file_size + 1L
  } else {
    pak$header$ft_offset = pak_header_size + 1L; # place filetable directly after header.
  }
  return(pak);
}


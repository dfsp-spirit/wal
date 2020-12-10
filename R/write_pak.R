
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
  pak_contents_filenames_pre = list.files(path = contents_dir, full.names = FALSE, recursive = TRUE);

  pak_contents_filenames = c(); # Filtered valid filenames.
  for(file_entry in pak_contents_filenames_pre) {
    if(nchar(file_entry) <= 56L) {
      pak_contents_filenames = c(pak_contents_filenames, file_entry);
    } else {
      warning(sprintf("Skipping invalid entry '%s', entry names are limited to 56 characters.\n", file_entry));
    }
  }

  num_files = length(pak_contents_filenames);

  pak_contents_offsets = rep(NA, num_files); # offset of file in PAK archive.
  pak_contents_lengths = rep(NA, num_files); # length of file in bytes.

  pak_header_length = 4L + 1L + 4L + 4L; # id char + eos + size int + offset int.
  current_offset = pak_header_length; # start AFTER header.
  if(num_files > 0L) {
    for(file_idx in 1:num_files) {
      fname = pak_contents_filenames[[file_idx]];
      if(nchar(fname) > 56L) {
        stop(sprintf("Invalid entry '%s', entry names are limited to 56 characters.\n", fname));
      }
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
pak.write <- function(out_pakfile, contents_dir, filetable) {

  pak = pak.header.create(filetable);

  # Write data.
  fh = file(out_pakfile, "wb");
  on.exit({ close(fh) });

  endian = 'little';
  writeChar(pak$header$id, fh, useBytes = TRUE, eos = NULL);
  #writeBin(pak$header$id, fh, endian = endian, useBytes = TRUE);
  cat(sprintf("Writing ft offset %d, size %d (%d entries).\n", pak$header$ft_offset, pak$header$ft_size, (pak$header$ft_size / 64)))
  writeBin(pak$header$ft_offset, fh, size = 4L, endian = endian);
  writeBin(pak$header$ft_size, fh, size = 4L, endian = endian);

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
      current_pos = seek(fh, where = NA);
      if(current_pos != (filetable$offset[[file_entry_idx]] - 1L)) {
        warning(sprintf("At position %d before writing %s data, expected %d.\n", current_pos, source_file, (filetable$offset[[file_entry_idx]])));
      }
      writeBin(raw_data, fh, endian = endian);
    }
  }

  # Write filetable after data.
  current_pos = seek(fh, where = NA);
  if(current_pos != (pak$header$ft_offset - 1L)) {
    warning(sprintf("At position %d before writing file table, expected %d.\n", current_pos, (pak$header$ft_offset)));
  }
  cat(sprintf("Writing file table at position %d.\n", current_pos));

  for(file_entry_idx in 1:nrow(filetable)) {
    entry_name = filetable$name[[file_entry_idx]];
    #cat(sprintf("Writing file table entry for file '%s'.\n", entry_name));
    writeChar(entry_name, fh, useBytes = TRUE);
    if(nchar(entry_name) < 56L) {
      if(nchar(entry_name) < 1L) {
        stop("Entry file name has 0 length.");
      } else {
        pad_length = 56L - nchar(entry_name);
        cat(sprintf("Entry '%s' has %d chars, adding padding of %d.\n", entry_name, nchar(entry_name), pad_length));
        writeBin(as.raw(rep(0L, pad_length)), fh, endian = endian); # fill remaining space up to max 56 bytes with zeroes.

      }

    }

    writeBin(filetable$offset[[file_entry_idx]], fh, size = 4L, endian = endian);
    writeBin(filetable$size[[file_entry_idx]], fh, size = 4L, endian = endian);
  }

}


#' @title Generate PAK header from filetable.
#'
#' @param filetable data.frame, the PAK contents filetable.
#'
#' @return named list, the PAK stub. The only entry is the 'header' list.
#'
#' @keywords internal
pak.header.create <- function(filetable) {
  pak = list('header' = list());
  pak$header$id = "PACK";
  pak$header$ft_size = as.integer(nrow(filetable) * 64L);

  pak_header_size = 12L; # id chars + size + offset.
  total_contents_size = sum(filetable$size); # length of file data.
  pak$header$ft_offset = pak_header_size + total_contents_size;
  return(pak);
}


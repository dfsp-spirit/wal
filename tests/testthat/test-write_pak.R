testthat::test_that("We can write and re-read a PAK file.", {
  #pakf = system.file("extdata", "test.pak", package = "wal", mustWork = TRUE);
  pakf = file.path("~/data/PAK0.PAK");
  if(! file.exists(pakf)) {
    testthat::skip("Test PAK file available");
  }

  pak = read.pak(pakf);

  td = tempdir(); # temp dir to extract PAK archive.

  pak.extract(pakf, outdir = td);

  # Check that the correct number of files exists.
  #testthat::expect_equal(length(list.files(path = td, recursive = TRUE)), 339L);

  # Repack into new archive.
  new_pak_file = tempfile(fileext = ".pak");
  pak.create(new_pak_file, contents_dir = td);

  testthat::expect_true(file.exists(new_pak_file));

  # The following check is disabled, as an identical file size is not required:
  # PAK files can contain unused data chunks (garbage) between entries and still be valid.
  #testthat::expect_equal(file.size(pakf), file.size(new_pak_file));

  pak_reread = read.pak(new_pak_file);

  testthat::expect_false(is.null(pak_reread$header));
  testthat::expect_false(is.null(pak_reread$contents));
  testthat::expect_equal(nrow(pak_reread$contents), 339L);
  testthat::expect_true(is.data.frame(pak_reread$contents));
})

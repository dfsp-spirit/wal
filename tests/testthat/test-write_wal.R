
testthat::test_that("We can write and re-read a WAL texture if available.", {
  walf = system.file("extdata", "bricks.wal", package = "wal", mustWork = TRUE);
  wal = wal::read.wal(walf);

  testthat::expect_equal(wal$header$tex_name, "e1u1/black");

  walf_written = tempfile(fileext = ".wal");
  writeWAL(walf_written, wal);
  wal_reread = read.wal(walf_written);

  testthat::expect_equal(wal$header$tex_name, wal_reread$header$tex_name);
  testthat::expect_equal(wal$header$width, wal_reread$header$width);
  testthat::expect_equal(wal$header$height, wal_reread$header$height);
  testthat::expect_equal(wal$header$flags, wal_reread$header$flags);
  testthat::expect_equal(wal$header$contents, wal_reread$header$contents);
  testthat::expect_equal(wal$header$value, wal_reread$header$value);

  testthat::expect_equal(wal$file_data_all_mipmaps, wal_reread$file_data_all_mipmaps);
})

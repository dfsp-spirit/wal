
testthat::test_that("We can extract a WAD file using qarchive.extract", {
  #wadf = system.file("extdata", "test.wad", package = "wal", mustWork = TRUE);
  wadf = file.path("~/data/knave.wad"); # get it from quaddicted.com
  if(! file.exists(wadf)) {
    testthat::skip("Test WAD file available");
  }

  # extract WAD
  td = tempdir();
  qarchive.extract(wadf, outdir = td);

  testthat::expect_true(file.exists(file.path(td, 'brimstone2.qrs')));
})


testthat::test_that("We can extract a PAK file using qarchive.extract", {
  #pakf = system.file("extdata", "test.pak", package = "wal", mustWork = TRUE);
  pakf = file.path("~/data/PAK0.PAK");
  if(! file.exists(pakf)) {
    testthat::skip("Test PAK file available");
  }

  # extract PAK
  td = tempdir();
  qarchive.extract(pakf, outdir = td);

  testthat::expect_true(file.exists(file.path(td, 'default.cfg')));
})


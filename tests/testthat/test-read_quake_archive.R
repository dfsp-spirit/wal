
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

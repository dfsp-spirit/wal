
testthat::test_that("We can read a WAD file.", {
  #wadf = system.file("extdata", "test.wad", package = "wal", mustWork = TRUE);
  wadf = file.path("~/data/knave.wad"); # get it from quaddicted.com
  if(! file.exists(wadf)) {
    testthat::skip("Test WAD file available");
  }

  wad = read.wad(wadf);

  testthat::expect_false(is.null(wad$header));
  testthat::expect_false(is.null(wad$contents));
  testthat::expect_equal(nrow(wad$contents), 455L);
  testthat::expect_true(is.data.frame(wad$contents));
})




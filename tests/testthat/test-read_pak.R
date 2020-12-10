
testthat::test_that("We can read a PAK file.", {
  #pakf = system.file("extdata", "test.pak", package = "wal", mustWork = TRUE);
  pakf = file.path("~/data/PAK0.PAK");
  if(! file.exists(pakf)) {
    testthat::skip("Test PAK file available");
  }

  pak = read.pak(pakf);

  testthat::expect_false(is.null(pak$header));
  testthat::expect_false(is.null(pak$contents));
  testthat::expect_equal(nrow(pak$contents), 339L);
  testthat::expect_true(is.data.frame(pak$contents));

  # extract PAK file
  td = tempdir();
  pak.extract(pakf, outdir = td);
})




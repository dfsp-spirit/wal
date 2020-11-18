
testthat::test_that("The Q1 palette is available.", {
  pal = wal::pal_q1();

  testthat::expect_true(is.matrix(pal));
  testthat::expect_true(is.integer(pal));
  testthat::expect_equal(dim(pal), c(256L, 3L));
})


testthat::test_that("The Q2 palette is available.", {
  pal = wal::pal_q2();

  testthat::expect_true(is.matrix(pal));
  testthat::expect_true(is.integer(pal));
  testthat::expect_equal(dim(pal), c(256L, 3L));
})

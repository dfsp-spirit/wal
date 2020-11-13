

testthat::test_that("We can read the Quake 2 Quad Damage base tex if available.", {
  walf = '~/data/q2_pak/textures/e1u2/basic1_7.wal';
  if( ! file.exists(walf)) {
    testthat::skip("Quake 2 texture e1u2/basic1_7 available");
  }
  wal = read.wal(walf);

  testthat::expect_true(is.matrix(pcx$palette));
})


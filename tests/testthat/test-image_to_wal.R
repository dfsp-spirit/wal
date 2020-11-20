
testthat::test_that("We can convert a JPEG image to WAL.", {

  jpgf = system.file("extdata", "Bricks050_256_Color.jpg", package = "wal", mustWork = TRUE);

  wal = wal::img.to.wal(jpeg::readJPEG(jpgf));

  testthat::expect_equal(dim(wal$image), c(256L, 256L));
})


testthat::test_that("We can convert a PNG image to WAL.", {

  pngf = system.file("extdata", "Bricks050_256_Color.png", package = "wal", mustWork = TRUE);

  wal = wal::img.to.wal(png::readPNG(pngf));

  testthat::expect_equal(dim(wal$image), c(256L, 256L));
})

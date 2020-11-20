
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


testthat::test_that("We can half a single-channel image", {

  pngf = system.file("extdata", "Bricks050_256_Color.png", package = "wal", mustWork = TRUE);
  png_img = png::readPNG(pngf);
  testthat::expect_equal(dim(png_img), c(256L, 256L, 3L));
  red_channel = png_img[,,1];
  testthat::expect_equal(dim(red_channel), c(256L, 256L));

  small_red_channel = wal:::half.image(red_channel);
  testthat::expect_equal(dim(small_red_channel), c(128L, 128L));
})

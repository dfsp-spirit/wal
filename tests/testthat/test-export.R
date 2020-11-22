

testthat::test_that("We can convert a WAL file to JPEG format.", {

  walf = system.file("extdata", "bricks.wal", package = "wal", mustWork = TRUE);

  wal = wal::read.wal(walf);
  tex_jpg = tempfile(fileext = ".jpg");
  wal::wal.export.to.jpeg(wal, tex_jpg);
})


testthat::test_that("We can convert a WAL file to PNG format.", {

  walf = system.file("extdata", "bricks.wal", package = "wal", mustWork = TRUE);

  wal = wal::read.wal(walf);
  tex_png = tempfile(fileext = ".png");
  wal::wal.export.to.png(wal, tex_png);
})


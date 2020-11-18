

testthat::test_that("We can convert a WAL file to JPEG format.", {

  #walf = '~/data/q2_pak/textures/e1u2/basic1_7.wal';
  walf = '~/data/q2_pak/textures/e1u1/endsign2.wal';
  if( ! file.exists(walf)) {
    testthat::skip("Quake 2 texture is available");
  }

  wal = wal::read.wal(walf);
  tex_jpg = tempfile(fileext = ".jpg");
  wal::wal.export.to.jpeg(wal, tex_jpg);
})

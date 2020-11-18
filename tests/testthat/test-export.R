

testthat::test_that("We can convert a WAL file to JPEG format.", {

  #walf = '~/data/q2_pak/textures/e1u2/basic1_7.wal';
  walf = '~/data/q2_pak/textures/e1u1/endsign2.wal';
  if( ! file.exists(walf)) {
    testthat::skip("Quake 2 texture e1u2/basic1_7 available");
  }

  wal = wal::read.wal(walf);

  testthat::expect_true(is.array(wal$image));

  # Write as JPEG:
  tex_jpg = tempfile(fileext = '.jpg');
  jpeg::writeJPEG(wal$image/255., target = tex_jpg);
})

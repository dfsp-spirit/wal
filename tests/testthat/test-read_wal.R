

testthat::test_that("We can read the Quake 2 Quad Damage base tex if available.", {
  walf = '~/data/q2_pak/textures/e1u2/basic1_7.wal';
  if( ! file.exists(walf)) {
    testthat::skip("Quake 2 texture e1u2/basic1_7 available");
  }
  wal = wal::read.wal(walf);

  testthat::expect_equal(wal$header$tex_name, "e1u2/basic1_7");
  testthat::expect_false(is.null(wal$image));
})


#' @importFrom jpeg writeJPEG
testthat::test_that("We can convert a WAL file to JPEG format.", {

  if( ! requireNamespace("jpeg", quietly = TRUE)) {
    testthat::skip("The 'jpeg' package needs to be installed for this test.");
  }

  walf = '~/data/q2_pak/textures/e1u2/basic1_7.wal';
  if( ! file.exists(walf)) {
    testthat::skip("Quake 2 texture e1u2/basic1_7 available");
  }

  wal = wal::read.wal(walf);

  testthat::expect_true(is.array(wal$image));

  # Write as JPEG:
  tex_jpg = tempfile(fileext = '.jpg');
  jpeg::writeJPEG(wal$image/255., target = tex_jpg);
})


# read the Quake 2 palette (requires extracted pak0.pak):
# pcx_palette = pcx::read.pcx("~/data/q2_pak/pics/colormap.pcx")
# pal = pcx$palette_rgb;


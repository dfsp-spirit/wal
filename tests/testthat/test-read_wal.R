

testthat::test_that("We can read the Quake 2 Quad Damage base tex if available.", {
  walf = '~/data/q2_pak/textures/e1u2/basic1_7.wal';
  if( ! file.exists(walf)) {
    testthat::skip("Quake 2 texture e1u2/basic1_7 available");
  }
  wal = wal::read.wal(walf);

  testthat::expect_equal(wal$header$tex_name, "e1u2/basic1_7");
  testthat::expect_false(is.null(wal$image));
})




# read the Quake 2 palette (requires extracted pak0.pak):
# pcx_palette = pcx::read.pcx("~/data/q2_pak/pics/colormap.pcx")
# pal = pcx$palette_rgb;


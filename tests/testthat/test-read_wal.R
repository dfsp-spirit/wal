

testthat::test_that("We can read the Quake 2 Quad Damage base tex if available.", {
  walf = system.file("extdata", "bricks.wal", package = "wal", mustWork = TRUE);
  wal = wal::read.wal(walf);

  testthat::expect_equal(wal$header$tex_name, "e1u1/black");
  testthat::expect_false(is.null(wal$image));
})


testthat::test_that("We can plot a WAL instance including mipmaps, and with custom palettes.", {
  walf = system.file("extdata", "bricks.wal", package = "wal", mustWork = TRUE);
  wal = wal::read.wal(walf);

  plotwal.mipmap(wal, apply_palette = wal::pal_q2(), mip_level = 0);
  plotwal.mipmap(wal, apply_palette = wal::pal_q2(), mip_level = 1);
  plotwal.mipmap(wal, apply_palette = wal::pal_q2(), mip_level = 2);
  plotwal.mipmap(wal, apply_palette = wal::pal_q2(), mip_level = 3);

  plotwal.mipmap(wal, apply_palette = wal::pal_q1());

  testthat::expect_error(plotwal.mipmap(wal, apply_palette = wal::pal_q2(), mip_level = 4)); # invalid mip_level
})





# read the Quake 2 palette (requires extracted pak0.pak):
# pcx_palette = pcx::read.pcx("~/data/q2_pak/pics/colormap.pcx")
# pal = pcx$palette_rgb;


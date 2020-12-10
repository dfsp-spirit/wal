

testthat::test_that("We can read a Quake II model in MD2 format.", {
  #md2f = system.file("extdata", "test.md2", package = "wal", mustWork = TRUE);
  md2f = file.path("~/data/q2_pak/models/items/quaddama/tris.md2"); # extract PAK0.pak from your Quake II CD.
  if(! file.exists(md2f)) {
    testthat::skip("Test MD2 file available");
  }

  md2 = read.quake.md2(md2f);

  testthat::expect_true(is.quakemodel(md2));
  testthat::expect_true(is.quakemodel_md2(md2));
  testthat::expect_false(is.quakemodel_mdl(md2));

  testthat::expect_false(is.null(md2$header));
  testthat::expect_false(is.null(md2$skins));
  testthat::expect_false(is.null(md2$triangles));
  testthat::expect_false(is.null(md2$frames));
  testthat::expect_false(is.null(md2$glcmds));

  # check model in in first frame (animation position)
  testthat::expect_equal(nrow(md2$frames[[1]]$vertex_coords), 78L); # vertex count.
  testthat::expect_equal(ncol(md2$frames[[1]]$vertex_coords), 3L);   # x,y,z coords are in columns.

  # test conversion to fs.surface
  fs_surf_from_md2 = quakemodel.to.fs.surface(md2);
  testthat::expect_false(is.null(fs_surf_from_md2$faces));
  testthat::expect_false(is.null(fs_surf_from_md2$vertices));
})

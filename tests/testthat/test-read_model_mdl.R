
testthat::test_that("We can read a Quake I model in MDL format.", {
  #mdlf = system.file("extdata", "test.mdl", package = "wal", mustWork = TRUE);
  mdlf = file.path("~/data/q1_pak/progs/shambler.mdl"); # extract PAK0.PAK from your Quake CD.
  if(! file.exists(mdlf)) {
    testthat::skip("Test MDL file available");
  }

  mdl = read.quake.mdl(mdlf);

  testthat::expect_true(is.quakemodel(mdl));
  testthat::expect_false(is.quakemodel_md2(mdl));
  testthat::expect_true(is.quakemodel_mdl(mdl));

  testthat::expect_false(is.null(mdl$header));
  testthat::expect_false(is.null(mdl$skins));
  testthat::expect_false(is.null(mdl$triangles));
  testthat::expect_false(is.null(mdl$frames));

  # check model in in first frame (animation position)
  testthat::expect_equal(mdl$frames[[1]]$name, "stand1");
  testthat::expect_equal(nrow(mdl$frames[[1]]$vertex_coords), 144L); # vertex count.
  testthat::expect_equal(ncol(mdl$frames[[1]]$vertex_coords), 3L);   # x,y,z coords are in columns.

  # test conversion to fs.surface
  fs_surf_from_mdl = quakemodel.to.fs.surface(mdl);
  testthat::expect_false(is.null(fs_surf_from_mdl$faces));
  testthat::expect_false(is.null(fs_surf_from_mdl$vertices));
})

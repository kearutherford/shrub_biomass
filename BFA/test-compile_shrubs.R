
test_that("Missing columns throw an error", {

  expect_error(CompileShrubs(shrub_data = bad_srs_shrubs_1, design = "SRS"),
               'shrub_data is missing necessary columns! For a SRS design, shrub_data must include:\ntime, site, plot, total_ag_Mg_ha, cover_perc, sc_tran_length')

  expect_error(CompileShrubs(shrub_data = bad_strs_shrubs_1, design = "STRS", wt_data = g_strs_wh_1),
               'shrub_data is missing necessary columns! For a STRS design, shrub_data must include:\ntime, site, stratum, plot, total_ag_Mg_ha, cover_perc, sc_tran_length')

  expect_error(CompileShrubs(shrub_data = bad_ffs_shrubs_1, design = "FFS"),
               'shrub_data is missing necessary columns! For a FFS design, shrub_data must include:\ntime, trt_type, site, plot, total_ag_Mg_ha, cover_perc, sc_tran_length')

})


test_that("Wrong column class throws an error", {

  expect_error(CompileShrubs(shrub_data = bad_srs_shrubs_8, design = "SRS"),
               'For shrub_data, time must be a character variable.\nThe time column is currently class: numeric')

  expect_error(CompileShrubs(shrub_data = bad_srs_shrubs_9, design = "SRS"),
               'For shrub_data, site must be a character variable.\nThe site column is currently class: numeric')

  expect_error(CompileShrubs(shrub_data = bad_srs_shrubs_10, design = "SRS"),
               'For shrub_data, plot must be a character variable.\nThe plot column is currently class: numeric')

  expect_error(CompileShrubs(shrub_data = bad_srs_shrubs_11, design = "SRS"),
               'For shrub_data, total_ag_Mg_ha must be a numeric variable.\nThe total_ag_Mg_ha column is currently class: character')

  expect_error(CompileShrubs(shrub_data = bad_srs_shrubs_12, design = "SRS"),
               'For shrub_data, cover_perc must be a numeric variable.\nThe cover_perc column is currently class: character')

  expect_error(CompileShrubs(shrub_data = bad_srs_shrubs_13, design = "SRS"),
               'For shrub_data, sc_tran_length must be a numeric variable.\nThe sc_tran_length column is currently class: character')

  expect_error(CompileShrubs(shrub_data = bad_strs_shrubs_3, design = "STRS", wt_data = g_strs_wh_1),
               'For shrub_data, stratum must be a character variable.\nThe stratum column is currently class: numeric')

  expect_error(CompileShrubs(shrub_data = bad_ffs_shrubs_3, design = "FFS"),
               'For shrub_data, trt_type must be a character variable.\nThe trt_type column is currently class: numeric')

})


test_that("Missing value handling works", {

  expect_error(CompileShrubs(shrub_data = bad_srs_shrubs_2, design = "SRS"),
               'For shrub_data, there are missing values in the time column.')

  expect_error(CompileShrubs(shrub_data = bad_srs_shrubs_3, design = "SRS"),
               'For shrub_data, there are missing values in the site column.')

  expect_error(CompileShrubs(shrub_data = bad_srs_shrubs_4, design = "SRS"),
               'For shrub_data, there are missing values in the plot column.')

  expect_error(CompileShrubs(shrub_data = bad_srs_shrubs_5, design = "SRS"),
               'For shrub_data, there are missing values in the total_ag_Mg_ha column.')

  expect_error(CompileShrubs(shrub_data = bad_srs_shrubs_6, design = "SRS"),
               'For shrub_data, there are missing values in the cover_perc column.')

  expect_error(CompileShrubs(shrub_data = bad_srs_shrubs_7, design = "SRS"),
               'For shrub_data, there are missing values in the sc_tran_length column.')

  expect_error(CompileShrubs(shrub_data = bad_strs_shrubs_2, design = "STRS", wt_data = g_strs_wh_1),
               'For shrub_data, there are missing values in the stratum column.')

  expect_error(CompileShrubs(shrub_data = bad_ffs_shrubs_2, design = "FFS"),
               'For shrub_data, there are missing values in the trt_type column.')

})


test_that("Output classes are as expected", {

  expect_equal(class(CompileShrubs(shrub_data = srs_shrubs, design = "SRS", wt_data = "not_needed")), "data.frame")
  expect_equal(class(CompileShrubs(shrub_data = strs_shrubs, design = "STRS", wt_data = g_strs_wh_1)), "list")
  expect_equal(class(CompileShrubs(shrub_data = ffs_shrubs, design = "FFS", wt_data = "not_needed")), "list")

})


test_that("STRS outputs have expected column names and classes", {

  strs_shrubs <- CompileShrubs(shrub_data = strs_shrubs,
                               design = "STRS",
                               wt_data = g_strs_wh_1)

  expect_named(strs_shrubs$stratum, c("time", "site", "stratum", "avg_ag_Mg_ha", "se_ag_Mg_ha", "avg_cover_perc", "se_cover_perc"))
  expect_equal(class(strs_shrubs$stratum$time), "character")
  expect_equal(class(strs_shrubs$stratum$site), "character")
  expect_equal(class(strs_shrubs$stratum$stratum), "character")
  expect_equal(class(strs_shrubs$stratum$avg_ag_Mg_ha), "numeric")
  expect_equal(class(strs_shrubs$stratum$se_ag_Mg_ha), "numeric")
  expect_equal(class(strs_shrubs$stratum$avg_cover_perc), "numeric")
  expect_equal(class(strs_shrubs$stratum$se_cover_perc), "numeric")

  expect_named(strs_shrubs$site, c("time", "site", "avg_ag_Mg_ha", "se_ag_Mg_ha", "avg_cover_perc", "se_cover_perc"))
  expect_equal(class(strs_shrubs$site$time), "character")
  expect_equal(class(strs_shrubs$site$site), "character")
  expect_equal(class(strs_shrubs$site$avg_ag_Mg_ha), "numeric")
  expect_equal(class(strs_shrubs$site$se_ag_Mg_ha), "numeric")
  expect_equal(class(strs_shrubs$site$avg_cover_perc), "numeric")
  expect_equal(class(strs_shrubs$site$se_cover_perc), "numeric")

})


test_that("SRS outputs have expected column names and classes", {

  srs_shrubs <- CompileShrubs(shrub_data = srs_shrubs,
                              design = "SRS",
                              wt_data = "not_needed")

  expect_named(srs_shrubs, c("time", "site", "avg_ag_Mg_ha", "se_ag_Mg_ha", "avg_cover_perc", "se_cover_perc"))
  expect_equal(class(srs_shrubs$time), "character")
  expect_equal(class(srs_shrubs$site), "character")
  expect_equal(class(srs_shrubs$avg_ag_Mg_ha), "numeric")
  expect_equal(class(srs_shrubs$se_ag_Mg_ha), "numeric")
  expect_equal(class(srs_shrubs$avg_cover_perc), "numeric")
  expect_equal(class(srs_shrubs$se_cover_perc), "numeric")

})


test_that("FFS outputs have expected column names and classes", {

  ffs_shrubs <- CompileShrubs(shrub_data = ffs_shrubs,
                              design = "FFS",
                              wt_data = "not_needed")

  expect_named(ffs_shrubs$site, c("time", "trt_type", "site", "avg_ag_Mg_ha", "se_ag_Mg_ha", "avg_cover_perc", "se_cover_perc"))
  expect_equal(class(ffs_shrubs$site$time), "character")
  expect_equal(class(ffs_shrubs$site$trt_type), "character")
  expect_equal(class(ffs_shrubs$site$site), "character")
  expect_equal(class(ffs_shrubs$site$avg_ag_Mg_ha), "numeric")
  expect_equal(class(ffs_shrubs$site$se_ag_Mg_ha), "numeric")
  expect_equal(class(ffs_shrubs$site$avg_cover_perc), "numeric")
  expect_equal(class(ffs_shrubs$site$se_cover_perc), "numeric")

  expect_named(ffs_shrubs$trt_type, c("time", "trt_type", "avg_ag_Mg_ha", "se_ag_Mg_ha", "avg_cover_perc", "se_cover_perc"))
  expect_equal(class(ffs_shrubs$trt_type$time), "character")
  expect_equal(class(ffs_shrubs$trt_type$trt_type), "character")
  expect_equal(class(ffs_shrubs$trt_type$avg_ag_Mg_ha), "numeric")
  expect_equal(class(ffs_shrubs$trt_type$se_ag_Mg_ha), "numeric")
  expect_equal(class(ffs_shrubs$trt_type$avg_cover_perc), "numeric")
  expect_equal(class(ffs_shrubs$trt_type$se_cover_perc), "numeric")

})



test_that("Output classes are as expected", {

  expect_equal(class(CompileSurfaceFuels(fwd_data = g_srs_fwd_m, cwd_data = g_srs_cwd_m, design = "SRS", wt_data = "not_needed", units = "metric")), "data.frame")
  expect_equal(class(CompileSurfaceFuels(fwd_data = g_strs_fwd_m, cwd_data = g_strs_cwd_m, design = "STRS", wt_data = g_strs_wh_1, units = "metric")), "list")
  expect_equal(class(CompileSurfaceFuels(fwd_data = g_ffs_fwd_m, cwd_data = g_ffs_cwd_m, design = "FFS", wt_data = "not_needed", units = "metric")), "list")

})


test_that("STRS outputs have expected column names and classes", {

  strs_1m <- CompileSurfaceFuels(fwd_data = g_strs_fwd_m,
                                 cwd_data = "none",
                                 design = "STRS",
                                 wt_data = g_strs_wh_1,
                                 units = "metric")

  expect_named(strs_1m$stratum, c("time", "site", "stratum", "avg_1h_Mg_ha", "se_1h_Mg_ha", "avg_10h_Mg_ha", "se_10h_Mg_ha", "avg_100h_Mg_ha", "se_100h_Mg_ha"))
  expect_equal(class(strs_1m$stratum$time), "character")
  expect_equal(class(strs_1m$stratum$site), "character")
  expect_equal(class(strs_1m$stratum$stratum), "character")
  expect_equal(class(strs_1m$stratum$avg_1h_Mg_ha), "numeric")
  expect_equal(class(strs_1m$stratum$se_1h_Mg_ha), "numeric")
  expect_equal(class(strs_1m$stratum$avg_10h_Mg_ha), "numeric")
  expect_equal(class(strs_1m$stratum$se_10h_Mg_ha), "numeric")
  expect_equal(class(strs_1m$stratum$avg_100h_Mg_ha), "numeric")
  expect_equal(class(strs_1m$stratum$se_100h_Mg_ha), "numeric")

  expect_named(strs_1m$site, c("time", "site", "avg_1h_Mg_ha", "se_1h_Mg_ha", "avg_10h_Mg_ha", "se_10h_Mg_ha", "avg_100h_Mg_ha", "se_100h_Mg_ha"))
  expect_equal(class(strs_1m$site$time), "character")
  expect_equal(class(strs_1m$site$site), "character")
  expect_equal(class(strs_1m$site$avg_1h_Mg_ha), "numeric")
  expect_equal(class(strs_1m$site$se_1h_Mg_ha), "numeric")
  expect_equal(class(strs_1m$site$avg_10h_Mg_ha), "numeric")
  expect_equal(class(strs_1m$site$se_10h_Mg_ha), "numeric")
  expect_equal(class(strs_1m$site$avg_100h_Mg_ha), "numeric")
  expect_equal(class(strs_1m$site$se_100h_Mg_ha), "numeric")

})


test_that("SRS outputs have expected column names and classes", {

  srs_1m <- CompileSurfaceFuels(fwd_data = g_srs_fwd_m,
                                cwd_data = "none",
                                design = "SRS",
                                wt_data = "not_needed",
                                units = "metric")

  expect_named(srs_1m, c("time", "site", "avg_1h_Mg_ha", "se_1h_Mg_ha", "avg_10h_Mg_ha", "se_10h_Mg_ha", "avg_100h_Mg_ha", "se_100h_Mg_ha"))
  expect_equal(class(srs_1m$time), "character")
  expect_equal(class(srs_1m$site), "character")
  expect_equal(class(srs_1m$avg_1h_Mg_ha), "numeric")
  expect_equal(class(srs_1m$se_1h_Mg_ha), "numeric")
  expect_equal(class(srs_1m$avg_10h_Mg_ha), "numeric")
  expect_equal(class(srs_1m$se_10h_Mg_ha), "numeric")
  expect_equal(class(srs_1m$avg_100h_Mg_ha), "numeric")
  expect_equal(class(srs_1m$se_100h_Mg_ha), "numeric")

})


test_that("FFS outputs have expected column names and classes", {

  ffs_1m <- CompileSurfaceFuels(fwd_data = g_ffs_fwd_m,
                                cwd_data = "none",
                                design = "FFS",
                                wt_data = "not_needed",
                                units = "metric")

  expect_named(ffs_1m$site, c("time", "trt_type", "site",  "avg_1h_Mg_ha", "se_1h_Mg_ha", "avg_10h_Mg_ha", "se_10h_Mg_ha", "avg_100h_Mg_ha", "se_100h_Mg_ha"))
  expect_equal(class(ffs_1m$site$time), "character")
  expect_equal(class(ffs_1m$site$trt_type), "character")
  expect_equal(class(ffs_1m$site$site), "character")
  expect_equal(class(ffs_1m$site$avg_1h_Mg_ha), "numeric")
  expect_equal(class(ffs_1m$site$se_1h_Mg_ha), "numeric")
  expect_equal(class(ffs_1m$site$avg_10h_Mg_ha), "numeric")
  expect_equal(class(ffs_1m$site$se_10h_Mg_ha), "numeric")
  expect_equal(class(ffs_1m$site$avg_100h_Mg_ha), "numeric")
  expect_equal(class(ffs_1m$site$se_100h_Mg_ha), "numeric")

  expect_named(ffs_1m$trt_type, c("time", "trt_type", "avg_1h_Mg_ha", "se_1h_Mg_ha", "avg_10h_Mg_ha", "se_10h_Mg_ha", "avg_100h_Mg_ha", "se_100h_Mg_ha"))
  expect_equal(class(ffs_1m$trt_type$time), "character")
  expect_equal(class(ffs_1m$trt_type$trt_type), "character")
  expect_equal(class(ffs_1m$trt_type$avg_1h_Mg_ha), "numeric")
  expect_equal(class(ffs_1m$trt_type$se_1h_Mg_ha), "numeric")
  expect_equal(class(ffs_1m$trt_type$avg_10h_Mg_ha), "numeric")
  expect_equal(class(ffs_1m$trt_type$se_10h_Mg_ha), "numeric")
  expect_equal(class(ffs_1m$trt_type$avg_100h_Mg_ha), "numeric")
  expect_equal(class(ffs_1m$trt_type$se_100h_Mg_ha), "numeric")

})


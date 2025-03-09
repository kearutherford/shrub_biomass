
test_that("Invalid settings throw an error", {

  expect_error(ShrubBiomass(data = good_shrubs,
                            results = "by_plots"),
               'The "results" parameter must be set to either "by_shrub" or "by_plot".')

})


test_that("Missing columns throw an error", {

  expect_error(ShrubBiomass(data = bad_shrubs_1),
               'Input data is missing the necessary "site" column.')

  expect_error(ShrubBiomass(data = bad_shrubs_2),
               'Input data is missing the necessary "plot" column.')

  expect_error(ShrubBiomass(data = bad_shrubs_3),
               'Input data is missing the necessary "transect" column.')

  expect_error(ShrubBiomass(data = bad_shrubs_4),
               'Input data is missing the necessary "tran_length" column.')

  expect_error(ShrubBiomass(data = bad_shrubs_5),
               'Input data is missing the necessary "species" column.')

  expect_error(ShrubBiomass(data = bad_shrubs_6),
               'Input data is missing the necessary "ht" column.')

  expect_error(ShrubBiomass(data = bad_shrubs_7),
               'Input data is missing the necessary "crown_axis_1" column.')

  expect_error(ShrubBiomass(data = bad_shrubs_8),
               'Input data is missing the necessary "crown_axis_2" column.')

  expect_error(ShrubBiomass(data = bad_shrubs_33),
               'Input data is missing the necessary "slope" column.')

})


test_that("Column class handling works", {

  expect_error(ShrubBiomass(data = bad_shrubs_9),
               '"site" must be a character variable.\nYou have input a variable of class: numeric')

  expect_error(ShrubBiomass(data = bad_shrubs_10),
               '"plot" must be a character variable.\nYou have input a variable of class: numeric')

  expect_error(ShrubBiomass(data = bad_shrubs_11),
               '"transect" must be a character variable.\nYou have input a variable of class: numeric')

  expect_error(ShrubBiomass(data = bad_shrubs_12),
               '"tran_length" must be a numerical variable.\nYou have input a variable of class: character')

  expect_error(ShrubBiomass(data = bad_shrubs_13),
               '"species" must be a character variable.\nYou have input a variable of class: factor')

  expect_error(ShrubBiomass(data = bad_shrubs_14),
               '"ht" must be a numerical variable.\nYou have input a variable of class: character')

  expect_error(ShrubBiomass(data = bad_shrubs_15),
               '"crown_axis_1" must be a numerical variable.\nYou have input a variable of class: character')

  expect_error(ShrubBiomass(data = bad_shrubs_16),
               '"crown_axis_2" must be a numerical variable.\nYou have input a variable of class: character')

  expect_error(ShrubBiomass(data = bad_shrubs_34),
               '"slope" must be a numerical variable.\nYou have input a variable of class: character')

})


test_that("NA handling works for site, plot and transect columns", {

  expect_error(ShrubBiomass(data = bad_shrubs_17),
               'There are missing values in the site column in the provided dataframe.')

  expect_error(ShrubBiomass(data = bad_shrubs_18),
               'There are missing values in the plot column in the provided dataframe.')

  expect_error(ShrubBiomass(data = bad_shrubs_19),
               'There are missing values in the transect column in the provided dataframe.')

})


test_that("Transect length handling works", {

  expect_error(ShrubBiomass(data = bad_shrubs_20),
               'There are missing transect lengths in the provided dataframe.')

  expect_error(ShrubBiomass(data = bad_shrubs_21),
               'There are transect lengths <= 0 in the provided dataframe. All tran_length values must be > 0.')

  expect_error(ShrubBiomass(data = bad_shrubs_22),
               'Each site:plot:transect should have the same transect length recorded.\nThe following site:plot:transect combinations have multiple tran_length values: SEKI_1_1   ')

})


test_that("Species code handling works", {

  expect_error(ShrubBiomass(data = bad_shrubs_23),
               'Not all species codes were recognized!\nUnrecognized codes: AMALL')

  expect_error(ShrubBiomass(data = bad_shrubs_24),
               'There are missing species codes in the provided dataframe. Consider using OTHER or NONE for these shrubs.')

  expect_error(ShrubBiomass(data = bad_shrubs_25),
               'There are transects with a recorded species code of NONE, but with more than one row.\nTransects with no shrubs should be represented by a single row with site, plot, transect, and tran_length filled in as appropriate and a species code of NONE.')

  expect_warning(ShrubBiomass(data = bad_shrubs_26),
                 'There are transects with a recorded species code of NONE, but with non-NA ht, crown_axis_1 and/or crown_axis_2.\nTransects with no shrubs should be represented by a single row with site, plot, transect, and tran_length filled in as appropriate,\na species code of NONE, and NA ht, crown_axis_1, and/or crown_axis_2. Consider investigating these entries.')

})


test_that("Height handling works", {

  expect_error(ShrubBiomass(data = bad_shrubs_27),
               'There are missing shrub heights in the provided dataframe - outside of transects with species NONE, signifying transects with no shrubs, which should have NA ht.')

  expect_error(ShrubBiomass(data = bad_shrubs_28),
               'There are shrub heights <= 0 in the provided dataframe. All heights must be > 0.')

})


test_that("Crown axis handling works", {

  expect_error(ShrubBiomass(data = bad_shrubs_29),
               'There are missing crown axis 2 values in the provided dataframe - outside of transects with species NONE, signifying transects with no shrubs, which should have NA crown_axis_1.')

  expect_error(ShrubBiomass(data = bad_shrubs_30),
               'There are missing crown axis 2 values in the provided dataframe - outside of transects with species NONE, signifying transects with no shrubs, which should have NA crown_axis_2.')

  expect_error(ShrubBiomass(data = bad_shrubs_31),
               'There are crown axis 1 values <= 0 in the provided dataframe. All values must be > 0.')

  expect_error(ShrubBiomass(data = bad_shrubs_32),
               'There are crown axis 2 values <= 0 in the provided dataframe. All values must be > 0.')

})


test_that("Slope handling works", {

  expect_error(ShrubBiomass(data = bad_shrubs_35),
               'There are missing slopes in the provided dataframe.')

  expect_error(ShrubBiomass(data = bad_shrubs_36),
               'There are slopes < 0 in the provided dataframe. All slopes must be >= 0.')

  expect_error(ShrubBiomass(data = bad_shrubs_37),
               'Each site:plot:transect should have the same slope recorded.\nThe following site:plot:transect combinations have multiple slopes: SEKI_1_1   ')

})


test_that("Dataframes have expected column names", {

  expect_named(ShrubBiomass(data = good_shrubs,
                            results = "by_shrub"),
               c("site","plot","transect","tran_length","species","ht","crown_axis_1","crown_axis_2","total_ag_kg","extra_col"))

  expect_named(ShrubBiomass(data = good_shrubs,
                            results = "by_plot"),
               c("site","plot","total_ag_Mg_ha"))

})


test_that("Final column classes are as expected", {

  data_by_shrub <- ShrubBiomass(data = good_shrubs, results = "by_shrub")

  expect_equal(class(data_by_shrub$site), "character")
  expect_equal(class(data_by_shrub$plot), "character")
  expect_equal(class(data_by_shrub$transect), "character")
  expect_equal(class(data_by_shrub$tran_length), "numeric")
  expect_equal(class(data_by_shrub$species), "character")
  expect_equal(class(data_by_shrub$ht), "numeric")
  expect_equal(class(data_by_shrub$crown_axis_1), "numeric")
  expect_equal(class(data_by_shrub$crown_axis_2), "numeric")
  expect_equal(class(data_by_shrub$total_ag_kg), "numeric")

  data_by_shrub <- ShrubBiomass(data = good_shrubs, results = "by_plot")

  expect_equal(class(data_by_shrub$site), "character")
  expect_equal(class(data_by_shrub$plot), "character")
  expect_equal(class(data_by_shrub$total_ag_Mg_ha), "numeric")

})


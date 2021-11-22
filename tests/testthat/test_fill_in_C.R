#####################################################
### Tests for function fill_in_C 0.0.8 2021-11-22 ###
#####################################################

## Changelog:
# MH 0.0.8 2021-11-22: renamed build_C to fill_in_C
# MH 0.0.6 2021-09-21: update due to change in structure of list C
# MH 0.0.4 2021-09-08: initial tests

# package testthat
require( testthat )

# source fill_in_C and necessary functions (only when running this file manually)
# source( "../../R/verbose_argument_handling.R" )
# source( "../../R/create_empty_list.R" )
# source( "../../R/fill_in_info_model.R" )
# source( "../../R/lav_parTable_fill_labels.R" )
# source( "../../R/fill_in_C.R" )

# file with expected outcomes
file.expected <- file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/expected_test_results/fill_in_C-0.0.8-2021-11-22_expected.Rdata" )
load( file.expected )


### test object 00_lavaan_test_object
load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/00_lavaan_test_object.Rdata" ) )
o00_internal_list <- create_empty_list()
o00_internal_list <- fill_in_info_model( o00_internal_list, o00_lavaan_test_object )
o00_internal_list <- fill_in_C( o00_internal_list )
# o00__build_C__expected__internal_list_info_model_C_values <- o00_internal_list$info_model$C$values
# o00__build_C__expected__internal_list_info_model_C_labels <- o00_internal_list$info_model$C$labels
expect_identical( o00_internal_list$info_model$C$values, o00__build_C__expected__internal_list_info_model_C_values )
expect_identical( o00_internal_list$info_model$C$labels, o00__build_C__expected__internal_list_info_model_C_labels )


### test object 01_lavaan_test_object
load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/01_lavaan_test_object.Rdata" ) )
o01_internal_list <- create_empty_list()
o01_internal_list <- fill_in_info_model( o01_internal_list, o01_lavaan_test_object )
o01_internal_list <- fill_in_C( o01_internal_list )
# o01__build_C__expected__internal_list_info_model_C_values <- o01_internal_list$info_model$C$values
# o01__build_C__expected__internal_list_info_model_C_labels <- o01_internal_list$info_model$C$labels
expect_identical( o01_internal_list$info_model$C$values, o01__build_C__expected__internal_list_info_model_C_values )
expect_identical( o01_internal_list$info_model$C$labels, o01__build_C__expected__internal_list_info_model_C_labels )


### test object 02_lavaan_test_object
load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/02_lavaan_test_object.Rdata" ) )
o02_internal_list <- create_empty_list()
o02_internal_list <- fill_in_info_model( o02_internal_list, o02_lavaan_test_object )
o02_internal_list <- fill_in_C( o02_internal_list )
# o02__build_C__expected__internal_list_info_model_C_values <- o02_internal_list$info_model$C$values
# o02__build_C__expected__internal_list_info_model_C_labels <- o02_internal_list$info_model$C$labels
expect_identical( o02_internal_list$info_model$C$values, o02__build_C__expected__internal_list_info_model_C_values )
expect_identical( o02_internal_list$info_model$C$labels, o02__build_C__expected__internal_list_info_model_C_labels )


### test object 03_lavaan_test_object
load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/03_lavaan_test_object.Rdata" ) )
o03_internal_list <- create_empty_list()
o03_internal_list <- fill_in_info_model( o03_internal_list, o03_lavaan_test_object )
o03_internal_list <- fill_in_C( o03_internal_list )
# o03__build_C__expected__internal_list_info_model_C_values <- o03_internal_list$info_model$C$values
# o03__build_C__expected__internal_list_info_model_C_labels <- o03_internal_list$info_model$C$labels
expect_identical( o03_internal_list$info_model$C$values, o03__build_C__expected__internal_list_info_model_C_values )
expect_identical( o03_internal_list$info_model$C$labels, o03__build_C__expected__internal_list_info_model_C_labels )


### save expected
# save( o00__build_C__expected__internal_list_info_model_C_values,
      # o00__build_C__expected__internal_list_info_model_C_labels,
      # o01__build_C__expected__internal_list_info_model_C_values,
      # o01__build_C__expected__internal_list_info_model_C_labels,
      # o02__build_C__expected__internal_list_info_model_C_values,
      # o02__build_C__expected__internal_list_info_model_C_labels,
      # o03__build_C__expected__internal_list_info_model_C_values,
      # o03__build_C__expected__internal_list_info_model_C_labels,
      # file=file.expected )

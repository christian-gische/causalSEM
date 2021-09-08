###################################################
### Tests for function build_C 0.0.4 2021-09-08 ###
###################################################

## Changelog:
# MH 0.0.4 2021-09-08: initial tests

# package testthat
require( testthat )

# source build_C and necessary functions
source( "../../R/verbose_argument_handling.R" )
source( "../../R/make_empty_list.R" )
source( "../../R/populate_model_info.R" )
source( "../../R/lav_parTable_fill_labels.R" )
source( "../../R/build_C.R" )

# file with expected outcomes
file.expected <- file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/68_causalSEM/07_expected/build_C-0.0.4-2021-09-08_expected.Rdata" )
load( file.expected )


### test object 00_lavaan_test_object
load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/00_lavaan_test_object.Rdata" ) )
o00_internal_list <- make_empty_list()
o00_internal_list$fitted_object <- o00_lavaan_test_object
o00_internal_list$fitted_object_class <- class( o00_lavaan_test_object )
o00_internal_list <- populate_model_info( o00_internal_list )
o00_internal_list <- build_C( o00_internal_list )
# o00_internal_list$info_model$C
# o00__build_C__expected__internal_list_info_model_C <- o00_internal_list$info_model$C
expect_identical( o00_internal_list$info_model$C, o00__build_C__expected__internal_list_info_model_C )


### test object 01_lavaan_test_object
load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/01_lavaan_test_object.Rdata" ) )
o01_internal_list <- make_empty_list()
o01_internal_list$fitted_object <- o01_lavaan_test_object
o01_internal_list$fitted_object_class <- class( o01_lavaan_test_object )
o01_internal_list <- build_C( o01_internal_list )
o01_internal_list <- populate_model_info( o01_internal_list )
# o01_internal_list$info_model$C
# o01__build_C__expected__internal_list_info_model_C <- o01_internal_list$info_model$C
expect_identical( o01_internal_list$info_model$C, o01__build_C__expected__internal_list_info_model_C )


### test object 02_lavaan_test_object
load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/02_lavaan_test_object.Rdata" ) )
o02_internal_list <- make_empty_list()
o02_internal_list$fitted_object <- o02_lavaan_test_object
o02_internal_list$fitted_object_class <- class( o02_lavaan_test_object )
o02_internal_list <- build_C( o02_internal_list )
o02_internal_list <- populate_model_info( o02_internal_list )
# o02_internal_list$info_model$C
# o02__build_C__expected__internal_list_info_model_C <- o02_internal_list$info_model$C
expect_identical( o02_internal_list$info_model$C, o02__build_C__expected__internal_list_info_model_C )


### test object 03_lavaan_test_object
load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/03_lavaan_test_object.Rdata" ) )
o03_internal_list <- make_empty_list()
o03_internal_list$fitted_object <- o03_lavaan_test_object
o03_internal_list$fitted_object_class <- class( o03_lavaan_test_object )
o03_internal_list <- build_C( o03_internal_list )
o03_internal_list <- populate_model_info( o03_internal_list )
# o03_internal_list$info_model$C
# o03__build_C__expected__internal_list_info_model_C <- o03_internal_list$info_model$C
expect_identical( o03_internal_list$info_model$C, o03__build_C__expected__internal_list_info_model_C )


### save expected
# save( o00__build_C__expected__internal_list_info_model_C,
      # o01__build_C__expected__internal_list_info_model_C,
	  # o02__build_C__expected__internal_list_info_model_C,
	  # o03__build_C__expected__internal_list_info_model_C,
      # file=file.expected )

intervention_effect <- function(model, intervention, outcome = NULL, intervention_level,
                                effect.type = NULL , lower.bound = NULL,
                                upper.bound = NULL, verbose = 0, ...){

# check if arguments are well-specified FORMALLY; 
# give warnings, error, etc.
# later task
# Check of supported class
# Check if all variables are "manifest"
# Check of verbosity argument 
# using (or merging) the verbose_argument_handling function
# todo: allow object of class causalSEM as input to the 
# intervention effect function
  
check_arguments(model, intervention, outcome, levels, effect.type, 
                lower.bound, upper.bound, verbose ...)  
  

# creates empty list
internal_list <- make_empty_list()
  
# fills model info into list
# todo: fills two slots of internal list: 
# 1. fitted_object
# 2. fitted_object_class


internal_list <- populate_model_info(internal_list = internal_list, 
                                     model = model)
                                     
                                     
  
# fills intervention info into list
internal_list <- populate_intervention_info(internal_list = internal_list, 
                                            intervention = intervention,
                                            outcome = outcome, 
                                            intervention_level = intervention_level,
                                            effect.type = effect.type, 
                                            lower.bound = lower.bound,
                                            upper.bound = upper.bound )


# build matrix of structural coefficients
internal_list <- build_c(internal_list = internal_list) 

#
# add other functions from the flow chart analogously
#
#

# prepare output 
internal_list <- prepare_output(internal_list = internal_list,)

# assign class to list
class(internal list) <- "causalSEM"

# create output
return(internal_list)
}
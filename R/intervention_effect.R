intervention_effect <- function(model, intervention, outcome = NULL, intervention_level,
                                effect.type = NULL , lower.bound = NULL,
                                upper.bound = NULL, verbose = 0, ...){

# check if arguments are well-specified FORMALLY; 
# give warnings, error, etc.
# later task
  
check_arguments(model, intervention, outcome, levels, effect.type, 
                lower.bound, upper.bound, ...)  
  

# creates empty list
internal_list <- make_empty_list()
  
# fills model info into list
internal_list <- populate_model_info(internal_list = internal_list, 
                                     model = model, )
  
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
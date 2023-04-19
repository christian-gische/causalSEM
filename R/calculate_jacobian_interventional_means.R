## Changelog:
# CG 0.0.4 2023-02-23: include check of user-specified arguments model
# CG 0.0.3 2022-01-13:  changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MA 0.0.2 2021-11-19: function makes use of calculate_constant_matrices and
#                      other small changes. Should accept scalar and vector x 
#                      now.
# CG 0.0.1 2021-11-10: initial programming

## Documentation
#' @title Calculate Jacobian of Interventional Mean
#' @description Calculate Jacobian of the interventional mean Vector for a 
#' specific interventional level (see, for example, Eq. 18a in Gische and 
#' Voelkle, 2022).
#' @param model Object of class \code{causalSEM}.
#' @param x Numveric vector of interventional levels.
#' @param intervention_names Character vector of names of interventional 
#' variables.
#' @param outcome_names Character vector of names of outcome variables.
#' @param use_model_values Logical indicating if values stored in model object 
#' should be used (TRUE).
#' @param verbose Integer number setting verbosity of console outputs.
#' @return The Jacobian of interventional mean vector (numeric matrices)
#'    as defined in Eq. 18a in Gische and Voelkle (2022).
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
calculate_jacobian_interventional_means <- 
  function(model = NULL,
           x = NULL,
           intervention_names = NULL,
           outcome_names = NULL,
           verbose = NULL,
           use_model_values = FALSE){

  # function name
  fun_name <- "calculate_jacobian_interventional_means"

  # function version
  fun_version <- "0.0.3 2022-01-13"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")
  
  # CG 0.0.4 2023-02-23: include check of user-specified arguments model
  # get class of model object
  model_class <- class(model)
  
  # set supported classes of model objects
  supported_model_classes <- c( "causalSEM" )
  
  # check if argument model is supported
  if(!any(model_class %in% supported_model_classes)) stop(
    paste0(
      fun.name.version, ": model of class ", model_class,
      " not supported. Supported fit objects are: ",
      paste(supported_model_classes, collapse = ", ")
    )
  )
  
  # get verbose argument
  verbose <- handle_verbose_argument(verbose) 

  # console output
  if(verbose >= 2) cat(paste0( "start of function ", fun_name_version, " ",
                               Sys.time(), "\n" ))

  if(use_model_values == TRUE) {
    
    # assign values from the model objects to function arguments
    x <- model$info_interventions$intervention_levels
    intervention_names <- model$info_interventions$intervention_names
    outcome_names <- model$info_interventions$outcome_names
    
    # Create identity matrix
    I_n <- diag(model$info_model$n_ov)
    
    # Selection
    ONE_I <- model$constant_matrices$select_intervention
    I_N <- model$constant_matrices$eliminate_intervention
    
    # C and Psi matrices
    C <- model$info_model$C$values
    
    # Jacobian matrices
    jac_C <- model$info_model$C$derivative
    
    # Asymptotic covariance matrix of the model parameters
    acov <- model$info_model$param$varcov_par_unique
    
    # Compute transformation matrix
    
    C_trans <- solve(I_n - I_N %*% C)
   
  } else {
    
    # TODO: include argument check of 
    # x = NULL,
    # intervention_names = NULL,
    # outcome_names = NULL,
    # verbose = NULL
    
    # Calculate zero one matrices
    constant_matrices <- calculate_constant_matrices(
      model = model,
      intervention_names = intervention_names,
      outcome_names = outcome_names,
      verbose = verbose)
    
    # Create identity matrix
    I_n <- diag(model$info_model$n_ov)
    
    # Selection
    ONE_I <- constant_matrices$select_intervention
    I_N <- constant_matrices$eliminate_intervention
    
    # C and Psi matrices
    C <- model$info_model$C$values
    
    # Jacobian matrices
    jac_C <- model$info_model$C$derivative
    
    # Asymptotic covariance matrix of the model parameters
    acov <- model$info_model$param$varcov_par_unique
    
    # Compute transformation matrix
    
    C_trans <- solve(I_n - I_N %*% C)
    
  }
  

  
  # Calculate Jacobian g1 ----
  # TODO: allow x to be a vector

  jac_g1 <- kronecker(X = t(x) %*% t(ONE_I) %*% t(C_trans), 
                      Y = C_trans %*% I_N) %*% jac_C

  # return jacobian matrix
  jac_g1

}




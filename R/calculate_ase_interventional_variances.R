## Changelog:
# CG 0.0.3 2022-01-13: changed structure of internal_list
#                      cleaned up code (documentation, 80 char per line)
#                      changed dot-case to snake-case
# CG 0.0.2 2021-11-24: replaced $variance with $covariance in internal list path
# MA 0.0.1 2021-11-19: initial programming

## Documentation
#' @title Calculate Asymptotics of the Interventional Variances
#' @description Calculates the asysmptotic covariance matrix,
#' the aysmptotic standard errors, and the approximate z-values 
#' of the of the interventional variances for a specific interventional level.
#' @param model internal_list or object of class causalSEM
#' @param x interventional level
#' @param intervention_names names of interventional variables
#' @param outcome_names name of outcome variable
#' @param verbose verbosity of console outputs
#' @return The asysmptotic covariance matrix, the aysmptotic standard errors, 
#' and the approximate z-values of the the interventional variances for a 
#' specific interventional level.
#' (see Corollaries, 10, 11 in Gische and Voelkle, 2021).
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z

calculate_ase_interventional_variances <- function(model = NULL,
                                                   x = NULL,
                                                   intervention_names = NULL,
                                                   outcome_names = NULL,
                                                   use_model_values = FALSE,
                                                   verbose = NULL) {

  ######## TODO: REMOVE DEBUG
  # START DEBUG
  
  #model <- internal_list_no_outcome
  #use_model_values <- FALSE
  #intervention_names <- "x2" 
  #outcome_names <- "y3"
  #x <- 11.54
  # END DEBUG
  
  # function name
  fun_name <- "calculate_ase_interventional_variances"

  # function version
  fun_version <- "0.0.3 2022-01-13"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")
  
  # CG 0.0.3 2023-02-23: include check of user-specified arguments model
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
  
  if(use_model_values == TRUE) {
    
    
    verbose <- model$control$verbose
    x <- model$info_interventions$intervention_levels
    intervention_names <- model$info_interventions$intervention_names
    outcome_names <- model$info_interventions$outcome_names
    
    gamma_2 <- model$interventional_distribution$covariance_matrix$values
    jac_g2 <- model$interventional_distribution$covariance_matrix$jacobian
    constant_matrices <- model$constant_matrices
      
    
  } else {
    
    #TODO: argument check
    
    verbose <- handle_verbose_argument(verbose)
    x <- x
    intervention_names <- intervention_names
    outcome_names <- outcome_names
    
    # get zero one matrices
    constant_matrices <- calculate_constant_matrices(
      model = model,
      intervention_names = intervention_names,
      outcome_names = outcome_names)
    
    # get interventional variances
    # TODO: check: do we need function call here?
    gamma_2 <- calculate_interventional_covariance_matrix(
      C = model$info_model$C$values,
      Psi= model$info_model$Psi$values,
      x = x,
      SI = constant_matrices$select_intervention,
      n = model$info_model$n_ov,
      IN = constant_matrices$eliminate_intervention)
    
    # compute jacobian of the interventional variances
    jac_g2 <- calculate_jacobian_interventional_variances(
      model = model,
      intervention_names = intervention_names,
      outcome_names = outcome_names,
      verbose = verbose
    )
    
    # get variable names of interventional variables
    # TODO: reconsider setting defaults
    # if (is.character(intervention_names) &&
    #    all(intervention_names %in% model$info_model$var_names)) {
    #  x_labels <- intervention_names
    #} else {
    #  x_labels <- model$info_interventions$intervention_names
    #}
    
    # get interventional levels
    # TODO: reconsider setting defaults
    #if (is.numeric(x) && length (x) == length(intervention_names)) {
    #  x_values <- x
    #} else {
    #  x_values <- model$info_interventions$intervention_levels
    #}
    
  }

  # console output
  if (verbose >= 2) {
    cat(paste0("start of function ", fun_name_version, " ", Sys.time(), "\n"))
  }

  # get total number of variables
  # get number of unique parameters
  n <- model$info_model$n_ov
  n_unique <- model$info_model$param$n_par_unique

  D_n <- constant_matrices$duplication_matrix

  # compute vectorized jacobian
  jac_g2_vec <- D_n %*% jac_g2

  # get AV of parameter vector
  acov <- model$info_model$param$varcov_par_unique

  # compute asymptotic variance
  #acov_gamma_2 <- jac_g2 %*% acov %*% t(jac_g2)
  acov_gamma_2_vec <- jac_g2_vec %*% acov %*% t(jac_g2_vec)

  labels_acov_gamma_2_vec <- 
    expand.grid(model$info_model$var_names,model$info_model$var_names)
  labels_acov_gamma_2_vec <- 
    paste0(labels_acov_gamma_2_vec$Var1,labels_acov_gamma_2_vec$Var2)

  acov_gamma_2_vec <- as.matrix(acov_gamma_2_vec)
  colnames(acov_gamma_2_vec) <- labels_acov_gamma_2_vec
  rownames(acov_gamma_2_vec) <- labels_acov_gamma_2_vec

  variance_labels <- 
    paste0(model$info_model$var_names,model$info_model$var_names)

  acov_variances <- acov_gamma_2_vec[variance_labels,variance_labels]

  # compute asymptotic standard errors
  ase_gamma_2 <- sqrt(diag(acov_variances))

  # compute approximate z-value
  z_gamma_2 <- gamma_2 / ase_gamma_2

  # Prepare output ----

  # Console output
  if (verbose >= 2) {
    cat(paste0("  end of function ", fun_name_version, " ", Sys.time(), "\n"))
  }

  # Output
  list(gamma_2 = gamma_2,
       acov_gamma_2 = acov_variances,
       ase_gamma_2 = ase_gamma_2,
       z_gamma_2 = z_gamma_2)

}

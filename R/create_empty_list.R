## Changelog:
# CG 0.0.12 2023-02-21: changes in preamble and comments 
# MH 0.0.11 2022-03-17: removed "\code" from "\code{\tabular{lll}..."
#                       solve WARNING in package checking 
# CG 0.0.10 2022-01-13: changed structure of internal_list 
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MH 0.0.9 2021-11-30: changed defaults of interventional_distribution$
# density_function$values to empty list
#                      changed "pdf" to "pdfs"
#                      updated documentation accordingly 
# MH 0.0.8 2021-11-22: renamed from make_empty_list to create_empty_list
# MH 0.0.7 2021-11-22: moments$variance_matrix changed to covariance_matrix
# CG 0.0.6 2021-11-22: create slots for select outcome matrix
# CG 0.0.5 2021-11-09: create slots for eliminiation, duplication
#                      and commutation matrix
# CG 0.0.4 2021-09-24: create slot 'interventional_distribution'
#                      update Documentation 
# MA 0.0.3 2021-09-09: added 'derivative' slot to 'C' and 'Psi'
# CG 0.0.2 2021-09-02: create slot 'param'
# MH 0.0.1 2021-09-01: initial programming


## Documentation
#' @title Create Empty Internal List
#' @description Creates an empty internal list of predefined structure. The 
#' list will subsequently be filled while the function  
#' \code{\link{intervention_effect}} is running. 
#' @param verbose Integer number describing verbosity of console output.
#' 0...no output (default); 1...user messages; 2...debugging-relevant messages. 
#' @return An empty internal list with the following structure:\cr
#'  \tabular{lll}{
#'     List of 6\cr
#'     \code{$fitted_object}: List       \tab \tab Object containing fitted 
#'     model. \cr
#'     \code{$fitted_object_class}: chr(0) \tab \tab Class of fitted model. \cr
#'     \code{$info_model}: List of 6  \tab \tab \cr
#'      \code{..$n_obs}: int(0)       \tab \tab Number of observations.\cr
#'      \code{..$n_ov}: int(0)        \tab \tab Number of manifest variables.\cr
#'      \code{..$var_names}: chr(0)   \tab \tab Names of observed variables.\cr
#'      \code{..$C}: List of 3        \tab \tab \cr
#'      \code{.. ..$values}: num[0, 0]   \tab \tab Parameter values of matrix of 
#'      structural coefficients.\cr
#'      \code{.. ..$labels}: chr[0, 0]   \tab \tab Parameter labels of matrix of 
#'      structural coefficients.\cr
#'      \code{.. ..$derivative}: chr[0, 0]  \tab \tab Partial derivative of the 
#'      vectorized C matrix with \cr
#'                                 \tab \tab respect to the parameters.\cr
#'      \code{..$ Psi}: List of 3         \tab \tab \cr
#'      \code{.. ..$values}: num[0, 0]   \tab \tab Parameter values of model 
#'      implied covariance matrix.\cr
#'      \code{.. ..$labels}: chr[0, 0]   \tab \tab Parameter labels of model 
#'      implied covariance matrix\cr
#'      \code{.. ..$derivative}: chr[0, 0]  \tab \tab Partial derivative of the 
#'      vectorized Psi matrix with\cr
#'                                 \tab \tab respect to the parameters.\cr
#'      \code{..$param}: List of 5       \tab \tab \cr
#'      \code{.. ..$n_par}: int(0)        \tab \tab Total number of estimated 
#'      parameters.\cr
#'      \code{.. ..$n_par_unique}: int(0) \tab \tab Number of distinct and 
#'      functionally unrelated parameters.\cr
#'      \code{.. ..$labels_par_unique}: chr(0)  \tab \tab Labels of distinct and 
#'      functionally unrelated parameters.\cr
#'      \code{.. ..$values_par_unique}: num(0)  \tab \tab Values of distinct
#'       and functionally unrelated parameters.\cr
#'      \code{.. ..$varcov_par_unique}: num[0, 0] \tab \tab Covariance 
#'      matrix of the estimator of distinct\cr
#'                          \tab \tab and functionally unrelated parameters.\cr
#'     \code{$info_interventions}: List of 8      \tab \tab \cr
#'      \code{..$n_intervention}: int(0)    \tab \tab Number of interventional 
#'      variables.\cr
#'      \code{..$intervention_names}: chr(0) \tab \tab Names of interventional 
#'      variables.\cr
#'    \code{..$n_outcome}: int(0)   \tab \tab Number of outcome variables.\cr
#'    \code{..$outcome_names}: chr(0) \tab \tab Names of outcome variables.\cr
#'    \code{..$intervention_levels}: num(0) \tab \tab Interventional levels.\cr
#'    \code{..$effect_type}: chr(0)   \tab \tab Features of the interventional 
#'      distribution to be analyzed.\cr
#'      \code{..$lower_bound}: num(0)   \tab \tab Lower bound of critical range 
#'      of outcome variables.\cr
#'      \code{..$upper_bound}: num(0)   \tab \tab Upper bound of critical range
#'       of outcome variables.\cr
#'     \code{$matrices_constant}: List of 7   \tab \tab \cr
#'      \code{.. .. $select_intervention}: num[0, 0]  \tab \tab Selection matrix 
#'      for entries that are intervened on. \cr
#'      \code{.. .. $select_non_intervention}: num[0, 0] \tab \tab Selection  
#'      matrix for entries that are NOT intervened on. \cr
#'      \code{.. .. $select_outcome}: num[0, 0]          \tab \tab Selection 
#'      matrix for outcomes of interest. \cr
#'      \code{.. .. $eliminate_intervention}: num[0, 0]  \tab \tab Matrix that 
#'      replaces entries that are intervened on by zero. \cr
#'      \code{.. .. $duplication_matrix}: num[0, 0]      \tab \tab Maps vech(A)  
#'      onto vec(A) for symmetric A. \cr
#'      \code{.. .. $elimination_matrix}: num[0, 0]      \tab \tab Maps vec(A) 
#'      onto vech(A). \cr
#'      \code{.. .. $commutation_matrix}: num[0, 0]      \tab \tab Maps vech(A) 
#'      onto vec(A'). \cr
#'     \code{$interventional_distribution} : List of 4   \tab \tab \cr
#'      \code{..$means}: List of 4        \tab \tab \cr
#'      \code{.. .. $values}: num(0)      \tab \tab Mean vector of the  
#'      interventional distribution. \cr
#'      \code{.. .. $jacobian}: num[0, 0] \tab \tab Jacobian of interventinoal 
#'      mean vector. \cr
#'      \code{.. .. $ase}: num(0)       \tab \tab Asymptotic standard errors of 
#'      interventional means.  \cr
#'      \code{.. .. $z_values}: num[0, 0] \tab \tab z-values of interventional 
#'      means.\cr
#'      \code{..$covariance_matrix}: List of 4    \tab \tab \cr
#'      \code{.. .. $values}: num(0)      \tab \tab Covariance matrix of the 
#'      interventional distribution.  \cr
#'      \code{.. .. $jacobian}: num[0, 0] \tab \tab Jacobian of VECTORIZED 
#'      interventional covariance matrix. \cr
#'      \code{.. .. $ase}: num(0)        \tab \tab Asymptotic standard errors of 
#'      interventional (co)variances.  \cr
#'      \code{.. .. $z_values}: num[0, 0] \tab \tab z-values of interventional 
#'      interventional (co)variances. \cr
#'      \code{..$density_function}: List of 4    \tab \tab \cr
#'      \code{.. .. $values}: num(0)      \tab \tab Probability density function 
#'      (pdf) of the\cr
#'      \tab \tab interventional distribution.  \cr
#'      \code{.. .. $jacobian}: num[0, 0] \tab \tab Jacobian of the 
#'      interventional pdf.\cr
#'      \code{.. .. $ase}: num(0)       \tab \tab asymptotic standard errors of 
#'      interventional pdf.   \cr
#'      \code{.. .. $z_values}: num[0, 0] \tab \tab z-values of interventional 
#'      pdf. \cr
#'      \code{..$probabilities}: List of 4  \tab \tab \cr
#'      \code{.. .. $values}: num(0)      \tab \tab Probabilities of  
#'      interventional events. \cr
#'      \code{.. .. $jacobian}: num[0, 0] \tab \tab Jacobian of interventional 
#'      probabilities. \cr
#'      \code{.. .. $ase}: num(0)         \tab \tab Asymptotic standard error of 
#'      interventional probabilities.  \cr
#'      \code{.. .. $z_values}: num[0, 0] \tab \tab z-value of the 
#'      interventional probabilities. \cr
#'     \code{$control}: List of 1     \tab \tab \cr
#'      \code{..$verbose}: num(0)     \tab \tab Verbosity of console output.\cr
#'     \code{$tables}: List of 2      \tab \tab \cr
#'      \code{..$interventional_means}: data.frame  \tab \tab Information for 
#'      \code{\link{summary.causalSEM}} \cr 
#'      \tab \tab and \code{\link{print.causalSEM}}  functions.\cr
#'      \code{..$interventional_means}: data.frame  \tab \tab Information for 
#'      \code{\link{summary.causalSEM}}\cr 
#'      \tab \tab and \code{\link{print.causalSEM}} 
#'      functions.\cr
#'  }
#'  
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
create_empty_list <- function( verbose = NULL ){

	# function name
	fun.name <- "create_empty_list"

	# function version
	fun.version <- "0.0.12 2023-02-21"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# check verbose argument
	verbose <- handle_verbose_argument( verbose )

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# internal list
	
	internal_list <- list(

		# fitted object
		"fitted_object" = NULL,

		# class of fitted object
		"fitted_object_class" = NULL,

		# model info
		info_model = list(

			# number of observations
			# a single number, normally an integer
			"n_obs" = as.integer(0),

			# number of manifest variables
			# a single number, normally an integer
			"n_ov" = as.integer(0),

			# names of observed variables
			# character vector of length(n_ov)
			# general: using all-y LISREL notation
			# here: using the notation from Gische and Voelkle (2022).
			"var_names" = character(0),

			# C list
			C = list( 
			
			# parameter values of matrix of structural coefficients
			# numeric matrix of dimension n_ov * n_ov
			"values" = matrix(numeric(0))[-1,-1],

			# parameter names of matrix of structural coefficients
			# character matrix of dimension n_ov * n_ov
			# same labels display equality constraints
			# general: using all-y LISREL notation
			# here: using the notation from Gische and Voelkle (2022)
			"labels" = matrix(character(0))[-1,-1],

			# Partial derivative of the vectorized C matrix with respect
			# to the parameters
			"derivative" = matrix(numeric(0))[-1,-1]
					  
			), # end of C list

			# Psi list
			Psi = list( 
			
			# parameter values of model implied covariance matrix
			# numeric matrix of dimension n_ov * n_ov
			"values" = matrix(numeric(0))[-1,-1],

			# parameter names of model implied covariance matrix
			# character matrix of dimension n_ov * n_ov
			# same labels display equality constraints
			# general: using all-y LISREL notation
			# here: using the notation from Gische and Voelkle (2022)
			"labels" = matrix(character(0))[-1,-1],

			# Partial derivative of the vectorized Psi matrix with respect
			# to the parameters
			"derivative" = matrix(numeric(0))[-1,-1]
						
			), # end of Psi list

			param = list( 
			
			# total number of estimated parameters
			# normally an integer
			# equals the total number of non-NA entries in
			# structural_coeff_names and covariance_names
			# does NOT take into account other specified equality constraints
			"n_par" = integer(0),

			# number of distinct and functionally unrelated parameters
			# normally an integer
			"n_par_unique" = as.integer(0),

			# names of distinct and functionally unrelated parameters
			# in the order they appear rowwise in the matrices
			# structural_coeff_names and covariance_names
			# character vector of length(n_par_unique)
			"labels_par_unique" = character(0),

			# parameter values (estimates) of distinct and functionally unrelated
			# parameters
			# numeric vector of length(n_par_unique)
			"values_par_unique" = numeric(0),

			# variance-covariance matrix of estimator of vector
			# of distinct and functionally unrelated parameters
			# numeric matrix of dimension n_par_unique * n_par_unique
			"varcov_par_unique" = matrix(numeric(0))[-1,-1]
						 
			) # end of param list

		), # end of info_model list

		# interventions
		info_interventions = list(

			# number of interventional variables
			# normally an integer
			# default: zero
			"n_intervention" = as.integer(0),

			# names of interventional variables
			# character vector of length n_intervention
			# default: empty vector (no intervention)
			"intervention_names" = character(0),

			# number of outcome variables
			# normally an integer
			# default: n_ov - n_intervention
			"n_outcome" = as.integer(0),

			# names of outcome variables
			# character vector of length(n_outcome)
			# default: all non-interventional variables
			"outcome_names" = character(0),

			# interventional level
			# numeric vector of length(n_intervention)
			# default: vector of ones
			"intervention_levels" = numeric(0),

			# parts of the interventional distribution the user is interested in
			# character vector
			# if "probability" is specified, user specified values of
			# lower_bound and upper_bound required
			# default: "mean"
			# possible values: c("mean", "variance", "density", "probability")
			"effect_type" = character(0),

			# lower bound of critical range of outcome variables
			# numeric vector
			"lower_bounds" = numeric(0),

			# upper bound of critical range of outcome variables
			# numeric vector
			"upper_bounds" = numeric(0)

		), # end of info_interventions list
		
		# constant matrices for computations
		constant_matrices = list(
		    
		    # selection matrix for entries that are intervened on
		    # see Definition 1 point 3 in Gische and Voelkle (2022)
		    # numeric matrix of dimension n_ov x n_intervention
		    "select_intervention" = matrix(numeric(0))[-1,-1],  
		    
		    # selection matrix for entries that are NOT intervened on
		    # see Definition 1 point 3 in Gische and Voelkle (2022)
		    # numeric matrix of dimension n_ov x (n_ov-n_intervention)
		    "select_non_intervention" = matrix(numeric(0))[-1,-1],
		    
		    # matrix that replaces entries that are intervened on by zero
		    # see Definition 1 point 4 in Gische and Voelkle (2022)
		    # numeric matrix of dimension n_ov x n_ov
		    "eliminate_intervention" = matrix(numeric(0))[-1,-1],
		    
		    # selection matrix for outcome variables of interest
		    # numeric matrix of dimension n_ov x (number of outcome variables 
		    # of interest)
		    "select_outcome" = matrix(numeric(0))[-1,-1],  
		    		    
		    # zero-one matrix that maps vech(A) onto vec(A) 
		    # for symmetric A
		    # numeric matrix of dimension (n_ov^2) x (1/2*n_ov*(n_ov+1))
		    "duplication_matrix" = matrix(numeric(0))[-1,-1],  
		    
		    # zero-one matrix that eliminates from vec(A)
		    # the supradiagonal elements of A. In other words,
		    # it maps vec(A) onto vech(A).
		    # numeric matrix of dimension (1/2*n_ov*(n_ov+1)) x (n_ov^2)
		    "elimination_matrix" = matrix(numeric(0))[-1,-1],  
		    
		    # zero-one matrix that transforms vec(A) into vec(A')
		    # numeric matrix of dimension (n_ov^2) x (n_ov^2)
		    "commutation_matrix" = matrix(numeric(0))[-1,-1]  
		    
		  ), # end of matrices_constant list	
		
		# interventional distribution
		interventional_distribution = list(
		
		means = list(
		
		    # mean vector of the interventional distribution 
		    # see Equation (6a) in Gische and Voelkle (2022)
		    # numeric vector of length n_ov 
		    "values" = numeric(0),
		    
		    # jacobian of the mean vector
		    # numeric matrix of dimension n_ov x n_par_unique
		    "jacobian" = matrix(numeric(0))[-1,-1],
		    
		    # asymptotic covariance matrix of the interventional mean vector
		    # numeric matrix of dimension n_outcomes x n_outcomes 
		    "acov" = matrix(numeric(0))[-1,-1],
		    
		    # asymptotic standard errors of the interventional means
		    # numeric vector of length n_ov
		    "ase" = numeric(0),
		    
		    # z-values of the interventional means
		    # numeric vector of length n_ov
		    "z_values" = numeric(0)
			
		  ), # end of means
		  		 		  
	    covariance_matrix = list(
		
		  # covariance matrix of the interventional distribution
		  # see Equation (6b) in Gische and Voelkle (2022)
		  # numeric matrix of dimension n_ov x n_ov
		  "values" = matrix(numeric(0))[-1,-1],
		  
		  # jacobian of the VECTORIZED variance-covariance matrix
		  # numeric matrix of dimension n_ov^2 x n_par_unique
		  "jacobian" = matrix(numeric(0))[-1,-1],
		  
		  # asymptotic covariance matrix of the VECTORIZED covariance matrix of the 
		  # interventional distribution
		  # numeric matrix of dimension n_outcomes^2 x n_outcomes^2 
		  "acov" = matrix(numeric(0))[-1,-1],
		  
		  # asymptotic standard errors of the interventional VARIANCES 
		  # (i.e., the diagonal entries of the interventional covariance matrix)
		  # numeric vector of length n n_outcomes
		  "ase" = matrix(numeric(0))[-1,-1],
		  
		  # z-values of the the interventional VARIANCES 
		  # (i.e., the diagonal entries of the interventional covariance matrix)
		  "z_values" = matrix(numeric(0))[-1,-1]
		  
		   ), # end of covariance_matrix
		  
		  density_function = list(
		  
		    # probability density function (pdf) of the interventional distribution
		    # see Equation (9) in Gische and Voelkle (2022)
		    # numeric matrix of with two columns 
		    # 1 column: grid that captures the range of the outcome variable
		    # 2 column: values of the pdf
		    "values" = matrix(numeric(0))[-1,-1],
		    
		    # jacobian of the density function (pdf) of the interventional 
		    # distribution
		    # numeric matrix of with two columns 
		    # 1 column: grid that captures the range of the outcome variable
		    # 2 column: values of the jacobian
		    "jacobian" = matrix(numeric(0))[-1,-1],
		    
		    # asymptotic standard errors (ase) of the probability density function 
		    # (pdf) # of the interventional distribution
		    # numeric matrix of with two columns 
		    # 1 column: grid that captures the range of the outcome variable
		    # 2 column: values of the ase
		    "ase" = matrix(numeric(0))[-1,-1],
		    
		    # z-values of the probability density function (pdf) 
		    # of the interventional distribution
		    # 1 column: grid that captures the range of the outcome variable
		    # 2 column: values of the z-values
		    "z_values" = matrix(numeric(0))[-1,-1]		    
		    
		  ), # end of density
		  
		  probabilities = list(
		  
		    # probabilities of interventional events
		    # see Equation (10) in Gische and Voelkle (2022)
		    # numeric vector of length n_outcomes 
		    "values" = numeric(0),
		    
		    # jacobian of the probability of a UNIVARIATE interventional event
		    # if n_outcomes > 1, the computation is done separately for each 
		    # outcome variable
		    # numeric vector of length n_outcomes
		    "jacobian" = matrix(numeric(0))[-1,-1],
		    
		    # asymptotic covariance of the probability of a UNIVARIATE
		    # interventional event 
		    # if n_outcomes > 1, the computation is done separately for each 
		    # outcome variable
		    # numeric vector of length n_outcomes 
		    "acov" = matrix(numeric(0))[-1,-1],
		    
		    # asymptotic standard error (ase) of the probability of a UNIVARIATE
		    # interventional event 
		    # if n_outcomes > 1, the computation is done separately for each 
		    # outcome variable
		    # numeric vector of length n_outcomes 
		    "ase" = numeric(0),
		    
		    # asymptotic z-value of the probability of a UNIVARIATE
		    # interventional event 
		    # if n_outcomes > 1, the computation is done separately for each 
		    # outcome variable
		    # numeric vector of length n_outcomes 
		    "z_values" = numeric(0)
			
		  ) # end of probability
		  
		), # end of interventional distribution 
		
		# control list
		control = list( 
		
		# verbosity of console output
		# a number, 0...no output (default), 1...user messages, 
		# 2...debugging-relevant messages
		"verbose" = verbose

		) # end of control list

	) # end of internal list

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# return internal list
	return( internal_list )
}

## test/development
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/
# handle_verbose_argument.R" )
# internal_list <- create_empty_list()
# str( internal_list )

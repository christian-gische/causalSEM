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
#' @title Build Empty Internal List
#' @description Internal function that builds an empty internal list.
#' @param verbose Single number, integer. 0...no output (default); 
#' 1...user messages;
#' 2...debugging-relevant messages. 
#' Valid default is set by function \code{\link{handle_verbose_argument}}.
#' @return Returns an empty internal list with the 
#' following structure:\cr
#'  \tabular{lll}{
#'     List of 5\cr
#'     $ fitted_object: NULL       \tab \tab \cr
#'     $ fitted_object_class: NULL \tab \tab \cr
#'     $ info_model: List of 6     \tab \tab \cr
#'      ..$ n_obs: int 0           \tab \tab number of observations\cr
#'      ..$ n_ov: int 0            \tab \tab number of manifest variables\cr
#'      ..$ var_names: chr(0)      \tab \tab names of observed variables\cr
#'      ..$ C: List of 3           \tab \tab \cr
#'      .. ..$ values: num[0, 0]   \tab \tab parameter values of matrix of 
#'      structural coefficients\cr
#'      .. ..$ labels: chr[0, 0]   \tab \tab parameter labels of matrix of 
#'      structural coefficients\cr
#'      .. ..$ derivative: chr[0, 0]  \tab \tab partial derivative of the 
#'      vectorized C matrix with respect\cr
#'                                 \tab \tab   to the parameters\cr
#'      ..$ Psi: List of 3         \tab \tab \cr
#'      .. ..$ values: num[0, 0]   \tab \tab parameter values of model implied 
#'      covariance matrix\cr
#'      .. ..$ labels: chr[0, 0]   \tab \tab parameter lables of model implied 
#'      covariance matrix\cr
#'      .. ..$ derivative: chr[0, 0]  \tab \tab partial derivative of the 
#'      vectorized Psi matrix with respect \cr
#'                                 \tab \tab to the parameters\cr
#'      ..$ param: List of 5       \tab \tab \cr
#'      .. ..$ n_par: int 0        \tab \tab total number of estimated 
#'      parameters\cr
#'      .. ..$ n_par_unique: int 0 \tab \tab number of distinct and 
#'      functionally\cr
#'                                        \tab \tab unrelated parameters\cr
#'      .. ..$ labels_par_unique: chr(0)  \tab \tab labels of distinct and 
#'      functionally unrelated parameters\cr
#'      .. ..$ values_par_unique: num(0)  \tab \tab parameter values of distinct
#'       and functionally unrelated parameters\cr
#'      .. ..$ varcov_par_unique: num[0, 0] \tab \tab variance-covariance matrix
#'       of the estimator of distinct\cr
#'                                          \tab \tab and functionally unrelated
#'                                           parameters\cr
#'     $ info_interventions: List of 8      \tab \tab \cr
#'      ..$ n_intervention: int 0       \tab \tab number of interventional 
#'      variables\cr
#'      ..$ intervention_names: chr(0)  \tab \tab names of interventional 
#'      variables\cr
#'      ..$ n_outcome: int 0            \tab \tab number of outcome variables\cr
#'      ..$ outcome_names: chr(0)       \tab \tab names of outcome variables\cr
#'      ..$ intervention_levels: num(0) \tab \tab interventional levels\cr
#'      ..$ effect_type: chr(0)         \tab \tab parts of the interventional 
#'      distribution to be analyzed\cr
#'      ..$ lower_bound: num(0)         \tab \tab lower bound of critical range 
#'      of univariate outcome variable\cr
#'      ..$ upper_bound: num(0)         \tab \tab upper bound of critical range
#'       of univariate outcome variable\cr
#'     $ matrices_constant: List of 7   \tab \tab \cr
#'      .. .. $ select_intervention: num[0, 0]     \tab \tab selection matrix 
#'      for entries that are intervened on \cr
#'      .. .. $ select_non_intervention: num[0, 0] \tab \tab selection matrix 
#'      for entries that are NOT intervened on \cr
#'      .. .. $ select_outcome: num[0, 0]          \tab \tab selection matrix 
#'      for outcomes of interest \cr
#'      .. .. $ eliminate_intervention: num[0, 0]  \tab \tab matrix that 
#'      replaces entries that are intervened on by zero \cr
#'      .. .. $ duplication_matrix: num[0, 0]      \tab \tab maps vech(A) onto 
#'      vec(A) for symmetric A \cr
#'      .. .. $ elimination_matrix: num[0, 0]      \tab \tab maps vec(A) onto 
#'      vech(A) \cr
#'      .. .. $ commutation_matrix: num[0, 0]      \tab \tab maps vech(A) onto 
#'      vec(A') \cr
#'     $ interventional_distribution : List of 4   \tab \tab \cr
#'      ..$ means: List of 4        \tab \tab \cr
#'      .. .. $ values: num(0)      \tab \tab mean vector of the interventional 
#'      distribution  \cr
#'      .. .. $ jacobian: num[0, 0] \tab \tab jacobian of interventinoal mean 
#'      vector \cr
#'      .. .. $ ase: num(0)         \tab \tab asymptotic standard errors of 
#'      interventional means  \cr
#'      .. .. $ z_values: num[0, 0] \tab \tab z-values of interventional 
#'      means\cr
#'      ..$ covariance_matrix: List of 4    \tab \tab \cr
#'      .. .. $ values: num(0)      \tab \tab covariance matrix of the 
#'      interventional distribution  \cr
#'      .. .. $ jacobian: num[0, 0] \tab \tab jacobian of VECTORIZED 
#'      interventional covariance matrix \cr
#'      .. .. $ ase: num(0)         \tab \tab asymptotic standard errors of 
#'      interventional (co)variances  \cr
#'      .. .. $ z_values: num[0, 0] \tab \tab z-values of interventional 
#'      interventional (co)variances \cr
#'      ..$ density_function: List of 4    \tab \tab \cr
#'      .. .. $ values: num(0)      \tab \tab probability density function 
#'      (pdf) of the interventional distribution  \cr
#'      .. .. $ jacobian: num[0, 0] \tab \tab jacobian of the pdf of 
#'      interventional distribution\cr
#'      .. .. $ ase: num(0)         \tab \tab asymptotic standard errors of 
#'      interventional pdf   \cr
#'      .. .. $ z_values: num[0, 0] \tab \tab z-values of interventional pdf \cr
#'      ..$ probabilities: List of 4  \tab \tab \cr
#'      .. .. $ values: num(0)      \tab \tab probabilities of interventional 
#'      events \cr
#'      .. .. $ jacobian: num[0, 0] \tab \tab jacobian of interventional 
#'      probabilities \cr
#'      .. .. $ ase: num(0)         \tab \tab asymptotic standard error of 
#'      interventional probabilities  \cr
#'      .. .. $ z_values: num[0, 0] \tab \tab z-value of the interventional 
#'      probabilities \cr
#'     $ control: List of 1         \tab \tab \cr
#'      ..$ verbose: num(0)         \tab \tab verbosity of console output\cr
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

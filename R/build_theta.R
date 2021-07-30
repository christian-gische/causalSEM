## Changelog:
# CG 0.0.1 2021-07-29: initial programming

# function definition
build_theta <- function( fit, verbose=c(0,1,2) ){
  
  # function name
  fun.name <- "build_theta"
  
  # function version
  fun.version <- "0.0.1 2021-07-29"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # handle verbose argument
  verbose <- verbose_argument_handling( verbose )
  
  # console output
  if( verbose >= 1 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )
  
  # supported fit objects
  supported.fit.objects <- c( "lavaan" )
  
  # get class of fit object
  fit.class <- class( fit )
  
  # check if supported
  if( !any( supported.fit.objects %in% fit.class ) ) stop( fun.name.version, ": fit object of class ", fit.class, " not supported. Supported fit objects are: ", paste( supported.fit.objects, collapse=", " ) )
  
  # require package
  if( fit.class %in% "lavaan" ) require( lavaan )
  
  # model representation must be "LISREL"
  model.rep <- fit@Model@representation
  if( !model.rep %in% "LISREL" ) stop( fun.name.version, ": model representation as defined in fit@Model@representation must be LISREL, but it is ", paste( model.rep, collapse=", " ) )
  
  # check whether beta is present in fit object
  # todo: EXTEND CHECK TO INCLUDE PSI
  GLIST.names <- names( fit@Model@GLIST )
  if( !any( GLIST.names %in% "beta" ) ) stop( fun.name.version, ": fit@Model@GLIST does not contain beta, but only ", paste( GLIST.names, collapse=", " ) )
  
  # get vector of unique coefficients in a predefined order.
  # First: structural coefficients from the C matrix (rowwise)
  # Second: variance covariance parameters from the Psi matrix (rowwise)

  coef_c<-as.vector(t(lavInspect(fit_100, what = "free")$beta))
  coef_c<-coef_c[coef_c != 0]
  
  coef_psi<-as.vector(t(lavInspect(fit_100, what = "free")$psi))
  coef_psi<-coef_psi[coef_psi != 0]
  coef_joint<-c(coef_c,coef_psi)
  
  # get parameter labels
  
  par_names <- names(coef(fit_100))[coef_joint]
  par_names_coef <- names(coef(fit_100))
  par_names_unique <- unique(par_names)
  
  # get total number of parameters in c and psi
  # get number of parameters ignoring duplicates due to symmetry
  # get number of parameters ignoring duplicates due to symmetry or equality constratins
  
  n_par <- length(par_names)
  n_par_coef <- length(coef(fit_100))
  n_par_unique <- length(par_names_unique)
  
  # get numeric value of parameters 
  
  par <- coef(fit_100)[par_names]
  par_coef <- coef(fit_100)
  par_unique <- coef(fit_100)[par_names_unique]
  
  # get variance covariance matrices 
  
  varcov_par <- vcov(fit_100)
  varcov_par_r <- varcov_par[par_names, ]
  varcov_par <- varcov_par_r[, par_names]
  
  varcov_par_coef <- vcov(fit_100)
  
  varcov_par_unique <- vcov(fit_100)
  varcov_par_unique_r <- varcov_par_unique[par_names_unique, ]
  varcov_par_unique <- varcov_par_unique_r[, par_names_unique]
  
  # todo: DO DIMENSION CHECK
  
  dim(varcov_par) == c(n_par, n_par)
  dim(varcov_par_coef) == c(n_par_coef, n_par_coef)
  dim(varcov_par_unique) == c(n_par_unique, n_par_unique)
  
  # prepare output
  theta_list <- list(
    par_total = list(par_names = par_names,
                     n_par = n_par,
                     par = par,
                     varcov_par = varcov_par), 
    par_coef = list(par_names_coef = par_names_coef,
                    n_par_coef = n_par_coef,
                    par_coef = par_coef,
                    varcov_par_coef = varcov_par_coef),
    par_unique = list(par_names_unique = par_names_unique,
                      n_par_unique = n_par_unique,
                      par_unique = par_unique,
                      varcov_par_unique = varcov_par_unique))
  
   
  # console output
  if( verbose >= 1 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )
  
  # return C matrix
  return( theta_list )
}

# test/development
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/verbose_argument_handling.R" )
# load( "c:/Users/martin/Dropbox/68_causalSEM/91_zeug/fit.lcs2.Rdata" )
# build_C( fit, verbose=2 )
# build_C( fit )
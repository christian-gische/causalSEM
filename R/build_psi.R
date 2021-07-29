#' @title Extracts the Psi matrix from a SEM
#' @description Internal function that extracts the Psi matrix from a lavaan
#' model. Currently, this function will only work if all variance parameters
#' are stored in lavaan's psi matrix (covariance of the latent errors) and the
#' theta matrix (covariance of the manifest errors) is empty.
#' @param internal_list a list with various information extracted from the
#' model.
#' @return \code{build_psi} returns a list with various information extracted
#' from the model. \code{build_psi} overwrites the (hopefully empty) covariance
#' slot with a list containing two matrices with the values and labels of the
#' Psi matrix.
#' @references
#' Gische, C. & Voelkle, M. C. (under review???). Beyond the mean: A flexible framework for studying causal effects using linear models
#' \href{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}

build_psi <- function(internal_list) {
  
  # !!! This is only for the hardcoded example. Remove late !!!
  internal_list <- internal_list_fix
  internal_list$info_raw <- list(fitted_object = fit_100) # I guess this should have been there.
  
  # handle verbose argument
  verbose <- verbose_argument_handling(internal_list$control$verbose)
  
  # console output
  if (verbose >= 1) {
    fun_version <- "0.0.1 2021-07-29"
    fun_name_version <- paste0("build_psi", " (", fun.version, ")")
    cat(paste0("start of function ", fun.name.version, " ", Sys.time(), "\n" ))
  }
  
  # number of manifest variables
  n_ov <- internal_list$info_model$n_var
  
  # get psi matrix with numeric values
  values <- internal_list$info_raw$fitted_object@Model@GLIST$psi[seq_len(n_ov), 
                                                                 seq_len(n_ov)]
  
  # empty matrix for the labels of the free parameters
  labels <- matrix(data = NA, nrow = n_ov, ncol = n_ov)
  
  # get parameter table
  lav_ParTable <- lavaan::lavMatrixRepresentation(
    partable = internal_list$info_raw$fitted_object@ParTable
  )
  
  # keep only the entries of lavaan's psi and theta and matrices
  ### I do not know if this works all the time
  ### Selects all parameters from the psi matrix that are to be estimated or
  ### have a nonzero value (e.g. fixed at 1)
  lav_ParTable <- lav_ParTable[lav_ParTable$mat %in% c("psi", "theta") &
                                 (lav_ParTable$free != 0 |
                                    lav_ParTable$start != 0) , , drop = FALSE]
  
  # label unlabelled parameters
  ### Would be better to do this in some preprocessing step
  unlabelled_params <- which(lav_ParTable[, "label"] == "")
  lav_ParTable[unlabelled_params, "label"] <- apply(
    lav_ParTable[unlabelled_params, 2:4], MARGIN = 1, FUN = paste, collapse = ""
  )
  
  # fill in the parameter labels
  for (i in seq_len(NCOL(lav_ParTable))) {
    labels[lav_ParTable$row[i], lav_ParTable$col[i]] <- lav_ParTable[i, "label"]
  }
  
  # label the values and labels matrix
  rownames(values) <- colnames(values) <- rownames(labels) <-
    colnames(labels) <- internal_list$info_raw$fitted_object@Model@dimNames[[1]][[1]]
  
  # prepare output
  psi_list <- list(values = values, labels = labels)
  
  # overwrite the covariance slot
  internal_list$info_model$covariance <- psi_list
  
  # console output
  if(verbose >= 1) {
    cat(paste0("  end of function ", fun_name_version, " ", Sys.time(), "\n" ))
  }
  
  # return updated internal list
  internal_list
  
}
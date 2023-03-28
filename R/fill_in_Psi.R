## Changelog:
# CG 0.0.4 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# 
# MA 0.0.3 2021-11-26: changed function name from "build_Psi" to "fill_in_Psi"
# MA 0.0.2 2021-10-31: fixed some variables names related to verbosity

#' @title Extract Covariance Matrix
#' @description Extracts the variance covariance matrix (Psi-matrix) 
#' from a fitted SEM model. Currently, this function will only work if all 
#' variance parameters are stored in lavaan's psi matrix (covariance of the 
#' latent errors) and the theta matrix (covariance of the manifest errors) is 
#' empty.
#' @param internal_list A list with various information extracted from the
#' model.
#' @return The inputted list with slots in the sublist \code{..$Psi} 
#' filled in:
#' \tabular{lll}{
#' .. ..$values: num[0, 0]   \tab \tab numeric matrix containing 
#'      parameter values of covariance matrix.\cr
#' .. ..$labels: chr[0, 0]   \tab \tab character matrix containing 
#'      parameter labels of covariance matrix.}
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z

fill_in_Psi <- function(internal_list) {

  # Verbosity and bug tracking ----

  # function name
  fun_name <- "fill_in_Psi"

  # function version
  fun_version <- "0.0.4 2022-01-13"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if (verbose >= 2) {
    cat(paste0("start of function ", fun_name_version, " ", Sys.time(), "\n"))
  }

  # number of manifest variables
  n_ov <- internal_list$info_model$n_ov

  # get psi matrix with numeric values
  values <- internal_list$fitted_object@Model@GLIST$psi[seq_len(n_ov),
                                                        seq_len(n_ov)]

  # empty matrix for the labels of the free parameters
  labels <- matrix(data = NA, nrow = n_ov, ncol = n_ov)

  # get parameter table
  lav_ParTable <- lavaan::lavMatrixRepresentation(
    partable = internal_list$fitted_object@ParTable
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
  # TODO Call the function add_labels_in_lavaan_parTable.R
  # (see fill_in_C.R function [old name: build_C-R])
  # for example:
  # lav_ParTable <- add_labels_in_lavaan_parTable( internal_list )
  
  unlabelled_params <- which(lav_ParTable[, "label"] == "")
  lav_ParTable[unlabelled_params, "label"] <- apply(
    lav_ParTable[unlabelled_params, 2:4], MARGIN = 1, FUN = paste, collapse = ""
  )

  # fill in the parameter labels
  for (i in seq_len(NCOL(lav_ParTable))) {
    labels[lav_ParTable$row[i], lav_ParTable$col[i]] <- lav_ParTable[i, "label"]
  }

  # copy labels in upper triangle to lower triangle
  labels[lower.tri(labels)] <- t(labels)[lower.tri(labels)]

  # label the values and labels matrix
  rownames(values) <- colnames(values) <- rownames(labels) <-
    colnames(labels) <- internal_list$fitted_object@Model@dimNames[[1]][[1]]

  # populate slots of Psi
  internal_list$info_model$Psi$values <- values
  internal_list$info_model$Psi$labels <- labels

  # console output
  if(verbose >= 2) {
    cat(paste0("  end of function ", fun_name_version, " ", Sys.time(), "\n" ))
  }

  # return updated internal list
  return(internal_list)

}

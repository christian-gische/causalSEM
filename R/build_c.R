C_numeric <- lavInspect(fit_100, what = "est")$beta[lavNames(fit_100, type = "ov"), lavNames(fit_100, type = "ov")]
C_labels <- matrix(NA, ncol=ncol(lavInspect(fit_100, what = "data")), 
                   nrow=ncol(lavInspect(fit_100, what = "data")))

for (i in 1:length(coef(fit_100))){
  C_labels<- replace(
    C_labels, 
    lavInspect(fit_100, what = "free")$beta[lavNames(fit_100, type = "ov"), lavNames(fit_100, type = "ov")]==i, 
    names(coef(fit_100))[i])
}

# using the causal order


C_numeric_ord <- lavInspect(fit_100, what = "est")$beta[causalOrder,causalOrder]
C_labels_ord <- matrix(NA, ncol=ncol(lavInspect(fit_100, what = "data")), 
                       nrow=ncol(lavInspect(fit_100, what = "data")))

for (i in 1:length(coef(fit_100))){
  C_labels_ord<- replace(
    C_labels_ord, 
    lavInspect(fit_100, what = "free")$beta[causalOrder,causalOrder]==i, 
    names(coef(fit_100))[i])
}
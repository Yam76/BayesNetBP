
###########################################
## Index generator
###########################################

index.generator <- function(tab1, tab2){

  if (ncol(tab1)==0 && ncol(tab2)==0) {
    ind.exp <- matrix(c(1,1), nrow=1)
  } else if (ncol(tab1)==0) {
    ind.exp <- cbind(rep(1, nrow(tab2)), 1:nrow(tab2))
  } else if (ncol(tab2)==0) {
    ind.exp <- cbind(1:nrow(tab1), rep(1, nrow(tab1)))
  } else {
    #
    ind.exp <- index.gen.special(tab1, tab2)
  }
  return(ind.exp)

}

##################################
## Index generator special case
##################################

# tab1 <- bag.post@config; tab2 <- bag.lp@config

index.gen.special <- function(tab1, tab2){
  index.gen.special_test2(tab1, tab2)
}

index.gen.special_test3 <- function(tab1, tab2){

  # matrix(1:ncol(temp1), nrow = ncol(temp1), dimnames = list(colnames(temp1)))

  ###### index generation

  config.var.int <- sort(intersect(colnames(tab1), colnames(tab2)))
  ## No intersection
  if (length(config.var.int)==0){
    ind.exp <- expand.grid(1:nrow(tab1), 1:nrow(tab2))
    return(ind.exp)
  }
  ##

  tab1o <- cbind(tab1, 1:nrow(tab1))
  tab2o <- cbind(tab2, 1:nrow(tab2))

  # form.str <- paste0("~", paste0(config.var.int, collapse="+"))
  # fmr <- as.formula(form.str)

  if(length(tab1) <= length(tab2)){ # if tab1 is smaller or same size
    unique.vec <- unique(tab1[, config.var.int, drop=FALSE]) # do quick check to see which of tab1 or tab2 is smaller
  }
  else{
    unique.vec <- unique(tab2[, config.var.int, drop=FALSE])
  }




  # get columns specified by config.var.int and split them, then send them to do.call
  # tab1o.s<- orderBy(fmr, data=tab1o)

  # split(tab1o[, config.var.int], col(tab1o[, config.var.int]))
  # # c(list(tab1o), split(tab1o[, config.var.int], col(tab1o[, config.var.int]))) # WRONG
  # do.call(order, split(tab1o[, config.var.int], col(tab1o[, config.var.int])))
  # split(tab1o[, config.var.int], rep(1:ncol(tab1o[, config.var.int]), each = nrow(tab1o[, config.var.int])))


  config_cols_1 <- tab1o[, config.var.int, drop = FALSE]

  # split(config_cols_1, rep(1:ncol(config_cols_1), each = nrow(config_cols_1)))
  # lapply(seq_len(ncol(x)), function(i) x[,i])
  # as.list(data.frame(config_cols_1))
  # config_split_1 <- lapply(seq_len(ncol(config_cols_1)), function(i) config_cols_1[,i])


  ### SIMPLY RIP EACH COLUMN INTO A SEPARATE VECTOR IN A LIST
  config_split_1 <- vector("list", ncol(config_cols_1))
  for(j in 1:ncol(config_cols_1)){
    config_split_1[[j]] <- config_cols_1[,j]
  }


  tab1o.s <- tab1o[do.call(order, config_split_1), , drop = FALSE]


  # tab2o.s<- orderBy(fmr, data=tab2o)

  config_cols_2 <- tab2o[, config.var.int, drop = FALSE]

  # config_split_2 <- lapply(seq_len(ncol(config_cols_2)), function(i) config_cols_2[,i])

  config_split_2 <- vector("list", ncol(config_cols_2))
  for(j in 1:ncol(config_cols_2)){
    config_split_2[[j]] <- config_cols_2[,j]
  }

  tab2o.s <- tab2o[do.call(order, config_split_2), , drop = FALSE]

  n.uni <- nrow(unique.vec)
  r.1 <- nrow(tab1o.s)/n.uni
  r.2 <- nrow(tab2o.s)/n.uni

  order.1 <- tab1o.s[, ncol(tab1o.s)]
  order.2 <- tab2o.s[, ncol(tab2o.s)]
  ind.1 <- rep(order.1, each=r.2)
  # ind.2 <- rep(tab2o.s$order.2_, times=r.2)

  # ind.2 <- c()
  # ind.2 <- rep(NA, length(ind.1))
  ind.2 <- vector("list", n.uni)

  # order.1 <- tab1o.s$order.1_
  # print("new ind.2")

  # index <- 1
  for (i in 1:n.uni) {
    this.vec <- order.2[((i-1)*r.2+1):(i*r.2)]
    # ind.2 <- replace(ind.2, index:(length(full) + index - 1),full)
    # print(c(length(this.vec), r.1))
    # index <- index + length(full)
    ind.2[[i]] <- rep(this.vec, r.1)

  }
  ind.2 <- unlist(ind.2, use.names = FALSE)

  ind.exp <- cbind(ind.1, ind.2)

  class(ind.exp) <- "numeric"
  ind.exp <- orderBy(~ind.2+ind.1, ind.exp)

  return(ind.exp)

}

index.gen.special_test2 <- function(tab1, tab2){

  # matrix(1:ncol(temp1), nrow = ncol(temp1), dimnames = list(colnames(temp1)))

  ###### index generation

  config.var.int <- sort(intersect(colnames(tab1), colnames(tab2)))
  ## No intersection
  if (length(config.var.int)==0){
    ind.exp <- expand.grid(1:nrow(tab1), 1:nrow(tab2))
    return(ind.exp)
  }
  ##

  tab1o <- cbind(tab1, 1:nrow(tab1))
  tab2o <- cbind(tab2, 1:nrow(tab2))

  # form.str <- paste0("~", paste0(config.var.int, collapse="+"))
  # fmr <- as.formula(form.str)

  if(length(tab1) <= length(tab2)){ # if tab1 is smaller or same size
    unique.vec <- unique(tab1[, config.var.int, drop=FALSE]) # do quick check to see which of tab1 or tab2 is smaller
  }
  else{
    unique.vec <- unique(tab2[, config.var.int, drop=FALSE])
  }




  # get columns specified by config.var.int and split them, then send them to do.call
  # tab1o.s<- orderBy(fmr, data=tab1o)

  # split(tab1o[, config.var.int], col(tab1o[, config.var.int]))
  # # c(list(tab1o), split(tab1o[, config.var.int], col(tab1o[, config.var.int]))) # WRONG
  # do.call(order, split(tab1o[, config.var.int], col(tab1o[, config.var.int])))
  # split(tab1o[, config.var.int], rep(1:ncol(tab1o[, config.var.int]), each = nrow(tab1o[, config.var.int])))

  config_cols_1 <- tab1o[, config.var.int, drop = FALSE]

  # split(config_cols_1, rep(1:ncol(config_cols_1), each = nrow(config_cols_1)))
  # lapply(seq_len(ncol(x)), function(i) x[,i])
  # as.list(data.frame(config_cols_1))
  config_split_1 <- lapply(seq_len(ncol(config_cols_1)), function(i) config_cols_1[,i])

  tab1o.s <- tab1o[do.call(order, config_split_1), , drop = FALSE]


  # tab2o.s<- orderBy(fmr, data=tab2o)

  config_cols_2 <- tab2o[, config.var.int, drop = FALSE]

  config_split_2 <- lapply(seq_len(ncol(config_cols_2)), function(i) config_cols_2[,i])

  tab2o.s <- tab2o[do.call(order, config_split_2), , drop = FALSE]

  n.uni <- nrow(unique.vec)
  r.1 <- nrow(tab1o.s)/n.uni
  r.2 <- nrow(tab2o.s)/n.uni

  order.1 <- tab1o.s[, ncol(tab1o.s)]
  order.2 <- tab2o.s[, ncol(tab2o.s)]
  ind.1 <- rep(order.1, each=r.2)
  # ind.2 <- rep(tab2o.s$order.2_, times=r.2)

  # ind.2 <- c()
  # ind.2 <- rep(NA, length(ind.1))
  ind.2 <- vector("list", n.uni)

  # order.1 <- tab1o.s$order.1_
  # print("new ind.2")

  # index <- 1
  for (i in 1:n.uni) {
    this.vec <- order.2[((i-1)*r.2+1):(i*r.2)]
    # ind.2 <- replace(ind.2, index:(length(full) + index - 1),full)
    # print(c(length(this.vec), r.1))
    # index <- index + length(full)
    ind.2[[i]] <- rep(this.vec, r.1)

  }
  ind.2 <- unlist(ind.2, use.names = FALSE)

  ind.exp <- cbind(ind.1, ind.2)

  class(ind.exp) <- "numeric"
  ind.exp <- orderBy(~ind.2+ind.1, ind.exp)

  return(ind.exp)

}

index.gen.special_test <- function(tab1, tab2){

  # matrix(1:ncol(temp1), nrow = ncol(temp1), dimnames = list(colnames(temp1)))

  ###### index generation

  config.var.int <- sort(intersect(colnames(tab1), colnames(tab2)))
  ## No intersection
  if (length(config.var.int)==0){
    ind.exp <- expand.grid(1:nrow(tab1), 1:nrow(tab2))
    return(ind.exp)
  }
  ##

  tab1o <- cbind(tab1, 1:nrow(tab1))
  tab2o <- cbind(tab2, 1:nrow(tab2))

  # form.str <- paste0("~", paste0(config.var.int, collapse="+"))
  # fmr <- as.formula(form.str)

  if(length(tab1) <= length(tab2)){ # if tab1 is smaller or same size
    unique.vec <- unique(tab1[, config.var.int, drop=FALSE]) # do quick check to see which of tab1 or tab2 is smaller
  }
  else{
    unique.vec <- unique(tab2[, config.var.int, drop=FALSE])
  }




  # get columns specified by config.var.int and split them, then send them to do.call
  # tab1o.s<- orderBy(fmr, data=tab1o)

  # split(tab1o[, config.var.int], col(tab1o[, config.var.int]))
  # # c(list(tab1o), split(tab1o[, config.var.int], col(tab1o[, config.var.int]))) # WRONG
  # do.call(order, split(tab1o[, config.var.int], col(tab1o[, config.var.int])))
  # split(tab1o[, config.var.int], rep(1:ncol(tab1o[, config.var.int]), each = nrow(tab1o[, config.var.int])))

  config_cols_1 <- tab1o[, config.var.int, drop = FALSE]

  # split(config_cols_1, rep(1:ncol(config_cols_1), each = nrow(config_cols_1)))
  # lapply(seq_len(ncol(x)), function(i) x[,i])
  # as.list(data.frame(config_cols_1))
  config_split_1 <- lapply(seq_len(ncol(config_cols_1)), function(i) config_cols_1[,i])

  tab1o.s <- tab1o[do.call(order, config_split_1), , drop = FALSE]


  # tab2o.s<- orderBy(fmr, data=tab2o)

  config_cols_2 <- tab2o[, config.var.int, drop = FALSE]

  config_split_2 <- lapply(seq_len(ncol(config_cols_2)), function(i) config_cols_2[,i])

  tab2o.s <- tab2o[do.call(order, config_split_2), , drop = FALSE]

  n.uni <- nrow(unique.vec)
  r.1 <- nrow(tab1o.s)/n.uni
  r.2 <- nrow(tab2o.s)/n.uni

  order.1 <- tab1o.s[, ncol(tab1o.s)]
  order.2 <- tab2o.s[, ncol(tab2o.s)]
  ind.1 <- rep(order.1, each=r.2)
  # ind.2 <- rep(tab2o.s$order.2_, times=r.2)
  ind.2 <- c()
  # order.1 <- tab1o.s$order.1_
  # print("new ind.2")

  for (i in 1:n.uni) {
    this.vec <- order.2[((i-1)*r.2+1):(i*r.2)]
    ind.2 <- c(ind.2, rep(this.vec, r.1))
    # print(c(length(this.vec), r.1))

  }

  ind.exp <- cbind(ind.1, ind.2)

  class(ind.exp) <- "numeric"
  ind.exp <- orderBy(~ind.2+ind.1, ind.exp)

  return(ind.exp)

}

index.gen.special_orig <- function(tab1, tab2){

  ###### index generation
  config.var.1 <- colnames(tab1)
  config.var.2 <- colnames(tab2)
  config.var.int <- sort(intersect(config.var.1, config.var.2))
  ## No intersection
  if (length(config.var.int)==0){
    ind.exp <- expand.grid(1:nrow(tab1), 1:nrow(tab2))
    return(ind.exp)
  }
  ##
  order.1_ <- 1:nrow(tab1)
  order.2_ <- 1:nrow(tab2)

  tab1o <- cbind(tab1, order.1_)
  tab2o <- cbind(tab2, order.2_)

  form.str <- paste0("~", paste0(config.var.int, collapse="+"))
  fmr <- as.formula(form.str)

  tab1o.s<- orderBy(fmr, data=tab1o)
  tab2o.s<- orderBy(fmr, data=tab2o)
  unique.vec <- unique(tab1[, config.var.int, drop=FALSE])

  n.uni <- nrow(unique.vec)
  n.1 <- nrow(tab1o.s)
  nc.1 <- ncol(tab1o.s)
  n.2 <- nrow(tab2o.s)
  nc.2 <- ncol(tab2o.s)
  r.1 <- n.1/n.uni
  r.2 <- n.2/n.uni

  order.1 <- tab1o.s[,nc.1]
  order.2 <- tab2o.s[,nc.2]
  ind.1 <- rep(order.1, each=r.2)
  # ind.2 <- rep(tab2o.s$order.2_, times=r.2)
  ind.2 <- c()
  # order.1 <- tab1o.s$order.1_


  for (i in 1:n.uni) {
    this.vec <- order.2[((i-1)*r.2+1):(i*r.2)]
    ind.2 <- c(ind.2, rep(this.vec, r.1))
  }

  ind.exp <- cbind(ind.1, ind.2)

  class(ind.exp) <- "numeric"
  ind.exp <- orderBy(~ind.2+ind.1, ind.exp)

  return(ind.exp)

}

# system.time(exp0 <- index.gen.special.0(tab1,tab2))
# system.time(exp1 <- index.gen.special(tab1,tab2))

############################################################
## Factor product
############################################################

# pot1 <- tree.init.p@jpt[["HDL"]]; pot2 <- tree.init.p@jpt[["Cyp2b10"]]

factor.product <- function(pot1, pot2, normalize=TRUE){

  if (ncol(pot1$cpt)==0) { return(pot2) }
  if (ncol(pot2$cpt)==0) { return(pot1) }

  ind <- index.generator(pot1$cpt, pot2$cpt)

  ind.1 <- ind[,1]
  ind.2 <- ind[,2]
  p <- pot1$prob[ind.1]*pot2$prob[ind.2]
  if (normalize) {p <- p/sum(p)}

  ## processing distribution table
  config.var.1 <- colnames(pot1$cpt)
  config.var.2 <- colnames(pot2$cpt)
  config.var.int <- intersect(config.var.1, config.var.2)

  config.rem.1 <- setdiff(config.var.1, config.var.int)
  config.rem.2 <- setdiff(config.var.2, config.var.int)

  config.after <- cbind(pot1$cpt[ind.1, config.rem.1, drop=FALSE],
                        pot2$cpt[ind.2, config.rem.2, drop=FALSE],
                        pot2$cpt[ind.2, config.var.int, drop=FALSE])
  rownames(config.after) <- NULL
  result <- list(cpt=config.after, prob=p)
  return(result)

}

############################################################
## Factor divide
############################################################

factor.divide <- function(pot1, pot2){

  if (ncol(pot1$cpt)==0) { return(pot2) }
  if (ncol(pot2$cpt)==0) { return(pot1) }
  ind <- index.generator(pot1$cpt, pot2$cpt)
  ind.1 <- ind[,1]
  ind.2 <- ind[,2]
  p1 <- pot1$prob[ind.1]
  p2 <- pot2$prob[ind.2]
  p <- c()
  vld <- which(p2!=0)
  p[vld] <- p1[vld]/p2[vld]
  p[p2==0] <- 0

  ## processing distribution table

  config.var.1 <- colnames(pot1$cpt)
  config.var.2 <- colnames(pot2$cpt)
  config.var.int <- intersect(config.var.1, config.var.2)
  config.rem.1 <- setdiff(config.var.1, config.var.int)
  config.rem.2 <- setdiff(config.var.2, config.var.int)

  config.after <- cbind(pot1$cpt[ind.1, config.rem.1, drop=FALSE],
                        pot2$cpt[ind.2, config.rem.2, drop=FALSE],
                        pot2$cpt[ind.2, config.var.int, drop=FALSE])
  rownames(config.after) <- NULL
  result <- list(cpt=config.after, prob=p)
  return(result)

}

############################################################
## Conditional dsitribution
## vars: the variables conditioned on
############################################################

conditional <- function(pot, vars) {
  pot2 <- marginalize.discrete(pot, vars)
  pot3 <- factor.divide(pot, pot2)
  return (pot3)
}

############################################################
## Marginalize
############################################################

# pot <- tree.init.p@jpt[["HDL"]]; vars <- c("HDL")
# system.time(mg1 <- marginalize.discrete(pot, vars) )

marginalize.discrete <- function(pot, vars){
  marginalize.discrete_test(pot, vars)
}

# marginalize.discrete_test2 <- function(pot, vars){
#   # get intersection b/wn pot$cpt and vars names
#   pot.vars <- names(pot$cpt) ######
#   vars <- intersect(pot.vars, vars) ######
#   if(length(vars)==0) {
#     result <- list(cpt=data.frame(matrix(0,nrow=0,ncol=0)), prob=1)
#     return(result)
#   }
#
#   df <- cbind(pot$cpt, prob_ = pot$prob)
#
# }

marginalize.discrete_test <- function(pot, vars){
  # get intersection b/wn pot$cpt and vars names
  pot.vars <- names(pot$cpt) ######
  vars <- intersect(pot.vars, vars) ######
  if(length(vars)==0) {
    result <- list(cpt=data.frame(matrix(0,nrow=0,ncol=0)), prob=1)
    return(result)
  }


  #df <- data.frame(pot$cpt, prob_=pot$prob) # combine pot$cpt and pot$prob (under _prob)
  df <- cbind(pot$cpt, prob_ = pot$prob)

  fmr.str <- paste0("prob_~", paste0(rev(vars), collapse="+")) # ordering
  fmr <- as.formula(fmr.str)
  dfs <- aggregate(fmr, df, FUN = sum)
  dfs_vars <- dfs[,vars, drop = FALSE]
  result <- list(cpt=dfs_vars, prob=dfs[,(length(vars)+1)])

  return(result)

}

marginalize.discrete_orig <- function(pot, vars) {

  pot.vars <- names(pot$cpt) ######
  vars <- intersect(pot.vars, vars) ######

  if(length(vars)==0) {
    result <- list(cpt=data.frame(matrix(0,nrow=0,ncol=0)), prob=1)
    return(result)
  }

  df <- data.frame(pot$cpt, prob_=pot$prob)
  nc <- length(vars)
  fmr.str <- paste0("prob_~", paste0(vars, collapse="+"))
  fmr <- as.formula(fmr.str)
  dfs <- summaryBy(fmr, df, FUN=sum)
  result <- list(cpt=dfs[, 1:nc, drop=FALSE], prob=dfs[,(nc+1)])

  return(result)
}

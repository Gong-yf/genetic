setwd('C:\\Users\\gongyf\\documents\\genetic')
sourceCpp("TS_func.cpp")
sourceCpp("genetic.cpp")
{
  correlation <- function(x, y, d = 16){
    return(as.vector(roll_cor(as.matrix(x), as.matrix(y), width = d)))
  }
  covariance <- function(x, y, d = 16){
    return(as.vector(roll_cov(as.matrix(x), as.matrix(y), width = d)))
  }
  signedpower <- function(x, y, a = 2){
    return(sign(x)*(x^a))
  }
  rank_my <- function(x, y){
    return(rank(x))
  }
  scale_my <- function(x, y){
    return(scale(x))
  }
  func <- c(add, sub, mul, div, abs, sqrt, log, inv, rank_my, scale_my, 
            signedpower, argmin, argmax, delay, delta, correlation, covariance)
  data_prc <- c(quote(open), quote(close), quote(high), quote(low), quote(vwap))
  data_vol <- c(quote(volume))
  data_ret <- c(quote(umret))
  
  Node <- function(fun = NULL, left = NULL, right = NULL){
    tmp <- list(val = fun, left = left, right = right)
    class(tmp) <- "Node"
    return(tmp)
  }
  Leaf <- function(val = NULL, left = NULL, right = NULL){
    tmp <- list(val = val, left = left, right = right)
    class(tmp) <- "Leaf"
    return(tmp)
  }
  CreateTree <- function(root, func, data_prc, LeafPro = 0.1){
    i <- rbinom(1, length(func)-1, 0.5)+1
    root <- Node(func[[i]])
    if(class(root$left) == "Leaf" & class(root$right) == "Leaf"){
      return()
    }
    if(class(root$left) != "Leaf"){
      sign <- rbinom(1, 1, LeafPro)
      if(sign == 1){
        i <- rbinom(1, length(data_prc)-1, 0.5)+1
        root$left <- Leaf(data_prc[[i]])
      }else{
        root$left <- CreateTree(root$left, func, data_prc, LeafPro+0.3)
      }
    }
    if(class(root$right) != "Leaf"){
      sign <- rbinom(1, 1, LeafPro)
      if(sign == 1){
        i <- rbinom(1, length(data_prc)-1, 0.5)+1
        root$right <- Leaf(data_prc[[i]])
      }else{
        root$right <- CreateTree(root$right, func, data_prc, LeafPro+0.3)
      }
    }
    return(root)
  }
  TraverseTree <- function(root, n = 2){
    if(class(root$left) != "Leaf"){
      root$left$val <- eval(TraverseTree(root$left, n=n+1))
    }
    if(class(root$right) != "Leaf"){
      root$right$val <- eval(TraverseTree(root$right, n=n+1))
    }
    if(class(root$left) == "Leaf" & class(root$right) == "Leaf"){
      class(root) <- "Leaf"
      return(substitute(root$val(eval(root$left$val,parent.frame(n)), eval(root$right$val,parent.frame(n)))))
    }
    return(substitute(root$val(eval(root$left$val,parent.frame(n)), eval(root$right$val,parent.frame(n)))))
  }
}
set.seed(1234)
Factor <- list()
t_IC <- list()
IC <- data[, cor(umret_next, mom, use = "na.or.complete"), keyby = .(DataDate, ticktime)]

for (i in 1:50) {
  Factor[paste('alpha', i, sep = "_")] <- NA
  Factor[[i]] <- CreateTree(Node(), func, data_prc, LeafPro = 0.1)
  data[, pre_factor := eval(TraverseTree(Factor[[i]], n=2)), by = ukey]
  data[, factor := roll_mean(as.matrix(pre_factor), width = 16), by = ukey]
  data[, paste('alpha', i, sep = "_") := factor]
  
  temp     <- data[, cor(umret_next, factor, use = "na.or.complete"), by = .(DataDate, ticktime)]
  result <- tryCatch({
    t_IC[[i]]   <- t.test(temp$V1)$statistic
  }, error = function(err){
    t_IC[[i]]   <- 0
  })
  IC[, paste('alpha', i, sep = "_")] <- temp$V1
  temp <- NULL
}
{
  #invalid
  invalid <- vector()
  for (i in 50:1) {
    if(class(t_IC[[i]])!="numeric" || 
       base::abs(t_IC[[i]])<3 || 
       sum(is.na(IC[, paste('alpha', i, sep = "_"),with=FALSE]))>100 || 
       sum(is.na(data[, paste('alpha', i, sep = "_"),with=FALSE]))>10000){
      IC[,  paste('alpha', i, sep = "_")] <- NULL
      invalid <- append(invalid, paste('alpha', i, sep = "_"))
    }
  }
  
  rele <- function(cor_M, invalid){
    n <- nrow(cor_M)
    for(i in n:2){
      if(max(base::abs(cor_M[i,-i]))>0.9){
        invalid <- append(invalid, rownames(cor_M)[i])
        cor_M <- cor_M[-i,]
        cor_M <- cor_M[,-i]
      }
    }
    invalid <- sort(unique(invalid), decreasing = TRUE)
    return(invalid)
  }
  
  tt <- IC[,c(3:length(colnames(IC))),with=FALSE]
  cor_M <- cor(tt, use =  "na.or.complete")
  cov_M <- cov(tt, use =  "na.or.complete")
  invalid <- rele(cor_M, invalid)
  
  for(name in invalid){
    data[, paste(name) := NULL]
    Factor[name] <- NULL
    IC[,  paste(name) := NULL]
  }
  data[, pre_factor:= NULL]
  data[, factor:= NULL]
  alpha_name <- names(Factor)
}

{#weight
  weight <- function(IC, cov_M){
    std <- base::sqrt(diag(cov_M))
    a <- IC*std
    lambda <- solve(cov_M + 1e-7*diag(nrow(cov_M))) %*% a
    lambda <- lambda/sum(lambda)
    return(lambda)
  }
  n <- length(data)-2
  wt <- data[, {
    cov_M <- cov(.SD[,c(13:n),with=FALSE], use = "na.or.complete")
    IC_v <- as.numeric(lapply(.SD[,c(13:n),with=FALSE], cor, y=umret_next, use = "na.or.complete"))
    w <- weight(IC_v, cov_M)
  }, keyby = .(DataDate, ticktime)]
}

temp <- data[, {
  cov_M <- cov(.SD[,c(13:n),with=FALSE], use = "na.or.complete")
  IC_v <- as.numeric(lapply(.SD[,c(13:n),with=FALSE], cor, y=umret_next, use = "na.or.complete"))
  w <- weight(IC_v, cov_M)
  factor <- as.matrix(.SD[,c(13:n),with=FALSE])%*%w
}, keyby = .(DataDate, ticktime)]

data[, factor := temp$V1]
{
  IC_test     <- data[tradable == 1, cor(umret_next, factor, use = "na.or.complete"), by = .(DataDate, ticktime)]
  t_IC_test   <- t.test(IC_test$V1)
  IC_test[!is.na(V1), plot(cumsum(V1))]
}

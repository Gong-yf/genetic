setwd('C:\\Users\\gongyf\\documents\\genetic')
sourceCpp("TS_func.cpp")
# sourceCpp("genetic.cpp")
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
  select_subtree <- function(root, son_pro = 1){
    if(class(root) == "Leaf"){
      return(root)
    }else{
      rtn <- root
      sgn <- rbinom(1,1,son_pro)
      if(sgn != 0){
        dr <- rbinom(1,1,0.5)+1
        if(dr == 1){
          rtn <- select_subtree(root$left, son_pro-0.5)
        }else{
          rtn <- select_subtree(root$left, son_pro-0.5)
        }
      }
      return(rtn)
    }
  }
  crossover <- function(root1, root2){
    stree <- select_subtree(root2)
    if(class(root1$left)=="Leaf"){
      root1$left <- stree
      return(root1)
    }
    if(class(root1$right)=="Leaf"){
      root1$right <- stree
      return(root1)
    }
    sgn <- sample(1:4, 1)
    switch(sgn,
           root1$left$left <- stree,
           root1$left$right <- stree,
           root1$right$left <- stree,
           root1$right$right <- stree)
    return(root1)
  }
  subtree_mutation <- function(root1, func, data_prc){
    stree <- CreateTree(Node(), func, data_prc, LeafPro = 0.7)
    if(class(root1$left)=="Leaf"){
      root1$left <- stree
      return(root1)
    }
    if(class(root1$right)=="Leaf"){
      root1$right <- stree
      return(root1)
    }
    sgn <- sample(1:4, 1)
    switch(sgn,
           root1$left$left <- stree,
           root1$left$right <- stree,
           root1$right$left <- stree,
           root1$right$right <- stree)
    return(root1)
  }
  point_mutation <- function(root1, func, data_prc, pro = 0.3){
    sgn <- runif(1)
    if(sgn < pro){
      if(class(root1) == "Leaf"){
        i <- rbinom(1, length(data_prc)-1, 0.5)+1
        root1$val <- data_prc[[i]]
      }else{
        i <- rbinom(1, length(func)-1, 0.5)+1
        root1$val <- func[[i]]
      }
    }
    if(class(root1)!="Leaf"){
      root1$left <- point_mutation(root1$left, func, data_prc, pro = 0.2)
      root1$right <- point_mutation(root1$right, func, data_prc, pro = 0.2)
    }
    return(root1)
  }
  genetic <- function(father_Factor, t_IC1, data, func, data_prc,
                      p_crossover = 0.8, p_subtree = 0.02, p_point = 0.02){
    n <- length(father_Factor)
    n2 <- floor(n/5)
    father <- father_Factor[order(t_IC1,decreasing=TRUE)[1:n2]]
    Factor <- c()
    for(i in 1:n){
      rtn <- father_Factor[[i]]
      p1 <- runif(1)
      if(p1 < p_crossover){
        rt1 <- floor(runif(1)*n2)+1
        rt2 <- floor(runif(1)*n2)+1
        rtn <- crossover(father[[rt1]], father[[rt2]])
      }
      p2 <- runif(1)
      if(p2 < p_subtree){
        rtn <- subtree_mutation(rtn, func, data_prc)
      }
      p3 <- runif(1)
      if(p3 < p_point){
        rtn <- point_mutation(rtn, func, data_prc)
      }
      Factor[[i]] <- rtn
    }
    
    t_IC <- vector()
    for(i in 1:n){
      data[, pre_factor := eval(TraverseTree(Factor[[i]], n=2)), by = ukey]
      data[, factor := roll_mean(as.matrix(pre_factor), width = 16), by = ukey]
      
      temp   <- data[tradable == 1, cor(umret_next, factor, use = "na.or.complete"), by = .(DataDate, ticktime)]
      result <- tryCatch({
        t_IC[[i]]   <- base::abs(mean(temp$V1, na.rm = TRUE))
      }, error = function(err){
        t_IC[[i]]   <- 0
      })
      temp <- NULL
    }
    return(list(f = Factor, IC = t_IC))
  }
}

set.seed(1234)
Factor1 <- list()
t_IC1 <- vector()
IC <- data[, cor(umret_next, mom, use = "na.or.complete"), keyby = .(DataDate, ticktime)]
{
n1 <-  500
for (i in 1:n1) {
  Factor1[[i]] <- CreateTree(Node(), func, data_prc, LeafPro = 0.1)
  data[, pre_factor := eval(TraverseTree(Factor1[[i]], n=2)), by = ukey]
  data[, factor := roll_mean(as.matrix(pre_factor), width = 16), by = ukey]
  
  temp     <- data[tradable == 1, cor(umret_next, factor, use = "na.or.complete"), by = .(DataDate, ticktime)]
  result <- tryCatch({
    t_IC1[[i]]   <- base::abs(mean(temp$V1, na.rm = TRUE))
  }, error = function(err){
    t_IC1[[i]]   <- 0
  })
  temp <- NULL
}
r1 <- mean(t_IC1, na.rm = TRUE)

temp <- genetic(Factor1, t_IC1, data, func, data_prc, p_crossover = 0.4, p_subtree = 0.02, p_point = 0.02)
Factor2 <- temp$f
t_IC2 <- temp$IC
r2 <- mean(t_IC2, na.rm = TRUE)

temp <- genetic(Factor2, t_IC2, data, func, data_prc, p_crossover = 0.4, p_subtree = 0.02, p_point = 0.02)
Factor3 <- temp$f
t_IC3 <- temp$IC
r3 <- mean(t_IC3, na.rm = TRUE)
}

{
Factor <- list()
t_IC <- list()
IC <- data[, cor(umret_next, mom, use = "na.or.complete"), keyby = .(DataDate, ticktime)]

for (i in 1:n1) {
  Factor[paste('alpha', i, sep = "_")] <- NA
  Factor[[i]] <- Factor3[[i]]
  data[, pre_factor := eval(TraverseTree(Factor[[i]], n=2)), by = ukey]
  data[, factor := roll_mean(as.matrix(pre_factor), width = 16), by = ukey]
  # data[, paste('alpha', i, sep = "_") := factor]
  
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
  for (i in n1:1) {
    if(class(t_IC[[i]])!="numeric" || 
       base::abs(t_IC[[i]])<3 || 
       sum(is.na(IC[, paste('alpha', i, sep = "_"),with=FALSE]))>100){
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
}

library(MASS)
library(Mediana)

dunnett <- function(p, m){
  f <- AdjustPvalues(c(p, rep(0.999, m-1)),proc = "DunnettAdj",par = parameters(n = Inf))[1]
  return(f)
}
### Parameters ################
# S1N <- c(50, 55, 60, 65)
# S2N <- c(332, 332, 332, 332)
# Rho_list <- seq(0,1,length.out=11)
# NCP_Bio <- c(0.4016097, 1.2247449, 1.8774462)
# NCP_Pri <- c(1.101149, 1.101149, 1.101149)
# NCP_Pri_Final <- c(2.837461,2.837461,2.837461)
# num_events_s1 <- c(59,59,59)
# num_events_final <- c(331,331,331)

NCP_binary <- function(N0, N1, p0, p1){
  (p1-p0)/sqrt(p0*(1-p0)/N0+p1*(1-p1)/N1)
}

NCP_normal <- function(N0, N1, mu0, sig0, mu1, sig1){
  (mu1-mu0)/sqrt(sig0^2/N0+sig1^2/N1)
}

NCP_TTE <- function(Num_event, HR){
  -log(HR)*sqrt(Num_event/4)
}

rank_based_Dunnett <- function(S1N, S2N, NCP_Bio, NCP_Pri, NCP_Pri_Final, rep=100, alpha=0.025, Rho_list, prog=FALSE, seed=1234, TTE_Primary=FALSE, num_events_s1=NULL, num_events_final=NULL){
  # S1N: in stage 1, sample size of control, g1, g2, ....
  # S2N: in stage 2, additional sample size of control, selected group
  # NCP_Bio: biomarker effect at IA, g1 vs control, g2 vs control, ....
  # NCP_Pri: primary endpoint effect at IA, g1 vs control, g2 vs control, ....
  # NCP_Pri_Final: primary endpoint effect (at 2nd stage only!! Except for TTE, where this is cumulative at FA), g1 vs control, g2 vs control, ..... at 2nd stage
  # NCP stands for Noncentrality Parameter
  # num_events_s1, num_events_final: cumulative number of total events at IA and FA
  
  set.seed(seed)
  
  # m is number of trt groups
  m <- length(S1N)-1
  # 
  Nj <- matrix(c(S1N[2:(m+1)]), nrow=m, ncol=m)
  Np <- matrix(S1N[2:(m+1)], byrow = T, nrow=m, ncol=m)
  sig_norho <- sqrt(Nj*Np/(S1N[1]+Nj)/(S1N[1]+Np))
  diag(sig_norho) <- 1
  
  # Save results for No adjustment, with Dunnett adjustments
  cols <- c(paste0('Rank', 1:m, '_NoAdj'), paste0('Rank', 1:m, '_Dunnett'))
  output <- as.data.frame(matrix(NA, length(Rho_list), length(cols)))
  colnames(output) <- cols
  rownames(output) <- paste0("Rho=", Rho_list)
  Z_info <- list()
  for (i in seq(length(Rho_list))){
    rho <- Rho_list[i]
    print(paste0("rho=", rho))
    if (prog){
      setProgress(message = paste("Estimating Rho=", rho), value = which(rho==Rho_list)/length(Rho_list))
    }
    sig <- cbind(rbind(sig_norho, sig_norho*rho), rbind(sig_norho*rho, sig_norho))
    z1 <- mvrnorm(n=rep,mu=c(NCP_Pri, NCP_Bio),Sigma=sig) # stage 1 test stats
    z2 <- mvrnorm(n=rep,mu=NCP_Pri_Final, diag(1, m)) # stage 2 test stats only (except for TTE, which is cumulative)
    
    x=z1[,(m+1):(2*m)] # biomarker s1 stat
    y=z1[,1:m] # efficacy s1 stat
    r1s=apply(x,1,rank) # stage 1 biomarker rank
    
    # D for Dunnet adjustment, N for No adjustment
    # final p values
    p.D <- matrix(nrow=rep,ncol=m) 
    p.N <- matrix(nrow=rep,ncol=m)
    
    Z_all.primary_s1.D <- NULL # Z stat for primary endpoint at s1 with Dunnet
    Z_all.primary_s1.N <- NULL # Z stat for primary endpoint at s1 without Dunnet
    Z_all.primary_s2 <- NULL #  Z stat for primary endpoint only for s2 (except for TTE)
    Z_all.D <- NULL # final Z stat with Dunnet
    Z_all.N <- NULL # final Z stat without Dunnet
    for (k in 1:m){
      selected <- which(r1s==k, arr.ind = T)[,1]
      sy <- t(y)[r1s==k] # Z_s1_N: select primary endpoint Z stat based on biomarker ranking
      Z_all.primary_s1.N <- cbind(Z_all.primary_s1.N, sy)
      p0 <- pnorm(sy,lower.tail=F) # stage 1 raw p-value
      p1D <- unlist(lapply(p0, FUN=function(x2) dunnett(x2, m=k))) # stage 1 Dunnet p-value
      z1D <-qnorm(p1D, lower.tail = F)  # Z_s1_D: primary endpoint Z stat with Dunnet
      Z_all.primary_s1.D <- cbind(Z_all.primary_s1.D, z1D)
      
      sz2 <- t(z2)[r1s==k] # Z_s2: primary Z stat only for s2 (except for TTE)
      
      N1 <- S1N[1]+S1N[selected+1] # control + trt sample size at S1
      N2 <- S2N[1]+S2N[selected+1] # control + trt sample size only at S2
      
      if (TTE_Primary){
        # page 1515 on East 6 manual
        num_e_s1 <- num_events_s1[selected] # number of total events at IA
        num_e_final <- num_events_final[selected] # cumulative number of total events at FA
        r1 <- S1N[1]/N1
        r2 <- (S2N[1]+S1N[1])/(N1+N2)
        I1 <- r1*(1-r1)*num_e_s1
        I2 <- r2*(1-r2)*num_e_final
        # sz2.D <- (sqrt(I2)*sz2-sqrt(I1)*z1D)/sqrt(I2-I1) # incremental statistics at stage 2 with Dunnet 
        # sz2.N <- (sqrt(I2)*sz2-sqrt(I1)*sy)/sqrt(I2-I1) # incremental statistics at stage 2 without Dunnet 
        sz2 <- (sqrt(I2)*sz2-sqrt(I1)*sy)/sqrt(I2-I1) # incremental statistics at stage
        w1 <- sqrt(num_e_s1/num_e_final)
        w2 <- sqrt((num_e_final-num_e_s1)/num_e_final) # stage 1 and 2 weight
        z.D <- w1*z1D+w2*sz2
        z.N <- w1*sy+w2*sz2
      }else{
        w1 <- sqrt(N1/(N1+N2))
        w2 <- sqrt(N2/(N1+N2)) # stage 1 and 2 weight
        z.D <- w1*z1D+w2*sz2
        z.N <- w1*sy+w2*sz2
      }
      Z_all.primary_s2 <- cbind(Z_all.primary_s2, sz2)
      
      Z_all.D <- cbind(Z_all.D, z.D)
      Z_all.N <- cbind(Z_all.N, z.N)
      p.D[,k] <- pnorm(z.D, lower.tail=F) #final combined p-value
      p.N[,k] <- pnorm(z.N, lower.tail=F) #final combined p-value
    }
    rejectN <- (p.N<=alpha) # reject null
    rejectD <- (p.D<=alpha) # reject null
    output[i,] <- c(apply(rejectN,2,mean),apply(rejectD,2,mean))
    Z_info[[paste0('rho=',rho)]] <- list(Z.primary_s1.D=Z_all.primary_s1.D, 
                                         Z.primary_s1.N=Z_all.primary_s1.N,
                                         Z.primary_s2=Z_all.primary_s2, 
                                         Z.D=Z_all.D, Z.N=Z_all.N)
  } 
  return(list(Power=output, Z_stat=Z_info))
}

# ddd <- rank_based_Dunnett(S1N, S2N, NCP_Bio, NCP_Pri, NCP_Pri_Final, rep=300, alpha=0.025, Rho_list, seed=1234)
# ddd <- rank_based_Dunnett(S1N, S2N, NCP_Bio, NCP_Pri, NCP_Pri_Final, rep=300, alpha=0.025, Rho_list, seed=1234, prog = F, TTE_Primary=T, num_events_s1, num_events_final)
# eee <- ddd$Z_stat$`rho=0.5`
# dim(eee$Z.biomarker)
# cdf1 <- Ecdf(eee$Z.biomarker[,1], lwd = 3, col = "red", xlab = "", ylab = "Empirical CDF", lty = 1) # r=1 for biomarker
# cdf2 <- Ecdf(eee$Z.biomarker[,2], lwd = 3, add=T) # r=2 for biomarker
# cdf3 <- Ecdf(eee$Z.biomarker[,3], lwd = 3, add=T, lty=5) # r=3 for biomarker

library(nlme)
library(sandwich)
setwd("C:\\Users\\sg137\\Documents\\Economics Masters\\Applied Longitudinal Analysis")

leadl<- read.table("leadlong.txt", header=TRUE)


head(leadl)

model<- gls(data=leadl,level~treatment*time,
             correlation = corSymm(form = ~ 1 | ID),
             weights = varIdent(form = ~ 1 | occur))

#extracting the sigma, design matrix,covariance of beta and fitted values from the model
model=model
data=leadl

N=100
n=4

emp_var_gls <- function(N=N, n=n, model=model, data=data){
    cov_beta<- summary(model)$varBeta
    X=as.data.frame(model.matrix(model, data))

    sigma<- getVarCov(model)
    fit<- data.frame(t(t(summary(model)$fitted)), data$ID, data$time)
    names(fit)<- c("fit", "ID", "time")
    
    indiv_variance<- matrix(0L, nrow=N*n, ncol=n)
    
    for (j in 1:N){
      indiv_variance[j]<-lapply(split(data, data$ID),function(i){ 
          (t(t(i$level))-fit[(n*(j-1)+1):(n*(j-1)+n),1])%*%
          t((t(t(i$level))-fit[(n*(j-1)+1):(n*(j-1)+n),1]))}
          )
    }

    p=(dim(X)[2]) #no of parameters
    middle<- matrix(0L, nrow=N*p, ncol=p)
    for (j in 1:N){
       middle[(p*(j-1)+1):(p*(j-1)+p),1:p]<- t(X[(n*(j-1)+1):(n*(j-1)+n),])%*%solve(sigma)%*%indiv_variance[[j]]%*%
                                          solve(sigma)%*%
                                          t(t(X[(n*(j-1)+1):(n*(j-1)+n),]))
    }

    #not sure why not working without having two transpose for X but it's the same thing

    middlesplit<- rep(1:N, each=n) #split by ID
    tmp=split.data.frame(middle, middlesplit)
    sum_middle<- Reduce("+",tmp) #add by ID

    emp_var_estimate<- (cov_beta)%*%sum_middle%*%(cov_beta)
    return(emp_var_estimate)
}

emp_var_estimate= emp_var_gls(data=leadl, model = model, N=N, n=n)
round(emp_var_estimate, digits=4)
round(summary(model)$varBeta, digits=4)


model_s <- lm(data=leadl,level~treatment*time)

#the same function for lm models 
emp_var_lm <- function(N=N, n=n, model=model, data=data){

    X=as.data.frame(model.matrix(model, data))
  
    sigma<- ((summary(model)$sigma)^2)*diag(n)
  
    fit<- data.frame(t(t(fitted(model))), data$ID, data$time)
    names(fit)<- c("fit", "ID", "time")
  
    indiv_variance<- matrix(0L, nrow=N*n, ncol=n)
  
  for (j in 1:N){
    indiv_variance[j]<-lapply(split(data, data$ID),function(i){ 
      (t(t(i$level))-fit[(n*(j-1)+1):(n*(j-1)+n),1])%*%
        t((t(t(i$level))-fit[(n*(j-1)+1):(n*(j-1)+n),1]))}
    )
  }
  
  p=(dim(X)[2]) #no of parameters
  middle<- matrix(0L, nrow=N*p, ncol=p)
  for (j in 1:N){
    middle[(p*(j-1)+1):(p*(j-1)+p),1:p]<- t(X[(n*(j-1)+1):(n*(j-1)+n),])%*%solve(sigma)%*%indiv_variance[[j]]%*%
      solve(sigma)%*%
      t(t(X[(n*(j-1)+1):(n*(j-1)+n),]))
  }
  
  #not sure why not working without having two transpose for X but it's the same thing
  
  middlesplit<- rep(1:N, each=n)
  tmp=split.data.frame(middle, middlesplit)
  sum_middle<- Reduce("+",tmp)
  
  #estimate the beginning and end parts
  sigma_full <-((summary(model)$sigma)^2)*diag(N*n)
  
  beg <- solve(t(X)%*%solve(sigma_full)%*%t(t(X)))
  
  emp_var_estimate_lm<- (beg)%*%sum_middle%*%(beg)
  return(emp_var_estimate_lm)
}

emp_var_lm= emp_var_lm(data=leadl, model = model_s, N=N, n=n)
round(sandwich(model_s), digits =4)  #slightly different values why?


# 
# X=as.data.frame(model.matrix(model_s, data))
# beg_emp <- solve(t(X)%*%t(t(X)))
#   
# fit<- data.frame(t(t(fitted(model_s))), data$ID, data$time)
# indiv_variance<-0
#   
# variance=(data[,4]-fit[,1])%*%t((data[,4]-fit[,1]))
# middle_lm<- t(X)%*%variance%*%(t(t(X)))
# 
# emp_sandwich<- (beg_emp%*%middle_lm%*%beg_emp)
#   

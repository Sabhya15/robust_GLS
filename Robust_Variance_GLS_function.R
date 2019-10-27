library(nlme)
library(sandwich)
setwd("C:\\Users\\sg137\\Documents\\Economics Masters\\Applied Longitudinal Analysis")

leadl<- read.table("leadlong.txt", header=TRUE)


head(leadl)

model<- gls(data=leadl,level~treatment+time,
             correlation = corSymm(form = ~ 1 | ID),
             weights = varIdent(form = ~ 1 | occur))

#extracting the sigma, design matrix,covariance of beta and fitted values from the model
model=model
data=leadl

N=100
n=4

emp_var_gls <- function(N=N, n=n, model=model, data=data, ID=ID,time=time, level=level){
    cov_beta<- summary(model)$varBeta
    X=as.data.frame(model.matrix(model, data)) #add zeros maybe when missing?
    #defiena na empty x 
 
    sigma<- getVarCov(model) #subject specific matrices maybe? 
    fit<- data.frame(t(t(summary(model)$fitted)), data$ID, data$time)
    names(fit)<- c("fit", "ID", "time")
    
    indiv_variance<- matrix(0L, nrow=N*n, ncol=n)
    
    for (j in 1:N){
      indiv_variance[(n*(j-1)+1):(n*(j-1)+n), 1:n]<- (data$level[(n*(j-1)+1):(n*(j-1)+n)]-fit[(n*(j-1)+1):(n*(j-1)+n),1])%*%
        t((data$level[(n*(j-1)+1):(n*(j-1)+n)]-fit[(n*(j-1)+1):(n*(j-1)+n),1]))
    } #checked for id 1 and 2 - working fine


    p=(dim(X)[2]) #no of parameters
    middle<- matrix(0L, nrow=N*p, ncol=p)
    for (j in 1:N){
       middle[(p*(j-1)+1):(p*(j-1)+p),1:p]<- t(X[(n*(j-1)+1):(n*(j-1)+n),])%*%solve(sigma)%*%indiv_variance[(n*(j-1)+1):(n*(j-1)+n),]%*%
                                          solve(sigma)%*%
                                          t(t(X[(n*(j-1)+1):(n*(j-1)+n),]))
    }

    #not sure why not working without having two transpose for X but it's the same thing
    
    middle_array<- array(0L, dim = c(p,p,N))
    
    for (j in 1:N){
      middle_array[,,j]= middle[(p*(j-1)+1):(p*(j-1)+p),1:p]
      
    }
    
    middle_sum<- rowSums(middle_array, dim=2)

    middlesplit<- rep(1:N, each=p) #split by ID - each person has p rows
    tmp=split.data.frame(middle, middlesplit)
    sum_middle<- Reduce("+",tmp) #add by ID
    
    #middle sum and sum_middle are identical

    emp_var_estimate<- (cov_beta)%*%middle_sum%*%(cov_beta)
    return(emp_var_estimate)
}

emp_var_estimate= emp_var_gls(data=leadl, model = model, N=N, n=n)
round(emp_var_estimate, digits=4)
round(summary(model)$varBeta, digits=4)



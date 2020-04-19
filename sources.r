
linear.model<-function(beta,covariate){
  # beta: a 2-vector, where the first entry is the intercept
  yout=covariate*beta[2]+beta[1]
  return(yout);
  # Note: this function only works with simple linear regression model
  # How would you generalize it?
}

fit.linear.model<-function(covariate,outcome){
  # I will write down the function for (multiple) linear regression here
  X=cbind(1,covariate);
  beta.fit=solve( t(X)%*%X )%*%t(X)%*%outcome;
  return(beta.fit)
}

sum.of.squares<-function(beta,covariate,outcome){
  yout=linear.model(beta=beta,covariate=covariate);
  res=outcome-yout;
  sos= sum(res^2);
  return(sos)
}

estimate.sigma.sq<-function(beta,covariate,outcome){
  residual.sum.of.squares=sum.of.squares(beta=beta,covariate=covariate,outcome=outcome)
  n=length(outcome)
  sigma.sq.hat=residual.sum.of.squares/(n-2)
  return(sigma.sq.hat)
}

estimate.coef.var<-function(beta,covariate,outcome){
  sigma.sq.hat=estimate.sigma.sq(beta,covariate,outcome)
  var.hat.beta=beta;
  var.hat.beta[2]=sigma.sq.hat/sum(  (covariate-mean(covariate))^2  )
  n=length(outcome)
  var.hat.beta[1]=sigma.sq.hat*sum(covariate^2)/sum((covariate-mean(covariate))^2  )/n
  return( var.hat.beta)
}

estimate.coef.sd<-function(beta,covariate,outcome){
  var.hat.beta=estimate.coef.var(beta,covariate,outcome)
  sd.hat.beta=sqrt(var.hat.beta);
  return(sd.hat.beta)
}

conf.int.quantile<-function(alpha,type,...){
  if(type=="t"){
    out=qt(c(1-alpha/2,alpha/2), ... )
  }else if (type=="normal"){
    out=qnorm(c(1-alpha/2,alpha/2), ... )
  }
  return(out)
}

boot.fit<-function(covariate,outcome){
  n=length(outcome);
  sample_indices = sample(1:n,n,replace=TRUE) # sampling with replacement
  covariate.boot= covariate[sample_indices]; outcome.boot= outcome[sample_indices];

  beta.hat=fit.linear.model(covariate=covariate.boot,outcome=outcome.boot);
  return(t(beta.hat ))
}

conf.int<-function(alpha,type,covariate,outcome,B=1e5){

  beta.hat=fit.linear.model(covariate,outcome);
  beta.sd=estimate.coef.sd(beta=beta.hat,covariate,outcome);
  if(type=='bootstrap'){
    beta.hat.boot=replicate(B,boot.fit(covariate,outcome));
    out=t(apply(beta.hat.boot[1,,],1,quantile,probs=c(alpha/2,1-alpha/2)));
  }else if(type=='t'){
    quants<-conf.int.quantile(alpha,type='t',df=n-2)
    out=beta.hat%*%c(1,1)-beta.sd%*%quants;
  }else{
    quants<-conf.int.quantile(alpha,type='normal')
    out=beta.hat%*%c(1,1)-beta.sd%*%quants;
  }
  colnames(out)=c( paste(round(alpha*50,digits=3),'%'), paste(100-round(alpha*50,digits=3),'%')  )
  return(out)
}

calculate.t<-function(covariate,outcome){
  beta.hat=fit.linear.model(covariate=covariate,outcome=outcome)
  beta.hat.sd=estimate.coef.sd(beta=beta.hat,covariate=covariate,outcome=outcome)
  beta.hat.t = (beta.hat-0)/beta.hat.sd;
  return(beta.hat.t)
}

## Wrap up the above code into one function
calculate.pvalue<-function(covariate,outcome,type){
  beta.hat.t=calculate.t(covariate,outcome);
  if (type=='t'){
    n=length(outcome);
    pval=2*apply(cbind(pt(beta.hat.t,df=n-2),(1-pt(beta.hat.t,df=n-2))),1,min);

  }else if (type=='z'){
    pval=2*apply(cbind(pnorm(-abs(beta.hat.t)),(1-pnorm(abs(beta.hat.t)))),1,min);

  }else if (type == 'bootstrap'){

    beta.hat=fit.linear.model(covariate=covariate,outcome=outcome)
    beta.hat.boot=replicate(1e5,boot.fit(covariate=covariate,outcome=outcome));
    pval=numeric(length(beta.hat));
    for(i in 1:length(beta.hat)){
      boot.est=beta.hat.boot[1,i,]
      pval[i]=2*min( mean(0<boot.est), mean(0>boot.est) )
    }
  }
  return(pval)
}

permutation.test<-function(covariate,outcome){
  n=length(outcome);
  sample_indices = sample(1:n,n,replace=FALSE) # sampling without replacement
  covariate.perm= covariate[sample_indices]; outcome.perm= outcome;

  beta.hat.t=calculate.t(covariate.perm,outcome.perm)

  return(beta.hat.t[2])
}


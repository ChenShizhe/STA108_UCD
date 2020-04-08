# Inverse of a matrix
set.seed(1)
A=matrix(rnorm(9),3,3)
B=matrix(rnorm(9),3,3)
# Pseudorandomness

dim(A)

A.inv =solve(A)
A.inv

A.inv %*% A
mat.tmp=A%*%A.inv
mat.tmp[2,3]==0
abs(mat.tmp[2,3])< 1e-10

AB.inv = solve(A%*%B)
B.inv=solve(B);

identical(AB.inv,B.inv%*%A.inv)
AB.inv-B.inv%*%A.inv
max(abs( AB.inv-B.inv%*%A.inv))<1e-10

# AB.inv satisfies that
# AB%*%AB.inv = I
# AB=A%*%B
#  AB%*%(B.inv)%*%(A.inv)
# = A%*%B%*%(B.inv)%*%(A.inv)
# = A%*% (B%*%B.inv)%*%(A.inv)
# = A%*%I%*%A.inv
# = A%*%A.inv = I

set.seed(1)
X=matrix(rnorm(6),3,2)

P=X%*%solve(t(X)%*%X)%*%t(X)

P%*%P%*%P%*%P%*%P-P
(diag(3)-P)%*%P

P%*%X-X
(diag(3)-P)%*%X


dim(P)
dim(X)
dim(P%*%X)
P%*%t(X)

# Rank of a matrix
qr(X)$rank
dim(X)

#
XXt=X%*%t(X)
qr(XXt)$rank
dim(XXt) # Not full rank!

solve(XXt)

# Determinant of a matrix
det(A)
1/det(solve(A))
det(A%*%B)
det(A)*det(B)

det(2*A)/det(A)
2^3

tr<-function(A){
  # Need to check whether A is a squared matrix or not
  out=sum(diag(A))
  return(out)
}
tr(A)

A.symm= (A+t(A))/2;
identical(A.symm,t(A.symm))

sum(eigen(A.symm)$values)
tr(A.symm)

det(A.symm)
prod(eigen(A.symm)$values)

# Eigenvalue and eigenvector
A.symm.eigenvalues = eigen(A.symm)$values
A.symm.eigenvectors = eigen(A.symm)$vectors

(A.symm%*%A.symm.eigenvectors[,1])/A.symm.eigenvectors[,1]
A.symm.eigenvalues[1]


eigen(XXt)
qr(XXt)$rank

eigen(P)

# Data set
library(AER)

#install.package('AER')

data('Fatalities')
?Fatalities

data('CreditCard')
?CreditCard

head(CreditCard)

dat.CC=CreditCard;
Y.CC=dat.CC[,1]=='yes';
X.CC=as.matrix(dat.CC[,c('reports','age','income','share','expenditure','dependents','months','majorcards','active')])


qr(X.CC)$rank
dim(X.CC) # Full rank, i.e., no multicolinearity!


fit.CC=lm(Y.CC~X.CC-1)
summary(fit.CC)

beta.hat=solve(t(X.CC)%*%X.CC)%*%t(X.CC)%*%Y.CC;
fit.CC$coefficients-beta.hat

P.CC=X.CC%*%solve(t(X.CC)%*%X.CC)%*%t(X.CC)

Yhat.CC=P.CC%*%Y.CC
max(abs(fit.CC$fitted.values-Yhat.CC))




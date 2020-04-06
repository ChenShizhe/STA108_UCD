## Linear algebra
1:10

x=seq(from=3.1, to=8.9, length.out=10)
x[1]
x[2]

x.t=t( x ) # transpose of x
length(x.t)
length(x)

x
x.t

y=x^2
y

x/y
y-x*x

dim(x %o% y) # outer product

x%*%y # inner product

sum(x*y)

### Create a matrix with 2 rows and 3 columns
M=matrix(0,2,3);
M

Xmat1=matrix(x,2,5)
Xmat1
Xmat2=matrix(x,2,5,byrow=T)
Xmat2

M<-Xmat1;
dimnames(M)

rownames(M)<-c("first","second")
M
colnames(M)<-c('a','b','c','d','e')
M

identical(M[,1],M[,'a'])

identical(M[2,],M['second',])


M[,1:3]
M[,c("a","d","e")]


M2=rbind(M, c(1,2,3,4,5))
rownames(M2)[3]<-'third'
M2
?cbind

length(M)
dim(M)


### Operations on matrices

M.t=t(M)
M.t
identical(t(M.t),M)


### Diagonal and identity matrix
diag(5) # identity matrix
diag(c(1,2,3,4,5)) # diagonal matrix


### Symmetric matrix
dim(M.t)
dim(M)

A=matrix(rnorm(9),3,3)
identical(A,t(A))
A= A+t(A);
identical(A,t(A))
# A is a symmetric matrix!

### Trace of a matrix
# ?tr()
diag(diag(c(1,2,3,4,5)))

A
sum(diag(A))
### Create our own function to calculate the trace
tr<-function(A){
  # Need to check whether A is a squared matrix or not
  out=sum(diag(A))
  return(out)
}
tr(A)

## Verify the properties of trace
B=matrix(rnorm(9),3,3)
B
A

tr(A)
tr(t(A))

tr(A+B)
tr(A)+tr(B)

A
B
A+B # A-B?

tr(A%*%B)
tr(B%*%A)

tr(A)
sum(eigen(A)$values)

## Matrix product:

## Matrix-vector product
dim(M)
xvec=rnorm(5)
xvec
M%*%xvec
# M: 2 by 5 matrix
# xvec: 5 by 1 vector (matrix)
# (2 by 5) %*% (5 by 1)
# 2 by 1

xvec2=rnorm(7)
M%*%xvec2


### Matrix-matrix product
dim(M)

C<- matrix(rnorm(15),5,3)
M%*%C # a 2 (by 5) by (5 by) 3 matrix

M2<- matrix(rnorm(10),2,5)

M*M2 # elementwise product between two matrices
dim(M)
dim(M2)

M%*%M2
M%*%t(M2)

t(M%*%t(M2))

M2%*%t(M)

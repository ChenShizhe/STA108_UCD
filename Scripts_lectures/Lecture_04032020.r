## Basics in R

## Generate a vector with seq()
?seq

# Generate a sequence from 3.1 to 8.9, with gap between adjacent unit being 0.8
seq(from = 3.1, to= 8.9, by=0.8)
seq(from=3.1, to =8.9, length.out=10)
#seq(from=3.1, to =8.9, by=1, length.out=10)

a=1:3
(a= 1:3)
(b=c(1,2,3))

a == b

?identical
identical(a,b)

typeof(a)
typeof(b)

(u=c(TRUE,T,FALSE,F,T))
typeof(u)

(w=c("a", 'b', "c", "d"))
typeof(w)
paste(w[1],w[2],sep="")

y=c(1:3,w)
typeof(y[1])

z=as.numeric(y)

?NA
is.na(y[4])
is.na(z[4])

length(y)
k= 3;
y[k]

x = c(1,3,4,6.7,-4,NA,-2.9)
x

x == 1
x>1
x<1
x>=1

x[which(x>=1)]
x[x>=1]


x[  x>0 ]

x[  x<2 ]

x[(x>0) & (x<2)] # &: and
x[(x>0) | (x<2)] # |: or

(x<0)
!(x<0)
x[ which(!(x>0) & (x<2)) ]

##
x

length(x)
sum(x)
?sum
sum(x,na.rm=TRUE)
sd(x,na.rm=TRUE)
mean(x,na.rm=TRUE)

0/0
?NaN
1/0
-1/0

is.na()
is.nan(0/0)
y=NULL
is.null(y)
y

?sum # <- find the help file for the function sum.
args(sum)
example(sum)

## Strings:
wrd = c("a","new","sentence")
u = c("_","!","?")
wrd
typeof(wrd)

print("this is a sentence")
length("this is a sentence")
length(wrd)

paste(wrd,u,sep="")


split.str=strsplit("this is a sentence", split=" ")
typeof(split.str)
length(split.str)
split.str[[1]]


## Data frame

name =c("Bob","Jane")
age = c(19,21)
GPA = c(3.2,3.6)

students = data.frame(name,age,GPA,stringsAsFactors=F)
students
str(students)

?rbind
students = rbind(students,c("Ashley",20,3.4))
students


students = cbind(students,resident=c(T,F,T))
students


## Functions
?rbind
?seq

x = seq(0,1,0.05)
x.check=seq(from = 0, to= 1, by=0.05)

identical(x,x.check)

y = sin(2*pi*x)*exp(-4*x^2) #<- reproduce this function on our own!


sinexp = function(z){
  out=sin(2*pi*z)*exp(-4*z^2);
  return(out)
}

y.new=sinexp(z=x)
identical(y.new,y)


plot(y~x, type='l')
lines(x=x,y=y.new,col='red')

lm() #<- This is what we will reproduce in this quarter



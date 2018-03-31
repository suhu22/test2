#1
x = c(17,16,20,24,22,15,21,18)
x[x>=20]  <- 100
y = x
y[which(x>20)]
y=c()
for (i in 1:length(x)){
  if(x[i]>=20){
    y[i] <- 100
  } 
  else {
    y[i] <- x[i]
  } 
}
y


#2
x1 = c(3,-1,-1,-1,-1)
x2= c(-1,4,-1,-1,-1)
x3 = c(-1,-1,5,-1,-1)
x4 = c(-1,-1,-1,6,-1)
x5 = c(-1,-1,-1,-1,7)
x = rbind(x1,x2,x3,x4,x5)
#2) 
y <- x[,-5]
#3)
yinfo <- c(nrow(y),ncol(y))

y==-1
#4)
y[y==-1] <- 0
y1 <- y
y1 = matrix()
for(j in 1:ncol(y)){
  for(i in 1:nrow(y)){
    if (y[i,j] == -1){
      y1[i,j] == 0
    } 
    else {
      y1[i,j] == y[i,j]
    }
  }
}
y1

#3 데이터??
setwd('C://Temp')
is.na(y1)
is.na(y1[,2]) == F && is.na(y1[,3]) == F

#4
#1)
t1 <- c(TRUE,FALSE)
t2 <- matrix(c(1,0,0,1), nrow =2)
t3 <- c(seq(from=0, to=1, length =100))
temp <- list(t1 = t1, t2= t2, t3= t3, t4=1, t5=2, t6=3, t7=4)
#2
temp[-2]
#3 
temp[-2][3]
#4
length(temp[-2])


#해석은 쓰면서 

#R-prog

#1

 a <- function(n){
    b = c()
     for(i in 1:n){
      b[1] <-1
      b[2] <-3
       b[i+2] = 0.9*b[i+1] - 0.1*b[i] + 1
     } 
    return(b[n])
 }
 a(1)
a(2) 
a(3)
a(4)

'''for(i in 1:length(a)){
  if(a(i) >4){
    print(i)
  }
}'''

#2

#3
A <- matrix( runif(100), 50, 5)
rowmean = function(x)
{
  if ( class(x) != "matrix") stop()
  v = rep(0, nrow(x))
  for ( i in 1:nrow(x))
  {
    v[i] = mean( x[i,] )
  }
  return(v)  
}
rowmean(A)

#4
tmp = rep(0, 10)
a <- 10:1
idx = 1
for ( j in a)
{
  if (j<5)
  {
    tmp[idx] <- a[j]
    idx <- idx + 1
  }
}
tmp

#5
sample(1:10,3)
x = matrix(nrow=1000, ncol = 5)
x[1,] = sample(1:10, ncol(x))
sid = matrix(0,nrow(x),ncol(x))
for(j in 1:ncol(x)){
for(i in 1:nrow(x)){
  sid[i,j] <- sample(1:10,1)
}
}
sid[1,]


for(i in 1:1000){
  x[i,] = sample(1:10,5)
}

x = matrix(runif(5000),1000,5)
set.seed(1)

set.seed(1)
for(i in 1:1000){
  x[i,] = sample(1:10,5)
}


sid = c()
for(i in 1:1000){
  sid[i] <- sample(1:10, 1)
}
head(sid)
xx = cbind(x,sid)
m.mat1<- matrix(0,10,5)

for(i in 1:10){
  for(j in 1:1000){
    if(xx[j,6] == i){
      m.mat1[i,] <- m.mat1[i,] + xx[j,1:5]
    }
  }
}
head(m.mat1,3)
m.mat <- m.mat1 / c(table(xx[,6]))
head(m.mat,3)
sum(x[1,]*m.mat[1,])

idist = matrix(0,1000,10)
for(j in 1:10){
  for(i in 1:1000){
    idist[i,j] <- sum(x[i,]*m.mat[j,])/(sqrt(sum(x[i,]*x[i,]))*sqrt(sum(m.mat[j,]*m.mat[j,])))
  }
}
head(idist,3)
x[1,]
m.mat[1,]
a1 = sqrt(sum(x[1,]*x[1,]))
a2 = sqrt(sum(m.mat[1,]*m.mat[1,]))
a3 = sum(x[1,]*m.mat[1,])
a3/(a1*a2)
ivec = c()
for(i in 1:nrow(idist)){
  ivec[i] <- which.min(idist[i,])
}
ivec
table(ivec)




set.seed(1)
a = list()
for (i in 1:1000)
{
  x = rpois(1,4)+1
  x = min(x,10)
  a[[i]] = sample(1:10, x)
}
head(a,3)
b = rep(0,10)
for(j in 1:10){
  for(i in 1:1000){
    if(length(a[[i]]) == j){
      b[j] <- b[j] + 1
    }
  }
}




bk_1_4 = c()
bk_1_7 =c()
bk_2_7 = c()
bk_1_10 =c()
bk_2_10 =c()
bk_3_10 =c()
for(j in 1:10){
  for(i in 1:1000){
    if(1<length(a[[i]]) && length(a[[i]])<4){
      bk_1_4[i] <- a[[i]][1]
    } else if (4<=length(a[[i]]) && length(a[[i]]) <7){
      bk_1_7[i] <- a[[i]][1]
      bk_2_7[i] <- a[[i]][2]
    } else if ( 7<=length(a[[i]])){
      bk_1_10[i] <- a[[i]][1]
      bk_2_10[i] <- a[[i]][2]
      bk_3_10[i] <- a[[i]][3]
    }
  }
}
table(bk_1_4)

win1_1 <- matrix(table(bk_1_4), nrow = 10)
win1_2 <- matrix(table(bk_1_7), nrow = 10)
win1_3 <- matrix(table(bk_1_10), nrow = 10)
win2_1 <- matrix(table(bk_2_7), nrow = 10)
win2_2 <- matrix(table(bk_2_10), nrow = 10)
win3_1 <- matrix(table(bk_3_10), nrow = 10)
wintable <- cbind(win1_1, win1_2, win1_3, win2_1, win2_2, win3_1)
score = c()
for(j in 1:10){
  score[j] <- wintable[j,1]+2*wintable[j,2]+3*wintable[j,3] + wintable[j,4]+
    wintable[j,5]*2+ wintable[j,6]
} 
score
39+51*2+16*3+63+20*2+24
which.max(score)



set.seed(1)
m1= 10
m2 = 15
Bwincount = 0
Awincount = 0

a = c()
for(i in 1:200){
  a[i] <- rbinom(1,1,1/2)
}
for(k in 1:200)
for(k in 1:){
     if(a[k] == 0){
         m1 <- m1-1
         m2 <- m2+1
         Bwincount <- Bwincount + 1
       if(m1 == 0) {
             print(k)
            print('B win')
             stop()
           }
       } else if (a[k] == 1){
           m1 <- m1 +1
           m2 <- m2 -1
           Awincount <- Awincount + 1
           if(m2 == 0) {
              print(k)
               print('A win')
               stop()
             }
         }
}

Awincount
Bwincount


#1
x <- c(17,16,20,24,22,15,21,18)
x[x>=20]
x[x>=20] <- 100
y <- x
y

#2
x1 <- c(3,-1,-1,-1,-1)
x2 <- c(-1,4,-1,-1,-1)
x3 <- c(-1,-1,5,-1,-1)
x4 <- c(-1,-1,-1,6,-1)
x5 <- c(-1,-1,-1,-1,7)
xx <- cbind(x1,x2,x3,x4,x5)
x <- matrix(xx, nrow = 5)
x
#2.2
y <- x[,-5]
y
#2.3
yinfo <- c(nrow(y), ncol(y))
yinfo
#2.4
y1 = matrix(0,nrow(y),ncol(y))
for(j in 1:ncol(y)){
  for(i in 1:nrow(y)){
         if(y[i,j] == -1){
             y1[i,j] <- 0
          } else{
               y1[i,j] <- y[i,j]
             }
       }
}
y1
#3
rowdata <- read.csv("C:/TEMP/rowdata.txt")
rdata <- rowdata
rdata

#3 .2
is.na(x)

#3.3
for(j in 1:4){
   if (is.na(rdata[j,2]) == F && is.na(rdata[j,3]) == F)
        {
         print(j)
       }
}

#3.4
rdata1 <-rdata[c(2,4),]
rdata1


#4
a1 <- -1:2
a2 <- 1:2
a1 + a2


a1 <- -(1:2)
a2 <- 1:2
a1 + a2


a1 <- matrix(0,2,2)
a2 <- c(3,4)
a1 + a2

a1 <- matrix(1:4,2,2)
a1[a1>2] = 0
a1

a1 <- 1:5
a1[-1] - a1[-length(a1)]


#R-program 1
a <-function(n){
  a1 = c()
  a1[1] <- 1
  a1[2] <- 3
  for(i in 1:n){
    a1[i+2] <- 0.9*a1[i+1]-0.1*a1[i] +1
  }
  return(a1[i])
}
a(20)

#2
for(i in 1:100){
    if(a(i) > 4){
         print(i)
         break
       }
}


#3
A <- matrix(runif(100), 50, 5)

if (class(A) != 'matrix') stop()
v = rep(0, nrow(A))
for (i in 1:nrow(A)) 
  {
  v[i] = sum(A[i, ])
  }

v


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

set.seed(1)
x = matrix(nrow=1000, ncol = 5)
for(i in 1:1000){
     x[i,] = sample(1:10,5)
   }
sid = c()
for(i in 1:1000){
  sid[i] <- sample(1:10, 1)
}
head(sid)


#6
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

#6-2
idist = matrix(0,1000,10)
for(j in 1:10){
  for(i in 1:1000){
    idist[i,j] <- sum(x[i,]*m.mat[j,])/(sqrt(sum(x[i,]*x[i,]))*sqrt(sum(m.mat[j,]*m.mat[j,])))
  }
}
idist  
#7
  ivec = c()
  for(i in 1:nrow(idist)){
    ivec[i] <- which.min(idist[i,])
  }
}
ivec
table(ivec)
#8
set.seed(1)
a = list()
for (i in 1:1000)
{
  x = rpois(1,4)+1
  x = min(x,10)
  a[[i]] = sample(1:10, x)
}


b = rep(0,10)
for(j in 1:10){
  for(i in 1:1000){
    if(length(a[[i]]) == j){
      b[j] <- b[j] + 1
    }
  }
}

b

#8.2

score = rep(0,10)
for(i in 1:length(a)){
  if(0<length(a[[i]]) && length(a[[i]])<4){
    c1 <- a[[i]][1]
    score[c1] <- score[c1] + 1
  } else if(4<= length(a[[i]])&& length(a[[i]])<7){
    c11 <- a[[i]][1]
    score[c11] <- score[c11] + 2
    c2 <- a[[i]][2]
    score[c2] <- score[c2] + 1
  } else {
    c111 <- a[[i]][1]
    score[c111] <- score[c111] +3
    c22 <- a[[i]][2]
    score[c22] <- score[c22] + 2
    c3 <- a[[i]][3]
    score[c3] <- score[c3] + 1
  }
}
score

#9
set.seed(1)
winpoint = rep(0,2)
m1= 10
m2 =5
for(j in 1:4){
  p <- rbinom(1,1,1/2)
  if(p == 0){
    m1 <- m1-1
    m2 <- m2+1
  }
  else {
    m1 <- m1 +1
    m2 <- m2 -1
  }
  if(m1 == 0 | m2 ==0){
    if(m1 > m2){
      winpoint[1] <- winpoint[1] +1
    } else {
      winpoint[2] <- winpoint[2] +1
    }
    break
    
  }
}
m1
m2

#9.2
set.seed(1)
winpoint = rep(0,2)
m1= 10
m2 =5
for(j in 1:10000){
     p <- rbinom(1,1,1/2)
     if(p == 0){
         m1 <- m1-1
         m2 <- m2+1
       }
     else {
         m1 <- m1 +1
         m2 <- m2 -1
       }
     if(m1 == 0 | m2 ==0){
         if(m1 > m2){
             winpoint[1] <- winpoint[1] +1
             print(j)
             print("Winner is A")
           } else {
             winpoint[2] <- winpoint[2] +1
               print(j)
               print("Winner is B")
             }
        stop()
         
         }
}


#9.3
winpoint = rep(0,2)
for(k in 1:200){
  set.seed(k)
  m1= 10
  m2 = 5
  for(j in 1:10000000){
    p <- rbinom(1,1,1/2)
    if(p == 0){
      m1 <- m1-1
      m2 <- m2+1
    }
    else {
      m1 <- m1 +1
      m2 <- m2 -1
    }
    if(m1 == 0 | m2 ==0){
      if(m1 > m2){
        winpoint[1] <- winpoint[1] +1
      } else {
        winpoint[2] <- winpoint[2] +1
      }
      break
      
    }
  }
}

winpoint

#9.4
winpoint = rep(0,2)
for(k in 1:200){
  set.seed(k)
  m1= 10
  m2 = 10
  for(j in 1:10000000){
    p <- rbinom(1,1,1/2)
    if(p == 0){
      m1 <- m1-1
      m2 <- m2+1
    }
    else {
      m1 <- m1 +1
      m2 <- m2 -1
    }
    if(m1 == 0 | m2 ==0){
      if(m1 > m2){
        winpoint[1] <- winpoint[1] +1
      } else {
        winpoint[2] <- winpoint[2] +1
      }
      break
      
    }
  }
}
winpoint

winpoint = rep(0,2)
for(k in 1:200){
  set.seed(k)
  m1= 10
  m2 = 15
  for(j in 1:10000000){
    p <- rbinom(1,1,1/2)
    if(p == 0){
      m1 <- m1-1
      m2 <- m2+1
    }
    else {
      m1 <- m1 +1
      m2 <- m2 -1
    }
    if(m1 == 0 | m2 ==0){
      if(m1 > m2){
        winpoint[1] <- winpoint[1] +1
      } else {
        winpoint[2] <- winpoint[2] +1
      }
      break
      
    }
  }
}
winpoint

winpoint = rep(0,2)
for(k in 1:200){
  set.seed(k)
  m1= 10
  m2 = 20
  for(j in 1:10000000){
    p <- rbinom(1,1,1/2)
    if(p == 0){
      m1 <- m1-1
      m2 <- m2+1
    }
    else {
      m1 <- m1 +1
      m2 <- m2 -1
    }
    if(m1 == 0 | m2 ==0){
      if(m1 > m2){
        winpoint[1] <- winpoint[1] +1
      } else {
        winpoint[2] <- winpoint[2] +1
      }
      break
      
    }
  }
}

winpoint

winpoint = rep(0,2)
for(k in 1:200){
  set.seed(k)
  m1= 10
  m2 = 25
  for(j in 1:10000000){
    p <- rbinom(1,1,1/2)
    if(p == 0){
      m1 <- m1-1
      m2 <- m2+1
    }
    else {
      m1 <- m1 +1
      m2 <- m2 -1
    }
    if(m1 == 0 | m2 ==0){
      if(m1 > m2){
        winpoint[1] <- winpoint[1] +1
      } else {
        winpoint[2] <- winpoint[2] +1
      }
      break
      
    }
  }
}

winpoint



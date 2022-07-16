

n = 12

t = 4

b = 3

X = c(1,1,0,0,0,1,0,0,
1,1,0,0,0,0,1,0,
1,1,0,0,0,0,0,1,
1,0,1,0,0,1,0,0,
1,0,1,0,0,0,1,0,
1,0,1,0,0,0,0,1,
1,0,0,1,0,1,0,0,
1,0,0,1,0,0,1,0,
1,0,0,1,0,0,0,1,
1,0,0,0,1,1,0,0,
1,0,0,0,1,0,1,0,
1,0,0,0,1,0,0,1)


X = matrix(X,12,8, T)
X

XTX = t(X)%*%X ; XTX

Y = datos$rendimiento

XTY = t(X)%*%Y; XTY

beta = c(mean(datos$rendimiento),
         (XTY*1/diag(XTX))[2:8]-mean(datos$rendimiento)
         )
beta

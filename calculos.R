
# Paquetes ####

library(readxl)

# Datos ####

# lectura de la base datos
datos <- read_excel("datos.xlsx")

# definición del factor del Experimento
datos$Aerosol = factor(datos$Aerosol) 

# replicas del exp
datos$Replica = factor(datos$Replica)

# Experimento ####

# Numero de experimentos
t = length(levels(datos$Aerosol))

# Numero de replicas
r = length(levels(datos$Replica))

# Tamaño de experimento
n = r*t

# Matriz  de diseño

mu = rep(1,n);mu

t1 = c(rep(1,r),rep(0,n-r));t1

t2 = c(rep(0,r),rep(1,r),rep(0,r));t2

t3 = c(rep(0,n-r),rep(1,r));t3

X = cbind(mu,t1,t2,t3)

# X'X

XTX = t(X)%*%X

# Y

Y = datos$Efectividad


# XTY

XTY = t(X) %*% Y

# Agregar nueva ecuación
XTX
XTX[1,2:(t+1)] <- 0

# Solución
beta <- solve(XTX, XTY)
beta

# Factor de correccion 
FC <- n*beta[1]^2

# R(beta) = beta'X'Y
R_beta <- t(beta) %*% XTY

# Sum of squares Treatments =  R(Tau|mu) = R_beta - R_mu
SStrt <- R_beta - FC
SStrt

# Sum of squares Total
SST <- t(Y)%*%Y - FC
SST


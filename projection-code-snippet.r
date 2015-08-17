dat =read.csv("210projrates.csv")
head(dat)
attach(dat)

leslie <- function(l,f) {
  L = matrix(0,10,10)
  L[1,] = .4886 * l[1]*(f[1:10]+f[2:11]*l[2:11]/l[1:10])/2  # top row
  diag(L[2:10,1:9]) = l[2:10] / l[1:9]  # subdiagonal
return(L)
}

TFR <- function(f) return(5 * sum(f))
GRR <- function(f, ffab=0.4886) return(5 * sum(f) * ffab)
NRR <- function(l,f,ffab=0.4886) return( ffab*sum(l*f))
mu <- function(l,f) {
   x=seq_along(l)*5 - 2.5
   return( sum(x*l*f)/sum(l*f)) }
   

CB = leslie(Canada.Lx,Brazil.Fx)
eigen.CB = eigen(CB)
r.CB = log(Re(eigen.CB$value[1]))/5
v = Re(eigen.CB$vectors[,1])
k.CB = v/sum(v)

CN = leslie(Canada.Lx,Niger.Fx); eigen.CN = eigen(CN)
KB = leslie(Kenya.Lx,Brazil.Fx); eigen.KB = eigen(KB)
KN = leslie(Kenya.Lx,Niger.Fx); eigen.KN = eigen(KN)

rm(list = ls())

zmien_x <- function(){
  x <<- 10
}
source("test_script.R")
zmien_x()
x <- 20
zmien_x()
typeof(x)

slowo1 <- "I"
slowo2 <- "like"
slowo3 <- "trains"
slowa <- c(slowo1, slowo2, slowo3)
paste(slowo1,slowo2,slowo3,sep=" ")
paste(slowa, collapse = " ")
?paste
w <- seq(-10, 10, 2)
rep(TRUE, 5)
rep(c(1,2,3), c(3,5, 10))
?rep

wyksztalcenie <- factor(c("podstawowe", "wyższe", "średnie", "średnie", "wyższe"))
wyksztalcenie
L <- list(inty = 1:10, x = 2.71, tekst = c("a", "b", "c"), log = rep(T, 5))

A <- matrix(1:8, 4, 2); A
A <- matrix(1:8, 4, 2, byrow = TRUE); A
?matrix
A <- array(1:27, dim = c(3,3,3)); A 
L
ramka <- data.frame(liczby = 5:1, logiczne = T); ramka
w <- c(1,2)
v <- c(3,4)
A <- matrix(1:4, 2, 2)
B <- matrix(4:1, 2, 2)
w; v; A; B
w %*% v
A %*% B
x <- 1:10
ifelse(x %% 3, "Nie dzieli się przez 3", "Dzieli się przez 3")
x %% 3
length(x)
sort(x, index = T)
x <- c(1, -2,4,5,-8)
order(x)
which(x >0)
x[x >0]
x[x == 1]
sum(x == 1)
which(x == 4)
dim(A)
?crossprod
tcrossprod(v, w)
1:10 %o% 1:10
w^3
M <- matrix(1:9, 3, 3)
M
M[c(1,2),]

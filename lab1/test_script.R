# Funkcja 
f <- function(x, y) {
  x <- 2*x
  y <<- 2*y
  return(y)
}

# Główna część skryptu

x <- 2
y <- 2

print(x)
print(y)
x

f(x,y)

cat("x =",x,"\n")
cat("y =",y,"\n")
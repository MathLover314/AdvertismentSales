polyRegr = function(X, Y, n) {
  A = matrix(0, nrow = n+1, ncol = n+1)
  for (i in 0:(n)) {
    for (j in 0:(n)) {
      A[n-i+1, n-j+1] = moment(X, i+j)
    }
  }
  b = integer(n)
  for (i in 0:(n)) {
    b[n-i+1]=moment(Y*X^(i), 1)
  }
  SVD = svd(A)
  SVD$v %*% diag(1/SVD$d) %*% t(SVD$u) %*% b
}

values = polyRegr(TV, Sales, 5)
x = seq(0, 300, 50)
m = length(values)
f = function(x, c) {
  y = 0
  for (i in 1:m) {
    y = y + c[i] * x^(m-i)
  }
  y
}

lines(x, f(x, values))

moment = function(X, k) {
  sum(X^k)
}

rsquared = function(Y) {
  m = mean(Y)
  TSS = sum((Y-m)^2)
  RSS = sum((Y-f(TV, values))^2)
  (TSS-RSS)/TSS
}
rsquared(Sales)

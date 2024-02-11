Adv = read.csv("Advertising.csv")
TV = unlist(Adv["TV"])
Sales = unlist(Adv["sales"])

a = cov(Sales, TV)/(sd(TV)^2)
b = mean(Sales) - a * mean(TV)

plot(TV, Sales)
x=seq(0, 300, 300)
y=a*x+b
lines(x, y)

TV_s = TV[order(TV)]
Sales_s = Sales[order(TV)]

err = rep(0, 200)
for (i in 1:200) {
  err[i] = Sales_s[i] - a*TV_s[i] - b
}
lines(x, y-sd(err))

TV_up = TV_s[which(err > 0)]
Sales_up = Sales_s[which(err > 0)]

TV_down = TV_s[which(err < 0)]
Sales_down = Sales_s[which(err < 0)]


regr = function(X, Y) {
  c = cov(X, Y)
  d = sd(X)^2
  mx = mean(X)
  my = mean(Y)
  c(c/d, my - c*mx/d)
}

regr_up = regr(TV_up, Sales_up)
regr_down = regr(TV_down, Sales_down)

plot(TV_up, Sales_up, col="red", xlim=c(0, 300), ylim=c(0, 30))
par(new=TRUE)
plot(TV_down, Sales_down, col="green",xlim=c(0, 300), ylim=c(0, 30), add=TRUE)
a = regr_up[1]
b = regr_up[2]
y = a*x+b
lines(x, y)

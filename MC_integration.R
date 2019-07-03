# Monte-Carlo integration
B <- 1000
a <- 0
b <- 2
inter <- 0

for (i in 1:B){
  inter <- inter + (runif(1, a, b))^2
}

(b-a)*inter/B
8/3


library(stat297)

mc_int(x_range = c(1,3), fun = "exp(sin(x))/x", B = 10^5)

my_fun = "x^2"
x = 0:3
eval(parse(text = my_fun))

# Normal Distribution
my_fun = "1/sqrt(3.125*pi)*exp(-((x - 4)^2)/3.125)"
(prob = mc_int(x_range = c(1, 4.5), fun = my_fun, B = 10^7))
# P(1 < X < 4.5) ~ 0.647
pnorm(4.5, 4, 1.25) - pnorm(1, 4, 1.25)

sqrt(prob$var)
# St. error ~ 0.0001

# Non-elementary integrals
B = 4^(4:13)
results = matrix(NA, length(B), 2)
for (i in 1:length(B)){
  mc_res = mc_int(c(0, 2), "sin(x^2)", B = B[i], seed = i+12)
  results[i, ] = c(mc_res$I, sqrt(mc_res$var))
}


trans_blue = hcl(h = seq(15, 375, length = 3), l = 65, c = 100, alpha = 0.15)[2]
plot(NA, xlim = range(B), ylim = range(cbind(results[, 1] + results[,2], 
                                             results[, 1] -results[,2])), log = "x", ylab = "Estimated Integral",
     xlab = "Number of Simulations B", xaxt = 'n')
grid()
axis(1, at = B, labels = parse(text = paste("4^", 4:13, sep = "")))
polygon(c(B, rev(B)), c(results[, 1] + results[, 2], 
                        rev(results[, 1] - results[, 2])), border = NA, col = trans_blue)
lines(B, results[, 1], type = "b", col = "blue4", pch = 16)
abline(h = 0.8048208, col = "red4", lty = 2)
legend("topright", c("Estimated value", "Standard error interval", "Good approximation (MatLab)"), bty = "n",
       pch = c(16, 15, NA), lwd = c(1, NA, 1), lty = c(1, NA, 2), 
       pt.cex = c(1, 2, NA), col = c("blue4", trans_blue, "red4"))

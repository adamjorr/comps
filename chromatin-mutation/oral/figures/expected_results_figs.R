
pdf('mut_rate.pdf')
x <- seq(0,20,by=1)
normal <- dpois(x,lambda = 2)
cancer <- dpois(x,lambda = 10)
plot(x,normal,t = 'l', lwd=3,
     xlab = "Number of mutations",
     ylab = "Density",
     main = "SNP mutations in normal tissue")
points(x,cancer,t = 'l', col = 'red', lwd=3)
legend('topright',legend = c("Closed chromatin","Open chromatin"), col = c('black','red'), lty=1)
dev.off()

pdf('chr_state.pdf')
x  <- seq(0,30,by=1)
y <- (dpois(x,lambda = 1.5) + dpois(x, lambda = 14))/2
z <- (dpois(x,lambda = 2.5) + dpois(x, lambda = 9))/2
plot(x,y, t = 'l', lwd = 3, xlab = "Number of ATAC Reads Overlapping Mutation", ylab = "Density", main = "Number of ATAC reads overlapping an SNP")
points(x,z,t = 'l', lwd=3, col = 'red')
legend('topright',legend = c("Normal sample","Cancer sample"), col = c('black','red'), lty=1)
dev.off()

pdf('logistic_regression.pdf')
# x <- rpois(500,10)
# y <- sample(c(TRUE,FALSE), size = 500, replace = TRUE, prob = c(.3,.7))

x <- c(1,1,1,2,3,7,7,8,9,9)
y <- c(T,F,F,F,T,F,F,F,F,F)

reg <- glm(y ~ x, family = binomial)
xpred <- seq(1,10,by = 0.01)
ypred <- predict(reg, list(x = xpred), type="response")
plot(xpred,ypred, ylim=c(0,1), t = 'l', lwd = 3, xlab = 'Relative enrichment of ATAC reads', ylab = 'Probability of mutation', main = "Logistic Regression of SNPs")
points(x,y)
dev.off()

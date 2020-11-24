cols <- rev(viridis::viridis(4))

beta <- cbind(pmcmc1$pars[,'beta'], pmcmc2$pars[,'beta'], pmcmc3$pars[,'beta'], pmcmc4$pars[,'beta'])
gamma <- cbind(pmcmc1$pars[,'gamma'], pmcmc2$pars[,'gamma'], pmcmc3$pars[,'gamma'], pmcmc4$pars[,'gamma'])

par(mar = c(3, 3, 2, 1), 
    mgp = c(2, 0.5, 0), oma = c(1, 1, 1, 1),
    fig=c(0,1,0.67,1))
matplot(beta, type = "l", lty = 1, 
        xlab = "Iteration", 
        ylab = "beta", col = cols)

par(fig=c(0,1,0.33,0.67), new = TRUE)
matplot(gamma, type = "l", lty = 1, 
        xlab = "Iteration", 
        ylab = "gamma", col = cols)

par(fig=c(0.4,0.9,0,0.33), new = TRUE)
hist(c(beta)/c(gamma), main = "", xlab = "R0")

par(fig=c(0,0.3,0,0.33), xpd=TRUE, new = TRUE)
plot.new()
#plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("left", cex = 0.75, fill = cols, bty = "n", legend = paste("chain", seq_len(4)))


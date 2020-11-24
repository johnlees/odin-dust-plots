matplot(seq_len(200), t(x[1, ,]), type = "l",
        xlab = "Time", ylab = "x",
        col =  rgb(red = 0.8, green = 0.8, blue = 0.8, alpha = 0.8), lty = 1, ylim = range(x))
matlines(seq_len(200), x[1,2,], col = "#000000", lty = 1)
matlines(seq_len(200), predict(loess(apply(x[1,,], MARGIN = 2, mean) ~ seq_len(200))), col = "#ff0000", lty = 1)

par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
matplot(incidence_data$day, t(history[2, , -1]), type = "l",
        xlab = "Time", ylab = "Number of individuals",
        col = rgb(0x99/256, 0x99/256, 0x66/256, alpha = 0.05), lty = 1, ylim = range(history))
matlines(incidence_data$day, t(history[3, , -1]), col =  rgb(0x8c/256, 0x8c/256, 0xd9/256, alpha = 0.05), lty = 1)
matlines(incidence_data$day, t(history[4, , -1]), col = rgb(0xcc/256, 0x00/256, 0x44/256, alpha = 0.05), lty = 1)
matpoints(incidence_data$day, t(true_history[2:4, , -1]), pch = 16,
          col = c(rgb(0x99/256, 0x99/256, 0x66/256, alpha = 0.8), rgb(0x8c/256, 0x8c/256, 0xd9/256, alpha = 0.8), rgb(0xcc/256, 0x00/256, 0x44/256, alpha = 0.8)))
legend("left", lwd = 1, col = c("#999966", "#8c8cd9", "#cc0044"), 
       legend = c("S", "I", "R"), bty = "n")

matplot(time*dt, t(mini_forecast[2, , -1]), type = "l",
        xlab = "Time", ylab = "Number of individuals",
        col = "#999966", lty = 1, ylim = range(mini_forecast))
matlines(time*dt, t(mini_forecast[3, , -1]), col = "#8c8cd9", lty = 1)
matlines(time*dt, t(mini_forecast[4, , -1]), col = "#cc0044", lty = 1)
matpoints(incidence_data$day[1:50], t(true_history[2:4, , 2:51]), pch = 19,
          col = c("#999966", "#8c8cd9", "#cc0044"))
matpoints(incidence_data$day[51:100], t(true_history[2:4, , 52:101]), pch = 4,
          col = c("#999966", "#8c8cd9", "#cc0044"))
legend("left", lwd = 1, col = c("#999966", "#8c8cd9", "#cc0044"), 
       legend = c("S", "I", "R"), bty = "n")


plot_particle_filter <- function(history, true_history, times, obs_end = NULL) {
  if (is.null(obs_end)) {
    obs_end = max(times)
  }
  
  par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
  cols <- c(S = "#999966", I = "#8c8cd9", R = "#cc0044")
  col_alpha <- c(rgb(0x99/256, 0x99/256, 0x66/256, alpha = 0.05), 
                       rgb(0x8c/256, 0x8c/256, 0xd9/256, alpha = 0.05), 
                       rgb(0xcc/256, 0x00/256, 0x44/256, alpha = 0.05))
  matplot(times, t(history[1, , -1]), type = "l",
          xlab = "Time", ylab = "Number of individuals",
          col = col_alpha[[1]], lty = 1, ylim = range(history))
  matlines(times, t(history[2, , -1]), col = col_alpha[[2]], lty = 1)
  matlines(times, t(history[3, , -1]), col = col_alpha[[3]], lty = 1)
  matpoints(times[1:100], t(true_history[1:3, , 2:101]), pch = 21,
            bg = cols, col = "#000000", lwd = 1)
  legend("left", lwd = 1, col = cols, legend = names(cols), bty = "n")
}

plot_particle_filter(filter$history(), true_history, seq_len(100))

incidence_modelled <- apply(filter$history()[4,,], 1, diff)
avg <- apply(incidence_modelled, 1, mean)

matplot(seq_len(100), incidence_modelled, type = "l",
        xlab = "Time", ylab = "Cases",
        col = rgb(0x99/256, 0x99/256, 0x99/256, alpha = 0.05), lty = 2,
        ylim = range(incidence_modelled))
matlines(seq_len(100), incidence$cases, lty = 1,
          col = "#000000", lwd = 1)
matpoints(seq_len(100), avg, pch = 20,
         col = "#000000")
legend("topright", lwd = 1, lty = c(2,1), col = c("#999999", "#000000"),
       legend = c("Modelled", "Observed"), bty = "n")

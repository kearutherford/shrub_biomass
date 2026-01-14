diag_plots_nl <- function(model, data, y, type) {

  data$yhat <- fitted(model)

  # standardized residuals
  if(type == "nls") {

    data$resid <- resid(model, type = 'pearson')

  } else if (type == "gnls") {

    data$resid <- resid(model, type = 'n')

  }

  # fitted line plot
  plot1 <- data %>%
    ggplot() +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey30") +
    geom_point(aes({{ y }}, yhat), size = 2) +
    labs(title = "Fitted Line Plot") +
    ylab("yhat") +
    xlab("y") +
    theme_bw() +
    theme(axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.text.y = element_text(size = 9),
          axis.text.x = element_text(size = 9),
          plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
          panel.grid.minor = element_blank())

  # residual plot
  plot2 <- data %>%
    ggplot() +
    geom_abline(slope = 0, intercept = 0, linetype = 2, color = "grey30") +
    geom_point(aes(yhat, resid), size = 2) +
    labs(title = "Residual Plot") +
    ylab("residual") +
    xlab("yhat") +
    theme_bw() +
    theme(axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.text.y = element_text(size = 9),
          axis.text.x = element_text(size = 9),
          plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
          panel.grid.minor = element_blank())

  # normality (Q-Q) plot
  plot3 <- data %>%
    ggplot(aes(sample = resid)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey30") +
    geom_qq(size = 2) +
    labs(title = "Normality Plot") +
    ylab("sample quantiles") +
    xlab("theoretical quantiles") +
    theme_bw() +
    theme(axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.text.y = element_text(size = 9),
          axis.text.x = element_text(size = 9),
          plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
          panel.grid.minor = element_blank())

  # residuals histogram
  plot4 <- data %>%
    ggplot() +
    geom_histogram(aes(resid), binwidth = 1, fill = "grey83", color = "black") +
    labs(title = "Residuals Distribution") +
    ylab("frequency") +
    xlab("residual") +
    theme_bw() +
    theme(axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.text.y = element_text(size = 9),
          axis.text.x = element_text(size = 9),
          plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
          panel.grid.minor = element_blank())

  return_plot <- plot1/plot2/plot3/plot4 + plot_layout(ncol = 2)
  return(return_plot)

}

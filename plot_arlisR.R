library(ggplot2)
library(tidyverse)

plot_arlis_sort <- function(inputRef){
  #inputRef <- "Arctic"
  arlis <- nrow(subset_by_refuge(df_arlis, inputRef))
  total <- nrow(subset_by_refuge(df_total, inputRef))
  other <- total - arlis
  
  group <- c("ARLIS", "Other")
  value <- c(arlis, other)
  percent <- c(paste(round(100*arlis/total, 0),"%", sep=""), paste(round(100*other/total, 0),"%", sep=""))
  percent

  df <- data.frame(group, value)

  df2 <- df %>%
    mutate(csum = rev(cumsum(rev(value))),
           pos = value/2 + lead(csum, 1),
           pos = if_else(is.na(pos), value/2, pos))

  pie <- ggplot(df, aes(x = "", y = value, fill = group)) +
    geom_col(color = "white") +
    geom_text(aes(label = percent), position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y") +
    guides(fill = guide_legend(title = "Group")) +
    scale_y_continuous(breaks = df2$pos, labels = paste(df$group, " (", df$value, ")", sep="")) +
    # geom_label_repel(data = df2, aes(y = pos, label = paste(group, " (", value, ")", sep="")),
    #                  size = 4.5, nudge_x = 1, show.legend = FALSE) +
    scale_fill_manual(values = c("#FFFFFF", "black")) +
    theme(axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 15, color = "white", face = "bold"),
          legend.position = "none", # Removes the legend
          plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          )
  
  # #Pie attempt 2
  # cat_var <- factor(c(rep("ARLIS", arlis), rep("Other", other)))
  # 
  # # Store the variable as data frame
  # cat <- data.frame(cat_var)
  # 
  # # Pie
  # pie2 <- PieChart(x = cat_var, y = NULL, hole = 0,
  #          fill = c("#FFFFFF", "#56B4E9"),
  #          labels_cex = 0.6)
  
  return(pie)
}

get_arlis_sort <- function(inputRef){
  df <- subset_by_refuge(df_arlis, inputRef)
  return(nrow(df))
}

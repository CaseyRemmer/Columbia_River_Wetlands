



simmr_out2 <- simmr_out_spring$output[[1]]$BUGSoutput$sims.list$p
simmr_out2 <- simmr_out_summer$output[[1]]$BUGSoutput$sims.list$p

colnames(simmr_out2) <- simmr_out_summer$input$source_names

# Now turn into a proper data frame
library(reshape2)
df <- reshape2::melt(simmr_out2)
colnames(df) <- c("Num", "Source", "Proportion")


# And create the plot
ggplot(df, aes_string(
  y = "Proportion",
  fill = "Source")) +
  geom_density(alpha = 0.5)+
  scale_fill_manual(values = c("red", "blue", "green"))+
  theme_bw() +
  coord_flip()



##attempting a loop
##for the first two line the season being used needs to be listed, the other lines run without edits
plot_list = list()
for (i in 1:32) {
  simmr_out2 <- simmr_out_Fall$output[[i]]$BUGSoutput$sims.list$p
  colnames(simmr_out2) <- simmr_out_Fall$input$source_names
  df <- reshape2::melt(simmr_out2)
  colnames(df) <- c("Num", "Source", "Proportion")
  p = # And create the plot
    ggplot(df, aes_string(
      y = "Proportion",
      fill = "Source")) +
    geom_density(alpha = 0.5)+
    scale_fill_manual(values = c("red", "blue", "green"))+
    theme_bw() +
    coord_flip()
  plot_list[[i]] = p
}


all_dens<-ggarrange(plotlist=plot_list,
          common.legend = TRUE,  legend = "top",
          ncol = 2, nrow = 2)
all_dens$`1` 

##also need to specify the coreect season here
all_dens %>% ggexport(filename = "all_dens_Fall.pdf")
all_dens %>% ggexport(filename = "all_dens_Fall.png")

ggexport(plotlist=all_dens, filename = "all_dens.pdf")

ggsave("all_dens_summer.pdf")
ggsave("all_dens.png")


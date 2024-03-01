# read in environmental data:
env_info <- read.csv("WorkingDirectory/zim/Env_info.csv", header=TRUE, row.names = 1)
env_info$Region <- as.factor(env_info$Region)

# library(dplyr)
# Load the ggTS master file:
source("WorkingDirectory/AE1812_CTD_Data/ggTS-master/ggTS_DK.R")
library("viridis")
# plot TS diagram:
TempSal_Diag<-ggTS(sal=env_info$salinity, pot.temp = env_info$pot_temp, col.par = env_info$Region, col.name="Region", reference.p = 0) +
  scale_fill_manual(values=c("#800000FF","#767676FF","#FFA319FF")) +
  guides(fill=guide_legend(title="Region")) +
  theme(text = element_text(size=22),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "none",
        plot.margin = margin(1,1.2,1,1, "cm"))

TempSal_Diag

ggsave("WorkingDirectory/AE1812_Nutrients/Figure_2a.pdf", plot = TempSal_Diag, device = "pdf", width = 14, height = 14, path = NULL, scale = 1, units = "cm", dpi = 1000)

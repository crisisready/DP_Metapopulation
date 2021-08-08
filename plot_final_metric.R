library(tidyverse)
library(viridis)
library(hrbrthemes)
library(gridExtra)

data_final<-read_rds(choose.files())

NMcol<-names(data_final[,-c(1,2,4,5)])


for (i in 2:length(NMcol)){
  assign(paste0("p", i), 
         data_final %>% 
           select(eps, mtrc = NMcol[i]) %>% 
           ggplot(aes(x = eps, y = mtrc, fill=eps)) +
           geom_boxplot()+
           ylab(NMcol[i])+
           theme_ipsum() 
  )
}
gridExtra::grid.arrange(p2, p3, p4, p5, nrow = 2, ncol = 2)
gridExtra::grid.arrange(p6, p7, p8, p9, nrow = 2, ncol = 2)
gridExtra::grid.arrange(p10, p11, p12, p13, nrow = 2, ncol = 2)
gridExtra::grid.arrange(p14, p15, p16, nrow = 2, ncol = 2)
library(tidyverse)

datar <- read_csv("RESULTS_V2_181209_Magnetic Media Survey - Some Calculations.csv") %>%
    filter(complete.cases(.)) 

pdatar <- datar %>% 
    gather(variable,value,-`Institution-CollectionKey`) %>%
    mutate(value=as.numeric(value)) 

pdatar %>%
    ggplot()+theme_bw()+
    geom_histogram()+
    scale_x_log10()+
    facet_wrap(~variable)+
    aes(x=value)

matrix_by_inst <- as.matrix(datar[,-c(1,2)])
rownames(matrix_by_inst) <- datar[,1][[1]]
cluster_inst <- hclust(dist(scale(matrix_by_inst))) 

matrix_by_media <- t(as.matrix(datar[,-c(1,2)]))
cluster_media <- hclust(dist(scale(matrix_by_media)))

g <- pdatar %>% filter(variable!="Total Items") %>%
    mutate(fvariable=factor(variable,levels=rownames(matrix_by_media)[cluster_media$order])) %>%
    mutate(finst=factor(`Institution-CollectionKey`,levels=rownames(matrix_by_inst)[cluster_inst$order])) %>%
    ggplot()+theme_bw()+
    aes(x=fvariable,y=finst,fill=log10(value))+
    geom_tile()+
    theme(axis.text.x=element_text(angle=90))+
    scale_fill_distiller(direction=1,na.value="white")
g
ggsave("hawtmap.png",g,width=9,height=7)

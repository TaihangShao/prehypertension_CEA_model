df<-read.csv("ceac_data.csv")
library("tidyverse")   # Mainly for plotting
library(ggsci)

ceac = df %>% gather(key = "Group", value = "Prob",-WTP) %>% mutate(Group = factor(Group))


ceac1<-ggplot(data = ceac)+geom_line(aes(x=WTP,y=Prob,colour=Group),size=1.5)+
        labs(x = "WTP", y = "Probability", title = "CEAC") +
        scale_color_lancet()+
        scale_x_continuous(breaks = c(seq(from=0,to=40000,by=10000)))+
        scale_y_continuous(breaks =c(seq(from=0,to=1,by=0.2)))+
        geom_hline(yintercept = c(seq(from=0,to=1,by=0.1)),size=1,colour="gray",alpha=0.3)+
        geom_vline(xintercept = 12728,size=1,linetype="dashed",colour="royalblue")+
        geom_vline(xintercept = 38184,size=1,linetype="dashed",colour="royalblue")+
        geom_text(aes(x= 16000, y = 0.8, label = "threshold = $12,728"),size=6)+
        geom_text(aes(x= 35000, y = 0.8, label = "threshold = $38,184"),size=6)+
        theme(axis.title.x= element_text(size=20,color="black"))+
        theme(axis.title.y= element_text(size=20,color="black"))+
        theme(panel.grid= element_blank())+
        theme(axis.line = element_line(colour = "black",size = 1))+
        theme(axis.text.x= element_text(size=15, color="black"))+
        theme(axis.text.y= element_text(size=15, color="black"))+
        theme(title = element_text(size=20, color="black"))+
        theme(panel.background = element_blank())+
        theme(panel.border=element_blank())
ceac1
ggsave("CEAC.png",plot = ceac1,width = 12,height = 8,dpi = 600)



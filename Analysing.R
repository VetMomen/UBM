dir.create('data sets')


target<-read_excel('./data sets/target_sept.xlsx',sheet = 2)


target<-data.table(target)


target[,.('total'=sum(`Net Sales`)),by=.(Customer)][,Customer:=factor(Customer,unique(Customer))][order(total)]%>%
        ggplot(aes(x = reorder(Customer,-total),y=total))+
        geom_col()+
        scale_x_discrete(label=abbreviate)+
        theme(axis.text.x = element_text(angle = 45))


target[,.(Q,Products,`Net Sales`)][,.('total'=sum(`Net Sales`)),keyby= .(Products)][order(total)]%>%
        ggplot(aes(x=reorder(Products,-total),y=total))+
        geom_col()+
        theme(axis.text.x = element_text(angle = 45))+
        scale_x_discrete(label=abbreviate)

target%>%ggplot(aes(x = Products,y = `Net Sales`))+
        geom_boxplot()+
        theme(axis.text.x = element_text(angle = 45))+
        scale_y_continuous(labels = comma,breaks = 20000*c(1:25))


setkey(target,Products)


act_prod<-target[c('Calfostonic  25kg','Cholipearl','Fixfin Dry','Lysigem 25 KG','NUTRICAB DRY  (LA)','RB51 Vaccine','SMARTAMINE M','TRI-APHTHOVAC 50 ml')]


act_prod%>%ggplot(aes(x = Products,y = `Net Sales`))+
        geom_boxplot(color='blue',fill='brown')+
        theme(axis.text.x = element_text(angle = 45))+
        scale_y_continuous(labels = comma,limits = c(-10000,500000))+
        facet_wrap(.~Q)

act_prod%>%ggplot(aes(x = Products,y = `Net Sales`))+
        geom_boxplot(color='blue',fill='brown')+
        theme(axis.text.x = element_text(angle = 45))+
        scale_y_continuous(labels = comma,limits = c(-10000,500000))+
        facet_grid(.~Customer)

act_prod%>%ggplot(aes(x = reorder(Customer,-`Net Sales`),y=`Net Sales`,fill=Customer))+
        geom_boxplot()+
        theme(axis.text.x = element_text(angle = 45))+
        scale_y_continuous(labels = comma,limits = c(-10000,500000))

setkey(act_prod,Customer)

only_act<-act_prod[c('Copenhagen','Dina','Farm Stars','Reta Keriasy',"Enma'a",'Delta Misr','Al Safa','Lamar','Anba Beshoy','Al- Pramos')]

only_act%>%ggplot(aes(x = reorder(Customer,-`Net Sales`),y=`Net Sales`,size=`Net Sales`))+
        geom_point(color='darkgreen')+
        theme(axis.text.x = element_text(angle = 45))+
        scale_y_continuous(labels = comma,limits = c(-10000,500000))+
        facet_grid(Products~Q)
only_act%>%ggplot(aes(x = Customer,y=Products,color=Q,size=`Net Sales`))+
        geom_point()+
        theme(axis.text.x = element_text(angle=45))

only_act%>%ggplot(aes(x = Date,y = `Net Sales`))+
        geom_smooth()+
        facet_wrap(.~Products)
#################################################################

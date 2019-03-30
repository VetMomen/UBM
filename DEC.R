# importing the file
total<-read_excel(path = './data sets/finalTarget Com till 30-11.xlsx',2)
#making object contain the name of variables
vars<-names(total)
#making object contains uniques of ech variable
uniques<-apply(total,2,unique)
# knowing the total sale YTD
total%>%summarize(sum(`Net Sales`))
# distribution of total sales on each product 

total%>%group_by(Products)%>%summarize(sale=sum(`Net Sales`))%>%arrange(sale)%>%View

# adding the percent share of each product
share_product<-total%>%group_by(Products)%>%summarize(sale=sum(`Net Sales`),
                                       share=(sum(`Net Sales`)/sum(total$`Net Sales`))*100)
#graphing it 

total%>%group_by(Products)%>%summarize(sale=sum(`Net Sales`),
                                       share=(sum(`Net Sales`)/sum(total$`Net Sales`))*100)%>%ggplot(aes(x = reorder(Products,sale),y=sale))+
        geom_col()+geom_label(data = share_product,aes(label=sale),size=3)+
        theme(axis.text.x = element_text(angle = 45))


# knowing the distribution of EAch product 

share_product%>%ggplot(aes(x = '',y=sale))+
        geom_boxplot()+
        scale_y_continuous(limits = c(0,1000000))

total%>%ggplot(aes(x = '',y=`Net Sales`))+
        geom_boxplot()+scale_y_continuous(labels = comma)+
        theme(axis.text.x = element_text(angle = 45))

total%>%ggplot(aes(`Net Sales`))+
        geom_histogram()+
        facet_wrap(.~Products)

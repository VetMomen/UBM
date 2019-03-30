data<-read_excel('./data sets/finalTarget Com till 30-11.xlsx',sheet = 2)
target2018<-12133020
target2018T<-10295020	

uniques<-apply(data,2,unique)
vars<-names(data)

str(data)

#converting time to date 

data<-data%>%mutate(Date=as.Date(Date))

str(data)

data%>%ggplot(aes(x = Date,y=`Net Sales`))+
        geom_smooth()+
        scale_y_continuous(limits = c(0,400000))+
        facet_wrap(.~Products)

#summary of All Products


prod_cust_x<-xtabs(data$`Net Sales`~data$Customer+data$Products)
prod_cust_x<-data.frame(prod_cust_x)
prod_cust_x<-spread(data = prod_cust_x,key = data.Products,value = Freq)
writeWorksheetToFile(file = './data sets/products.xlsx',data = prod_cust_x,sheet = 'prod_cust_x')


prod_cust_x2<-xtabs(data$Qty~data$Customer+data$Products)
prod_cust_x2<-data.frame(prod_cust_x2)
prod_cust_x2<-spread(data = prod_cust_x2,key = data.Products,value = Freq)
writeWorksheetToFile(file = './data sets/products.xlsx',data = prod_cust_x2,sheet = 'prod_cust_x2')


percent_X<-data%>%group_by(Products,Customer)%>%summarize(share=(sum(`Net Sales`)/target2018)*100)
percent_X<-percent_X%>%spread(key = Products,value = share)


Prod_summary<-data%>%group_by(Products)%>%summarize(qty=sum(Qty),value=sum(`Net Sales`))
writeWorksheetToFile(file = './data sets/products.xlsx',data = Prod_summary,sheet = 'products summary')


#analysing Each product
#TriAphthoVac

TRA<-data%>%filter(Products=="TRI-APHTHOVAC 50 ml"|Products=="TRI-APHTHOVAC 100 ml")

TRA%>%summarize(sum(`Net Sales`),sum(Qty))

writeWorksheetToFile(file = './data sets/products.xlsx',data = TRA,sheet = 'TRA')
##########################################################

rb51<-data%>%filter(Products=="RB51 Vaccine")
writeWorksheetToFile(file = './data sets/products.xlsx',data = rb51,sheet = 'rb51')

rb51%>%summarize(sum(Qty),sum(`Net Sales`))
##########################################################

nutricap<-data%>%filter(Products=="NUTRICAB DRY  (LA)")
writeWorksheetToFile(file = './data sets/products.xlsx',data = nutricap,sheet = 'nutricab')

nutricap%>%summarize(sum(Qty),sum(`Net Sales`))

##########################################################
#importing prodXCustomer2019

prodXCustomer2019<-read_excel(path = './data sets/Target summary.xlsx',2)
prodXCustomer2019<-prodXCustomer2019[-c(43:46),]
varss<-names(prodXCustomer2019)


prodXCustomer2019<-prodXCustomer2019%>%gather(key = Products,value = Qty,-data.Customer)

prodXCustomer2019<-na.omit(prodXCustomer2019)

for(i in 1:length(prodXCustomer2019$Qty)){
        if(prodXCustomer2019$Qty[i]==0){
                prodXCustomer2019$Qty[i]<-NA
        }else{prodXCustomer2019$Qty[i]}
}

prodXCustomer2019<-na.omit(prodXCustomer2019)


prodXCustomer2019%>%filter(Products=="NUTRICAB DRY  (LA)")%>%spread(key = Products,value = Qty)
prodXCustomer2019%>%filter(Products=='Axcelera C')%>%spread(key = Products,value = Qty)
prodXCustomer2019%>%filter(Products=='Cholipearl')%>%spread(key = Products,value = Qty)

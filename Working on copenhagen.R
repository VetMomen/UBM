cop<-target%>%group_by(Customer)%>%filter(Customer=='Copenhagen')%>%arrange(Date)
final<-cop%>%group_by(Products)%>%summarize(Q=sum(Qty),t=sum(`Net Sales`))%>%mutate('single_price'=t/Q,
                                                                             'single_price_u'=single_price/25)
final<-data.frame(final)
bb<-loadWorkbook('./data sets/Copen.xlsx',create = T)
createSheet(object = bb,name = 'cop')
writeWorksheet(object = bb,data = final,sheet = 'cop')
saveWorkbook(object = bb,file ='./data sets/Copen.xlsx' )

bb<-loadWorkbook('./data sets/Copen.xlsx',create = T)
createSheet(object = bb,name = 'Total Transaction')
writeWorksheet(object = bb,data = cop,sheet = 'Total Transaction')
saveWorkbook(object = bb,file ='./data sets/Copen.xlsx' )

date<-Sys.time()

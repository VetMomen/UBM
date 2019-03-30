dir.create('./plots')

fmd<-read_excel(path = './data sets/FMD form (Responses).xlsx',col_types = c('guess','guess','guess','guess','guess','numeric','numeric','guess','guess','guess','guess','guess','guess'))

str(fmd)

uniqus<-apply(fmd,2,unique)
#frequancy of Each type and location
xtab<-with(fmd,
     table(`Production Type`,Location))
par(las=1)
plot(xtab,main='distribution of production type and location')

#total cap in Eche loc
cap_loc<-fmd%>%group_by(Location)%>%summarize(Cap=sum(Cap.))
cap_loc%>%ggplot(aes(Location,Cap))+
        geom_col(aes(fill=Location),width = .7)+
        geom_text(data = cap_loc,aes(label=Cap),vjust=.003,color='darkseagreen2')+
        ylab('Capacity')+
        ggtitle('Capacity of each area')+
        theme(panel.background = element_rect(fill = 'black'),panel.grid.minor = element_line(colour = 'black'))

#total cap of each type

cap_type<-fmd%>%group_by(`Production Type`)%>%summarize(Cap=sum(Cap.))
cap_type%>%ggplot(aes(`Production Type`,Cap,fill=`Production Type`))+
        geom_col(width = .7)+
        geom_text(data = cap_type,aes(label=Cap),vjust=.003,color='darkseagreen2')+
        ylab('Capacity')+
        ggtitle('Capacity of each type')+
        theme(panel.background = element_rect(fill = 'black'),panel.grid.minor = element_line(colour = 'black'))

#mixing area with type 

cap_type<-fmd%>%group_by(`Production Type`,Location)%>%summarize(Cap=sum(Cap.))
cap_type%>%ggplot(aes(`Production Type`,Cap,fill=`Production Type`))+
        geom_col(width = .7)+
        geom_text(data = cap_type,aes(label=Cap),vjust=.003,color='darkseagreen2')+
        ylab('Capacity')+
        ggtitle('Capacity of each type in each area')+
        theme(panel.background = element_rect(fill = 'black'),panel.grid.minor = element_line(colour = 'black'))+
        facet_wrap(.~Location)

#adding factor of infection 

cap_type<-fmd%>%group_by(`Production Type`,Location,infected)%>%summarize(Cap=sum(Cap.))
cap_type%>%ggplot(aes(`Production Type`,Cap,fill=infected))+
        geom_col(width = .7)+
        geom_text(data = cap_type,aes(label=Cap),vjust=.003,color='darkseagreen2')+
        ylab('Capacity')+
        ggtitle('Capacity of each type in each area illustrating infected herd')+
        theme(panel.background = element_rect(fill = 'black'),panel.grid.minor = element_line(colour = 'black'))+
        facet_grid(.~Location)

#adding vaccine type 
cap_type<-fmd%>%group_by(`Production Type`,Location,infected,`vacc. Type`)%>%summarize(Cap=sum(Cap.))
cap_type%>%ggplot(aes(`Production Type`,Cap,fill=infected))+
        geom_col(width = .7)+
        geom_text(data = cap_type,aes(label=Cap),vjust=.003,color='darkseagreen2')+
        ylab('Capacity')+
        ggtitle('Capacity of each area illustrating infected herd & vaccine type')+
        theme(panel.background = element_rect(fill = 'black'),panel.grid.minor = element_line(colour = 'black'))+
        facet_grid(`vacc. Type`~Location)

#farm location
color<-colorFactor(palette =c('blue','red') ,domain = fmd$infected)

fmd%>%leaflet()%>%
        addProviderTiles(providers$OpenStreetMap.BlackAndWhite)%>%addCircleMarkers(lat = fmd$lat,
                                lng = fmd$lon,color = ~color(fmd$infected),
                                radius = fmd$Cap./1000)%>%
        addLegend(position = 'topright',pal = color,values = ~factor(fmd$infected),title = 'Infection')

perc<-fmd%>%group_by(`vacc. Type`)%>%summarize(percent=(sum(Cap.)/sum(fmd$Cap.))*100)

perc$percent<-round(perc$percent,1)

fmd%>%group_by(`vacc. Type`)%>%summarize(total=sum(Cap.),
                                         percent=round((sum(Cap.)/sum(fmd$Cap.))*100,1))%>%
        ggplot(aes(x='',y=total,fill=`vacc. Type`))+
        geom_col(width = .3)+
        coord_polar(theta = 'y',start = 0,direction = 1,clip = 'on')+
        theme(axis.title = element_text(face = 'bold'),axis.line = element_blank(),
              panel.background = element_blank(),
              axis.text = element_blank(),panel.grid = element_blank(),
              axis.title.y.left = element_blank(),
              axis.title.x.bottom = element_blank())+
        geom_text(aes(label=percent),nudge_x = .2,hjust=.5)+
        scale_fill_brewer(type = qualitative,palette = 'Dark2')+
        labs(title = 'Vaccination type share')+
        theme(plot.title  = element_text(hjust = .5))


gpx<-read_html(x = './data sets/locations.gpx')
Names<-gpx%>%html_nodes(xpath = '//wpt/name')%>%html_text()

loc<-gpx%>%html_nodes(xpath = '//wpt')%>%html_attrs()%>%data.frame()%>%t()
rownames(loc)<-NULL

Locations<-data.frame(Names,loc)
Sys.setlocale('LC_ALL','Arabic')

writeWorksheetToFile(file = './data sets/Locations.xlsx',data = Locations,sheet = 'locations')

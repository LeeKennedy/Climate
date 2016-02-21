#daily code = 136
#monthy code = 139

bomdata<- function(station,code){
        for(i in 1: length(station)){
                p.url<-paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_stn_num=",station[i],"&p_display_type=availableYears&p_nccObsCode=",code,sep ="")
                download.file(p.url,"test.txt")
                filelist <- list.files(pattern = ".txt")
                foo<- file(filelist,"r")
                text<- suppressWarnings(readLines(foo))
                close(foo)
                l<- regexpr(":",text[1])
                m<- unlist(gregexpr(",", text[1], perl = TRUE))
                pc<- substr(text[1],l[[1]]+1,l[[1]]+(m[2]-(l[[1]]+1)))
                url<-paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=",station[i],"&p_c=",pc,"&p_nccObsCode=",code,"&p_startYear=2013", sep ="")
                suppressWarnings(download.file(url,paste(station[i],".zip",sep= ""), mode = "wb"))
                unlink("test.txt")
        }
}


# Charlton---------
bomdata(080128,136)

# Kerang--------
bomdata(080023,136)

# Melbourne--------
bomdata(086071,136)

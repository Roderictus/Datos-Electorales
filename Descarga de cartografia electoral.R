#Descarga de la cartografia electoral 
#http://cartografia.ife.org.mx/sige7/?distritacion=federal
library(downloader)
library(stringr)
  
URL <- "http://cartografia.ife.org.mx//descargas/distritacion2017/federal/"

for (i in 13:32) {
  num<-str_pad(i,2, "left", "0")
  destfile <- str_c("./Cartografía electoral zipeada/", i)
  URL2 <- str_c(URL, num, "/", num, ".zip")
  print(URL2)
  download(url = URL2, destfile)
  unzip(zipfile = str_c("./Cartografía electoral zipeada/", i),exdir = "Cartografía electoral")
  #print(str_c("./Cartografía electoral/", i))
  #file.remove(str_c("./Cartografía electoral/", i))
}

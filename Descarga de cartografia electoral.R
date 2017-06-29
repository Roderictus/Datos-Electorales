#Descarga de la cartografia electoral 
library(downloader)

URL <- "http://cartografia.ife.org.mx//descargas/distritacion2017/federal/"

http://cartografia.ife.org.mx//descargas/distritacion2017/federal/01/01.zip)


for (i in 11:32) {
  num<-str_pad(i,2, "left", "0")
  download(str_c(URL, num, "/" , num, ".zip"), 
           str_c( i), mode = "wb")
  unzip(str_c("Cartografía electoral/", i))
  file.remove(str_c(, i))
}




unzip(zipfile = "Cartografía electoral/10", exdir = "Cartografía electoral")
unzip(zipfile = "Cartografía electoral/11", exdir = "Cartografía electoral")


http://cartografia.ife.org.mx//descargas/distritacion2017/federal/10/10.zip


Cartografía electoral 

download(str_c(URL, str_pad(i, 2, "left", "0"), "_csv.zip&ht=02"),
         str_c("data/", i), mode = "wb")
unzip(str_c("data/", i), exdir = "data")
file.remove(str_c("data/", i))


http://cartografia.ife.org.mx//descargas/distritacion2017/federal/01/01.zip
http://cartografia.ife.org.mx//descargas/distritacion2017/federal/02/02.zip
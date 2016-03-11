##
##
##         R Selenium   
##
##

## ulrs:
## http://rpubs.com/johndharrison/12843
## 
## Additional data:
## http://blog.datacamp.com/scraping-javascript-generated-data-with-r/




if (!require(Rtools)) install.packages('Rtools')
if (!require(devtools)) install.packages('devtools')
if (!require(httr)) install.packages('httr')
if (!require(grid)) install.packages('grid')
if (!require(png)) install.packages('png')
if (!require(RCurl)) install.packages('RCurl')
if (!require(RSelenium)) install.packages('RSelenium')
if (!require(RSelenium)) devtools::install_github("ropensci/RSelenium")


library(devtools)
library(httr)
library(xlsx)
require(RSelenium)
library(grid)
library(png)


options(internet.info = 0)
set_config(config(ssl.verifypeer = 0L))
mywd <- 'C:/Users/Ivan.Petrov/Documents/GitHub/G_Y_parser'
setwd (mywd)
getwd()

##   --- Installing Rselenium

#install Selenium browser





# Configuring settings for PhantomJS
#phantomjsdir <- paste(mywd, "/phantomjs-2.0.0-windows/bin/phantomjs.exe", sep="" )
pjsextr <- "--proxy=mskwebp01101:8080  --proxy-auth=Ivan.Petrov@mecglobal.com:London114q --remote-debugger-port=9000"
phantomjsUserAgent <- "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.89 Safari/537.36 OPR/28.0.1750.48"
#eCap <- list(phantomjs.binary.path = phantomjsdir, phantomjs.page.settings.userAgent = phantomjsUserAgent  )


# starting phantom server driver
#pJS <- phantom(pjs_cmd = phantomjsdir, extras = pjsextr)

RSelenium::startServer(invisible = FALSE, log = TRUE)
Sys.sleep(3)
#remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = eCap)
#remDr$open()
#remDr$value$message # in case of errors run this line for debugging

firefoxbinarypath = list("firefox.binary" = "'C:/Program Files (x86)/Mozilla Firefox/firefox.exe'")
remDr <- remoteDriver(browserName = "firefox", extraCapabilities = firefoxbinarypath)
remDr$open()


as.character.factor <- function(x) {as.character(levels(x))[x]}
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

##
## Loading Words and Regions
##

dt <- read.xlsx2("Mazda_data.xlsx", "Requests", colClasses = c('character'))
dt[,1] <- as.character.factor(dt[,1])
mywords <- c(dt[,1])


dt <- read.xlsx2("Mazda_data.xlsx", "Regions", colClasses = c('character'))
dt <- dt[,1:2]
dt[,1] <- as.character.factor(dt[,1])
dt[,2] <- as.numeric.factor(dt[,2])
dt_regions <- dt[!is.na(dt[,2]),]
regions <- c(dt_regions[,2])




timeout <- 0.2

datacollected <- data.frame(query=character(),
                            SE=character(),
                            Region = character(),
                            Screenshot.full=character(), 
                            Screenshot.snippet=character(), 
                            Position.type=character(),  # Ad or Organic
                            Position.count=character(),
                            Short.link=character(),
                            Full.Link=character(), 
                            Snippet.title=character(),
                            Snippet.text=character(), 
                            Snippet.source=character(), 
                            Issue=character(),
                            Recommendations=character(),
                            stringsAsFactors=FALSE) 



#' create folders for screenshots
#' 
folderss <-  paste0(mywd,"/screenshots/")
dir.create(folderss, showWarnings = F, recursive = FALSE, mode = "0777")
foldersspos <-  paste0(mywd,"/screenshots.pos/")
dir.create(foldersspos, showWarnings = F, recursive = FALSE, mode = "0777")


for (region_id in regions){
    
    for (word in mywords) {
    
      ss.word <- word
      
      region_id <- 
      ss.se <- "google"
      
      
      region_text <- dt_regions[dt_regions[,2]==region_id,1]
      remDr$navigate("http://google.com")
      
      #' remDr$screenshot(display = TRUE)
      #' 
      webElem <- remDr$findElement(using = "class", "gsfi")
      webElem$sendKeysToElement(list(enc2utf8(ss.word),key = "enter"))
      ## webElem$sendKeysToElement(list(enc2utf8(ss.word)))
      Sys.sleep(0.5)
    
      
      #' wait till document is loaded
      #' 
      totalwait <- 0
      Sys.sleep(timeout)
      totalwait <- totalwait + timeout
      while (remDr$executeScript("return document.readyState;")!= "complete" && totalwait<10) {
        Sys.sleep(timeout)
        totalwait <- totalwait + timeout
        print (remDr$executeScript("return document.readyState;"))
      } 
      
      print(paste0("page loaded: ",ss.word))
    
      ss.screenshotfull.file <- paste0(folderss,ss.word,".png")
      remDr$screenshot(display = F, file = ss.screenshotfull.file)
      
      print(paste0("screenshot saved: ",ss.word))
      
      img <- readPNG(ss.screenshotfull.file)
      
      print(paste0("image loaded: ",ss.word))
      
      #' remDr$screenshot(display = T)
    
      ## pagecode <- remDr$getPageSource(header = F)
      ## frontPage <- remDr$getPageSource()
      ## print(paste0("page source loaded: ",ss.word))
      
      #' reading all organic search elements from page
      elem.snippet <- remDr$findElements(using="class name",value = "rc")
      
      for (i in 1:length(elem.snippet)) {
      
        
        print(paste0("element opened: ",ss.word,"  pos",i))
        
        ss.pos <- i
        ss.snippet.code  <- elem.snippet[[i]]$getElementAttribute('innerHTML')
          
        #' reading title & link
        elemtitle <- elem.snippet[[i]]$findChildElement(using = "class name", value = "r")
        
        print(paste0("element title ok"))
        
        
        elemcode <- elemtitle$getElementAttribute('innerHTML')
        
        print(paste0("element innerHTML ok"))
        
        m <- regexec ('href=/"(.*?)/"', elemcode[[1]])
        ss.full.link <- regmatches(elemcode[[1]], m)[[1]][2]
        m <- regexec ('://(.*?)/', ss.full.link)
        ss.short.link <- regmatches(ss.full.link, m)[[1]][2]
        
        elemtext <- elem.snippet[[i]]$findChildElement(using = "class name", value = "st")
        ss.text <- elemtext$getElementText()[[1]]
        
       
        print(paste0("element loaded: ",ss.word,"  pos",i))
        
        ss.screenshotpos.file <- paste0(foldersspos,ss.word," pos_",i,".png")
        
        elemloc <- elem.snippet[[i]]$getElementLocation()
        elemsize <- elem.snippet[[i]]$getElementSize()
        imgshort <- img[elemloc$y:(elemloc$y+elemsize$height),elemloc$x:(elemloc$x+elemsize$width),]
        writePNG(imgshort, target=ss.screenshotpos.file)
        rm (imgshort)
        
        print(paste0("image loaded"))
        
        #' print(c(ss.word, ss.se,ss.screenshotfull.file, ss.screenshotpos.file, ss.pos,ss.full.link, ss.short.link, ss.text,  ss.snippet.code))
        #' 
        newrow <- c(ss.word, ss.se,ss.screenshotfull.file, ss.screenshotpos.file, ss.pos, ss.short.link, ss.full.link, ss.text,  ss.snippet.code,"","")
        
        datacollected[nrow(datacollected)+1,] <- newrow
    
        
      }
    
      rm(img)
      remDr$deleteAllCookies()
      Sys.sleep(2)
    }
  
}

remDr$close()
tools::pskill(4444)
pJS$stop()



#' Saving stored data to Excel
library(XLConnect)

#' Delete exisitng xlsx file
xlsxfiledata <- "mydata2.xlsx"
unlink(xlsxfiledata, recursive = FALSE, force = FALSE)

wb <- XLConnect::loadWorkbook(xlsxfiledata, create=TRUE)
if (!existsSheet(wb,"Sheet1")) createSheet(wb, "Sheet1")

clearSheet (wb,  "Sheet1")

writeWorksheet(wb, datacollected,"Sheet1", startRow = 1, startCol = 1, header = TRUE)

#' collect the data
#' 
for (i in 1:ncol(datacollected)) setColumnWidth (wb,"Sheet1",i,9000)
for (i in c(1,2,5)) setColumnWidth (wb,"Sheet1",i,3000)
for (i in 2:(1+nrow(datacollected))) setRowHeight (wb,"Sheet1",i,50)


#' insert hyperlinks
#' 
for (i in 2:(nrow(datacollected)+1)) setCellFormula(wb,"Sheet1",i,3,paste0('HYPERLINK("',datacollected[i-1,3],'","link")'))
for (i in 2:(nrow(datacollected)+1)) setCellFormula(wb,"Sheet1",i,4,paste0('HYPERLINK("',datacollected[i-1,4],'","link")'))

for (i in 1:(nrow(datacollected))){
createName(wb, name = paste0("graph",i),
            formula = paste("Sheet1", idx2cref(c(i+1, 4)), sep = "!"), overwrite = TRUE)

addImage(wb, filename = datacollected[i,4], name = paste0("graph",i),
          originalSize = FALSE)
}


saveWorkbook(wb)
rm(wb)





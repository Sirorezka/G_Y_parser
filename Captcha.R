captcha <- function(captchaUrl, key) {

# processing captcha
  captchaurl<- captchaUrl
download.file(captchaurl[[1]], "captcha.gif",mode = 'wb',quiet=T)
zz <- file("captcha.gif", "rb", raw=T)

filecaptcha<-URLencode (base64encode(zz))


setheaders <- add_headers(user_agent="Mozilla/5.0 (Windows NT 6.1; WOW64; rv:37.0) Gecko/20100101 Firefox/37.0",
                          Accept_Language="ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3",
                          Host = "antigate.com",  
                          accept= "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8", 
                          Accept_Encoding = "gzip, deflate", 
                          ContentType = "text/plain; charset=UTF-8")


r <- POST("http://antigate.com/in.php", body=list(method = "base64", key=key, body=filecaptcha)
          , verbose(), options(ssl.verifypeer = FALSE), encode =c("form"))

ID<-substr(content(r, as="text"),4,25)
# 
# html2 <- GET(paste0("http://anti-captcha.com/res.php?key=",key,"&action=get&id=",ID))
# htmlCode2 <- content (html2, as="text") 

repeat{
  html2 <- GET(paste0("http://anti-captcha.com/res.php?key=",key,"&action=get&id=",ID))
  htmlCode2 <- content (html2, as="text") 
  if(!htmlCode2=="CAPCHA_NOT_READY"){    
    break
  }
}


captchakey <<- substr(htmlCode2,4,25)
}
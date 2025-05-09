# 添加R session启动赞美语
# 以下内容追加至R_Home\etc\Rprofile.site
.First <- function(){
  library(praise)
  cat(praise("${EXCLAMATION}! ${EXCLAMATION}! Haobin,you have done this ${adverb_manner}!"),"\n",praise(),"\n",praise(),"\n",date(),"\n")
}
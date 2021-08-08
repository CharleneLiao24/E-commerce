library(rvest)   # 爬靜態網頁用
library(magrittr)
library(jsonlite)
library(RSelenium)  # 爬動態網頁用
library(selectr)
library(stringr)
str_replace_all(sample_list$smp_prd_price,"[$,]","")

# 原寫法
url_num1 = list("0011", "0021", "0023", "0019", "0008", "0005", "0022")
url_num2 = list("0009","0018", "0003", "0020", "0024")
url_num3 = list("0012", "0002", "0014", "0015", "0001")
url_num4 = list("0006", "0010", "0004", "0017", "0013", "0007")

pages = c(url_num1, url_num2, url_num3, url_num4)
result_urls = paste("https://www.ruten.com.tw/category/main?", pages, sep = "")


# ==========================================決定抽樣方法==========================================

require(xml2)
#----- 計時開始 -----#
ptm <- proc.time()
name_b = c("電子、電器設備", "男性、運動、居家用品", "女性、嬰幼兒用品", "娛樂相關用品")
scat = data.frame()
count_num = data.frame()


for (k in (1:4)){
  if (k == 1){pages = url_num1}
  else if (k == 2){pages = url_num2}
  else if (k == 3){pages = url_num3}
  else if (k == 4){pages = url_num4}
  result_urls = paste("https://www.ruten.com.tw/category/main?", pages, sep = "")
  
  for (j in 1:length(pages)){
    html_page = read_html(url(result_urls[j]))
    xpath_m = "//*[@class='rt-breadcrumb-static']" # 根據上圖的資訊，寫一個xpath
    xpath_surl = "//*[@class='rt-subcategory-item']/a"  # 找小類別網址
    xpath_s = "//*[@class='rt-subcategory-item']/a/h3"  # 找小類別名稱
    xpath_s1 = "//*[@class='rt-text-small rt-text-notice']"  # 找小類別商品數
    
    name_m = xml_text(xml_find_all(html_page, xpath_m))
    scat_url = unlist(xml_attr(xml_find_all(html_page, xpath_surl), "href"))
    name_s = xml_text(xml_find_all(html_page, xpath_s))
    number = xml_text(xml_find_all(html_page, xpath_s1))
    number1 = vector("numeric")  # 為解決number為character的問題
    for (i in 1:length(number)){
      number1[i] = as.numeric(substr(number[i], start = 2, stop = lengths(strsplit(number[i],""))-1))
    }
    scat = rbind(scat, cbind(name_b = name_b[k], name_m, name_s, number1, scat_url))  # 但此number1依舊為character
    count_num = rbind(count_num, cbind(name_m, num = as.numeric(sum(number1))))
  }
}
#----- 計時結束 -----#
proc.time() - ptm


# 另外增加成人專區資料
k = 4
j = 1

xpath_m = "//*[@class='rt-breadcrumb-static']" # 根據上圖的資訊，寫一個xpath
xpath_surl = "//*[@class='rt-subcategory-item']/a"  # 找小類別網址
xpath_s = "//*[@class='rt-subcategory-item']/a/h3"  # 找小類別名稱
xpath_s1 = "//*[@class='rt-text-small rt-text-notice']"  # 找小類別商品數

remDr = remoteDriver(
  browserName = "chrome",
  remoteServerAddr = "localhost",
  port = 4444
)
remDr$open()

url = "https://find.ruten.com.tw/search/adult_confirm.php?refer=http%3A%2F%2Fwww.ruten.com.tw%2Fcategory%2Fmain%3F0025"
remDr$navigate(url)
webpage = read_html(remDr$getPageSource()[[1]][1])

# 然後這邊手動按已滿18歲

webpage = read_html(remDr$getPageSource()[[1]][1])
name_m = xml_text(xml_find_all(webpage, xpath_m))
scat_url = unlist(xml_attr(xml_find_all(webpage, xpath_surl), "href"))
name_s = xml_text(xml_find_all(webpage, xpath_s))
number = xml_text(xml_find_all(webpage, xpath_s1))
number1 = vector("numeric")  # 為解決number為character的問題
for (i in 1:length(number)){
  number1[i] = as.numeric(substr(number[i], start = 2, stop = lengths(strsplit(number[i],""))-1))
}
scat = rbind(scat, cbind(name_b = name_b[k], name_m, name_s, number1, scat_url))  # 但此number1依舊為character
count_num = rbind(count_num, cbind(name_m, num = as.numeric(sum(number1))))




# 算每個中類別下有幾個小類別
# sum(scat$name_m == " 電腦、電子、周邊 ")
# unique(scat$name_m)  # 移除name_m的重複項的指令
# 計算 num_scat
num_scat = vector()
for (k in unique(scat$name_m)){
  num_scat = append(num_scat, sum(scat$name_m == k))
}
count_num = cbind(count_num, num_scat)


# 計算比例 p
total_num = sum(as.numeric(count_num$num))  # 總商品個數
count_num$p = as.numeric(count_num$num)/total_num


# 計算 電腦、電子、周邊 中，每個小類別各站的商品數目比例
comp = data.frame()
comp = scat[scat$name_m == " 電腦、電子、周邊 ",]
comp$p = as.numeric(comp$number1) / sum(as.numeric(comp$number1))
sum(comp$p)

# 計算 生活、居家 中，每個小類別各站的商品數目比例
kit = data.frame()
kit = scat[scat$name_m == " 生活、居家 ",]
kit$p = as.numeric(kit$number1) / sum(as.numeric(kit$number1))
sum(kit$p)

# 計算 保養、彩妝 中，每個小類別各站的商品數目比例
face = data.frame()
face = scat[scat$name_m == " 保養、彩妝 ",]
face$p = as.numeric(face$number1) / sum(as.numeric(face$number1))
sum(face$p)

# 計算 書籍、文創、科學 中，每個小類別各站的商品數目比例
office = data.frame()
office = scat[scat$name_m == " 書籍、文創、科學 ",]
office$p = as.numeric(office$number1) / sum(as.numeric(office$number1))
sum(office$p)


# ======================================修改公式-找出各小類的平均價格=====================================
# 具體做法是：總共要抽479個小類別
# 之前已經有作好的現成的了，是scat(data.frame)

# 把scat複製到price_avg
price_avg = data.frame(scat)

# 開始抓價格跟數量
remDr = remoteDriver(
  browserName = "chrome",
  remoteServerAddr = "localhost",
  port = 4444
)
remDr$open()

price2 = vector()
num2 = vector()
# user  system elapsed 
# 134.08    3.39 2375.30 

#----- 計時開始 -----#
ptm <- proc.time()
for(j in 1:(length(price_avg$name_s)-13)){
  url = price_avg$scat_url[j]
  remDr$navigate(url)
  webpage = read_html(remDr$getPageSource()[[1]][1])
  
  xpath_prd_price = "//*[@class='price']"
  sp = as.numeric(gsub(",", "", xml_text(xml_find_all(webpage, xpath_prd_price))))
  qq1 = quantile(sp, na.rm = TRUE)
  scat_price = sum(sp[sp >= qq1[[2]] & sp <= qq1[[4]]], na.rm = TRUE)
  price2 = append(price2, scat_price)
  num2 = append(num2, length(sp[sp >= qq1[[2]] & sp <= qq1[[4]]]) - sum(is.na(sp[sp >= qq1[[2]] & sp <= qq1[[4]]])))
}
#----- 計時結束 -----#
proc.time() - ptm


# 成人專區另外抓
remDr = remoteDriver(
  browserName = "chrome",
  remoteServerAddr = "localhost",
  port = 4444
)
remDr$open()
url = "https://find.ruten.com.tw/search/adult_confirm.php?refer=http%3A%2F%2Fwww.ruten.com.tw%2Fcategory%2Fmain%3F0025"
remDr$navigate(url)
webpage = read_html(remDr$getPageSource()[[1]][1])

# 然後這邊手動按已滿18歲

for(j in 1:13){
  webpage = read_html(remDr$getPageSource()[[1]][1])
  url = price_avg$scat_url[466 + j]
  remDr$navigate(url)
  webpage = read_html(remDr$getPageSource()[[1]][1])
  
  xpath_prd_price = "//*[@class='price']"
  sp = as.numeric(gsub(",", "", xml_text(xml_find_all(webpage, xpath_prd_price))))
  qq1 = quantile(sp, na.rm = TRUE)
  scat_price = sum(sp[sp >= qq1[[2]] & sp <= qq1[[4]]], na.rm = TRUE)
  price2 = append(price2, scat_price)
  num2 = append(num2, length(sp[sp >= qq1[[2]] & sp <= qq1[[4]]]) - sum(is.na(sp[sp >= qq1[[2]] & sp <= qq1[[4]]])))
}

# 最後加上去
price_avg = cbind(price_avg, price2, num2)
price_avg$pavg = round(price_avg$price2 / price_avg$num2, 0)


# ===後續處理===
# 發現有NA，有些可能沒抓到在抓一次補上去
url = "https://find.ruten.com.tw/c/00250011"
remDr$navigate(url)
webpage = read_html(remDr$getPageSource()[[1]][1])
xpath_prd_price = "//*[@class='price']"
sp = as.numeric(gsub(",", "", xml_text(xml_find_all(webpage, xpath_prd_price))))
qq1 = quantile(sp, na.rm = TRUE)
price_avg$price2[118] = sum(sp[sp >= qq1[[2]] & sp <= qq1[[4]]], na.rm = TRUE)
price_avg$num2[118] = length(sp[sp >= qq1[[2]] & sp <= qq1[[4]]]) - sum(is.na(sp[sp >= qq1[[2]] & sp <= qq1[[4]]]))
price_avg$pavg = round(price_avg$price2 / price_avg$num2, 0)

url = "https://find.ruten.com.tw/c/00020011"
remDr$navigate(url)
webpage = read_html(remDr$getPageSource()[[1]][1])
xpath_prd_price = "//*[@class='price']"
sp = as.numeric(gsub(",", "", xml_text(xml_find_all(webpage, xpath_prd_price))))
qq1 = quantile(sp, na.rm = TRUE)
price_avg$price2[262] = sum(sp[sp >= qq1[[2]] & sp <= qq1[[4]]], na.rm = TRUE)
price_avg$num2[262] = length(sp[sp >= qq1[[2]] & sp <= qq1[[4]]]) - sum(is.na(sp[sp >= qq1[[2]] & sp <= qq1[[4]]]))
price_avg$pavg = round(price_avg$price2 / price_avg$num2, 0)


# 發現有重複的類別，處理完變成使用price_avg1
price_avg$duplicate = duplicated(price_avg$scat_url)
price_avg1 = data.frame(price_avg)

# 這個會出現error
for (i in 1:479){
  if(price_avg1$duplicate[i] == "TRUE"){
    print(i)
    print(price_avg1$name_s[i])
    price_avg1 = price_avg1[-i,]
    i = i
  }
  else{
    i = i + 1
  }
}
# 可以再做這個?
for (i in 1:464){
  if(price_avg1$duplicate[i] == "TRUE"){
    print(i)
    print(price_avg1$name_s[i])
    price_avg1 = price_avg1[-i,]
    i = i
  }
  else{
    i = i + 1
  }
}
length(unique(price_avg1$scat_url))


# 也將 scat & count_num 更新(刪去重複類別)
scat1 = data.frame(scat)
scat1$duplicate = duplicated(scat1$scat_url)

for (i in 1:479){
  if(scat1$duplicate[i] == "TRUE"){
    print(i)
    print(scat1$name_s[i])
    scat1 = scat1[-i,]
    i = i
  }
  else{
    i = i + 1
  }
}

for (i in 1:464){
  if(scat1$duplicate[i] == "TRUE"){
    print(i)
    print(scat1$name_s[i])
    scat1 = scat1[-i,]
    i = i
  }
  else{
    i = i + 1
  }
}
length(unique(scat1$scat_url)) == length(scat1$scat_url)  # 出現TRUE即可


count_num1 = data.frame(count_num)
for (i in 1:length(count_num1$name_m)){
  count_num1$num[i] = sum(as.numeric(scat1$number1[scat1$name_m == count_num1$name_m[i]]))
  count_num1$num_scat[i] = length(scat1$number1[scat1$name_m == count_num1$name_m[i]])
}
# 計算比例 p
total_num1 = sum(as.numeric(count_num1$num))  # 總商品個數
count_num1$p = as.numeric(count_num1$num)/total_num1


# price_avg$name_s[duplicated(price_avg$scat_url) == "TRUE"]
# [1] "廚房小家電@"     "廚房與廚房用品@" "男士清潔保養"    "女性各類包包@"   "首飾配件"       
# [6] "女性各類包包"    "名牌精品包"      "食譜"            "男錶"            "女錶"           
# [11] "古書善本@"       "古董錶@"         "偶像簽名@"       "餐飲、外燴@"     "18禁玩具公仔"  

# price_avg$scat_url[duplicated(price_avg$scat_url) == "TRUE"]

# price_avg$name_s[grep("@", price_avg$name_s)]
price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00230014"]
price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00090004"]
price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00120016"]
price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00150007"]
price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00140003"]
# 與第四個又重複了：price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00150007"]
price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00150008"]
price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00060014"]
price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00170001"]
price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00170002"]
price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00060008"]
price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00170005"]
price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00040006"]
price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00240022"]
price_avg$name_s[price_avg$scat_url == "https://find.ruten.com.tw/c/00250011"]


# 整個露天網站的商品均價
prd_price_average = round(sum(price_avg$pavg)/length(price_avg$pavg), 0)
prd_price_average/price_avg$pavg[1]


# 找出四個類別中，平均價格最高的小類別
max(price_avg1$pavg[price_avg1$name_b == "電子、電器設備"])  # 機車
max(price_avg1$pavg[price_avg1$name_b == "男性、運動、居家用品"])  # 國外旅遊
max(price_avg1$pavg[price_avg1$name_b == "女性、嬰幼兒用品"])  # 女性首飾配件@
max(price_avg1$pavg[price_avg1$name_b == "娛樂相關用品"])  # 房屋出售

# 找出四個類別中，商品數目最多的小類別
price_avg1$number1 = as.numeric(price_avg1$number1)
max(price_avg1$number1[price_avg1$name_b == "電子、電器設備"])  # 電子零件、材料
max(price_avg1$number1[price_avg1$name_b == "男性、運動、居家用品"])  # 其他
max(price_avg1$number1[price_avg1$name_b == "女性、嬰幼兒用品"])  # 女性首飾配件@
max(price_avg1$number1[price_avg1$name_b == "娛樂相關用品"])  # 古書善本

# 找出四個類別中，第一個中類別的第一個小類別
# 電子、電器設備 -> 電腦、電子、周邊 -> 筆記型電腦
# 男性、運動、居家用品 -> 生活、居家 -> 廚房與廚房用品
# 女性、嬰幼兒用品 -> 保養、彩妝 -> 臉部保養
# 娛樂相關用品 -> 書籍、文創、科學 -> 文具、辦公用品


write.csv(price_avg1, file="price_avg1.csv",row.names = T)

# ===========================先將4類的100個頁面的商品URL抽出===========================
# 抓 筆記型電腦 下的100頁商品，但並未全部，以下只抓到7744樣商品，但總共應有94872樣
# 08/05 時抓到8238樣商品
# 08/11 時抓到8073樣商品
# 09/10 時抓到8319樣商品
# 09/11 時抓到8072樣商品

# 打開瀏覽器
remDr = remoteDriver(
  browserName = "chrome",
  remoteServerAddr = "localhost",
  port = 4444
)
remDr$open()

price_comp = vector()
# user  system elapsed 
# 25.01    0.74  457.19
#----- 計時開始 -----#
ptm <- proc.time()
N = 100
url_comp = paste('https://find.ruten.com.tw/c/00110002?p=', c(1:N), sep = "")
shop_url_comp = data.frame()
prd_url1 = vector()
for (k in 1:N){
  remDr$navigate(url_comp[k])
  webpage = read_html(remDr$getPageSource()[[1]][1])
  # 抓所有商品網址
  # 想要抓到每個商品的網址的最後編號
  xpath_prd_url = "//*[@class='site_loop']/dd"  # 找每個商品類別網址
  prd_url1 = paste("https://www.ruten.com.tw/item/show?", 
                   unlist(xml_attr(xml_find_all(webpage, xpath_prd_url), "_id")), sep = "")
  shop_url_comp = rbind(shop_url_comp, cbind(page_num = k, prd_url1))
  
  #抓所有價格
  xpath_prd_price = "//*[@class='price']"
  prd_price = xml_text(xml_find_all(webpage, xpath_prd_price))
  price_comp = append(price_comp, prd_price)
}
#----- 計時結束 -----#
proc.time() - ptm

# 算四分位數
price_comp = gsub(",", "", price_comp)
length(price_comp)
# 9066
# 8856
qq_comp = quantile(as.numeric(price_comp), na.rm = TRUE)  # 可忽略 NA
# 0%         25%         50%         75%        100% 
# 1.00      799.75     4292.00    14806.50 99999999.00 




# 抓 廚房與廚房用品 下的100頁商品
# 09/10 時抓到7987樣商品

price_kit = vector()
# user  system elapsed 
# 24.06    2.84  455.92 
#----- 計時開始 -----#
ptm <- proc.time()
N = 100
url_kit = paste('https://find.ruten.com.tw/c/00090004?p=', c(1:N), sep = "")
shop_url_kit = data.frame()
prd_url2 = vector()
for (k in 1:N){
  remDr$navigate(url_kit[k])
  webpage = read_html(remDr$getPageSource()[[1]][1])
  # 抓所有商品網址
  # 想要抓到每個商品的網址的最後編號
  xpath_prd_url = "//*[@class='site_loop']/dd"  # 找每個商品類別網址
  prd_url2 = paste("https://www.ruten.com.tw/item/show?", 
                   unlist(xml_attr(xml_find_all(webpage, xpath_prd_url), "_id")), sep = "")
  shop_url_kit = rbind(shop_url_kit, cbind(page_num = k, prd_url2))
  
  #抓所有價格
  xpath_prd_price = "//*[@class='price']"
  prd_price = xml_text(xml_find_all(webpage, xpath_prd_price))
  price_kit = append(price_kit, prd_price)
}
#----- 計時結束 -----#
proc.time() - ptm

# 算四分位數
price_kit = gsub(",", "", price_kit)
length(price_kit)
# 10661
qq_kit = quantile(as.numeric(price_kit), na.rm = TRUE)  # 可忽略 NA
# 0%    25%    50%    75%   100% 
# 1     140    350    755   999999 





# 抓 臉部保養 下的100頁商品
# 09/10 時抓到7991樣商品

price_face = vector()
# user  system elapsed 
# 27.04    1.43  552.31 
#----- 計時開始 -----#
ptm <- proc.time()
N = 100
url_face = paste('https://find.ruten.com.tw/c/00120007?p=', c(1:N), sep = "")
shop_url_face = data.frame()
prd_url3 = vector()
for (k in 1:N){
  remDr$navigate(url_face[k])
  webpage = read_html(remDr$getPageSource()[[1]][1])
  # 抓所有商品網址
  # 想要抓到每個商品的網址的最後編號
  xpath_prd_url = "//*[@class='site_loop']/dd"  # 找每個商品類別網址
  prd_url3 = paste("https://www.ruten.com.tw/item/show?", 
                   unlist(xml_attr(xml_find_all(webpage, xpath_prd_url), "_id")), sep = "")
  shop_url_face = rbind(shop_url_face, cbind(page_num = k, prd_url3))
  
  #抓所有價格
  xpath_prd_price = "//*[@class='price']"
  prd_price = xml_text(xml_find_all(webpage, xpath_prd_price))
  price_face = append(price_face, prd_price)
}
#----- 計時結束 -----#
proc.time() - ptm

# 算四分位數
price_face = gsub(",", "", price_face)
length(price_face)
# 9181
qq_face = quantile(as.numeric(price_face), na.rm = TRUE)  # 可忽略 NA
# 0%    25%    50%    75%   100% 
# 1     290    570    1019  999999 





# 抓 文具、辦公用品 下的100頁商品
# 09/10 時抓到8073樣商品

price_office = vector()
# user  system elapsed 
# 30.58    2.14  641.69 
#----- 計時開始 -----#
ptm <- proc.time()
N = 100
url_office = paste('https://find.ruten.com.tw/c/00060003?p=', c(1:N), sep = "")
shop_url_office = data.frame()
prd_url4 = vector()
for (k in 1:N){
  remDr$navigate(url_office[k])
  webpage = read_html(remDr$getPageSource()[[1]][1])
  # 抓所有商品網址
  # 想要抓到每個商品的網址的最後編號
  xpath_prd_url = "//*[@class='site_loop']/dd"  # 找每個商品類別網址
  prd_url4 = paste("https://www.ruten.com.tw/item/show?", 
                   unlist(xml_attr(xml_find_all(webpage, xpath_prd_url), "_id")), sep = "")
  shop_url_office = rbind(shop_url_office, cbind(page_num = k, prd_url4))
  
  #抓所有價格
  xpath_prd_price = "//*[@class='price']"
  prd_price = xml_text(xml_find_all(webpage, xpath_prd_price))
  price_office = append(price_office, prd_price)
}
#----- 計時結束 -----#
proc.time() - ptm

# 算四分位數
price_office = gsub(",", "", price_office)
length(price_office)
# 10530
qq_office = quantile(as.numeric(price_office), na.rm = TRUE)  # 可忽略 NA
# 0%     25%     50%     75%    100% 
# 1      60     250     555 1980088 







# ===========================================正式抽樣===========================================
# 結果放在 sample_list 中
# 先抽 1000 家，要列出：商品名稱、商品價格(原始)、商品價格(整理)、商品URL、是否海外、
#                       賣場名稱、賣場關於我網址

# user  system elapsed 
# 110.72    1.51 2666.58 

xpath_smp_prdn = '//*[@class="vmiddle"]'# 商品名稱
xpath_smp_prdp = '//*[@class="rt-text-xx-large rt-text-important"]'# 商品價格
xpath_shop = '//*[@class="seller-disc"]/div/a'# 賣場資訊
xpath_sea = '//*[@class="item-quick-fact"]/ul/li/span'
xpath_shop = '//*[@class="seller-disc"]/div/a' # 賣場名稱


# 這邊要改兩個地方!!!很重要!!!：shop_url_kit 跟 shop_url$prd_url2

M = 1400
shop_url = data.frame(shop_url_office)
# 避免抽到不同商品而先變註解
smp_num = sample(1:dim(shop_url)[1], size = M, replace = FALSE)
smp_prd_url_all = shop_url$prd_url4[smp_num]
sample_list = data.frame()

# 打開瀏覽器
remDr = remoteDriver(
  browserName = "chrome",
  remoteServerAddr = "localhost",
  port = 4444
)
remDr$open()

# ===comp===
# user  system elapsed 
# 74.98    0.72 1567.25 
# user  system elapsed 
# 29.74    0.25  634.11 
# user  system elapsed 
# 3.20    0.07   69.55 
# user  system elapsed 
# 6.39    0.09  140.18 

# ===kit===
# ===face===
# ===office===
# user  system elapsed 
# 159.33    1.61 3410.21 
#----- 計時開始 -----#
ptm <- proc.time()
for (k in 10:M){
  # 直接做
  smp_prd_url = smp_prd_url_all[k]
  remDr$navigate(smp_prd_url)
  webpage = read_html(remDr$getPageSource()[[1]][1], encoding = big-5)  # 所以加上big-5
  smp_prd_name = xml_text(xml_find_all(webpage, xpath_smp_prdn))[1]
  smp_prd_price = xml_text(xml_find_all(webpage, xpath_smp_prdp))   # 這邊出現編碼問題(可能不能判斷$)
  oversea = str_trim(xml_text(xml_find_all(webpage, xpath_sea))[2])  # 如果是"海外運送"，則為海外商品
  smp_shop_name = xml_text(xml_find_all(webpage, xpath_shop))[1]   # 賣場名稱
  smp_shop_url = unlist(xml_attr(xml_find_all(webpage, xpath_shop), "href"))[1]   # 賣場網址
  
  sample_list = rbind(sample_list, cbind(index = k, smp_prd_name, smp_prd_price, smp_prd_url,
                                         oversea, smp_shop_name, smp_shop_url))
  print(k)
}
#----- 計時結束 -----#
proc.time() - ptm

sample_list$smp_shop_about = gsub("index00", "about", sample_list$smp_shop_url)



# 針對 sample_list 中的價格做調整，調整前後的價格都要留下來
sample_list$smp_prd_price_adj = str_replace_all(sample_list$smp_prd_price,"[$,]","")
for (i in 1:lengths(sample_list)[[1]]){
  # TRUE，即有 "-"
  if(grepl("-",sample_list$smp_prd_price_adj[i]) == TRUE){
    sample_list$smp_prd_price_adj1[i] = 
      (as.numeric(strsplit(sample_list$smp_prd_price_adj[i], "-")[[1]][1]) 
       + as.numeric(strsplit(sample_list$smp_prd_price_adj[i], "-")[[1]][2]))/2
  }
  else if(grepl("-",sample_list$smp_prd_price_adj[i]) == FALSE){
    sample_list$smp_prd_price_adj1[i] = sample_list$smp_prd_price_adj[i]
  }
}

# 不同類別的sample_list分開
# sample_list_comp = data.frame(sample_list)
# sample_list_kit = data.frame(sample_list)
# sample_list_face = data.frame(sample_list)
sample_list_office = data.frame(sample_list)


# 在 sample_list 中，再 bind B2C & C2C 資訊。
# 作法是先做出seller，再 bind 到 sample_list 中

seller = data.frame(index = sample_list$index, sel_shop_about = sample_list$smp_shop_about)

company = vector()
phy_store = vector()
dealer = vector()
agent = vector()
wholesaler = vector()
personal = vector()
#shop_info = list()

# ===comp===
# user  system elapsed 
# 57.14    0.71  832.68 
# ===kit===
# user  system elapsed 
# 65.44    0.92 1379.09 
# ===office===

#----- 計時開始 -----#
ptm <- proc.time()
for(k in 1:lengths(sample_list)[[1]]){
  shop_about_url = seller$sel_shop_about[k]
  remDr$navigate(shop_about_url)
  webpage = read_html(remDr$getPageSource()[[1]][1])
  # 賣家資訊
  xpath_shop_about = '//*[@class="item"]'
  shop_about = str_trim(xml_text(xml_find_all(webpage, xpath_shop_about)))
  
  company = append(company, sum(grepl("公司行號", str_trim(shop_about))))
  phy_store = append(phy_store, sum(grepl("有實體店面", str_trim(shop_about))))
  dealer = append(dealer, sum(grepl("經銷商", str_trim(shop_about))))
  agent = append(agent, sum(grepl("代理商", str_trim(shop_about))))
  wholesaler = append(wholesaler, sum(grepl("批發商", str_trim(shop_about))))
  
  personal = append(personal, sum(grepl("個人", str_trim(shop_about))))
  
  print(k)
}
#----- 計時結束 -----#
proc.time() - ptm

seller = cbind(seller, company, phy_store, dealer, agent, wholesaler, personal)


# 判斷為B2C or C2C
seller$e_commerce[seller$company == 1 | seller$phy_store == 1] = "B2C"
seller$e_commerce[seller$company == 0 & seller$phy_store == 0 & seller$personal == 1] = "C2C"


# 再將不同類別的seller分開
# seller_comp = data.frame(seller)
# seller_kit = data.frame(seller)
# seller_face = data.frame(seller)
seller_office = data.frame(seller)


sample_final = data.frame(index = sample_list$index, smp_prd_name = sample_list$smp_prd_name,
                          smp_prd_price_adj1 = round(as.numeric(sample_list$smp_prd_price_adj1),0),
                          smp_prd_url = sample_list$smp_prd_url, oversea = sample_list$oversea, 
                          smp_shop_name = sample_list$smp_shop_name,
                          smp_shop_about = sample_list$smp_shop_about, e_commerce = seller$e_commerce)

# 再將不同類別的sample_final分開，等等下面還會更新一次
# sample_final_comp = data.frame(sample_final)
# sample_final_kit = data.frame(sample_final)
# sample_final_face = data.frame(sample_final)
sample_final_office = data.frame(sample_final)



# 看一下在 1000 樣商品中的所屬賣場，有多少屬於B2C，多少屬於C2C
table(sample_final_comp$e_commerce)
# B2C C2C 
# 231 290 
table(sample_final_kit$e_commerce)
# B2C C2C 
# 226 256 
table(sample_final_face$e_commerce)  # 總共抓1399個樣本進來
# B2C C2C 
# 197 457 
table(sample_final_office$e_commerce)
# B2C C2C 
# 309 320 


# ===========================================篩選出100家==========================================
# 下一個步驟才是篩選：

# qq = qq_comp
# qq = qq_kit
# qq = qq_face
qq = qq_office


smp_size = 100
price_lower_lim = qq[[2]]
price_upper_lim = qq[[4]]
f = 0   # f for fail
r = 0   # r for repeat
j = 0
smp_index = vector()

# user  system elapsed 
# 0.19    0.10    0.48 
#----- 計時開始 -----#
ptm <- proc.time()
for (k in 1:lengths(sample_list)[[1]]){
  if(is.na(sample_final$oversea[k])){
    sample_final$oversea[k] = ""
  }
  if(sample_final$oversea[k] != "海外運送"){
    # 其他平台商品
    if((sample_final$smp_prd_price_adj1[k] >= price_lower_lim) & (sample_final$smp_prd_price_adj1[k] <= price_upper_lim)){
      # Q1-Q3
      if(j == 0){
        # 第一個被選進的樣本
        if(is.na(sample_final$e_commerce[k]) == FALSE){
          smp_index = append(smp_index, k)
          j = j + 1
        }
        else{
          f = f + 1
        }
      }
      else if (j != 0){
        # 不是第一個被選進的樣本，要與前面的賣場進行比較
        if(length(unique(sample_final$smp_shop_name[c(smp_index, k)])) == k - j - f - r + 1){
          # 不重複
          if(is.na(sample_final$e_commerce[k]) == FALSE){
            # 可判斷是否為B或C，即可加入
            smp_index = append(smp_index, k)
          }
          else{
            f = f + 1
          }
        }
        else if (length(unique(sample_final$smp_shop_name[c(smp_index, k)])) != k - j - f - r + 1){
          r = r + 1
        }
      }
    }
    else{
      f = f + 1
    }
  }
  else if(sample_final$oversea[k] == "海外運送"){
    f = f + 1
  }
  if(length(smp_index) == smp_size){
    break
  }
}
#----- 計時結束 -----#
proc.time() - ptm


# 再將不同類別的sample_final分開
# sample_final_comp = data.frame(sample_final)
# sample_final_kit = data.frame(sample_final)
# sample_final_face = data.frame(sample_final)
sample_final_office = data.frame(sample_final)




# ===check===
# B2C & C2C 已經不一定要1:1了
table(sample_final[smp_index, ]$e_commerce)
# comp
# B2C C2C 
# 40  60  

#kit
# B2C C2C 
# 50  50

# face
# B2C C2C 
# 21  79 

# office
# B2C C2C 
# 44  56 

length(unique(sample_offical$smp_shop_name))
# [1] 100
unique(sample_offical$oversea)  # 確定沒有來自海外的

sample_offical = sample_final[smp_index, ]

# 再將不同類別的sample_offical分開
# sample_offical_comp = data.frame(sample_offical)
# sample_offical_kit = data.frame(sample_offical)
# sample_offical_face = data.frame(sample_offical)
sample_offical_office = data.frame(sample_offical)

# ===========================================爬賣出數量==========================================
'userid' = "reesia_2020",
'userpass' = "Y3r72VU89s7MXTg"

remDr = remoteDriver(
  browserName = "chrome",
  remoteServerAddr = "localhost",
  port = 4444
)
remDr$open()

# 打開網頁
url = 'https://mybid.ruten.com.tw/goods/historymore.php?g=21520389709365'
remDr$navigate(url)
webpage = read_html(remDr$getPageSource()[[1]][1])

# 然後這邊手動輸入帳密



# 開始做100個商品的出售狀況
# 先做網址
sample_offical$historymore_url = paste("https://mybid.ruten.com.tw/goods/historymore.php?g=", 
                                       substr(sample_offical$smp_prd_url, start = 36, stop = 49), sep = "")

# sample_offical = data.frame(sample_offical_comp)
# sample_offical = data.frame(sample_offical_kit)
# sample_offical = data.frame(sample_offical_face)
# sample_offical = data.frame(sample_offical_office)

index_value = vector()
prd_name = vector()
prd_price2 = vector()
shop_name1 = vector()
e_commerce = vector()

buy_id = vector()
buy_num = vector()
buy_time = vector()
xpath_buy = '//*[@class="rt-table rt-table-hover rt-mt rt-mb-4x"]/tbody/tr/td'

# user  system elapsed 
# 5.93    0.09   58.52 
#----- 計時開始 -----#
ptm <- proc.time()
for (k in 1:dim(sample_offical)[1]){
  url = sample_offical$historymore_url[k]
  remDr$navigate(url)
  webpage = read_html(remDr$getPageSource()[[1]][1])
  
  trade = length(xml_text(xml_find_all(webpage, xpath_buy)))/3
  for (j in 1:trade){
    index_value = append(index_value, sample_offical$index[k])
    prd_name = append(prd_name, sample_offical$smp_prd_name[k])
    prd_price2 = append(prd_price2, sample_offical$smp_prd_price_adj1[k])
    shop_name1 = append(shop_name1, sample_offical$smp_shop_name[k])
    e_commerce = append(e_commerce, sample_offical$e_commerce[k])
    
    buy_id = append(buy_id, xml_text(xml_find_all(webpage, xpath_buy))[3*j-2])
    buy_num = append(buy_num, str_trim(strsplit(xml_text(xml_find_all(webpage, xpath_buy))[3*j-1], "\n")[[1]][2]))
    buy_time = append(buy_time, xml_text(xml_find_all(webpage, xpath_buy))[3*j])
  }  
}
#----- 計時結束 -----#
proc.time() - ptm

selling = data.frame(index_value, prd_name, prd_price2, buy_id, buy_num, buy_time, shop_name1, e_commerce)

# 再將不同類別的selling分開
# selling_comp = data.frame(selling)
# selling_kit = data.frame(selling)
# selling_face = data.frame(selling)
# selling_office = data.frame(selling)


# 整理出最後的表格
index_s = vector()
prd_name_s = vector()
prd_price_s = vector()
seles_num_s = vector()
shop_name_s = vector()
e_commerce_s = vector()

smp_index = as.numeric(sample_offical$index)


sales = data.frame()
# user  system elapsed 
# 0.12    0.03    0.27
#----- 計時開始 -----#
ptm <- proc.time()
for (k in 1:dim(sample_offical)[1]){
  i = smp_index[k]
  
  if(lengths(selling[selling$index_value == i,])[[1]][1] == 1){
    # 若只有一條，還要判斷是有賣出一個還是沒有任何人購買
    if(selling[selling$index_value == i,]$buy_id == "沒有任何人購買"){
      # 沒有人買
      seles_num_s = append(seles_num_s, 0)
    }
    else{
      # 有一個人買，但還要判斷是不是交易被取消
      if(is.na(selling[selling$index_value == i,]$buy_num)){
        # 交易被取消
        seles_num_s = append(seles_num_s, 0)
      }
      else{
        # 有一筆交易
        seles_num_s = append(seles_num_s, 1)
      }
    }
  }
  else{
    # 不只賣出一樣，但還要判斷是不是交易被取消(要逐筆確定)
    n = sum(as.numeric(selling[selling$index_value == i,]$buy_num), na.rm = TRUE)
    seles_num_s = append(seles_num_s, n)
  }
  index_s = append(index_s, i)
  prd_name_s = append(prd_name_s, sample_offical$smp_prd_name[k])
  prd_price_s = append(prd_price_s, sample_offical$smp_prd_price_adj1[k])
  
  shop_name_s = append(shop_name_s, sample_offical$smp_shop_name[k])
  e_commerce_s = append(e_commerce_s, sample_offical$e_commerce[k])
}
#----- 計時結束 -----#
proc.time() - ptm

sales = data.frame(index_s, prd_name_s, prd_price_s, seles_num_s, shop_name_s, e_commerce_s)


# 再將不同類別的sales分開
# sales_comp = data.frame(sales)
# sales_kit = data.frame(sales)
# sales_face = data.frame(sales)
# sales_office = data.frame(sales)




# 此100樣商品的半年銷售額
sum(sales$prd_price_s*sales$seles_num_s)

#  B2C & C2C賣場的商品平均價格
mean(sales$prd_price_s[ sales$e_commerce_s == "B2C" ])
mean(sales$prd_price_s[ sales$e_commerce_s == "C2C" ])

# B2C & C2C賣場的商品銷售數量
sum(sales$seles_num_s[ sales$e_commerce_s == "B2C" ])
sum(sales$seles_num_s[ sales$e_commerce_s == "C2C" ])

# B2C & C2C賣場的商品銷售額
sum(sales$prd_price_s[ sales$e_commerce_s == "B2C" ] * sales$seles_num_s[ sales$e_commerce_s == "B2C" ])
sum(sales$prd_price_s[ sales$e_commerce_s == "C2C" ] * sales$seles_num_s[ sales$e_commerce_s == "C2C" ])



write.csv(selling_comp, file="selling_comp_0915.csv",row.names = T)
write.csv(sales_comp, file="sales_comp_0915.csv",row.names = T)

write.csv(selling_kit, file="selling_kit_0915.csv",row.names = T)
write.csv(sales_kit, file="sales_kit_0915.csv",row.names = T)

write.csv(selling_face, file="selling_face_0915.csv",row.names = T)
write.csv(sales_face, file="sales_face_0915.csv",row.names = T)

write.csv(selling_office, file="selling_office_0915.csv",row.names = T)
write.csv(sales_office, file="sales_office_0915.csv",row.names = T)


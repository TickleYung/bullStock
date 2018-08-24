setwd("/Users/surgery/Project/HOME/github/bullStock/tushare")
setwd("D:\\2.Code\\github\\bullStock\\tushare")
options(stringsAsFactors = F)
all_basics <- read.csv("stock_basics.csv", header = T, colClasses = "character")
rownames(all_basics) <- all_basics$code

# total shares
dim(all_basics)
# [1] 3538   22

# issue time 
all_basics[,"timeToMarket"] <- as.numeric(all_basics[,"timeToMarket"])
issueYear <- as.integer(all_basics[,"timeToMarket"]/10000)
# exception
# all_basics[issueYear==0,]
barplot(table(issueYear)[names(table(issueYear))!=0])
# filter out company less than two years

filterCode <- all_basics[all_basics[,"timeToMarket"] > 0 & all_basics[,"timeToMarket"] < 20160820,]
write.csv(filterCode, file="filterCode.csv")

filterCode$tmpclose <- -1
filterCode$max2year <- -1
filterCode$max1year <- -1
filterCode$max6month <- -1
filterCode$min2year <- -1
filterCode$min1year <- -1
filterCode$min6month <- -1

for (i in filterCode$code) {
	print(i)
	#tmpdf <- read.csv(paste("price/",i,".csv",sep=""))
	tmpdf <- try(read.csv(paste("price/",i,".csv",sep="")), silent=TRUE)
	if (class(tmpdf) == "try-error") {next}
	if (dim(tmpdf)[1] < 200) {next}
	tmpdf$date2 <- as.integer(gsub("-", "", tmpdf$date))
	filterCode[i,]$tmpclose <- tail(tmpdf, n = 1)$close
	filterCode[i,]$max2year <- max(tmpdf[tmpdf$date2>20160820,]$close)
	filterCode[i,]$max1year <- max(tmpdf[tmpdf$date2>20170820,]$close)
	filterCode[i,]$max6month <- max(tmpdf[tmpdf$date2>20180220,]$close)
	filterCode[i,]$min2year <- min(tmpdf[tmpdf$date2>20160820,]$close)
	filterCode[i,]$min1year <- min(tmpdf[tmpdf$date2>20170820,]$close)
	filterCode[i,]$min6month <- min(tmpdf[tmpdf$date2>20180220,]$close)
}

# add volumn
filterCode$turnover <- NA
for (i in filterCode$code) {
	print(i)
	#tmpdf <- read.csv(paste("price/",i,".csv",sep=""))
	tmpdf <- try(read.csv(paste("price/",i,".csv",sep="")), silent=TRUE)
	# tmpdf$money <- tmpdf$close * tmpdf$volume
	# tmpdf$money <- tmpdf$volume
	if (class(tmpdf) == "try-error") {next}
	if (dim(tmpdf)[1] < 200) {next}
	tmpdf$date2 <- as.integer(gsub("-", "", tmpdf$date))
	turnover <- tmpdf[tmpdf$date2>=20180621 & tmpdf$date2<=20180821,]$volume
	#lastWeek <- tmpdf[tmpdf$date2>20180809 & tmpdf$date2<20180821,]$volume
	#lastMonthAve <- mean(lastMonth)
	filterCode[i,]$turnover <- round(sum(turnover/10^5)/filterCode[i,]$totals,digits = 1)
}

library(reshape)
meanMoney <- data.frame()
tmpdf <- melt(filterCode[,c("industry","money")], id.vars = "industry")
for (i in names(sort(table(tmpdf$industry), decreasing = T))) {
	tmpmeanMoney <- mean(tmpdf[tmpdf$industry == i,]$value)
	meanMoney <- rbind(meanMoney, data.frame(industry=i,meanMoney=tmpmeanMoney,count=table(tmpdf$industry)[i]))
}

save(filterCode, file="filterCode.Rdata")

filterCode <- filterCode[filterCode$tmpclose != -1 & filterCode$max2year != -1 & filterCode$max1year != -1 & filterCode$max6month != -1 & filterCode$min2year != -1 &filterCode$min1year != -1 &filterCode$min6month != -1,]

filterCode$pos2year <- (filterCode$tmpclose - filterCode$min2year)/(filterCode$max2year - filterCode$min2year)
filterCode$pos1year <- (filterCode$tmpclose - filterCode$min1year)/(filterCode$max1year - filterCode$min1year)
filterCode$pos6month <- (filterCode$tmpclose - filterCode$min6month)/(filterCode$max6month - filterCode$min6month)

filterCode$de2year <- (filterCode$max2year - filterCode$tmpclose)/filterCode$max2year
filterCode$de1year <- (filterCode$max1year - filterCode$tmpclose)/filterCode$max1year
filterCode$de6month <- (filterCode$max6month - filterCode$tmpclose)/filterCode$max6month 

filterCode$totals <- as.numeric(filterCode$totals)
filterCode$outstanding <- as.numeric(filterCode$outstanding)
filterCode$totalvalue <- filterCode$totals * filterCode$tmpclose
filterCode$subvalue <- filterCode$outstanding * filterCode$tmpclose

write.csv(filterCode, file="filterCode.csv")
save(filterCode, file="filterCode.Rdata")

# filter ST stock
STstock <- filterCode[grep("ST", filterCode$name),]
filterCode <- filterCode[!filterCode$code %in% STstock$code,]
# filter total less than 100
filterCode <- filterCode[filterCode$subvalue>=50,]

# add stock ZY
ZY <- read.csv("gpzyhgmx_20180812_20180818.txt", header = T, colClasses = "character", sep="\t")
rownames(ZY) <- ZY$code
filterCode$ZYratio <- 0
common <- filterCode$code[filterCode$code %in% ZY$code]
filterCode[common,]$ZYratio <- ZY[common,]$Zyratio
#
save(filterCode, file="filterCode2.Rdata")

filterCode3 <- filterCode[,c(1,2,3,4,5,13:16,20:22,24,31:39)]
# sort column
filterCode3 <- filterCode3[,c("name","industry","tmpclose","de1year","pos1year","pe","profit","totalvalue" ,"subvalue","ZYratio", "pos2year","pos6month","de2year","de6month", "esp","bvps","pb","npr","gpr","code","timeToMarket","area")]
filterCode3$tmpclose <- round(filterCode3$tmpclose,digits = 1)
filterCode3$de1year <- round(filterCode3$de1year,digits = 2)
filterCode3$pos1year <- round(filterCode3$pos1year,digits = 2)
filterCode3$pe <- round(as.numeric(filterCode3$pe),digits = 0)
filterCode3$profit <- round(as.numeric(filterCode3$profit),digits = 0)
filterCode3$totalvalue <- round(as.numeric(filterCode3$totalvalue),digits = 0)
filterCode3$subvalue <- round(as.numeric(filterCode3$subvalue),digits = 0)
filterCode3$ZYratio <- round(as.numeric(filterCode3$ZYratio),digits = 1)
filterCode3$pos2year <- round(filterCode3$pos2year,digits = 2)
filterCode3$pos6month <- round(filterCode3$pos6month,digits = 2)
filterCode3$de2year <- round(filterCode3$de2year,digits = 2)
filterCode3$de6month <- round(filterCode3$de6month,digits = 2)
filterCode3$esp <- round(as.numeric(filterCode3$esp),digits = 1)
filterCode3$bvps <- round(as.numeric(filterCode3$bvps),digits = 1)
filterCode3$pb <- round(as.numeric(filterCode3$pb),digits = 1)
filterCode3$npr <- round(as.numeric(filterCode3$npr),digits = 1)
filterCode3$gpr <- round(as.numeric(filterCode3$gpr),digits = 1)

save(filterCode3, file="filterCode3.Rdata")
# train the strategy on filterCode3

# add volumn
filterCode3$volumnRatio <- NA
for (i in filterCode3$code) {
	print(i)
	#tmpdf <- read.csv(paste("price/",i,".csv",sep=""))
	tmpdf <- try(read.csv(paste("price/",i,".csv",sep="")), silent=TRUE)
	if (class(tmpdf) == "try-error") {next}
	if (dim(tmpdf)[1] < 200) {next}
	tmpdf$date2 <- as.integer(gsub("-", "", tmpdf$date))
	lastMonth <- tmpdf[tmpdf$date2>=20180621 & tmpdf$date2<=20180809,]$volume
	lastWeek <- tmpdf[tmpdf$date2>20180809 & tmpdf$date2<20180821,]$volume
	lastMonthAve <- mean(lastMonth)
	lastWeekAve <- mean(sort(lastWeek, decreasing = T)[1:3])
	filterCode3[i,]$volumnRatio <- round(lastWeekAve/lastMonthAve,digits = 1)
}

## select stocks
# filter ZYratio > 30
filterCode4 <- filterCode3[filterCode3$ZYratio < 30,]
# filter profit < 0
filterCode4 <- filterCode4[filterCode4$profit>0,]
# filter pos < 0.5, de > 0.4
filterCode5 <- filterCode4[filterCode4$de1year>0.4 & filterCode4$pos1year<0.4 & filterCode4$pos6month<0.2,]
# check
t(filterCode5["002400",])

save(filterCode4, file="filterCode4.Rdata")

################################################################################
filterCode <- read.csv("filterCode.csv", header = T, colClasses = "character")

today <- 20180821

# 60XXXX是上海证券A股票 00XXXX深圳中小板股票 30XXXX是创业板股票

# GEM, 30.... : 730
GEM300 <- all_basics$code[grep("^30....", all_basics$code)]

# hu, 60.... : 1432
hu600 <- all_basics$code[grep("^60....", all_basics$code)]

# shen, 00.... : 1376
table(sapply(as.character(all_basics$code), nchar)<6)

# check one stock
all_basics[all_basics$code==300676,]

#code               300676
#name             华大基因
#industry         医疗保健
#area                 深圳
#pe                  70.29 (市盈率)
#outstanding          2.47 (流通股数)
#totals                  4 （总股本数）2.小中大的投资方法完全不同，胆子大就玩小盘股。
#totalAssets      504988.6 （总资产，负债+权益）
#liquidAssets     369129.9 （流动资产）
#fixedAssets      77748.93 （固定资产）
#reserved         296825.6 （资本公积）
#reservedPerShare     7.42 （每股公积）
#esp                 0.252 （每股收益）
#bvps                10.54 （每股净资）
#pb                   6.71 （市净率）
#timeToMarket     20170714 （上市时间）
#undp             80426.09 （未分配利润）
#perundp              2.01 （每股未分配）
#rev                 33.13 （营业收入同比增长）
#profit               6.71 （净利润同比增长）
#gpr                 53.68 （毛利率）1.不能过低，毛利率和规模是决定因素。20%以下过滤。蓝海。软件互联网，医药。
#npr                 19.51 （净利润率）
#holders             37289 （股东人数）
#总市值
#3. 净资产收益率（ROE），衡量盈利能力，5%以上。
#4. 主营业务比，不务正业的过滤（80%以下）。
#5. 需求，决定供给，决定价格，不要买资源股。
#6. 股东结构，必须有靠谱的单一大股东，专心经营。

## 精筛
# 1. 看不懂业务结构，看不清利润来源（关联交易，非主业）的过滤。
# 2. 影响未来的因素分析。
# 3. 大盘下行压力大时不要入场，选择领头行业，选择低价股。
# 4. 过滤掉存在强阻力区域的股票。
# 5. 选优
# 6. 平仓未放量突破的股票，保留放量突破的股票。
# 新晋玩家想在一个古老的行业里发财致富是非常困难的事情！
# 行业准入门槛越低，利润越低，要在低门槛行业生存，你必须是第一个吃螃蟹的人。
# 股市是零和博弈游戏。
# 把投资视为业余爱好，却指望靠业余技能赚钱
# 人们买张卫生纸都要货比三家，审查一只股票却只花3分钟。
# 有赚它10个亿的野心，却没有坚持3个月的耐性
# 想赢怕输是绝大多数人的心态。经过我多年的观察，普通股民只能忍受连续3笔亏损、持续被套3个月，个股浮亏30%； 我把这种现象称为333定律。
# 绝大多数人只是假装很勤奋，假装在思考
# 只买生，不买熟
# 买时过度自信，买后过度恐慌
# 我来到这市场不是为了跟别人辩论的，我是来赚钱的。抓住核心因素，忽略其他因素。
# 技术止损or逻辑止损
# 如果我卖出一个票，那么一定是因为我发现自己的逻辑判断错误了，如果我觉得自己逻辑没错，那么这个股票即使跌了50%，我也不会卖，我不光不会卖，我反而会越跌越买。但是一旦我发现自己逻辑判断有误，那么我会毫不犹豫地一刀砍了，我不会在乎当前股价，哪怕已经盈利也坚持卖出。简而言之，我炒股是靠逻辑炒股，而不是靠技术分析。
# 技术止损的原罪在于，他会让你变得玩世不恭，浮躁。只有禁止你轻易止损，你才会真正用脑子去审查股票而不是用下半身去审查。
# 追求确定性和买点精准性
# 知识碎片化， 建议读巴菲特的《致股东信》
# 靠意念炒股
# 很多股民买卖股票并不是建立在事实依据之上，而是建立在臆想之上。
# 天命
# 真正做起来，都是靠07年和09年那波大牛市，成长的晚一点的，是靠15年那波大牛市。
# 运气
# 在股票市场上失败才是必然，成功只是偶然。
# 


## check company num in one industry
data.frame(sort(table(all_basics[,"industry"]), decreasing = T))








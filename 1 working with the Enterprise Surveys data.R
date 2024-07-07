 
library(ggplot2) 
library(MatchThem)
library(cobalt)
set.seed(123)

rm(list=ls())
options(stringsAsFactors = FALSE)

theme_set(theme_bw() + theme(strip.text.x =element_text(size=12, family="serif"), 
                             strip.text.y =element_text(size=12, family="serif"),  
                             axis.title.x =element_text(size=12, family="serif"), 
                             axis.title.y =element_text(size=12, family="serif"), 
                             axis.text=element_text(size=11, family="serif"),
                             legend.text=element_text(size=12, family="serif"),
                             strip.background = element_rect(fill="white")))

setwd("C:/Users/az310/Dropbox/COVID and institutions/India paper/code/replication")
out_dir <- "./output"
in_dir <- "./data"

## labels
sect.labs <- 
  c('formal'='Formal sector',
    'exception'='Exception from closures',
    'formal:exception'='Formal sector X Exception from closures',
    'm_city_Tezpur'='City: Tezpur',
    'm_city_Mumbai'='City: Mumbai',
    'm_city_Hyderabad'='City: Hyderabad',
    'm_city_Surat'='City: Surat',
    'm_city_Jaipur'='City: Jaipur',
    'm_city_Kochi'='City: Kochi',
    'm_city_Varanasi'='City: Varanasi',
    'm_city_Ludhiana'='City: Ludhiana',
    'm_city_Sehore'='City: Sehore',
    'm_sector_Manufacturing'='Manufacturing',
    'm_sector_Reselling'='Retail and wholesale',
    'm_sector_Services'='Services except retail and wholesale',
    'm_rcode_beauty'='Goods and services: beauty products and services',
    'm_rcode_cleaning'='Goods and services: cleaning and washing',
    'm_rcode_clothes+homeware+computers'='Goods and services: clothes, household items, electronics',
    'm_rcode_construct'='Goods and services: construction',
    'm_rcode_decor'='Goods and services: decoration',
    'm_rcode_distret'='Goods and services: online retail',
    'm_rcode_edu'='Goods and services: education',
    'm_rcode_fin'='Goods and services: financial',
    'm_rcode_food'='Goods and services: food',
    'm_rcode_funeral'='Goods and services: funeral',
    'm_rcode_furniture'='Goods and services: furniture',
    'm_rcode_hardware'='Goods and services: hardware and metal products',
    'm_rcode_home'='Goods and services: household staff',
    'm_rcode_housing'='Goods and services: accommodation and housing',
    'm_rcode_jewel'='Goods and services: jewelry',
    'm_rcode_lottery'='Goods and services: lotteries',
    'm_rcode_med+prof'='Goods and services: medical and other professional',
    'm_rcode_pet'='Goods and services: pets',
    'm_rcode_power'='Goods and services: fuel',
    'm_rcode_printed'='Goods and services: books and stationary, printing',
    'm_rcode_religion'='Goods and services: religion-related items',
    'm_rcode_repair'='Goods and services: repairs of  auto and hardware',
    'm_rcode_sports'='Goods and services: fitness',
    'm_rcode_subst'='Goods and services: pan, tobacco, alcohol',
    'm_rcode_toy'='Goods and services: toys',
    'm_rcode_tran'='Goods and services: transportation',
    'm_rcode_waste'='Goods and services: waste management and scrap sales',
    'm_rcode_Manufacturing:oth'='Goods and services: other manufacturing',
    'm_rcode_Reselling:oth'='Goods and services: reselling other goods and services',
    'm_rcode_Services:oth'='Goods and services: other services',
    'm_detcode_Manufacturing:clothes'='Manufacturing: clothes',
    'm_detcode_Manufacturing:computers'='Manufacturing: electronics',
    'm_detcode_Manufacturing:construct'='Manufacturing: construction goods',
    'm_detcode_Manufacturing:decor'='Manufacturing: decoration',
    'm_detcode_Manufacturing:food'='Manufacturing: food',
    'm_detcode_Manufacturing:furniture'='Manufacturing: furniture',
    'm_detcode_Manufacturing:hardware'='Manufacturing: hardware and metal products',
    'm_detcode_Manufacturing:homeware'='Manufacturing: household items',
    'm_detcode_Manufacturing:jewel'='Manufacturing: jewelry',
    'm_detcode_Manufacturing:med'='Manufacturing: medical and pharmaceuticals',
    'm_detcode_Manufacturing:printed'='Manufacturing: books and stationary',
    'm_detcode_Manufacturing:repair'='Manufacturing: spare parts',
    'm_detcode_Manufacturing:subst'='Manufacturing: tobacco, alcohol, pan',
    'm_detcode_Manufacturing:toy'='Manufacturing: toys',
    'm_detcode_Manufacturing:oth'='Manufacturing: other goods',
    'm_detcode_Reselling:beauty'='Retail and wholesale: beauty products',
    'm_detcode_Reselling:clothes+homeware'='Retail and wholesale: clothes, household items',
    'm_detcode_Reselling:computers'='Retail and wholesale: electronics',
    'm_detcode_Reselling:construct'='Retail and wholesale: construction goods',
    'm_detcode_Reselling:decor'='Retail and wholesale: decoration',
    'm_detcode_Reselling:food'='Retail and wholesale: food',
    'm_detcode_Reselling:furniture'='Retail and wholesale: furniture',
    'm_detcode_Reselling:hardware'='Retail and wholesale: hardware and metal products',
    'm_detcode_Reselling:jewel'='Retail and wholesale: jewelry',
    'm_detcode_Reselling:lottery'='Retail and wholesale: lotteries',
    'm_detcode_Reselling:med'='Retail and wholesale: medical and pharmaceuticals',
    'm_detcode_Reselling:power'='Retail and wholesale: fuel',
    'm_detcode_Reselling:printed'='Retail and wholesale: books and stationary',
    'm_detcode_Reselling:religion'='Retail and wholesale: religion-related items',
    'm_detcode_Reselling:repair'='Retail and wholesale: spare parts',
    'm_detcode_Reselling:sports'='Retail and wholesale: sports goods',
    'm_detcode_Reselling:subst'='Retail and wholesale: tobacco, alcohol, pan',
    'm_detcode_Reselling:toy'='Retail and wholesale: toys',
    'm_detcode_Reselling:waste'='Retail and wholesale: scrap',
    'm_detcode_Reselling:oth'='Retail and wholesale: other goods and services',
    'm_detcode_Services:beauty'='Services: beauty services',
    'm_detcode_Services:cleaning'='Services: cleaning and washing',
    'm_detcode_Services:clothes+homeware+computers'='Services: repairs of clothes, household items, electronics',
    'm_detcode_Services:construct'='Services: construction',
    'm_detcode_Services:decor'='Services: decoration',
    'm_detcode_Services:edu'='Services: education',
    'm_detcode_Services:fin'='Services: money services',
    'm_detcode_Services:home'='Services: household staff',
    'm_detcode_Services:housing'='Services: accommodation and housing',
    'm_detcode_Services:med+prof'='Services: medical and other professionals',
    'm_detcode_Services:printed'='Services: printing',
    'm_detcode_Services:repair'='Services: repairs of auto and hardware',
    'm_detcode_Services:sports'='Services: sports',
    'm_detcode_Services:tran'='Services: transport',
    'm_detcode_Services:waste'='Services: waste management',
    'm_detcode_Services:oth'='Services: other services',
    'm_lnage'='ln(years in operation by 2020)', 
    'm_edu_lt_prim'='Education < complete primary',
    'm_edu_prim'='Complete primary education',   
    'm_edu_sec'='Complete secondary education',       
    'm_edu_ter'='Complete tertiary education',
    'm_eduprim'='Complete primary education',   
    'm_edusec'='Complete secondary education',       
    'm_eduter'='Complete tertiary education',
    '(Intercept)'='Constant')
 
## read auxiliary files
xwalk.esm <- openxlsx::read.xlsx(file.path(in_dir,"industrial classification.xlsx"), sheet="ESM second")
xwalk.esis <- openxlsx::read.xlsx(file.path(in_dir,"industrial classification.xlsx"), sheet="ESIS")
xwalk.esis.other <- openxlsx::read.xlsx(file.path(in_dir,"industrial classification.xlsx"), sheet="ESIS other")

esm0 <- foreign::read.dta(file.path(in_dir,"IndiaMicro-2022-full-data.dta"), convert.factors=FALSE)
esis0 <- foreign::read.dta(file.path(in_dir,"India-2022-ESIS-full-data.dta"), convert.factors=FALSE)

## combine ESM with cross-walk
any(is.na(xwalk.esm$exception))
esm <- subset(esm0, b5 < 2020) 
esm$m_size <- esm$ml1
esm$m_size[which(esm$m_size<1)] <- esm$l2[which(esm$m_size<1)]
esm <- subset(esm, m_size<5) 
esm <- replace(esm, esm <0, NA)
esm <- merge(esm, xwalk.esm[,c("id","m_sector","m_prcode","exception")], by="id", all.x=TRUE )
esm <- within(esm, {
  m_prcode[which(m_prcode=="cooked")] <- "food"
  m_prcode[which(m_prcode=="tourism")] <- "oth"  
  ## detailed and rough  
  m_detcode <- paste0(m_sector, ":", m_prcode)
  m_rcode <- m_prcode
  m_rcode[which(m_rcode=="oth")] <- paste0(m_sector[which(m_rcode=="oth")],":oth")
  m_detcode[which(m_sector=="Services" & m_prcode %in% c("med","prof"))] <- "Services:med+prof"
  m_detcode[which(m_sector=="Services" & m_prcode %in% c("clothes","homeware","computers"))] <- "Services:clothes+homeware+computers"
  m_detcode[which(m_sector=="Reselling" & m_prcode %in% c("clothes","homeware"))] <- "Reselling:clothes+homeware"
  m_rcode[which(m_prcode %in% c("med","prof"))] <- "med+prof"
  m_rcode[which(m_prcode %in% c("clothes","homeware","computers"))] <- "clothes+homeware+computers"
  left <- right <- closures0 <- sector0 <- tcode <- m_prcode <-  NULL 
  m_city <- factor(a2, levels=attributes(esm0)$label.table$A2, labels=names(attributes(esm0)$label.table$A2))
  m_edu <- c('lt_prim','lt_prim', 'prim', 'sec','sec','sec', 'ter')[mb12] 
  t_year <- b5 
  t_year[which(t_year < 0)] <- NA
  m_lnage <- log(2020-t_year) 
  closed <- ifelse(COVb1a==1,1,0)
  re_loc <- ifelse(COVir1a==1,1,0)
  re_prod <- ifelse(COVir1b==1,1,0)
  re_ncsal <- ifelse(COVir1c==1,1,0)
  re_lsales <- ifelse(COVd1a==3,1,0) 
  re_benegov <- ifelse(imsc25==1,1,0)
  re_moneylenders <- ifelse(ik4b==1,1,0)
  re_microfin <- ifelse(ik4c==1,1,0)
  re_frel <- ifelse(ik4e==1,1,0)
  re_banks <- ifelse(ik4d==1,1,0) 
  re_bank <- ifelse(mk40==1,1,0)
  })

## combine ESIS with cross-walk
esis <- subset(esis0, b3< 2020)
esis <- within(esis, {
  m_size <- l3
  m_size[which(is.na(m_size))] <- sc2[which(is.na(m_size))] 
}) 

cat("The share of ESIS businesses with over 4 workers is ", mean(subset(esis, m_size>0)$m_size>4))

cat("The share of ESIS businesses with 5-6 workers is ", mean(subset(esis, m_size>0)$m_size %in% 5:6))

esis <- subset(esis, m_size < 5)
esis <- replace(esis, esis<0, NA)
esis <- within(esis, {
  sector <- rep("unk", nrow(esis))
  sector[which(a41a1==1)] <- paste0("man:", a4m[which(a41a1==1)])
  sector[which(a41a1==2)] <- paste0("ret:", a4r[which(a41a1==2)])               
  sector[which(a41a1==3)] <- paste0("ser:", a4s[which(a41a1==3)]) 
})
xwalk.esis <- within(xwalk.esis, {
  sector <- paste0(cat,":",code)
}) 

esis <- merge(esis, xwalk.esis[,c("sector","m_sector","m_prcode","exception")])
esis$left <-1
xwalk.esis.other$right <- 1
esis <- merge(esis, xwalk.esis.other[,c("id","sector0","closures0","tcode","right")], by="id", all=TRUE)
table(esis$right, esis$left, useNA="always")
esis <- within(esis, {
  exception[which(closures0=="exception")] <- 1
  exception[which(closures0=="closed")] <- 0  
  m_sector[which(!is.na(sector0))] <- sector0[which(!is.na(sector0))]
  m_prcode[which(!is.na(tcode))] <- tcode[which(!is.na(tcode))]
  m_prcode[which(m_prcode=="cooked")] <- "food"
## detailed and rough  
  m_detcode <- paste0(m_sector, ":", m_prcode)
  m_rcode <- m_prcode
  m_rcode[which(m_rcode=="oth")] <- paste0(m_sector[which(m_rcode=="oth")],":oth")
  m_detcode[which(m_sector=="Services" & m_prcode %in% c("med","prof"))] <- "Services:med+prof"
  m_detcode[which(m_sector=="Services" & m_prcode %in% c("clothes","homeware","computers"))] <- "Services:clothes+homeware+computers"
  m_detcode[which(m_sector=="Reselling" & m_prcode %in% c("clothes","homeware"))] <- "Reselling:clothes+homeware"
  m_rcode[which(m_prcode %in% c("med","prof"))] <- "med+prof"
  m_rcode[which(m_prcode %in% c("clothes","homeware","computers","clothes+homeware"))] <- "clothes+homeware+computers"
  left <- right <- closures0 <- sector0 <- tcode <- m_prcode <-  NULL 
  m_city <- factor(city, levels=attributes(esis0)$label.table$CITY, labels=names(attributes(esis0)$label.table$CITY))
  m_edu <- c('lt_prim','lt_prim', 'prim', 'sec','sec','sec', 'ter')[b11] 
  t_year <- b3 
  t_year[which(t_year < 0)] <- NA
  m_lnage <- log(2020-t_year) 
  closed <- ifelse(COVb1a==1,1,0)
  exception[which(exception==0.5)] <- NA
  re_loc <- ifelse(COVir1a==1,1,0)
  re_prod <- ifelse(COVir1b==1,1,0)
  re_ncsal <- ifelse(COVir1c==1,1,0) 
  re_lsales <- ifelse(COVd1a==3,1,0) 
  re_benegov <- ifelse(msc25==1,1,0)
  re_moneylenders <- ifelse(k4b==1,1,0)
  re_microfin <- ifelse(k4c==1,1,0)
  re_frel <- ifelse(k4e==1,1,0)
  re_banks <- ifelse(k4d==1,1,0)
  re_bank <- ifelse(k10==1,1,0)
  benegov <- ifelse(msc18==1,1,0)
  unaware <- ifelse(msc22==2,1,0)
  unaware[which(benegov==1)] <- 0
})

esis$formal <- 0 
esm$formal <- 1

## add new variables
 
(vars <- c(grep("^(m|re)\\_", colnames(esis), value=TRUE),  "exception", "formal", "closed",  "wmedian" ))

stargazer::stargazer(esis[setdiff(vars, c('formal','wmedian'))],
                     summary.stat=c("n","mean","sd","min","max"),
                     out=file.path(out_dir, "summary_stats_ESIS.tex"))
stargazer::stargazer(esm[setdiff(vars, c('formal','wmedian'))],
                     summary.stat=c("n","mean","sd","min","max"),
                     out=file.path(out_dir, "summary_stats_ESM.tex"))

df <- subset(rbind(esis[vars], esm[vars]), !is.na(m_sector) )
row.names(df) <- seq_len(nrow(df)) 

df$m_detcode <- factor(df$m_detcode)
df$m_rcode <- factor(df$m_rcode) 
df$m_edu[ df$m_edu<0 ] <- NA 
df$m_edu <- ordered(df$m_edu, levels=c('lt_prim','prim','sec','ter')) 

## add weights
totals <- c(ESIS=31783, ESM=5765) 
weights <- lapply(split(df, df$formal), function(u) u$wmedian/mean(u$wmedian))
df$wt.tr <- do.call("c", weights)
infl <- totals/sapply(weights, sum)
swt <- do.call("c", lapply(seq_along(weights), function(u) weights[[u]]*infl[u]))
df$wmedian <- NULL
df$wt <- swt/mean(swt) 
df$wt[df$wt>5] <- 5

## select variables to be used
dryMice <- mice::mice(data = df, m = 1 , maxit = 0)
prmat <- dryMice$predictorMatrix
for (v in c("wt","wt.tr","m_detcode")) {
  prmat[,v] <- 0
  prmat[v,] <- 0 
}

df.i <- mice::mice(df, m=40, predictorMatrix=prmat)

## which types of activity were more/less likely to stop
tapply(df$closed, df$formal, mean)


tb <- lapply(split(df, df$formal), function(u) {
  u$m_detcode <- as.character(u$m_detcode)
  temp <-  dplyr::summarize(
      dplyr::group_by(subset(u, !is.na(closed)), m_detcode), 
      closed=mean(closed), obs=dplyr::n()
      ) 
   temp[order(temp$closed),]
})
lapply(tb, head, n=10L)
lapply(tb, tail, n=10L)

tbc <- merge(tb[[1]],tb[[2]], by="m_detcode", all=TRUE, suffixes=c(".informal",".formal"))
tbc <- tbc[order(tbc$m_detcode),]
temp_labs <- sect.labs[grepl("^m\\_detcode", names(sect.labs))]
names(temp_labs) <- gsub("m\\_detcode\\_","",names(temp_labs))
tbc$activity <- temp_labs[tbc$m_detcode]
tbc <- within(tbc, { 
  closed.informal[which(obs.informal<10|is.na(obs.informal))] <- NA
  closed.formal[which(obs.formal<10|is.na(obs.formal))] <- NA
  u2f <- closed.formal*(1-closed.formal)/obs.formal
  u2i <- closed.informal*(1-closed.informal)/obs.informal
  z <- (closed.formal-closed.informal)/sqrt(u2f + u2i)
  sig <- ifelse(abs(z)>=qnorm(0.995), '**', ifelse(abs(z)>=qnorm(0.975), '*', ifelse(abs(z)>=qnorm(0.95), '\u2020', '')))
})
tbcx <- subset(tbc, obs.formal>=10|obs.informal>=10, 
               select=c("activity","closed.informal","obs.informal","closed.formal","obs.formal",'z'))
 
print(xtable::xtable(tbcx, type = "latex"), 
      file = file.path(out_dir,"survey_agg.tex"), include.rownames=FALSE, tabular.environment="longtable")
## make a figure with bars instead
tbc <- subset(tbc, obs.formal>=10|obs.informal>=10)
tbc <- within(tbc, {
  ## remove the averages with too few observations 
  closed.formal[which(obs.formal<10|is.na(obs.formal))] <- 0
  closed.informal[which(obs.informal<10|is.na(obs.informal))] <- 0 
  sig[which(obs.formal<10|is.na(obs.formal))] <- ''
  sig[which(obs.informal<10|is.na(obs.informal))] <- ''
})

tbc$sorter <- tbc$closed.informal
tbc$sorter[which(tbc$sorter==0)] <- tbc$closed.formal[which(tbc$sorter==0)]
tbc <- tbc[order(tbc$sorter),]
tbc$sorter <-  factor(nrow(tbc):1L, levels=1L:nrow(tbc), labels=rev(tbc$activity)) 
tbc.l <- tidyr::pivot_longer(tbc, cols=starts_with(c("closed","obs")))
tbc.l$vari <- sapply(strsplit(tbc.l$name, "\\."), "[",1)
tbc.l$sector <- sapply(strsplit(tbc.l$name, "\\."), "[",2)
tbc.l$name <- NULL
tbc.l <- tidyr::pivot_wider(tbc.l, names_from=vari, values_from=value)
tbc.l$sig[which(tbc.l$sector=='informal')] <- ''
  
pic <- ggplot(tbc.l, aes(y=closed, x=sorter, fill=sector)) +
  geom_bar(position="dodge", stat="identity", color="blue", width=0.6) +
  scale_fill_manual(breaks=c("informal","formal"), values=c("blue","white"))+
  scale_y_continuous(labels=scales::percent)+
  geom_text(aes(label=sig), y=1.03) +
  expand_limits(y=1.05) + coord_flip() +
  labs(x=element_blank(), y="Suspended operation during pandemic", fill="Sector") +
  theme(legend.position="bottom")
ggsave(file.path(out_dir,"figure1.pdf"), pic, height=10, width=8) 


### matching
## exclude types of activities that are specific to either formal or informal sector
df2 <- within(df, t_rcode <- as.character(df$m_rcode))
(uniqsectors <- tapply(df2$t_rcode, df2$formal, unique))
names(uniqsectors) <- NULL
keeper <- do.call("intersect", uniqsectors)
df2 <- subset(df2, t_rcode %in% keeper)
df2 <- within(df2, {
   m_rcode <- droplevels(m_rcode)
   m_detcode <- droplevels(m_detcode)
})

df2.i <- mice::mice(df2, m=40, predictorMatrix=prmat)

## sampling weights
## total number of businesses in sampling frames according to the ESIS and ESM documentation

matched.datasets <- matchthem(formal ~ m_sector +  m_rcode + m_detcode + m_lnage + m_edu,
                              df2.i,
                              exact= ~ m_city + exception,
                              approach = 'within',
                              distance='mahalanobis',
                              method="nearest"
                              )

smd0 <-  bal.tab(matched.datasets, un=TRUE, stats= "mean.diffs", quick=FALSE)
temp <- lapply(smd0[["Imputation.Balance"]], function(u) u[["Balance"]][,c("Diff.Un", "Diff.Adj", "M.0.Un", "M.0.Adj", "M.1.Un", "M.1.Adj")])
temp <- Reduce(`+`, temp)/6

smd.df <- data.frame(vari=row.names(temp), temp)
sorter <- match(intersect(names(sect.labs),smd.df$vari),smd.df$vari,  nomatch=0) 
smd.df <- smd.df[sorter,]
smd.df$vari <- sect.labs[smd.df$vari] 
print(xtable::xtable(smd.df, type = "latex"), file = file.path(out_dir,"balance.tex"), include.rownames=FALSE, tabular.environment="longtable")
 
smd <-  smd0[["Balance.Across.Imputations"]]
smd$vari <- row.names(smd)

## split into two pieces -- one with detailed classification, another with the other variables
## vertical lines at -0.1 and 0.1
smd.l <- tidyr::pivot_longer(smd, cols= starts_with(c("Min","Max","Mean")))
smd.l <- within(smd.l, {
  var <- sapply(strsplit(name, "\\."), "[", 1)
  when <- sapply(strsplit(name, "\\."), "[", 3) 
  Type <- name <- NULL
}) 
smd.w <- tidyr::pivot_wider(smd.l, names_from = var, values_from=value )

second.vars <- grep("^m\\_detcode", smd$vari, value=TRUE)
first.vars <- setdiff(smd$vari, second.vars)

temp <- subset(smd.w, vari %in% first.vars)
temp_labs <- rev(sect.labs[temp$vari])
any(is.na(temp_labs))
temp$vari <- factor(temp$vari, levels=names(temp_labs), labels=temp_labs)

pic <- ggplot(temp, aes(y=vari)) +
  geom_point(aes(x=Mean,  shape=when)) +
  geom_linerange(aes(xmin=Min, xmax=Max)) +
  scale_shape_manual(values=c(16, 0), breaks=c("Adj","Un"), labels=c("After matching", "Before matching")) +
  geom_vline(xintercept=c(-0.1,0.1), linetype="dashed") +
  labs(x="Difference in group means", y=element_blank(), shape=element_blank()) +
  theme(legend.position="bottom")
ggsave(file.path(out_dir,"balance1.pdf"), pic, width=7, height=7)   

temp <- subset(smd.w, vari %in% second.vars)
temp_labs <- sect.labs[temp$vari] 
any(is.na(temp_labs))
temp$vari <- factor(temp$vari, levels=rev(names(temp_labs)), labels=rev(temp_labs))

pic <- ggplot(temp, aes(y=vari)) +
  geom_point(aes(x=Mean,  shape=when)) +
  geom_linerange(aes(xmin=Min, xmax=Max)) +
  scale_shape_manual(values=c(16, 0), breaks=c("Adj","Un"), labels=c("After matching", "Before matching")) +
  geom_vline(xintercept=c(-0.1,0.1), linetype="dashed") +
  labs(x="Difference in group means", y=element_blank(), shape=element_blank()) +
  theme(legend.position="bottom")
ggsave(file.path(out_dir,"balance2.pdf"), pic, width=7, height=7)   

## analysis with the matched and imputed data
### estimation

ests <- list()
models <- with(df.i, lm(closed ~  formal)) 
(ests[["lpm.2.1"]] <- summary(pool(models)))

models <- with(df.i, lm(closed ~  formal + exception)) 
(ests[["lpm.2.2"]] <- summary(pool(models)))

models <- with(df.i, lm(closed ~  formal + exception +  m_sector + m_rcode + m_detcode + m_lnage + 
                          m_edu+ m_city, contrasts=list(m_edu="contr.treatment"))) 
(ests[["lpm.2.3"]] <- summary(pool(models))) 

models <- with(matched.datasets, lm(closed ~  formal ))
(ests[["lpm.1.1"]] <- summary(pool(models)))

models <- with(matched.datasets, lm(closed ~  formal + exception ))
(ests[["lpm.1.2"]] <- summary(pool(models)))

models <- with(matched.datasets, lm(closed ~  formal + exception +  m_sector + m_lnage)) 
(ests[["lpm.1.3"]] <- summary(pool(models)))

for (u in names(ests)) {
  ests[[u]][["model"]] <- u
  ests[[u]][["term"]] <- as.character(ests[[u]][["term"]]) 
}
ests.df <- do.call("rbind", ests)
ests.df <- subset(ests.df, !is.na(estimate))
ests.df$term <- gsub("[\\_+:]","", ests.df$term)
temp_labs <- sect.labs
names(temp_labs) <- gsub("[\\_+:]","",names(temp_labs))
ests.df$term.lab <- temp_labs[ests.df$term]
ests.df$modeln <- substring(ests.df$model, 5)

main.df <- subset(ests.df, !grepl("^m(rcode|detcode|city)", term)) 
main.df <- within(main.df, { 
  stars <- ifelse(p.value <=0.01,"**", ifelse(p.value <=0.05,"*", ifelse(p.value <=0.1,"\\dagger","" )))
  val.1 <- paste0(formatC(estimate, digits=2L, format='f'), stars)
  val.2 <- paste0("(", formatC(std.error, digits=2L, format='f'), ")")
  modeln <- NULL
})
main.df <- as.data.frame(
  tidyr::pivot_wider(
    tidyr::pivot_longer(main.df[c("term","term.lab","model","val.1","val.2")],  cols=starts_with("val")),
    names_from=model, values_from=value)
)
main.df$subrow <- as.numeric(sapply(strsplit(main.df$name, "\\."), "[", 2))
main.df$term.lab[which(main.df$subrow==2)] <- ""

sorter <- match( main.df$term, names(temp_labs),  nomatch=0) 
main.df <- main.df[order(sorter, main.df$subrow),c("term.lab","lpm.1.1","lpm.1.2","lpm.1.3","lpm.2.1","lpm.2.2","lpm.2.3")]
print(xtable::xtable(main.df, type = "latex"), file = file.path(out_dir,"table1.tex"), include.rownames=FALSE)

sorter <- match( ests.df$term, names(temp_labs),  nomatch=0) 
ests.df <- ests.df[order(sorter, ests.df$model),c("term.lab","modeln","estimate","std.error","df","p.value")] 
print(xtable::xtable(ests.df, type = "latex"), file = file.path(out_dir,"survey_est_long.tex"), include.rownames=FALSE, tabular.environment="longtable")

## use other outcome variables
vs <- c(
  'closed'='Suspended operations during pandemic',
  're_loc'='Changed location in response to pandemic',
  're_prod'='Changed  products/services in response to pandemic',
  're_ncsal'='Adapted contactless sales in response to pandemic',
  're_bank'='The owner has a bank account to run this business',
  're_moneylenders'='Used moneylenders to finance operations',
  're_microfin'='Used microfinance institutions to finance operations', 
  're_banks'='Used banks to finance operations',
  're_frel'='Used friends or relatives to finance operations', 
  're_benegov'='Received benefits from government'
)

diffs <- lapply(names(vs), function(v) { 
  e <- paste0("ms <- with(matched.datasets, lm(", v, " ~ formal))")
  eval(parse(text = e)) 
  s <- summary(pool(ms)) 
  data.frame(dv=vs[v], formal=s[1L,'estimate'] + s[2L,'estimate'], informal=s[1L,'estimate'],
             difference=s[2L,'estimate'], se = s[2L,'std.error'], p.value=s[2L,'p.value'])
})
diffsg <- do.call('rbind', diffs)  
diffsg <- within(diffsg, {
  stars <- ifelse(p.value <=0.01,"**", ifelse(p.value <=0.05,"*", ifelse(p.value <=0.1,"\\dagger","" )))
  diff <- paste0(formatC(difference, 2L, format='f', flag='+'), ' (',
                 formatC(se, 2L, format='f'), ')',
                 stars)
  p.value <- stars <- difference <- se <- NULL
})
print(xtable::xtable(diffsg, type = "latex", digits=2L), 
      file = file.path(out_dir,"table2.tex"), include.rownames=FALSE)

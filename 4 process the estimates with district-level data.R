
library(coda)
library(ggplot2)
library(patchwork)

rm(list=ls())
options(stringsAsFactors = FALSE)

theme_set(theme_bw() + theme(strip.text.x =element_text(size=12, family="serif"), 
                             strip.text.y =element_text(size=12, family="serif"),  
                             axis.title.x =element_text(size=12, family="serif"), 
                             axis.title.y =element_text(size=12, family="serif"), 
                             axis.text=element_text(size=11, family="serif"),
                             legend.text=element_text(size=12, family="serif"),
                             strip.background = element_rect(fill="white")))

out_dir <- "./output" 
im_dir <- "./temp"
in_dir <- "./data"

df <- readRDS(file.path(in_dir,"India_sample2.rds"))
probs <-  c((1-0.89)/2,1-(1-0.89)/2)

samples.list <- readRDS(file=file.path(im_dir,"postsamB.ml-India.mdv2.rds"))
diag <- lapply(samples.list, gelman.diag)
lapply(diag, function(x) x[["mpsrf"]])


dic <- readRDS(file=file.path(im_dir,"dicB.ml-India.mdv2.rds")) 
temp <- readRDS(file=file.path(im_dir,"nobsB.ml-India.mdv2.rds"))
nobs <- sapply(temp, "[[", "X")
nsta <- sapply(temp, "[[", "A")
lab.list <- readRDS(file=file.path(im_dir,"labsB.ml-India.mdv2.rds"))

varlabs <- c(
  int="11|Intercept",
  s_ppi="6|Protective policy index",
  s_ppi.2="6|Protective policy index",
  s_ppi.3="6|Protective policy index",
  s_ownrev="6|State's own revenue/ GDP",
  s_police_pc="6|Police strength per 100,000",
  h_ent_size="1|Avg establishment size",
  h_mpi="7|Multidimensional poverty index",
  h_mm_any="8|Any major morbidity",
  h_mm_diabetes="8|Prev of diabetes",
  h_mm_asthma="8|Prev of asthma",
  h_mm_thyroid ="8|Prev of thyroid diseases",
  h_mm_heart="8|Prev of heart diseases",
  h_mm_cancer="8|Prev of cancer",
  h_ag_o65="7|Prop over 65 years old",
  h_ag_25t44="7|Prop 25 to 44 years old",
  h_ag_b25="7|Prop under 25 years old",
  h_literacy="7|Prop literate",
  h_lnpopden="7|Population density (ln)",
  h_urbanization="7|Urbanization", 
  h_contract="2|Empl with written contracts", 
  h_elg_supp="2|Elig to paid leav/soc secty",
  h_regsal="2|Regular salaried employees", 
  h_a_contract="2|Empl with written contracts", 
  h_a_elg_supp="2|Elig to paid leave/soc secty",
  h_a_regsal="2|Regular salaried employees", 
  h_comhq="1|Ests with comm HQ",
  h_years_edu="7|Avg years of formal education",
  dic="DIC",
  nsta="Number of states",
  nobs="Number of observations",
  grocery="grocery stores",
  parks="Visits to parks",
  residential="residential areas",
  retail="retail and recreational areas", 
  transit="transit stations",   
  workplaces="places of work",
  h_excluded="3|Share of exempt activities",
  'h_regsal:h_excluded'= "5|Regular salaried employees X Exempt activities" ,
  'h_contract:h_excluded'="5|Empl with a written contract X Exempt activities",
  'h_elg_supp:h_excluded'="5|Elig to paid leave/soc sec X Exempt activities",
  'h_ent_size:h_excluded'="4|Average establishment size X Exempt activities",
  'h_comhq:h_excluded'="4|Ests with commercial HQ X Exempt activities", 
  '(Intercept)'="11|(Intercept)"
)

labs.df <- rbind(
  data.frame(type=1, var=paste0("theta[",names(varlabs), "]"), lab=paste0("\\multirow{2}{*}{$\\theta$[", varlabs,"]}")),
  data.frame(type=1, var=paste0("lambda[",names(varlabs), "]"), lab=paste0("\\multirow{2}{*}{$\\lambda$[", varlabs,"]}")),
  data.frame(type=1, var=names(varlabs), lab=paste0("\\multirow{2}{*}{", varlabs,"}")) 
)
for (v in c("dic","nsta","nobs")) {
  labs.df$lab[which(labs.df$var==v)] <- varlabs[v]
}
rlabs <- setNames(labs.df$lab, labs.df$var)

sam.nam <- names(samples.list)
names(sam.nam) <- sam.nam

mod.guide <- list(
  "main models without interaction terms" = paste0("1.", 1:6, ".1"),
  "main models with interaction terms" = paste0("1.", 1:6, ".2"),  
  "models with additional covariates" = paste0("2.", 1:6, ".1"),  
  "models with additional covariates and interaction terms" = paste0("2.", 1:6, ".2"),
  "the measures of informal economy ignore rule exceptions" = paste0("3.", 1:6, ".1")
)

y <- lapply(sam.nam, function(j) {
  ## summarize  
  rhat <- gelman.diag(samples.list[[j]])[["psrf"]][,1]
  rhat.df <- data.frame(coef=names(rhat), type=3, value=formatC(rhat, digits=2, format='f'))
  z <- do.call("rbind", samples.list[[j]]) 
  means.text <- formatC(colMeans(z), digits=2, format='f')
  means.text["theta[marker]"] <- "0"
  means.text["lambda[marker]"] <- "1" 
  means.text["dic"] <- formatC(dic[j], digits=1, format='f')
  means.text["nsta"] <- formatC(nsta[j], digits=0, format='f')
  means.text["nobs"] <- formatC(nobs[j], digits=0, format='f')
  ci <- t(apply(z, 2, quantile, probs=probs))
  ci.text <- setNames(paste0("[", 
                             formatC(ci[,1], digits=2, format='f'), ";", 
                             formatC(ci[,2], digits=2, format='f'), "]"),
                      rownames(ci)) 
  ## labs
  la <- data.frame(
    model =j,
    coef= c(
    paste0("theta[", seq_along(lab.list[[j]]$theta),"]"),  "theta[marker]",
    paste0("lambda[", seq_along(lab.list[[j]]$lambda),"]"), "lambda[marker]",
    paste0("kappa[", seq_along(lab.list[[j]]$lambda),"]"), "kappa[6]",
    paste0("beta[", seq_along(lab.list[[j]]$beta),"]"), 
    "tau.int", "tau", 'dic', 'nsta', 'nobs'),
    var=c(
      paste0("$\\theta$[", varlabs[lab.list[[j]]$theta],"]"), "$\\theta$[places of work]",
      paste0("$\\lambda$[", varlabs[lab.list[[j]]$lambda],"]"),  "$\\lambda$[places of work]",
      paste0("$\\tau(\\upsilon)$[", varlabs[lab.list[[j]]$lambda],"]"), "$\\tau(\\upsilon)$[places of work]",
      varlabs[lab.list[[j]]$beta],
      "$\\tau(\\xi)$", "$\\tau(\\epsilon)$", 'DIC', 'N (states)', 'N (observations)'))
  la <- within(la, {
    value.1 <- means.text[coef]
    value.2 <- ci.text[coef]
    part <- ifelse(grepl("^(theta|lambda|kappa)", coef), 'me',ifelse(coef %in% c('dic', 'nsta', 'nobs'), 'ms','co'))
    
    row <- 100*(1:nrow(la))
    coef <- NULL 
  })
  reshape(la, direction="long", idvar="row", varying=c("value.1","value.2"),
          v.names="value", timevar="type") 
})

y <- do.call("rbind",y)
rownames(y) <- NULL
y <- subset(y, !is.na(value))

## print
outf <- file.path(out_dir, "est_mdv.txt")
write("Tables of estimates: main model\n", file = outf)
for (nam in names(mod.guide)) { 
  models <- sort(mod.guide[[nam]])
  temp <- subset(y, model %in% models)
  temp$row <- tapply(temp$row, temp$var, FUN=max)[temp$var]
  p1 <- sapply(strsplit(temp$var, "\\|"), '[', 1L)
  p2 <- sapply(strsplit(temp$var, "\\|"), '[', 2L)
  temp$var[which(!is.na(p2))] <- p2[which(!is.na(p2))]
  temp$row[which(!is.na(p2))] <- as.numeric(p1[which(!is.na(p2))])
  temp.w <- tidyr::pivot_wider(temp, names_from=model, values_from=value) 
  con <- temp.w[,models]
  temp.w$value <- paste0(
    ifelse(temp.w$type==1, temp.w$var, ""), "&",
    apply(replace(con , is.na(con), ""), 1L, paste, collapse="&"),
    ifelse(temp.w$type==1, "\\\\*", "\\\\")
    )
  temp.w <- temp.w[with(temp.w, order(row, var, type)),]

  h <- paste0(" Parameter&", paste("\\multicolumn{1}{c}{(",models,")}", sep="", collapse="&"), '\\\\\\hline')
  cn <- paste0("
\\begin{longtable}{lcccccc}
\\caption{Mobility Response to NMIs, Parameter Estimates, ", nam, "}\\\\
\\hline", h, "\\endfirsthead\\hline", h, "\\endhead
\\hline\\multicolumn{", 1L+length(models),"}{r}{Continued on next page}\\\\
\\endfoot
\\hline\\hline
\\hline \\multicolumn{", 1L+length(models),"}{r}{Posterior means with 89\\% central posterior intervals in parentheses.}\\\\
\\endlastfoot
")
  write(cn, file = outf, append=TRUE)
  write(paste0("\\multicolumn{", 1L+length(models),"}{l}{Measurement component}\\\\\\hline"), file = outf, append=TRUE)
  write(subset(temp.w, part=='me', select='value', drop=TRUE), file = outf, append=TRUE)
  write(paste0("\\hline\\multicolumn{", 1L+length(models),"}{l}{Regression component}\\\\\\hline"), file = outf, append=TRUE)
  write(subset(temp.w, part=='co', select='value', drop=TRUE), file = outf, append=TRUE)
  write(paste0("\\hline\\multicolumn{", 1L+length(models),"}{l}{Model statistics}\\\\\\hline"), file = outf, append=TRUE)
  write(subset(temp.w, part=='ms', select='value', drop=TRUE), file = outf, append=TRUE)
  write("\\end{longtable}", file = outf, append=TRUE)
}
 
## estimated effect
touse <- grep("^1\\.[0-9]+\\.2$", names(samples.list), val=TRUE)
sel <- c("h_ent_size"=1,
         "h_comhq"=0.01,
         "h_regsal"=0.01,
         "h_elg_supp"=0.01,
         "h_contract"=0.01)

bulk <- lapply(touse, function(n) { 
  lambdas <- as.matrix(samples.list[[n]][,paste0("lambda[", seq_along(lab.list[[n]]$lambda),"]")])
  lambdas <- base::cbind(lambdas, 1.0)
  tsel <- intersect(names(sel), lab.list[[n]]$beta) 
  betas <- as.matrix(samples.list[[n]][,paste0("beta[", match(tsel, lab.list[[n]]$beta),"]")])
  r <- lapply(seq_along(tsel), function(s) {
    v <- exp(sel[tsel[s]]*betas[,s]*lambdas)-1 
    ci <- apply(v, 2L, quantile, probs=probs)
    data.frame(dv = c(lab.list[[n]]$lambda, "workplaces"),
               predictor = tsel[s],
               yhat = colMeans(v),
               lb = ci[1L,],
               ub = ci[2L,],
               model = n)
  })
  do.call('rbind', r)
})  
bulk$make.row.names <- NULL
bulk <- do.call("rbind", bulk)

prfx = "Predicted median change in community mobility\nassociated with a "
figlabs0 <-
  c(h_ent_size="1 worker increase in the average establishment size",
    h_comhq ="1 p.p. increase in the share of establishments with commercial HQ",
    h_regsal ="1 p.p. increase in the share of regular salaried employees",
    h_contract ="1 p.p. increase in the share of workers with a written contract",
    h_elg_supp ="1 p.p. increase in the share of workers eligible to paid leave or soc security"
  )  
figlabs <- setNames(paste0(prfx, figlabs0), names(figlabs0))
mlabs <- c(
  residential = "time\nspent in\nresidential\nareas",
  retail = "visits to\nretail and\nrecreational\nareas", 
  transit = "visits to\ntransit\nstations",
  workplaces = "visits to\nplaces\nof work",
  parks = "visits to\nparks",
  grocery = "visits to\ngrocery\n stores"
)

plabs <- c(h_ent_size="number of\nworkers",
    h_comhq ="businesses\nwith\ncommercial HQ\n(percentages)",
    h_regsal ="regular\nsalaried\nemployees\n(percentages)",
    h_contract ="workers with\nwritten\ncontracts\n(percentages)",
    h_elg_supp ="workers\neligible to\nbenefits\n(percentages)"
  )  
bulk$plab <- factor(bulk$predictor, levels=names(plabs), labels=plabs)

pic <-  ggplot(data=bulk, aes(x=model)) +
  geom_bar(aes(y=yhat), stat='identity', fill='lightgreen', color='black', width=0.5) +
  geom_linerange(aes(ymin=lb, ymax=ub))+
  geom_hline(yintercept=0) +
  scale_y_continuous(label=scales::percent) + 
  facet_grid(plab ~  dv, scale='free', labeller=labeller(dv=mlabs)) +
  theme(axis.text.x = element_text(angle=90), strip.text.y = element_text(angle = 0)) +
  labs(y=element_blank(), x='Model')
ggsave(file.path(out_dir,"figure3.pdf"), pic, width=7, height=9)


## estimated effect -- numbers
touse <- grep("^1\\.[0-9]+\\.2$", names(samples.list), val=TRUE)
sel <- c("h_regsal"=0.01, 
         "h_ent_size"=1,
         "h_comhq"=0.01,
         "h_elg_supp"=0.01,
         "h_contract"=0.01)
# second vector for the full range change... 
rg <- sapply(subset(df, metric=='workplaces' & !is.na(h_regsal), select=names(sel)),range) 
rg <- rg[2L,]-rg[1L,]
  
bulk <- lapply(touse, function(n) { 
  lambdas <- as.matrix(samples.list[[n]][,paste0("lambda[", seq_along(lab.list[[n]]$lambda),"]")])
  lambdas <- base::cbind(lambdas, 1.0)
  tsel <- intersect(names(sel), lab.list[[n]]$beta) 
  A <- diag(sel[tsel])
  B <- diag(rg[tsel])
  betas <- as.matrix(samples.list[[n]][,paste0("beta[", match(tsel, lab.list[[n]]$beta),"]")]) 
  u <- A %*% t(betas) %*% lambdas / nrow(betas)
  ut <- B %*% t(betas) %*% lambdas / nrow(betas)
  r <- apply(u, 2L, function(v) {
    paste0(formatC(100*(exp(v)-1), digits=2, format='f'), '%')
  })
  r <- as.data.frame(r)
  rt <- apply(ut, 2L, function(v) {
    paste0(formatC(100*(exp(v)-1), digits=2, format='f'), '%')
  })
  rt <- as.data.frame(rt)
  colnames(rt) <- colnames(r) <- c(lab.list[[n]]$lambda, "workplaces") 
  
  rt$predictor <- r$predictor <- varlabs[tsel]
  rt$model <- r$model <- n 
  r$change <- 'unit'
  rt$change <- 'range'
  r <- rbind(r, rt)
  r[,c('predictor','change','model',lab.list[[n]]$lambda, "workplaces")]
})
effects <- do.call('rbind', bulk)
effects <- effects[with(effects, order(predictor,change, model)), ]
print(xtable::xtable(effects, type = "latex", digits=3L), 
      file = file.path(out_dir,"effects.tex"), include.rownames=FALSE)
write.csv(effects, file.path(out_dir,"effects.csv"))




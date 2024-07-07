
rm(list=ls())
options(stringsAsFactors = FALSE)

out_dir <- "./output"
im_dir <- "./temp"
in_dir <- "./data"

df <- readRDS(file.path(in_dir, "India_sample2.rds"))
 
### reshape and remove rows with missing values in workplaces and h_regsal
metrics <- sort(unique(df$metric))
names(metrics) <- metrics

dfx <- df
dfx$value <- NULL
dfx <- as.data.frame(tidyr::pivot_wider(dfx, names_from=metric, values_from=logratio))
dfx <- subset(dfx, !is.na(workplaces)&!is.na(h_regsal))

### scale all predictors except the ones that will be need to vary at the interpretation stage
expre  <-  c("h_regsal","h_contract","h_elg_supp",
            "h_a_regsal","h_a_contract","h_a_elg_supp", 
            "h_ent_size","h_comhq","h_excluded","s_ownrev")
for (v in setdiff(colnames(dfx), c("iso_state","district_num", expre, metrics))) { 
  dfx[[v]] <- (dfx[[v]]-mean(dfx[[v]], na.rm=TRUE))/sd(dfx[[v]], na.rm=TRUE)
}

stargazer::stargazer(dfx,
                     summary.stat=c("n","mean","sd","min","max"),
                     out=file.path(out_dir, "summary_districts.tex"))

### main predictors
ivs.p <- c("h_regsal","h_contract","h_elg_supp") 
ivs.o <- c("h_ent_size","h_comhq") 
ivs.main <- c(ivs.p, ivs.o, "h_excluded","s_ownrev") ## don't scale

ivs.df <- as.data.frame(expand.grid(list(ivs.p, ivs.o))) 
ivs <- apply(ivs.df, 1L, paste, collapse='+')
xvs <- c(
  setNames(paste0(ivs, ' + h_excluded'), paste0(seq_along(ivs),'.1')),
  setNames(paste0('(', ivs, ')*h_excluded'), paste0(seq_along(ivs),'.2'))
) 
 
hs <- list()
hs[["main"]] <- c("s_ownrev", "s_ppi", "h_mpi", "h_years_edu", "h_lnpopden", "h_urbanization", "h_ag_o65", "h_ag_25t44", "h_ag_b25")
hs[["long"]] <- c("s_ownrev", "s_ppi", "h_mpi", "h_years_edu", "h_lnpopden", "h_urbanization", 
                  "h_ag_o65", "h_ag_25t44", "h_ag_b25",
                  "h_mm_diabetes", "h_mm_asthma", "h_mm_thyroid", "h_mm_heart","h_mm_cancer")
dv <- 'logratio'  
  
frm <- list()
for (d in seq_along(hs)) {
  for (i in names(xvs)) {
    frm[[paste0(d,'.',i)]] <- as.formula(
      paste0(dv, '~',
             paste(c(xvs[i], hs[[d]]), collapse='+')))
  }
}

## the model without exception
ivs2.p <- c("h_a_regsal","h_a_contract","h_a_elg_supp") 
ivs.o <- c("h_ent_size","h_comhq") 
ivs2.df <- as.data.frame(expand.grid(list(ivs2.p, ivs.o))) 
ivs2 <- apply(ivs2.df, 1L, paste, collapse='+')
xvs2 <- c(
  setNames(paste0(ivs2, ' + h_excluded'), paste0(seq_along(ivs),'.1'))
) 
 
for (i in names(xvs2)) {
  frm[[paste0('3.',i)]] <- as.formula(
    paste0(dv, '~', paste(c(xvs2[i], hs[["main"]]), collapse='+')))
} 

output2 <- function(f, data) { 
  dv <- as.character(f[[2L]])
  pf <- f
  pf[[2L]] <- NULL
  Y.df <- subset(data, complete.cases(data[, all.vars(pf)]),
                 select= c("iso_state","district_num",metrics, all.vars(pf))) 
  Y.coln <- c(setdiff(metrics, "workplaces"), 'workplaces')
  Y.mat <- as.matrix(Y.df[,Y.coln])
  X.mat <- model.matrix(pf, Y.df)
  states <- sort(unique(Y.df$iso_state))
  sid <- match(Y.df$iso_state, states)
  list(dt=list(sid=sid,
               Y=Y.mat,
               X=X.mat,
               k=length(unique(sid)),
               ones.beta = diag(rep(1, ncol(X.mat))),
               ones.lambda = diag(rep(1, length(Y.coln)-1))),
       labs=list(theta=setdiff(metrics, "workplaces"), lambda=setdiff(metrics, "workplaces"), beta=colnames(X.mat))
       )
}
 
mydata.list <- list()
labs.list <- list() 
obs.list <- list() 
for (nam in names(frm)) {  
    out <- output2(f=frm[[nam]], data=dfx) 
    mydata.list[[nam]] <- out[["dt"]]
    obs.list[[nam]] <- list(A=length(unique(out[["dt"]][["sid"]])), X=nrow(out[["dt"]][["X"]]))
    labs.list[[nam]] <- out[["labs"]]
} 

saveRDS(mydata.list, file=file.path(im_dir, "data.mdv2.rds"))
saveRDS(labs.list, file=file.path(im_dir, "labsB.ml-India.mdv2.rds"))
saveRDS(obs.list, file=file.path(im_dir, "nobsB.ml-India.mdv2.rds"))


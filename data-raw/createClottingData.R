clotting = read.delim("./inst/extdata/clotting.tsv.INR.class.test.pred.result", col.names = c("obs","predicted","abnormal","normal"), header=FALSE)

inr.obs = clotting %>% mutate(obs = ifelse(obs==1,"abnormal","normal")) %>% pull(obs)
inr.predictions = clotting %>% select("abnormal","normal")

usethis::use_data(inr.obs,inr.predictions)

usethis::use_package("ggplot2")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("magrittr")
usethis::use_package("ggpubr")
usethis::use_package("factoextra")




get_MCA_plot<-function(experiment){
  expm_mca = experiment %>% dplyr::select(-response_id)
  expm_mca<-tidyr::as_tibble(sapply(expm_mca  , as.factor))
  res.mca <- FactoMineR::MCA(expm_mca,graph = F)
  factoextra::fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))
}



ballon_plot<-function(a,b,exp){
  tabla=prop.table(table(as.vector(unlist(exp[a])),as.vector(unlist(exp[b])))) %>% as.data.frame()
  ggpubr::ggballoonplot(tabla, fill = "value")+
    ggplot2::scale_fill_viridis_c(option = "C")+ theme(legend.position = "none")
}


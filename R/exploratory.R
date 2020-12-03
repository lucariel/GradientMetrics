usethis::use_package("ggplot2")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("magrittr")
usethis::use_package("ggpubr")
usethis::use_package("factoextra")


#' get_MCA_plot
#' Performs a multicorrelation analysis
#' on the experimient variables to
#' see if there is a pattern in the distribution
#' @importFrom magrittr %>%
#' @param experiment data.frame with "response_id" column
#'
#' @return ggplot2 object
#'
#' df = data.frame(
#'    response_id = runif(100),
#'    a = runif(100),
#'    b = runif(100)
#' )
#' get_MCA_plot(df)
#' @export
get_MCA_plot<-function(experiment){
  expm_mca = experiment %>% dplyr::select(-'response_id')
  expm_mca<-tidyr::as_tibble(sapply(expm_mca  , as.factor))
  res.mca <- FactoMineR::MCA(expm_mca,graph = F)
  factoextra::fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))
}


#'ballon_plot
#'Creates the ballon plot to
#'visualize how each phrase of the
#'experimient affects the answer
#' @importFrom magrittr %>%
#' @param a Parameter selected for the columns
#' @param b Parameter selected for the rows
#' @param exp Dataframe containing cols "a" and "b"
#' @example
#'  a = c(rep(x = 'red',round(runif(1)*100,0)),rep(x = 'blue',round(runif(1)*100,0)),rep(x = 'yellow',round(runif(1)*100,0)))
#'  b = c(rep(x = 'shoe',round(runif(1)*100,0)),rep(x = 'hat',round(runif(1)*100,0)),rep(x = 'shirt',round(runif(1)*100,0)))
#'  if(length(a)<length(b)){
#'      b = b[1:length(a)]
#'  }
#'  else{
#'     a = a[1:length(b)]
#'  }
#'
#'  exp = data.frame(
#'  column1 = a,
#'  column2 = b
#'  )
#'  ballon_plot('column1','column2',exp)
#'
#' @export
ballon_plot<-function(a,b,exp){
  tabla=prop.table(table(as.vector(unlist(exp[a])),as.vector(unlist(exp[b])))) %>% as.data.frame()
  ggpubr::ggballoonplot(tabla, fill = "value")+
    ggplot2::scale_fill_viridis_c(option = "C")+ ggplot2::theme(legend.position = "none")
}


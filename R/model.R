usethis::use_package("ggplot2")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("magrittr")
usethis::use_package("broom")

#'variable_selection_cramerV
#'
#'Uses Cramer's V to identify the variables
#'more correlated with the answer, and drops
#'those demographic variables which have no
#'correlation between it and the answer
#' @importFrom magrittr %>%
#' @param survey_experiment The dataframe with survey and experiment data, must contain "response_id" and "answer" columns
#' @param cramer_threshold  float: a threshold to consider a variable relevant or not according to Cramer's V
#'survey_exp = data.frame(
#'  response_id = runif(100),
#'  answer = c(rep(1,65), rep(0,35)),
#')
#'survey_exp['a']=rnorm(100,3)*survey_exp$answer
#'survey_exp['b']=(survey_exp$answer+0.1)/3
#'survey_exp['c']=runif(100)
#'variable_selection_cramerV(survey_exp,0.1)
#' @export
variable_selection_cramerV<-function(survey_experiment, cramer_threshold){
  coenf<-survey_experiment %>% dplyr::select(-'response_id')%>%
    tidyr::gather("variable", "measure", -answer)%>%
    dplyr::group_by(variable) %>% dplyr::filter(!is.na(measure)) %>%
    dplyr::do(broom::tidy(rcompanion::cramerV(.$answer, .$measure,bias.correct = T)))
  vars_interes<-coenf %>% dplyr::filter(variable%in%colnames(exp) | x > cramer_threshold )
  vars_interes
}



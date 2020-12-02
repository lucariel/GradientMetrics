usethis::use_package("ggplot2")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("magrittr")
#usethis::use_package("rcompanion")
usethis::use_package("broom")


variable_selection_cramerV<-function(survey_experiment, cramer_threshold){
  coenf<-survey_experiment %>% dplyr::select(-response_id)%>%
    tidyr::gather("variable", "measure", -answer)%>%
    group_by(variable) %>% dplyr::filter(!is.na(measure)) %>%
    do(broom::tidy(rcompanion::cramerV(.$answer, .$measure,bias.correct = T)))
  vars_interes<-coenf %>% dplyr::filter(variable%in%colnames(exp) | x > cramer_threshold )
  vars_interes
}

test_train_split<-function(p,dt_select){
  smp_size <- floor(p * nrow(dt_select))
  train_idx <- sample(seq_len(nrow(dt_select)), size = smp_size)
  train <- dt_select[train_idx, ]
  test <- dt_select[-train_idx, ]
  list(train,test)
}


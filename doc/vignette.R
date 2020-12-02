## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
source('script.R')

## -----------------------------------------------------------------------------
plots_ball[[1]]+coord_flip()+
theme(axis.text.x = element_text(angle = 90, hjust=1),text = element_text(size=8))


## -----------------------------------------------------------------------------
plots_ball[[2]]+coord_flip()+
theme(axis.text.x = element_text(angle = 90, hjust=1),text = element_text(size=8))

## -----------------------------------------------------------------------------
plots_ball[[3]]+coord_flip()+
theme(axis.text.x = element_text(angle = 90, hjust=1),text = element_text(size=8))

## -----------------------------------------------------------------------------
plots_ball[[4]]+coord_flip()+
theme(axis.text.x = element_text(angle = 90, hjust=1),text = element_text(size=8))

## -----------------------------------------------------------------------------
plots_ball[[5]]+coord_flip()+
theme(axis.text.x = element_text(angle = 90, hjust=1),text = element_text(size=8))

## -----------------------------------------------------------------------------
plots_ball[[6]]+coord_flip()+
theme(axis.text.x = element_text(angle = 90, hjust=1),text = element_text(size=8))

## -----------------------------------------------------------------------------
plots_ball[[7]]+coord_flip()+
theme(axis.text.x = element_text(angle = 90, hjust=1),text = element_text(size=8))


## ----message=F, warning=F-----------------------------------------------------
get_MCA_plot(experiment)

## -----------------------------------------------------------------------------
kmplot+theme_cleveland()

## -----------------------------------------------------------------------------
pca_plot

## ----message=F, warning=F-----------------------------------------------------
knitr::opts_chunk$set(fig.width=14, fig.height=14) 
knitr::kable(variable_selection_cramerV(survey_experiment,0.2))

## -----------------------------------------------------------------------------
gridExtra::grid.arrange(grobs=om_plots,nrow=6)

## -----------------------------------------------------------------------------
m1_philosophy_plot+ theme(legend.position="top")

## -----------------------------------------------------------------------------
m1_philosophy[,1:9] %>% map_chr(attr_getter("label"))

## -----------------------------------------------------------------------------
m2_attitudes_plot+ theme(legend.position="top")

## -----------------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=4, fig.height=4)
m2_attitudes[,1:9] %>% map_chr(attr_getter("label"))

## ----message=F, warning=F-----------------------------------------------------
knitr::kable(vdf_experiment)

## ----message=F, warning=F-----------------------------------------------------
vdf_survay

## ----message=F, warning=F-----------------------------------------------------
caret::confusionMatrix(predict(rfm_reg,test),as.factor(test$answer))

## ----results='asis'-----------------------------------------------------------
sjPlot::tab_model(linear_model)

## ----message=FALSE, warning=FALSE---------------------------------------------
hist(linear_model$residuals)

## ----warning=FALSE, results='asis'--------------------------------------------
sjPlot::tab_model(fitted_models_by_cluster$model)

## ----message=F, warning=F-----------------------------------------------------
cmTest2


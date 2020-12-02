#Gradient HW
library(GradientMetrics)
library(cluster)
library(caret)
library(reshape2)
library(tidyverse)
library(ggpubr)
library(factoextra)



experiment<-haven::read_sav('../data-raw/experiment_data.sav') %>% as_factor()




combinatory_answer_plot = experiment %>% mutate(
  p = paste(offer,outcome,price,rtb,social_proof)
) %>% group_by(p) %>% summarise(n=n(),
                                a = mean(answer)) %>% ggplot()+geom_point(aes(a,n))


MCA_plot=get_MCA_plot(experiment)


##Get the stratified sample
exp<-experiment  %>% group_by(answer) %>%
  sample_n(1500) %>% ungroup()


exp<-exp %>% dplyr::select(-response_id)


plots_ball<-list()
for(i in 1:7){
  plots_ball[[i]]<-ballon_plot(colnames(exp)[8],colnames(exp)[i], exp)
}

survet<-haven::read_sav('../data-raw/survey_data.sav') %>% as_factor()
survet<-survet%>%
  select_if(~ !any(is.na(.)))%>% select(where(~length(unique(.)) > 1))







dmat<-dist(survet, method="euclidean")

totw<-c()
for(i in 1:10){
  km1<-kmeans(dmat,centers=i)
  totw<-c(totw,km1$tot.withinss)
}
gc()
kmplot<-totw %>% enframe() %>% ggplot(aes(name,value))+geom_line()


km1<-kmeans(dmat,centers=5)

survet['cluster'] = as.factor(km1$cluster)

res.pca<-prcomp(dmat, scores = TRUE)
survet['pca1']<-res.pca$x[,1]
par(mar=c(1,1,1,1))
pca_plot = fviz_eig(res.pca)

m2_attitudes<-select(survet,contains("m2_attitudes"))
m2_attitudes['cluster']<-survet$cluster
m1_philosophy<-select(survet,contains("m1_philosophy"))
m1_philosophy['cluster']<-survet$cluster

other<-survet %>% dplyr::select(!c('cluster','pca1','response_id',colnames(m2_attitudes),colnames(m1_philosophy)))
other$weights<-as.numeric(other$weights)
other['cluster']<-survet$cluster



get_cluster_categorical_plot<-function(df,n,plot=T){
  df_m <- melt(df,measure.vars=colnames(df)[1:n]) %>% as_tibble()%>%count(cluster, variable, value)
  if(plot){
  df_m  %>%
    ggplot(aes(x=cluster,y=n,fill=value))+
    geom_bar(position="fill", stat="identity")+
    facet_wrap(~variable)}
  else{df_m}
}


m1_philosophy_plot<-get_cluster_categorical_plot(m1_philosophy,9)
m2_attitudes_plot<-get_cluster_categorical_plot(m2_attitudes,11)

other<-other %>% dplyr::select(-weights)
om<-get_cluster_categorical_plot(other,17,F)
om$value=gsub("\\s*\\([^\\)]+\\)","",as.character(om$value))

om_plots<-list()
j=1
for(i in unique(om$variable)){

  pldf=om %>% filter(variable==i)
  pl = pldf %>% ggplot(aes(x=cluster,y=n,fill=value))+geom_bar(position="fill", stat="identity")+ theme(legend.position="top", legend.title=element_blank(),
                                                                                                        legend.text=element_text(size=4))
  om_plots[[j]]=pl
  j=j+1
}
om_plots[[3]]

survey_experiment<-right_join(experiment,survet)
survey_experiment<-survey_experiment %>%
  select_if(~ !any(is.na(.)))%>% select(where(~length(unique(.)) > 1))

survey_experiment<-survey_experiment %>% group_by(answer) %>% sample_n(1500)


survey_experiment$answer<-as.factor(survey_experiment$answer)
vars_interest<-variable_selection_cramerV(survey_experiment,0.2)

dt_select<-survey_experiment %>% dplyr::select(c(vars_interest$variable,'cluster')) %>%
  ungroup()

smp_size <- floor(0.75 * nrow(dt_select))
train_idx <- sample(seq_len(nrow(dt_select)), size = smp_size)
train <- dt_select[train_idx, ]
test <- dt_select[-train_idx, ]



train$answer<-as.factor(train$answer)
rfm_reg <- randomForest::randomForest(answer~.,train,localImp = TRUE)
vdf = data.frame(varImp(rfm_reg))
vdf['Variables'] = rownames(vdf)
vdf<-vdf %>% as_tibble()
vdf_experiment<-vdf %>% dplyr::filter(Variables%in%colnames(exp))
vdf_survay<-vdf %>% dplyr::filter(!Variables%in%colnames(exp))

cmTest = caret::confusionMatrix(predict(rfm_reg,test),test$answer)

train$answer<-as.numeric(train$answer)
linear_model <- lm(answer~.,train)


f1 = formula(paste0("answer~",paste0(colnames(train)[2:21], collapse = "+")))
train %>% group_by(cluster) %>% count()
fitted_models_by_cluster = train %>% group_by(cluster) %>%
  do(model = glm(f1, data = .))

models_list<-list(linear_model,fitted_models_by_cluster$model)


dt_select2<-dt_select %>% mutate(
  answer = case_when(
    answer %in%c(1,2)~0,
    T~1
  )
)
dt_select2$answer<-as.factor(dt_select2$answer)

smp_size <- floor(0.75 * nrow(dt_select2))
train2 <- dt_select2[train_idx, ]
test2 <- dt_select2[-train_idx, ]

rfm_reg2 <- randomForest::randomForest(answer~.,train2,localImp = TRUE)
vdf2 = data.frame(varImp(rfm_reg2))
vdf2['Variables'] = rownames(vdf2)
vdf2<-vdf2 %>% as_tibble()
vdf_experiment2<-vdf %>% dplyr::filter(Variables%in%colnames(exp))
vdf_survay2<-vdf %>% dplyr::filter(!Variables%in%colnames(exp))

cmTest2 = caret::confusionMatrix(predict(rfm_reg2,test2),test2$answer)


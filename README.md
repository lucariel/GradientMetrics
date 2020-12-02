# GradientMetrics Homework
The interview's homework for Gradient Metrics. 

It can be installed by:

`
devtools::install_github('lucariel/GradientMetrics')
`

Check __./doc/vignette.html__ for the preliminar analysis and __./vignettes/script.R__ for the use example.

The functions of this package are:

`
variable_selection_cramerV(survey_experiment, cramer_threshold)`

Which requeries a dataframe with, at least, columnames in [response_id,answer] and other, to perform the Cramer's V calculations and filter.

`
get_MCA_plot(experiment)
`
Which requeries a dataframe with, at least, columnames in [response_id] and other, to perform the Multicorrespondence analysis.


`
ballon_plot(a,b,exp)
`

Which requeries three arguments:

  + a,b : variables to make the frequency grid
  + exp: dataframe of experimient which contains the variables "a" and "b"


# Maria's Suggestions, July 27:


* make a list of all constructed categorical  variables with 4 or fewer distinct values. These are likely to be binary (yes / no / not answered etc. )

* use glmnet (http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html) with the continuous variables in the best continuous model plus the constructed categorical variables. 

* you could also try to rerun your work with continuous variables by eliminating all variables that are to do with the design / representativeness of the study. Typically, these will have the key phrase "weight" in the description 
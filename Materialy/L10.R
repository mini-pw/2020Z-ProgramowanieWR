library(DALEX)
library(kernlab)

# install_dependencies()

data(spam)
# https://archive.ics.uci.edu/ml/datasets/spambase

dalex_friendly_spam <- spam
dalex_friendly_spam[["type"]] <- spam[["type"]] == "spam"

kernlab_model <- ksvm(type ~ ., data = dalex_friendly_spam, type = "C-svc")

kernlab_explainer <- explain(kernlab_model, 
                             data = dalex_friendly_spam, 
                             y = dalex_friendly_spam[["type"]], 
                             predict_function = predict,
                             label = "gaussian")

library(ingredients)

cp_exp <- ceteris_paribus(kernlab_explainer, dalex_friendly_spam[1, ])

plot(cp_exp)

plot(calculate_oscillations(cp_exp))


cp_exp <- ceteris_paribus(kernlab_explainer, dalex_friendly_spam[4500, ])

plot(cp_exp)

plot(calculate_oscillations(cp_exp))

library(iBreakDown)

bd_exp <- break_down(kernlab_explainer, dalex_friendly_spam[1, ])

plot(bd_exp)

describe(bd_exp)

bd_exp <- break_down(kernlab_explainer, dalex_friendly_spam[4500, ])

plot(bd_exp)

describe(bd_exp)

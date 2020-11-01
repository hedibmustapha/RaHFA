library(hypegrammaR)

survey <- readxl::read_excel("./input/questionnaire_kobo_rahfa_final_clean2.xlsx", sheet = "survey")
choices <- readxl::read_excel("./input/questionnaire_kobo_rahfa_final_clean2.xlsx", sheet = "choices")
data<-readxl::read_excel("./input/data.xlsx")

questionnaire <- koboquest::load_questionnaire(data = data,
                                               questions = survey,
                                               choices = choices,
                                               choices.label.column.to.use ="label")

dap<- load_analysisplan("./input/dap.csv")

results <- from_analysisplan_map_to_output(data = data,
                                           analysisplan = dap,
                                           questionnaire = questionnaire
                                           )
final_result<- results$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)

choice_indices <- match(final_result$dependent.var.value, choices$name)
choice_labels <- choices[["label"]]
question_indices <- match(final_result$dependent.var, survey$name)
question_labels <- survey[["label"]]

final_labeled_results <- final_result %>%
  mutate(

    dependent.var.label = ifelse(is.na(question_indices),
                                 dependent.var,
                                 ifelse(is.na(question_labels[question_indices]) | question_labels[question_indices] == "",
                                        dependent.var,
                                        question_labels[question_indices])),
    dependent.var.value.label = ifelse(is.na(choice_indices),
                                       dependent.var.value,
                                       choice_labels[choice_indices])
 )

write.csv(final_labeled_results,paste0("./output/results_",humanTime(),".csv"),row.names=F)

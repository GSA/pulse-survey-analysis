library(dplyr)

pay_v_flex_query <- "SELECT
    agency,
    component,
   -- sub_component,
   -- engagement1_q3 exhausted,
    equity1_q2 same_pay,
   -- equity1_q3 reasonable,
    reentry1_q3 flex
FROM
    dmennealy.pulse_ehri
WHERE equity1_q2 IS NOT NULL or reentry1_q3 IS NOT NULL
;"

pay_v_flex <- OracleSqlQuery(pay_v_flex_query)

pay_v_flex_component_table <- pay_v_flex %>%
  group_by(AGENCY, COMPONENT) %>%
  summarize(mean_pay = mean(SAME_PAY, na.rm = T),
            pay_n = sum(! is.na(SAME_PAY)),
            pay_strong_agree = sum(SAME_PAY == 5, na.rm = T),
            mean_flex = mean(FLEX, na.rm = T),
            flex_n = sum(! is.na(FLEX)),
            flex_strong_agree = sum(FLEX == 5, na.rm = T)) %>%
  filter(pay_n > 25 & flex_n > 25) %>%
  mutate(pay_strong_agree_pct = pay_strong_agree / pay_n,
         flex_strong_agree_pct = flex_strong_agree / flex_n)

boxplot(pay_v_flex_component_table[,c(3, 6)], 
        main = "Average response at components with >25 responses",
        xlab = "Questions: same pay and benefits or more workplace flexibilities")

p_vec <- c()

for(i in 1:nrow(pay_v_flex_component_table)){
  a = pay_v_flex_component_table$AGENCY[i]
  c = pay_v_flex_component_table$COMPONENT[i]
  print(sprintf("Working on %s in %s", c, a))
  df_a_c <- filter(pay_v_flex, AGENCY == a, COMPONENT == c)
  if(is.na(c)){
    df_a_c <- filter(pay_v_flex, AGENCY == a, is.na(c))
  }
  
  pay_vec <- filter(df_a_c, ! is.na(SAME_PAY)) %>% pull(SAME_PAY)
  flex_vec <- filter(df_a_c, ! is.na(FLEX)) %>% pull(FLEX)
  res <- t.test(pay_vec, flex_vec, alternative = "two.sided", var.equal = F)
  p_vec <- c(p_vec, res$p.value)
}

pay_v_flex_component_table <- data.frame(pay_v_flex_component_table, p_vec) %>%
  mutate(pay_or_flex = ifelse(mean_pay > mean_flex, "pay", "flex")) %>%
  rename(`Average response: pay` = mean_pay,
         `Average response: flexibility` = mean_flex,
         p_value = p_vec) %>%
  relocate(pay_strong_agree_pct, .after = pay_strong_agree)

write.csv(x = pay_v_flex_component_table, file = "pay_and_flexibility_by_components.csv",
          row.names = F)
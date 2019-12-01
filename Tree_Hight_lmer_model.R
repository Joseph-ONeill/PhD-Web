**Fieldwork Tree height and diameter are used to develop a model for these forests**
  
  ```{r}
ModelForHeight_Popa <- lmer(log(Height_m)~log(DBH_cm)+(1|Plot), REML = FALSE, data = Field_Data)
ModelForHeight_Popa
ranef(ModelForHeight_Popa)$Plot[,1] %>% hist(breaks = 5)
ranef(ModelForHeight_Popa)
```
Getting tree Height based on the best model and substracted the lowest random effect of the plot no. 115 from every plots of the dataframe that I need to predict Height. Plot no. 7 had the lowest random effect number.

```{r}
Popa_Forests_Full$H_pred <- predict(ModelForHeight_Popa, data.frame(DBH_cm = Popa_Forests_Full$DBH_cm, Plot=7))
Popa_Forests_Full <- Popa_Forests_Full %>%
  mutate(AdjustH_Pred=exp(H_pred)-0.003987937)
```
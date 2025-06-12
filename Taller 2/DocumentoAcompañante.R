library(wooldridge)
data("wage1")
wage1
View(wage1)
wage1 |> 
  mutate(marital = factor(married, levels = c(0, 1), labels = c("Soltero", "Casado"))) |> 
  ggplot(aes(educ, wage)) +  geom_point(aes(colour = marital), size = 3)+
  labs(title="Relación entre educación y salario",
       x = "Educación", 
       y = "Salario",
       color = "Estado marital",
       caption = "Datos de Wooldridge, usado en págs. como 7, 17, 33-34, 37, 76,...")


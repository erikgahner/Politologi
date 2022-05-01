library("tidyverse")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")
options(OutDec= ",")

ff <- read_csv("forsvarsforbeholdet.csv")

ff |> 
  mutate(across(c("svar_ja", "svar_nej", "svar_vedikke"), ~ ifelse(!is.na(svar_andet), (.x / (100 - svar_andet))*100, .x))) |> 
  select(-svar_andet) |> 
  pivot_longer(starts_with("svar_")) |> 
  mutate(usikkerhed = 1.96 * sqrt((value * (100 - value)) / n),
         name = case_when(
           name == "svar_ja" ~ "Ja",
           name == "svar_nej" ~ "Nej",
           name == "svar_vedikke" ~ "Ved ikke"
         )) |> 
  ggplot(aes(dato_end, y = value, ymin = value - usikkerhed, ymax = value + usikkerhed,
             col = name, shape = institut)) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_point(position = position_dodge(width = 1), size = 3.5) +
  geom_errorbar(position = position_dodge(width = 1), width = 0, size = 1.5, alpha = 0.5) +
  scale_shape_manual(values = c(16, 15, 17, 18, 4)) +
  geom_vline(xintercept = as.Date("2022-06-01"), linetype = "dotted", col = "#0074D9") +
  geom_vline(xintercept = as.Date("2022-03-06"), linetype = "dotted", col = "#0074D9") +
  annotate("text", x = as.Date("2022-05-15"), y = 25, label = "Folkeafstemningen\nfinder sted 1. juni", colour = "gray60") +
  geom_curve(aes(x = as.Date("2022-05-15"), y = 27, xend = as.Date("2022-05-30"), yend = 30), curvature = -0.5, colour = "black", size=0.2, arrow = arrow(length = unit(0.01, "npc"))) +
  scale_x_date(limits = c(as.Date("2022-03-01"), as.Date("2022-06-02")),
               date_breaks = "1 month", 
               labels = scales::date_format("%B")) +
  theme_minimal(base_size = 12) %+replace% 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey20", size = 0.3, linetype="dotted"),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        legend.box="vertical") +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Meningsmålinger om forsvarsforbeholdet",
       subtitle = "Skal forsvarsforbeholdet afskaffes? Med 'Ved ikke' svar inkluderet",
       caption = "Opbakning med 95% konfidensintervaller") +
  scale_colour_manual(values = c("#005AB5", "#DC3220", "gray"))

ggsave("forsvarsforbeholdet_vedikke.png", width = 7, height = 7, dpi = 400)

ff |> 
  mutate(n = n - n * (svar_vedikke / 100)) |> 
  mutate(across(c("svar_ja", "svar_nej"), ~ ifelse(!is.na(svar_andet), (.x / (100 - svar_andet - svar_vedikke))*100, 
                                                   (.x / (100 - svar_vedikke))*100))) |> 
  select(-svar_andet, -svar_vedikke) |> 
  pivot_longer(starts_with("svar_")) |> 
  mutate(usikkerhed = 1.96 * sqrt((value * (100 - value)) / n),
         name = case_when(
           name == "svar_ja" ~ "Ja",
           name == "svar_nej" ~ "Nej"
         )) |> 
  ggplot(aes(dato_end, y = value, ymin = value - usikkerhed, ymax = value + usikkerhed,
             col = name, shape = institut)) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_point(size = 3.5) +
  geom_errorbar(width = 0, size = 1.5, alpha = 0.5) +
  scale_shape_manual(values = c(16, 15, 17, 18, 4)) +
  geom_vline(xintercept = as.Date("2022-06-01"), linetype = "dotted", col = "#0074D9") +
  geom_vline(xintercept = as.Date("2022-03-06"), linetype = "dotted", col = "#0074D9") +
  annotate("text", x = as.Date("2022-05-15"), y = 35, label = "Folkeafstemningen\nfinder sted 1. juni", colour = "gray60") +
  geom_curve(aes(x = as.Date("2022-05-15"), y = 37, xend = as.Date("2022-05-30"), yend = 40), curvature = -0.5, colour = "black", size=0.2, arrow = arrow(length = unit(0.01, "npc"))) +
  scale_x_date(limits = c(as.Date("2022-03-01"), as.Date("2022-06-02")),
               date_breaks = "1 month", 
               labels = scales::date_format("%B")) +
  theme_minimal(base_size = 12) %+replace% 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey20", size = 0.3, linetype="dotted"),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        legend.box="vertical") +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Meningsmålinger om forsvarsforbeholdet",
       subtitle = "Skal forsvarsforbeholdet afskaffes? Med 'Ved ikke' svar ekskluderet",
       caption = "Opbakning med 95% konfidensintervaller") +
  scale_colour_manual(values = c("#005AB5", "#DC3220"))

ggsave("forsvarsforbeholdet_udenvedikke.png", width = 7, height = 7, dpi = 400)

library("gt")

ff |> 
  mutate(dato = paste0(format(dato_end, "%d"), ". ", str_to_lower(format(dato_end, "%B")))) |> 
  select(institut, dato, spoergsmaal_ordlyd) |> 
  gt() |> 
  cols_label(
    institut = "Institut",
    dato = "Dato",
    spoergsmaal_ordlyd = "Spørgsmålsformulering"
  ) |> 
  fmt_missing(columns = c(spoergsmaal_ordlyd), missing_text = "–") |> 
  cols_width(
    institut ~ px(100),
    dato ~ px(100)
  ) |> 
  as_raw_html()

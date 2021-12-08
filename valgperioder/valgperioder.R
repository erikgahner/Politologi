library("tidyverse")
library("lubridate")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")
options(OutDec= ",")

polls <- read_csv("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv",
                  col_types = cols(
                    party_p = col_double(),
                    party_q = col_double(),
                    party_e = col_double(),
                    party_g = col_double(),
                    party_moderaterne = col_double(),
                  ))


polls <- polls |> 
  mutate(date = make_date(year, month, day),
         across(starts_with("party"), ~ .x + 1.96 * sqrt((.x * (100 - .x)) / n), .names = "ci_max_{.col}"),
         across(starts_with("party"), ~ .x - 1.96 * sqrt((.x * (100 - .x)) / n), .names = "ci_min_{.col}")
         )

polls_2011 <- polls |> 
  filter(date > as.Date("2011-09-15"), date <= as.Date("2015-06-18"))

polls_2011 |>
  gather(party, support, party_a:party_moderaterne) |>
  filter(!party %in% c("party_e", "party_p", "party_q", "party_moderaterne")) |>
  drop_na(support) |> 
  ggplot(aes(x=as.Date(date), y=support, colour=party)) +
  geom_point(size=1, alpha=0.3) +
  geom_hline(yintercept = 0) +
  geom_smooth(se=FALSE, method="loess", span = .3) +
  geom_hline(yintercept=2, linetype = "dashed") +
  labs(y = "Stemmer (%)",
       x = NULL) +
  scale_colour_manual(labels = c("Socialdemokraterne", "Alternativet", "Radikale Venstre", "Konservative", "SF", 
                                 "Liberal Alliance", "Kristendemokraterne", "Dansk Folkeparti", "Enhedslisten", "Venstre"), 
                      values = c("#E3515D", "#AEFEAF", "#EB4295", "#429969",  "#9C1D2A", 
                                 "#EE9A5F", "#F4CE97", "#3D6F8D", "#914A4F", "#459BC8"),
                      guide = guide_legend(ncol = 3)) +
  theme_minimal(base_size = 12, base_family = "Barlow") %+replace% 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(hjust = 1, size = 10, margin = margin(t = -71), lineheight = 1.2),
        legend.justification = c(0, 0),
        legend.position = "bottom",
        plot.margin=unit(c(.5, .5, 1.5, .5),"cm"),
        axis.ticks.x = element_line(colour = "gray48"),
        axis.ticks.y = element_blank(),
        legend.title = element_blank()
  )

ggsave("fv_2011_15.jpg", width = 6, height = 8)

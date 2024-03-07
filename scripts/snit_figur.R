poll_recent <- poll_recent %>%
  mutate(image = case_when(
    party_name == "Venstre" ~ "partilogoer/v.png",
    party_name == "Socialdemokraterne" ~ "partilogoer/a.png",
    party_name == "Dansk Folkeparti" ~ "partilogoer/o.png",
    party_name == "SF" ~ "partilogoer/f.png",
    party_name == "Veganerpartiet" ~ "partilogoer/g.png",
    party_name == "Konservative" ~ "partilogoer/c.jpg",
    party_name == "Radikale Venstre" ~ "partilogoer/b.png",
    party_name == "Enhedslisten" ~ "partilogoer/oe.png",
    party_name == "Liberal Alliance" ~ "partilogoer/i.png",
    party_name == "Nye Borgerlige" ~ "partilogoer/d.png",
    party_name == "Moderaterne" ~ "partilogoer/moderaterne.png",
    party_name == "Danmarksdemokraterne" ~ "partilogoer/ae.jpg",
    party_name == "Alternativet" ~ "partilogoer/aa.jpg",
    party_name == "Kristendemokraterne" ~ "partilogoer/k.jpg",
    party_name == "Stram Kurs" ~ "partilogoer/p.jpg",
    party_name == "Frie Grønne" ~ "partilogoer/q.jpg"
  )) %>%
  mutate(lower = ifelse(lower < 0, 0, lower)) %>%
  mutate(election2019 = case_when(
    party_name == "Venstre" ~ 23.4,
    party_name == "Socialdemokraterne" ~ 25.9,
    party_name == "Dansk Folkeparti" ~ 8.7,
    party_name == "SF" ~ 7.7,
    party_name == "Veganerpartiet" ~ NA_real_,
    party_name == "Frie Grønne" ~ NA_real_,
    party_name == "Moderaterne" ~ NA_real_,
    party_name == "Danmarksdemokraterne" ~ NA_real_,
    party_name == "Konservative" ~ 6.6,
    party_name == "Radikale Venstre" ~ 8.6,
    party_name == "Enhedslisten" ~ 6.9,
    party_name == "Liberal Alliance" ~ 2.3,
    party_name == "Nye Borgerlige" ~ 2.4,
    party_name == "Alternativet" ~ 3.0,
    party_name == "Kristendemokraterne" ~ 1.7,
    party_name == "Stram Kurs" ~ 1.8
  )) %>%
  filter(!party_name %in% c("Stram Kurs", "Veganerpartiet"), party != "party_other")


ggplot(poll_recent, aes(x = fct_reorder(party_name, -est), y = est*100)) +
  geom_hline(yintercept=2, colour="gray70", linetype = "dashed") +
  geom_hline(yintercept=0, colour="gray80") +
  geom_text(data = poll_recent, aes(x = fct_reorder(party_name, -est), y = election2019), label = "—", alpha = 0.7, size = 10,
            colour = "gray70") +
  geom_errorbar(aes(ymin=lower*100, ymax=upper*100, colour = party_name), width = 0, size = 1.5, alpha = 0.6) +
  geom_point(size=2, aes(colour = party_name)) +
  geom_text(aes(label = str_replace_all(paste0(" ", round(est*100,1)), "\\.", ",")), colour = "black",
            #size = 12,
            size = 3,
            hjust=-0.1, vjust=0.5, nudge_x = 0.1) +
  scale_colour_manual(values = cols) +
  scale_y_continuous(breaks = seq(0, 50, 5), labels = seq(0, 50, 5)) +
  scale_x_discrete("", labels = NULL) +
  expand_limits(y = -4) +
  geom_image(aes(image = image), y = -2, size = 0.05) +
  labs(caption = paste0("Sidst opdateret:", gsub(" 0", " ",tolower(format(min(Sys.Date()), " %d %B %Y"))))) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "lightgray"),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.caption = element_text(vjust = 15.75)
  ) +
  annotate("text", x = 5.2, family = "Barlow", y = 23.1, label = "Folketingsvalget 2019", #size = 15,
           colour = "gray60") +
  annotate("text", x = 1.6, family = "Barlow", y = 1.5, label = "Spærregrænsen", #size = 15,
           colour = "gray60") +
  geom_curve(aes(x = 3.8, y = 23.7, xend = 2.45, yend = 23.45), colour = "black", size=0.2, arrow = arrow(length = unit(0.01, "npc"))) +
  labs(y = NULL)

ggsave("prognose.png", width = 7, height = 7, dpi = 400, bg = "white")

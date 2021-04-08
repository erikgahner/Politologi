library("tidyverse")
library("readxl")
library("hrbrthemes")

theme_set(theme_ipsum())

milgram <- read_xlsx("milgram.xlsx")

milgram %>% 
  mutate(f_beskrivelse = as.factor(paste0(forsoeg, ". ", beskrivelse_da))) %>% 
  mutate(f_beskrivelse = fct_reorder(f_beskrivelse, -forsoeg)) %>% 
  ggplot(aes(f_beskrivelse, volt450)) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_col() + coord_flip() + 
  labs(x = NULL,
       y = "Deltagere der gik til 450 volt (%)")

ggsave("milgram.png", height=6, width=6)
library(ggplot2)
library(scales)
library(readr)



df <- read_csv("data/output/pres16_state.csv")

ggplot(df, aes(x = cces_hrc, y = pct_hrc)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_label_repel(aes(label = st)) +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent) +
  coord_equal()

library(tidyverse)

frames_data %>%
  arrange(`Flat Time`) %>%
  select(Frame) %>%
  unique() %>%
  print(n=90)

included_frames <- c(
  "Zwift Concept Z1",
  "Spec Venge S-Works ",
  "Canyon Aero 2021",
  "Chapter2 Toa",
  "Scott Addict RC",
  "Spec Aethos"
)

wheels_data %>%
  arrange(`Total Saving`) %>%
  select(Wheelset) %>%
  unique() %>%
  print(n=40)

included_wheels <- c(
  "DT Swiss ARC 62",
  "DT Swiss ARC 62 Dicut Disc",
  "Zipp 858/Super9",
  "Zipp 858",
  "ENVE 7.8",
  "Zipp 808",
  "DT Swiss ARC 62",
  "Zipp 353 NSW",
  "ENVE 3.4",
  "Roval Alpinist",
  "Lightweight Meil"
)

expand.grid(included_frames,included_wheels) %>%
  data.frame() %>%
  write_csv(file = "Frames x Wheels.csv")

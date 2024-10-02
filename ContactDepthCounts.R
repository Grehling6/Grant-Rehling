library(dplyr)
library(ggplot2)
library(tidyverse)
library(png)

setwd("Downloads")
Contact_Data <- read.csv("Contact_Data.csv")

#Running Season Contact Depth 2K v Other 
Contact_Data <- read.csv("Contact_Data.csv")
OnlyInPlayBalls <- Contact_Data %>%
  filter(
    !is.na(TaggedHitType) & 
      TaggedHitType %in% c("Popup", "GroundBall", "LineDrive", "FlyBall") &
      PlayResult %in% c("Out", "Single", "Double", "Triple", "HomeRun", "Error", "FieldersChoice") &
      !is.na(ExitSpeed) &
      BatterTeam == "MIZ_TIG" &
      TaggedPitchType != "Undefined"
  ) %>%
  mutate(
    PitchType = case_when(
      TaggedPitchType %in% c("Fastball", "Cutter", "Sinker") ~ "Fastball",
      TaggedPitchType %in% c("Slider", "Curveball") ~ "Breaking Ball",
      TaggedPitchType %in% c("ChangeUp", "Splitter", "Knuckleball") ~ "Offspeed",
      TRUE ~ as.character(TaggedPitchType)  # for any other pitch type
    )
  )


 OnlyInPlayBalls <- OnlyInPlayBalls %>%
   filter(
    Batter == "Austin, Trevor"
)

OnlyInPlayBalls -> OnlyInPlayBalls

#Switch To All Right Handed Batters
midpoint_x <- mean(OnlyInPlayBalls$ContactPositionX)
OnlyInPlayBalls <- OnlyInPlayBalls %>%
  mutate(ContactPositionX = ifelse(BatterSide == "Left", 2 * midpoint_x - ContactPositionX, ContactPositionX))  

library(ggplot2)

OnlyInPlayBalls$TwoStrike = ifelse(OnlyInPlayBalls$Strikes == 2,'2 Strikes','Not 2 Strikes')
sum(OnlyInPlayBalls$TwoStrike != 'Not 2 Strikes')
unique(OnlyInPlayBalls$TwoStrike)


ggplot(data = OnlyInPlayBalls, aes(ContactPositionZ, ContactPositionX)) +
  geom_point(aes(color = PlayResult, shape = PitchType), size = 1.2) +
  facet_wrap(vars(TwoStrike), ncol = 2)+
  scale_color_manual(values = c(
    "Out" = "gray52",
    "Error" = "gray52",
    "FieldersChoice" = "gray52",
    "Single" = "forestgreen",
    "Double" = "forestgreen",
    "Triple" = "forestgreen", 
    "HomeRun" = "goldenrod2"
  )) +
  scale_shape_manual(values = c(
    "Fastball" = 16,
    "Breaking Ball" = 17,
    "Offspeed" = 18
  )) +
  geom_segment(x = -0.825, y = 1.414, xend = 0.825, yend = 1.414, linewidth = 0.5, color = "black") +
  geom_segment(x = -0.825, y = 1.414, xend = -0.825, yend = 1.414 - 0.708, linewidth = 0.5, color = "black") +
  geom_segment(x = 0.825, y = 1.414, xend = 0.825, yend = 1.414 - 0.708, linewidth = 0.5, color = "black") +
  geom_segment(x = -0.825, y = 1.414 - 0.708, xend = 0, yend = 0, linewidth = 0.5, color = "black") +
  geom_segment(x = 0, y = 0, xend = -0.825, yend = 1.414 - 0.708, linewidth = 0.5, color = "black") +
  geom_segment(x = 0, y = 0, xend = 0.825, yend = 1.414 - 0.708, linewidth = 0.5, color = "black") +
  geom_segment(x = 0.825 + 0.5, y = 1.414 - 0.708 - 2, xend = 0.825 + 0.5, yend = 1.414 - 0.708 + 3, linewidth = 0.5, color = "black") +
  geom_segment(x = -0.825 - 0.5, y = 1.414 - 0.708 - 2, xend = -0.825 - 0.5, yend = 1.414 - 0.708 + 3, linewidth = 0.5, color = "black") +
  geom_segment(x = 0.825 + 0.5, y = 1.414 - 0.708 + 3, xend = 0.825 + 0.5 + 1, yend = 1.414 - 0.708 + 3, linewidth = 0.5, color = "black") +
  geom_segment(x = -0.825 - 0.5, y = 1.414 - 0.708 + 3, xend = -0.825 - 0.5 - 1, yend = 1.414 - 0.708 + 3, linewidth = 0.5, color = "black") +
  coord_cartesian(xlim = c(-2,2), ylim = c(0,4)) + 
  labs(title = "Contact Point") +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), aspect.ratio = 1, axis.title = element_blank())

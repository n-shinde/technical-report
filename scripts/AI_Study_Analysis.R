install.packages("psych",dependencies=TRUE)
install.packages("ggpubr")
library(psych)
library(gginference)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(readxl)

Real_vs_AI_Data <- read_excel("~/Downloads/data/raw data/Real_vs_AI_Data.xlsx")
View(Real_vs_AI_Data) 

new_df <- Real_vs_AI_Data
new_df = subset(new_df, select = c("Treatment", "Number of Responses Answered Correctly"))

audio_df <- new_df[which(new_df$Treatment=="Audio"), ]
image_df <- new_df[which(new_df$Treatment=="Image"), ]

# Create data frame to run tests on
correct_audio <- audio_df$`Number of Responses Answered Correctly`
correct_image <- image_df$`Number of Responses Answered Correctly`

# Fisher's F test to make sure we can use unpaired t-test
# Are the variances homogenous?
p_vartest <- var.test(correct_audio, correct_image)
p_vartest

# p = 0.1856 > 0.05, so we can assume variances of both samples
# are homogenous

# Run the t-test assuming equal variance
p_ttest <- t.test(correct_audio, correct_image, var.equal = TRUE)
p_ttest

# p = 0.07 < 0.10 (by really small margin)
# True difference in means is not equal to 0

# Generate boxplot to compare audio and image groups

df_audio <- data.frame(Treatment = "Audio", Number = correct_audio)
df_image <- data.frame(Treatment = "Image", Number = correct_image)
df_both <- rbind(df_audio, df_image)
df_both

ggplot(df_both, aes(x = Treatment, y = Number)) +
       geom_boxplot() +
       labs(x = "Treatment", y = "Number of Correct Responses") +
       ggtitle("Number of Correct Responses for Image and Audio Clip Groups")


mean(audio_df$`Number of Responses Answered Correctly`)
# 3.708333
mean(image_df$`Number of Responses Answered Correctly`)
# 3.21875

sd(audio_df$`Number of Responses Answered Correctly`)
# 1.122078
sd(image_df$`Number of Responses Answered Correctly`)
# 0.8700899

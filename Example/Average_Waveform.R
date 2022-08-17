file_list <- list.files("5xFAD_Cohort 9_Click_6M_pre Donepezil_ASCII", full.names = TRUE)
info <- read.csv("Info.csv")
df <- data.frame()
for (i in 1:length(file_list)) {
        animalID <- tools::file_path_sans_ext(basename(file_list[i]))
        Waveform <- ASCII_extract(file_list[i])
        if (17-ncol(Waveform) != 0) {
                complete <- data.frame(matrix(NA, nrow = 1, ncol = 17-ncol(Waveform)))
                colnames(complete) <- seq(from = 5, to = 5*(17-ncol(Waveform)), by = 5)
                Waveform <- cbind(Waveform, complete)
        }
        animalInfo <- info[info[,1]==animalID,]
        Waveform <- cbind(Waveform, animalInfo)
        df <- rbind(df, Waveform)
}
write.csv(df, "5xFAD_Chort 9_pre Donepezil_Waveform.csv", row.names = F)

df <- read.csv("5xFAD_Chort 9_pre Donepezil_Waveform.csv")
# Average Waveform
df <- df %>% filter(Treatment == "Donepezil")
p <- ggplot(data = df, aes(x = Data_Pnt_ms, y = `75`))
p + stat_summary(aes(group = Genotype, color = Genotype), fun.data = mean_se, geom = "line", size = 1.5) +
        scale_color_manual(values=c("red","black")) +
        theme_classic(base_size = 20) + 
        theme(axis.text.x = element_text(face="bold", color="black", size = 20),
              axis.text.y = element_text(face="bold", color="black", size = 20)) + 
              #,aspect.ratio=1/1) +
        xlim(0, 7.5) + 
        coord_cartesian(ylim = c(-2,4)) + 
        labs(x="Latency (ms)",y="Amplitude (μV)",title = "Pre treatment - Donepezil")

df <- df %>% filter(Treatment == "Control")
df <- df[df$ID != 737,]
p <- ggplot(data = df, aes(x = Data_Pnt_ms, y = `X75`))
p + stat_summary(aes(group = Genotype, color = Genotype), fun.data = mean_se, geom = "line", size = 1.5) +
        scale_color_manual(values=c("red","black")) +
        theme_classic(base_size = 20) + 
        theme(axis.text.x = element_text(face="bold", color="black", size = 20),
              axis.text.y = element_text(face="bold", color="black", size = 20)) + 
        #,aspect.ratio=1/1) +
        xlim(0, 7.5) + 
        labs(x="Latency (ms)",y="Amplitude (μV)",title = "Pre Donepezil - Control")

df <- read.csv("5xFAD_Chort 9_post Donepezil_Waveform.csv")
df <- df %>% filter(Treatment == "Control")
df <- df[df$ID != 737,]
p <- ggplot(data = df, aes(x = Data_Pnt_ms, y = `X75`))
p + stat_summary(aes(group = Genotype, color = Genotype), fun.data = mean_se, geom = "line", size = 1.5) +
        scale_color_manual(values=c("red","black")) +
        theme_classic(base_size = 20) + 
        theme(axis.text.x = element_text(face="bold", color="black", size = 20),
              axis.text.y = element_text(face="bold", color="black", size = 20)) + 
        #,aspect.ratio=1/1) +
        xlim(0, 7.5) + 
        labs(x="Latency (ms)",y="Amplitude (μV)",title = "Post Donepezil - Control")

df <- read.csv("5xFAD_Chort 9_post Donepezil_Waveform.csv")
df <- df %>% filter(Treatment == "Donepezil")
p <- ggplot(data = df, aes(x = Data_Pnt_ms, y = `X75`))
p + stat_summary(aes(group = Genotype, color = Genotype), fun.data = mean_se, geom = "line", size = 1.5) +
        scale_color_manual(values=c("red","black")) +
        theme_classic(base_size = 20) + 
        theme(axis.text.x = element_text(face="bold", color="black", size = 20),
              axis.text.y = element_text(face="bold", color="black", size = 20)) + 
        #,aspect.ratio=1/1) +
        xlim(0, 7.5) + 
        coord_cartesian(ylim = c(-2,4)) + 
        labs(x="Latency (ms)",y="Amplitude (μV)",title = "Post Treatment - Donepezil")


Compile <- function(directory) {
        file_list <- list.files(directory,full.names = TRUE)
        info <- read.csv("Info.csv")
        df <- data.frame()
        for (i in 1:length(file_list)) {
                data <- read.csv(file_list[i], header = TRUE)
                animalID <- data[1,"ID"]
                animalInfo <- info[info[,1]==animalID,][,2:4] 
                process <- data.frame(WaveIItoILatDif = data$WaveIILat - data$WaveILat, 
                                      WaveIItoIAmpRat = data$WaveIIAmp / data$WaveIAmp, 
                                      WaveIIItoILatDif = data$WaveIIILat - data$WaveILat, 
                                      WaveIIItoIAmpRat = data$WaveIIIAmp / data$WaveIAmp, 
                                      WaveIVtoILatDif = data$WaveIVLat - data$WaveILat, 
                                      WaveIVtoIAmpRat = data$WaveIVAmp / data$WaveIAmp,
                                      WaveVtoILatDif = data$WaveVLat - data$WaveILat, 
                                      WaveVtoIAmpRat = data$WaveVAmp / data$WaveIAmp)
                data <- cbind(data, animalInfo, process, row.names = NULL)
                df <- rbind(df, data)
        }
        write.csv(df, paste(directory, ".csv", sep = ""), row.names = FALSE)
}
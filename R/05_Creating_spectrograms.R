##########################################################################################
# This script was used during the manual validation process. The WAV files were loaded and 
# played. Furthermore, a high-resolution spectrogram is generated for the time interval
# surrounding the detected call. The start and end time of the call is highlighted in the
# spectrogram. This was done in a loop.
##########################################################################################
library(tuneR)
library(seewave)
library(readxl)

# Loading the excel file
dat <- read_excel(".:/Human_validation_det_prob.xlsx")

# Converting the time columns from character to numeric
dat$start_time <- as.numeric(gsub(",", ".", dat$start_time))
dat$end_time <- as.numeric(gsub(",", ".", dat$end_time))

# # Defining the range of rows to process (e.g., a subset or full dataset), needs to be 
# changed to the rows that shall be processed
start_row <- 1 
end_row <- 25  # Falls du alle Zeilen nutzen willst

# Loop over each selected row
for (i in start_row:end_row) {
  # full path to WAV file
  wav_file <- dat$full_path[i]
  
  # start and end time of the call
  start_time <- as.numeric(dat$start_time[i])
  end_time <- as.numeric(dat$end_time[i])
  
  # Extracting frequency range and convert to kHz
  low_freq <- as.numeric(dat$low_freq[i]) / 1000  
  high_freq <- as.numeric(dat$high_freq[i]) / 1000  
  
  # Checking if the audio file exists
  if (file.exists(wav_file)) {
    # Read the WAV file
    wave <- readWave(wav_file)
    wave_duration <- length(wave@left) / wave@samp.rate  # Duration in seconds
    
    # Playing the audio for auditory inspection
    play(wave)  
    
    buffer <- 0.05  # Add 50 ms before and after
    zoom_start_time <- max(0, start_time - buffer)  
    zoom_end_time <- min(wave_duration, end_time + buffer)
    
    # creating the spectrogram
    spectrogram_title <- paste0("Spectrogram of ", dat$WAV_Name_neu[i])
    spectro(wave, f = wave@samp.rate, 
            flim = c(0, 100),  # Frequency range (0–100 kHz)
            tlim = c(zoom_start_time, zoom_end_time),  # Time window
            wl = 2048,  # Window length for high resolution
            ovlp = 90,  # High overlap for better detail
            collevels = seq(-40, 0, length = 30),  # Color contrast
            palette = spectro.colors,  
            main = spectrogram_title,
            cex.main = 0.6)
    
    # Adding vertical lines to indicate call boundaries
    abline(v = start_time, col = "red", lty = 2)  # Start of call
    abline(v = end_time, col = "blue", lty = 2)   # End of call
    
    # Free memory
    rm(wave)
    gc()
    
    # Waiting for user input before continuing
    readline(prompt = "Press [Enter] to continue to the next spectrogram...")
    
  } else {
    warning(paste("WAV file not found:", wav_file))
  }
}





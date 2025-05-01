# generate PRBS waveform for eye

library(tidyverse)
library(gsignal)

df_raw <- read_tsv("prbs7.txt", col_names = "nrz")
nrz <- df_raw$nrz * 2 - 1

F <- 1e9
Fs <- 40e9
OS <- Fs / F

x_ori <- rep(rep(nrz, each = OS), 10)
x_noisy <- x_ori + 0.02 * rnorm(length(x_ori))

# low pass filter, cutoff = 0.3 * Nyquist Freq
bf <- butter(4, 0.3, type = "low")
# freqz(bf, fs = Fs)

x_filtered <- filter(bf, x_noisy)

time <- (seq_along(x_ori) - 1) * 1 / Fs

t_ori <- tibble(time = time, v = x_ori, signal = "original")
t_noisy <- tibble(time = time, v = x_noisy, signal = "original + noise")
t_filtered <- tibble(time = time, v = x_filtered, signal = "filtered")

t <- rbind(t_ori, t_noisy, t_filtered)

p <- ggplot(
  t |>
    dplyr::filter(time < 127 * OS * 1 / Fs) |>
    mutate(
      signal = factor(
        signal,
        levels = c("filtered", "original + noise", "original")
      )
    ),
  aes(x = time, y = v, color = signal)
) +
  geom_line() +
  # geom_point() +
  theme_bw()
p

ggsave("wfm.png", p, width = 18, height = 6)

write_delim(
  t_filtered |> dplyr::select(-signal),
  "wfm.dat",
  delim = " ",
  col_names = FALSE
)

rm(list = ls())
library(tidyverse)
library(janitor)
library(here)
library(geomtextpath)

# Data fra Udirs statistikkbank: https://www.udir.no/tall-og-forskning/statistikk/statistikk-videregaende-skole/karakterer-vgs/?rapportsideKode=VGO_VGOkarakterer&filtre=EierformID(-10_2_8)_EnhetID(-12)_FagID(-12_127_157_224_315_498_553_690_815_890_976_996_1007_1064_1128_1144_1151_1168_1184_1232_1373_1484_1571_1621_1630_1714_1733_1964_2003_2151_2200_2221_2265_2330_2385_2462_2463_2521_2561_2619_2726_2735_2790_2828_2878_2900_2963_3126_3175_3227_3253_3283_3294_3354_3531_3552_3790_6457_27776_29033_29034_29035_29036_29767_29768_29939_29940_29941_30076_30117_30118_30119_30120_30121_30122_30123_30124_30125_30126_30127_30128_30129_30130_30131_30133_30135_30136_30137_30138_30139_30151_30152_30153_30154_30155_30156_30157_30158_30159_30160_30161_30182_30183_30979_30980_30982_30986_30988_30996_31000_35598_35599)_KaraktertypeID(1)_KjoennID(-10)_TidID(200806_200906_201006_201106_201206_201306_201406_201506_201606_201706_201806_201906_202006_202106_202206)_UtdanningsprogramvariantID(-10)_VisAntallPersoner(1)_VisKarakterfordeling(1)&radsti=F!(315)_(*)_(315.*)
dat.raw <- read_csv(here("data/2022-09-16-karakterer-utvalgte-fag.csv"))

parse_numeric <- function(nums) {
  nums %>%
    str_replace("," , ".") %>%
    str_replace(" ", "") %>%
    as.numeric()
}

data_cols <- "^(x|forelopige).+(snittkarakter|antall_elever|karakteren_(\\d+))"

dat <- dat.raw %>%
  mutate(
    across(matches(data_cols), ~parse_numeric(.x)),
    fag_kode = str_extract(vurderingsfagkode, "^.{3}"),
    fag_gruppe = case_when(
      fag_kode %in% c("SAF", "SAK") ~ "SAF/SAK",
      T ~ fag_kode
    )
  )

dat.tidy <- dat %>%
  pivot_longer(matches(data_cols)) %>%
  extract(
    name,
    into = c('period', 'type', 'eieform', 'kjonn', 'variable'),
    regex = "^(?:x|forelopige_tall_)([\\d_]+)_(standpunkt)_(alle_eierformer|offentlig_skole|privat_eiet)_(begge_kjonn)_((?:snittkarakter|karakteren_\\d|antall_elever)(?:_brudd)?)",
  ) %>%
  pivot_wider(names_from=variable, values_from=value)



#### Analyse 1: Andel seksere, privat vs offentlig

res_andel_seksere <- dat.tidy %>%
  filter(enhet_nivaa == 0) %>%
  group_by(period, eieform) %>%
  summarise(
    k6.n = sum((karakteren_6/100)*antall_elever, na.rm = T),
    N = sum(antall_elever, na.rm = T),
    k6.prop = (k6.n/N)*100,
    .groups = 'drop'
  )

# Analyse 2: Snittkarakter

res_snitt <- dat.tidy %>%
  filter(enhet_nivaa == 3) %>%
  group_by(period) %>%
  summarise(
    snitt = weighted.mean(snittkarakter, antall_elever, na.rm = T),
    .groups = 'drop'
  )

# ... etter eieform
res_snitt_eieform <- dat.tidy %>%
  filter(enhet_nivaa == 3) %>%
  group_by(period, eieform) %>%
  summarise(
    snitt = weighted.mean(snittkarakter, antall_elever, na.rm = T),
    .groups = 'drop'
  )

res_snitt_eieform

# ... per skole

res_snitt_skole <- dat.tidy %>%
  filter(enhet_nivaa == 3) %>%
  group_by(organisasjonsnummer, enhet_navn, period) %>%
  summarise(
    snittkarakter = weighted.mean(snittkarakter, antall_elever, na.rm = T),
    antall_elever = sum(antall_elever, na.rm = T),
    .groups = "drop_last"
  ) %>%
  arrange(period) %>%
  mutate(
    diff_abs = snittkarakter - lag(snittkarakter),
    diff_rel = (snittkarakter / lag(snittkarakter)) - 1
  ) %>%
  ungroup()

res_snitt_skole %>%
  filter(period %in% c('2018_19', '2019_20')) %>%
  pivot_wider(c(organisasjonsnummer, enhet_navn), names_from=period, values_from=c(diff_abs, diff_rel, antall_elever, snittkarakter)) %>%
  filter(!is.na(diff_rel_2019_20)) %>%
  arrange(desc(diff_rel_2019_20)) %>%
  transmute(
    Skole = enhet_navn,
    `2018/19` = round(snittkarakter_2018_19, 2),
    `2019/20` = round(snittkarakter_2019_20, 2),
    'Endring (%)' = round(diff_rel_2019_20 * 100, 1),
    'Endring' = round(diff_abs_2019_20, 2)
  )

# avvik fra prepandemisk trend
# forutsetter normalfordeling
z_score <- function(df) {
  df %>%
    arrange(period) %>%
    mutate(
      diff_abs = snitt - lag(snitt),
      diff_rel = (snitt / lag(snitt)) - 1,
      trend_period = period > '2007_08' & period < '2019_20',

      across(
        c(diff_abs, diff_rel),
        list(
          mean = ~mean(.x[trend_period]),
          sd = ~sd(.x[trend_period])
        )
      ),

      z_abs = (diff_abs - diff_abs_mean) / diff_abs_sd,
      z_rel = (diff_rel - diff_rel_mean) / diff_rel_sd,

      p_abs = 2 * pnorm(-abs(z_abs)),
      p_rel = 2 * pnorm(-abs(z_rel))
    )
}

res_snitt %>% z_score()

res_snitt %>%
  z_score() %>%
  filter(!is.na(diff_abs)) %>%
  pull(diff_abs) %>%
  shapiro.test()

res_snitt_eieform %>%
  group_by(eieform) %>%
  z_score() %>%
  ungroup() %>%
  filter(!is.na(diff_abs)) %>%
  arrange(period) %>%
  pivot_longer(c(z_abs, z_rel)) %>%
  ggplot(aes(x = period, y = value, group = eieform, color = eieform)) +
  geom_texthline(
    aes(yintercept = z, label = desc),
    linetype = "dashed",
    hjust = 0.2,
    data = tribble(
      ~z, ~desc,
      1.65, "p < .10",
      1.96, "p < .05",
      2.58, "p < .01",
      -1.65, "p < .10",
      -1.96, "p < .05",
      -2.58, "p < .01",
    )
  ) +
  geom_line() +
  facet_wrap(~name) +
  scale_x_discrete(breaks = unique(res_snitt_eieform$period)[seq(0,15,2)])


# per skole: ikke normalfordelt
res_snitt_skole %>%
  group_by(organisasjonsnummer, enhet_navn) %>%
  mutate(
    n_periods = n_distinct(period),
    students = mean(antall_elever)
  ) %>%
  ungroup() %>%
  filter(!is.na(diff_abs), n_periods > 10, students > 1000) %>%
  pull(diff_abs) %>%
  shapiro.test()

diff_abs <- res_snitt_skole %>%
  filter(!is.na(diff_abs), period < '2019_20') %>%
  pull(diff_abs)

diff_abs %>% fitdistrplus::descdist(discrete = F)
diff_abs_ecdf <- ecdf(diff_abs)

ggplot() +
  geom_density(
    aes(x=diff_abs, color = 'empirical')
  ) +
  geom_line(
    aes(color = 'normal', x=x, y=y),
    data = tibble(
      x = seq(min(diff_abs),max(diff_abs),0.01),
      y = dnorm(x, mean(diff_abs), sd(diff_abs))
    ),
    size = 1,
    alpha = .7,
    linetype = 'dashed'
  )

res_snitt_skole %>%
  arrange(desc(diff_abs)) %>%
  transmute(enhet_navn, period, diff_abs, antall_elever)





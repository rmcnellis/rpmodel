# test optimal vcmax predicitons

library(ggplot2)
library(patchwork)
library(dplyr)

## Helen's C4 Model
# source('/Users/risa/Git/NutNetPhys/Analysis/C4model/C4model.R')
# sourceDirectory('/Users/risa/Git/NutNetPhys/Analysis/C4model/functions', modifiedOnly = FALSE)
# 
# par_c4 = C4model(paro = seq(100, 1000, 100))
# t_c4 = C4model(tg_c = seq(10, 30, 5))
# vpd_c4 = C4model(vpdo = seq(0.5, 4, 0.5))
# z_c4 = C4model(z = seq(0, 1000, 100))
# ca_c4 = C4model(cao = seq(250, 1000, 50))

#### rpmodel ####
# library(rpmodel)
source('../R/rpmodel_mod.R')
source('../R/subroutines_mod.R')

# PAR
par_c3_rp_list = rpmodel(tc = 25, vpd = 1000, co2 = 400, fapar = 1, ppfd = seq(100, 1000, 100), elv = 0,
                         c4 = FALSE, method_jmaxlim = "smith19")
par_c3_rp <- as.data.frame(do.call(cbind, par_c3_rp_list))
par_c3_rp$par <- seq(100, 1000, 100)
par_c4_rp_list = rpmodel(tc = 25, vpd = 1000, co2 = 400, fapar = 1, ppfd = seq(100, 1000, 100), elv = 0,
                         c4 = TRUE, method_jmaxlim = "smith19")
par_c4_rp <- as.data.frame(do.call(cbind, par_c4_rp_list))
par_c4_rp$par <- seq(100, 1000, 100)

# Temperature
t_c3_rp_list = rpmodel(tc = seq(10, 30, 5), vpd = 1000, co2 = 400, fapar = 1, ppfd = 800, elv = 0,
                       c4 = FALSE, method_jmaxlim = "smith19")
t_c3_rp <- as.data.frame(do.call(cbind, t_c3_rp_list))
t_c3_rp$tg_c <- seq(10, 30, 5)
t_c4_rp_list = rpmodel(tc = seq(10, 30, 5), vpd = 1000, co2 = 400, fapar = 1, ppfd = 800, elv = 0,
                       c4 = TRUE, method_jmaxlim = "smith19")
t_c4_rp <- as.data.frame(do.call(cbind, t_c4_rp_list))
t_c4_rp$tg_c <- seq(10, 30, 5)

t_c4_rp_q_list = rpmodel(tc = seq(10, 30, 5), vpd = 1000, co2 = 400, fapar = 1, ppfd = 800, elv = 0,
                         c4 = TRUE, method_jmaxlim = "smith19", kphio = 0.257, do_ftemp_kphio = FALSE)
t_c4_rp_q <- as.data.frame(do.call(cbind, t_c4_rp_q_list))
t_c4_rp_q$tg_c <- seq(10, 30, 5)

# VPD
vpd_c3_rp_list = rpmodel(tc = 25, vpd = seq(500, 4000, 500), co2 = 400, fapar = 1, ppfd = 800, elv = 0,
                         c4 = FALSE, method_jmaxlim = "smith19")
vpd_c3_rp <- as.data.frame(do.call(cbind, vpd_c3_rp_list))
vpd_c3_rp$vpd <- seq(0.5, 4, 0.5)
vpd_c4_rp_list = rpmodel(tc = 25, vpd = seq(500, 4000, 500), co2 = 400, fapar = 1, ppfd = 800, elv = 0,
                         c4 = TRUE, method_jmaxlim = "smith19")
vpd_c4_rp <- as.data.frame(do.call(cbind, vpd_c4_rp_list))
vpd_c4_rp <- vpd_c4_rp %>% slice(rep(1, 8))
vpd_c4_rp$vpd <- seq(0.5, 4, 0.5)

# Elevation
z_c3_rp_list = rpmodel(tc = 25, vpd = 1000, co2 = 400, fapar = 1, ppfd = 800, elv = seq(0, 1000, 100),
                       c4 = FALSE, method_jmaxlim = "smith19")
z_c3_rp <- as.data.frame(do.call(cbind, z_c3_rp_list))
z_c3_rp$z <- seq(0, 1000, 100)
z_c4_rp_list = rpmodel(tc = 25, vpd = 1000, co2 = 400, fapar = 1, ppfd = 800, elv = seq(0, 1000, 100),
                       c4 = TRUE, method_jmaxlim = "smith19")
z_c4_rp <- as.data.frame(do.call(cbind, z_c4_rp_list))
z_c4_rp$z <- seq(0, 1000, 100)

# CO2
ca_c3_rp_list = rpmodel(tc = 25, vpd = 1000, co2 = seq(250, 1000, 50), fapar = 1, ppfd = 800, elv = 0,
                        c4 = FALSE, method_jmaxlim = "smith19")
ca_c3_rp <- as.data.frame(do.call(cbind, ca_c3_rp_list))
ca_c3_rp$ca <- seq(25, 100, 5)
ca_c4_rp_list = rpmodel(tc = 25, vpd = 1000, co2 = seq(250, 1000, 50), fapar = 1, ppfd = 800, elv = 0,
                        c4 = TRUE, method_jmaxlim = "smith19")
ca_c4_rp <- as.data.frame(do.call(cbind, ca_c4_rp_list))
ca_c4_rp$ca <- seq(25, 100, 5)

#### new rpmodel ####
source('../R/rpmodel_Scott.R')
source('../R/subroutines_Scott.R')

# PAR
par_c3_nrp <- rpmodel(tc = 25, vpd = 1000, co2 = 400, o2 = 209460, fapar = 1, ppfd = seq(100, 1000, 100), elv = 0,
                          c4 = FALSE, method_jmaxlim = "smith19")
par_c4_nrp <- rpmodel(tc = 25, vpd = 1000, co2 = 400, o2 = 209460, fapar = 1, ppfd = seq(100, 1000, 100), elv = 0,
                          c4 = TRUE, method_jmaxlim = "smith19")

# Temperature
t_c3_nrp <- rpmodel(tc = seq(10, 30, 5), vpd = 1000, co2 = 400, o2 = 209460, fapar = 1, ppfd = 800, elv = 0,
                        c4 = FALSE, method_jmaxlim = "smith19")
t_c4_nrp <- rpmodel(tc = seq(10, 30, 5), vpd = 1000, co2 = 400, o2 = 209460, fapar = 1, ppfd = 800, elv = 0,
                        c4 = TRUE, method_jmaxlim = "smith19")

t_c4_nrp_q <- rpmodel(tc = seq(10, 30, 5), vpd = 1000, co2 = 400, o2 = 209460, fapar = 1, ppfd = 800, elv = 0,
                          c4 = TRUE, method_jmaxlim = "smith19", kphio = 0.257, do_ftemp_kphio = FALSE)

# VPD
vpd_c3_nrp <- rpmodel(tc = 25, vpd = seq(500, 4000, 500), co2 = 400, o2 = 209460, fapar = 1, ppfd = 800, elv = 0,
                          c4 = FALSE, method_jmaxlim = "smith19")
vpd_c3_nrp$vpd <- vpd_c3_nrp$vpd_pa / 1000
vpd_c4_nrp <- rpmodel(tc = 25, vpd = seq(500, 4000, 500), co2 = 400, o2 = 209460, fapar = 1, ppfd = 800, elv = 0,
                          c4 = TRUE, method_jmaxlim = "smith19")
vpd_c4_nrp$vpd <- vpd_c4_nrp$vpd_pa / 1000

# Elevation
z_c3_nrp <- rpmodel(tc = 25, vpd = 1000, co2 = 400, o2 = 209460, fapar = 1, ppfd = 800, elv = seq(0, 1000, 100),
                        c4 = FALSE, method_jmaxlim = "smith19")
z_c4_nrp <- rpmodel(tc = 25, vpd = 1000, co2 = 400, o2 = 209460, fapar = 1, ppfd = 800, elv = seq(0, 1000, 100),
                        c4 = TRUE, method_jmaxlim = "smith19")

# CO2
ca_c3_nrp <- rpmodel(tc = 25, vpd = 1000, co2 = seq(250, 1000, 50), o2 = 209460, fapar = 1, ppfd = 800, elv = 0,
                         c4 = FALSE, method_jmaxlim = "smith19")
ca_c4_nrp <- rpmodel(tc = 25, vpd = 1000, co2 = seq(250, 1000, 50), o2 = 209460, fapar = 1, ppfd = 800, elv = 0,
                         c4 = TRUE, method_jmaxlim = "smith19")

#### Compare C4 Models vcmax ####
(vcmax_par <- ggplot() + 
     geom_line(data = par_c4_rp, aes(x = par, y = vcmax, color = "rpmodel"), size = 2) +
     geom_line(data = par_c4_nrp, aes(x = par, y = vcmax, color = "rpmodel with Scott & Smith 2022"), size = 2) +
     xlab("PAR") + 
     scale_y_continuous(name = "C4 vcmax", limits = c(0, 400)) +
     scale_color_manual(name = 'Model',
                       breaks = c("rpmodel", "rpmodel with Scott & Smith 2022"),
                       values = c("rpmodel" = "dark red", "rpmodel with Scott & Smith 2022" = "pink")))
(vcmax_temp <- ggplot() + 
        geom_line(data = t_c4_rp, aes(x = tg_c, y = vcmax, color = "rpmodel"),  size = 2) +
        geom_line(data = t_c4_nrp, aes(x = tg_c, y = vcmax, color = "rpmodel with Scott & Smith 2022"),  size = 2) +
        xlab("temperature") + 
        scale_y_continuous(name = "C4 vcmax", limits = c(0, 400)) +
        scale_color_manual(name = 'Model',
                          breaks = c("rpmodel", "rpmodel with Scott & Smith 2022"),
                          values = c("rpmodel" = "dark red", "rpmodel with Scott & Smith 2022" = "pink")))
(vcmax_vpd <- ggplot() + 
        geom_line(data = vpd_c4_rp, aes(x = vpd, y = vcmax, color = "rpmodel"),  size = 2) +
        geom_line(data = vpd_c4_nrp, aes(x = vpd, y = vcmax, color = "rpmodel with Scott & Smith 2022"),  size = 2) +
        xlab("VPD") + 
        scale_y_continuous(name = "C4 vcmax", limits = c(0, 400)) +
        scale_color_manual(name = 'Model',
                          breaks = c("rpmodel", "rpmodel with Scott & Smith 2022"),
                          values = c("rpmodel" = "dark red", "rpmodel with Scott & Smith 2022" = "pink")))
(vcmax_elv <- ggplot() + 
        geom_line(data = z_c4_rp, aes(x = z, y = vcmax, color = "rpmodel"),  size = 2) +
        geom_line(data = z_c4_nrp, aes(x = z, y = vcmax, color = "rpmodel with Scott & Smith 2022"),  size = 2) +
        xlab("elevation") + 
        scale_y_continuous(name = "C4 vcmax", limits = c(0, 400)) +
        scale_color_manual(name = 'Model',
                          breaks = c("rpmodel", "rpmodel with Scott & Smith 2022"),
                          values = c("rpmodel" = "dark red", "rpmodel with Scott & Smith 2022" = "pink")))
(vcmax_ca <- ggplot() + 
        geom_line(data = ca_c4_rp, aes(x = ca, y = vcmax, color = "rpmodel"),  size = 2) +
        geom_line(data = ca_c4_nrp, aes(x = ca, y = vcmax, color = "rpmodel with Scott & Smith 2022"),  size = 2) +
        xlab("CO2") + 
        scale_y_continuous(name = "C4 vcmax", limits = c(0, 400)) +
        scale_color_manual(name = 'Model',
                          breaks = c("rpmodel", "rpmodel with Scott & Smith 2022"),
                          values = c("rpmodel" = "dark red", "rpmodel with Scott & Smith 2022" = "pink")))

(C4_vcmax <- vcmax_par + vcmax_temp + vcmax_vpd + vcmax_elv + vcmax_ca + guide_area() +
        plot_layout(guides = 'collect'))

#### Compare C4 Models chi #### 
(chi_temp <- ggplot() + 
     geom_line(data = t_c4_rp, aes(x = tg_c, y = chi, color = "rpmodel"),  size = 2) +
     geom_line(data = t_c4_nrp, aes(x = tg_c, y = chi, color = "rpmodel with Scott & Smith 2022"),  size = 2) +
     xlab("temperature") + 
     scale_y_continuous(name = "C4 chi", limits = c(0.35, 1)) +
     scale_color_manual(name = 'Model',
                       breaks = c("rpmodel", "rpmodel with Scott & Smith 2022"),
                       values = c("rpmodel" = "dark red", "rpmodel with Scott & Smith 2022" = "pink")))
(chi_vpd <- ggplot() + 
        geom_line(data = vpd_c4_rp, aes(x = vpd, y = chi, color = "rpmodel"),  size = 2) +
        geom_line(data = vpd_c4_nrp, aes(x = vpd, y = chi, color = "rpmodel with Scott & Smith 2022"),  size = 2) +
        xlab("VPD") + 
        scale_y_continuous(name = "C4 chi", limits = c(0.35, 1)) +
        scale_color_manual(name = 'Model',
                          breaks = c("rpmodel", "rpmodel with Scott & Smith 2022"),
                          values = c("rpmodel" = "dark red", "rpmodel with Scott & Smith 2022" = "pink")))
(chi_elv <- ggplot() + 
        geom_line(data = z_c4_rp, aes(x = z, y = chi, color = "rpmodel"),  size = 2) +
        geom_line(data = z_c4_nrp, aes(x = z, y = chi, color = "rpmodel with Scott & Smith 2022"),  size = 2) +
        xlab("elevation") + 
        scale_y_continuous(name = "C4 chi", limits = c(0.35, 1)) +
        scale_color_manual(name = 'Model',
                          breaks = c("rpmodel", "rpmodel with Scott & Smith 2022"),
                          values = c("rpmodel" = "dark red", "rpmodel with Scott & Smith 2022" = "pink")))
(C4_chi <- chi_temp + chi_vpd + chi_elv + guide_area() +
        plot_layout(guides = 'collect'))

#### Compare C4 Models Ac #### 
(Ac_par <- ggplot() + 
     geom_line(data = par_c4_rp, aes(x = par, y = Ac, color = "rpmodel"),  size = 2) +
     geom_line(data = par_c4_nrp, aes(x = par, y = Ac, color = "rpmodel with Scott & Smith 2022"),  size = 2) +
     xlab("PAR") + 
     scale_y_continuous(name = "C4 photosynthesis", limits = c(0, 150)) +
     scale_color_manual(name = 'Model',
                       breaks = c("rpmodel", "rpmodel with Scott & Smith 2022"),
                       values = c("rpmodel" = "dark red", "rpmodel with Scott & Smith 2022" = "pink")))
(Ac_temp <- ggplot() + 
        geom_line(data = t_c4_rp, aes(x = tg_c, y = Ac, color = "rpmodel"),  size = 2) +
        geom_line(data = t_c4_nrp, aes(x = tg_c, y = Ac, color = "rpmodel with Scott & Smith 2022"),  size = 2) +
        xlab("temperature") +      
        scale_y_continuous(name = "C4 photosynthesis", limits = c(0, 150)) +
        scale_color_manual(name = 'Model',
                          breaks = c("rpmodel", "rpmodel with Scott & Smith 2022"),
                          values = c("rpmodel" = "dark red", "rpmodel with Scott & Smith 2022" = "pink")))
(Ac_vpd <- ggplot() + 
        geom_line(data = vpd_c4_rp, aes(x = vpd, y = Ac, color = "rpmodel"),  size = 2) +
        geom_line(data = vpd_c4_nrp, aes(x = vpd, y = Ac, color = "rpmodel with Scott & Smith 2022"),  size = 2) +
        xlab("VPD") + 
        scale_y_continuous(name = "C4 photosynthesis", limits = c(0, 150)) +
        scale_color_manual(name = 'Model',
                          breaks = c("rpmodel", "rpmodel with Scott & Smith 2022"),
                          values = c("rpmodel" = "dark red", "rpmodel with Scott & Smith 2022" = "pink")))
(Ac_elv <- ggplot() + 
        geom_line(data = z_c4_rp, aes(x = z, y = Ac, color = "rpmodel"),  size = 2) +
        geom_line(data = z_c4_nrp, aes(x = z, y = Ac, color = "rpmodel with Scott & Smith 2022"),  size = 2) +
        xlab("elevation") + 
        scale_y_continuous(name = "C4 photosynthesis", limits = c(0, 150)) +
        scale_color_manual(name = 'Model',
                          breaks = c("rpmodel", "rpmodel with Scott & Smith 2022"),
                          values = c("rpmodel" = "dark red", "rpmodel with Scott & Smith 2022" = "pink")))
(Ac_ca <- ggplot() + 
        geom_line(data = ca_c4_rp, aes(x = ca, y = Ac, color = "rpmodel"),  size = 2) +
        geom_line(data = ca_c4_nrp, aes(x = ca, y = Ac, color = "rpmodel with Scott & Smith 2022"),  size = 2) +
        xlab("CO2") +      
        scale_y_continuous(name = "C4 photosynthesis", limits = c(0, 150)) +
        scale_color_manual(name = 'Model',
                          breaks = c("rpmodel", "rpmodel with Scott & Smith 2022"),
                          values = c("rpmodel" = "dark red", "rpmodel with Scott & Smith 2022" = "pink")))
(C4_Ac <- Ac_par + Ac_temp + Ac_vpd + Ac_elv + Ac_ca + guide_area() +
        plot_layout(guides = 'collect'))

ggsave("C4_vcmax.jpeg", C4_vcmax, 
       width = 1000, height = 500, units = "px", dpi = 100)
ggsave("C4_chi.jpeg", C4_chi, 
       width = 1000, height = 500, units = "px", dpi = 100)
ggsave("C4_ac.jpeg", C4_Ac, 
       width = 1000, height = 500, units = "px", dpi = 100)

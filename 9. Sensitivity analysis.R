#Sensitivity analysis

#load package
library(sensemakr)

#load datat
Optho <- read.csv("Fixed optho num proxy.csv")
Optho3 <- Optho[2:26]

# runs regression model
#-----------------------------------------------------------------
#CVM
Optho.model.CVM <- lm(Cortical.visual.impairement ~ PBD.group, 
                      data = Optho3)
summary(Optho.model.CVM)

# runs sensemakr for sensitivity analysis
optho.sensitivity.cmv <- sensemakr(model = Optho.model.CVM, 
                                   treatment = "PBD.group")
optho.sensitivity.cmv
summary(optho.sensitivity.cmv)
#-----------------------------------------------------------------
#Cataract
Optho.model.Cataract <- lm(Cataract ~ PBD.group, 
                      data = Optho3)
summary(Optho.model.Cataract)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Cataract <- sensemakr(model = Optho.model.Cataract, 
                                   treatment = "PBD.group")
summary(optho.sensitivity.Cataract)
#-----------------------------------------------------------------
#Nyctalopia
Optho.model.Nyctalopia <- lm(Nyctalopia ~ PBD.group, 
                           data = Optho3)
summary(Optho.model.Nyctalopia)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Nyctalopia <- sensemakr(model = Optho.model.Nyctalopia, 
                                        treatment = "PBD.group")
optho.sensitivity.Nyctalopia
summary(optho.sensitivity.Nyctalopia)
#-----------------------------------------------------------------
#Maculopathy
Optho.model.Maculopathy <- lm(Maculopathy ~ PBD.group, 
                           data = Optho3)
summary(Optho.model.Maculopathy)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Maculopathy <- sensemakr(model = Optho.model.Maculopathy, 
                                          treatment = "PBD.group")
optho.sensitivity.Maculopathy
summary(optho.sensitivity.Maculopathy)
#-----------------------------------------------------------------
#Retinitis.pigmentosa
Optho.model.Retinitis.pigmentosa <- lm(Retinitis.pigmentosa ~ PBD.group, 
                              data = Optho3)
summary(Optho.model.Retinitis.pigmentosa)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Retinitis.pigmentosa <- sensemakr(model = Optho.model.Retinitis.pigmentosa, 
                                           treatment = "PBD.group")
optho.sensitivity.Retinitis.pigmentosa
summary(optho.sensitivity.Retinitis.pigmentosa)
#-----------------------------------------------------------------
#Bilateral.hyperopia
Optho.model.Bilateral.hyperopia <- lm(Bilateral.hyperopia ~ PBD.group, 
                              data = Optho3)
summary(Optho.model.Bilateral.hyperopia)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Bilateral.hyperopia <- sensemakr(model = Optho.model.Bilateral.hyperopia, 
                                                    treatment = "PBD.group")
optho.sensitivity.Bilateral.hyperopia
summary(optho.sensitivity.Bilateral.hyperopia)
#-----------------------------------------------------------------
#Blindness
Optho.model.Blindness <- lm(Blindness ~ PBD.group, 
                                      data = Optho3)
summary(Optho.model.Blindness)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Blindness <- sensemakr(model = Optho.model.Blindness, 
                                                   treatment = "PBD.group")
optho.sensitivity.Blindness
summary(optho.sensitivity.Blindness)
#-----------------------------------------------------------------
#Optic.Nerve.Atrophy
Optho.model.Optic.Nerve.Atrophy <- lm(Optic.Nerve.Atrophy ~ PBD.group, 
                                      data = Optho3)
summary(Optho.model.Optic.Nerve.Atrophy)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Optic.Nerve.Atrophy <- sensemakr(model = Optho.model.Optic.Nerve.Atrophy, 
                                         treatment = "PBD.group")
optho.sensitivity.Optic.Nerve.Atrophy
summary(optho.sensitivity.Optic.Nerve.Atrophy)
#-----------------------------------------------------------------
#Nystagmus..TRUE
Optho.model.Nystagmus..TRUE <- lm(Nystagmus..TRUE ~ PBD.group, 
                                      data = Optho3)
summary(Optho.model.Nystagmus..TRUE)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Nystagmus..TRUE <- sensemakr(model = Optho.model.Nystagmus..TRUE, 
                                                   treatment = "PBD.group")
optho.sensitivity.Nystagmus..TRUE
summary(optho.sensitivity.Nystagmus..TRUE)
#-----------------------------------------------------------------
#Macula..Normal.Appearing
Optho.model.Macula..Normal.Appearing <- lm(Macula..Normal.Appearing ~ PBD.group, 
                                  data = Optho3)
summary(Optho.model.Macula..Normal.Appearing)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Macula..Normal.Appearing <- sensemakr(model = Optho.model.Macula..Normal.Appearing, 
                                               treatment = "PBD.group")
optho.sensitivity.Macula..Normal.Appearing
summary(optho.sensitivity.Macula..Normal.Appearing)
#-----------------------------------------------------------------
#Macular.Pigmentary.changes 
Optho.model.Macular.Pigmentary.changes <- lm(Macular.Pigmentary.changes  ~ PBD.group, 
                                  data = Optho3)
summary(Optho.model.Macular.Pigmentary.changes)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Macular.Pigmentary.changes <- sensemakr(model = Optho.model.Macular.Pigmentary.changes, 
                                                        treatment = "PBD.group")
optho.sensitivity.Macular.Pigmentary.changes
summary(optho.sensitivity.Macular.Pigmentary.changes)
#-----------------------------------------------------------------
#Macular.edema
Optho.model.Macular.edema <- lm(Macular.edema ~ PBD.group, 
                                           data = Optho3)
summary(Optho.model.Macular.edema)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Macular.edema <- sensemakr(model = Optho.model.Macular.edema, 
                                                          treatment = "PBD.group")
optho.sensitivity.Macular.edema
summary(optho.sensitivity.Macular.edema)
#-----------------------------------------------------------------
#Foveal.hypoplasia
Optho.model.Foveal.hypoplasia <- lm(Foveal.hypoplasia  ~ PBD.group, 
                                             data = Optho3)
summary(Optho.model.Foveal.hypoplasia)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Foveal.hypoplasia <- sensemakr(model = Optho.model.Foveal.hypoplasia, 
                                             treatment = "PBD.group")
optho.sensitivity.Foveal.hypoplasia
summary(optho.sensitivity.Foveal.hypoplasia)
#-----------------------------------------------------------------
#Macular thinning
Optho.model.Macular.thinning <- lm(Macular.thinning ~ PBD.group, 
                                data = Optho3)
summary(Optho.model.Macular.thinning)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Macular.thinning <- sensemakr(model = Optho.model.Macular.thinning, 
                                                 treatment = "PBD.group")
optho.sensitivity.Macular.thinning
summary(optho.sensitivity.Macular.thinning)
#-----------------------------------------------------------------
#Optic.nerve.hypoplasia
Optho.model.Optic.nerve.hypoplasia <- lm(Optic.nerve.hypoplasia  ~ PBD.group, 
                                    data = Optho3)
summary(Optho.model.Optic.nerve.hypoplasia)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Optic.nerve.hypoplasia <- sensemakr(model = Optho.model.Optic.nerve.hypoplasia, 
                                                treatment = "PBD.group")
optho.sensitivity.Optic.nerve.hypoplasia
summary(optho.sensitivity.Optic.nerve.hypoplasia)
#-----------------------------------------------------------------
#Optic.nerve.drusen
Optho.model.Optic.nerve.drusen <- lm(Optic.nerve.drusen  ~ PBD.group, 
                                    data = Optho3)
summary(Optho.model.Optic.nerve.drusen)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Optic.nerve.drusen <- sensemakr(model = Optho.model.Optic.nerve.drusen, 
                                                      treatment = "PBD.group")
optho.sensitivity.Optic.nerve.drusen
summary(optho.sensitivity.Optic.nerve.drusen)
#-----------------------------------------------------------------
#Optic.nerve..Normal.appearing
Optho.model.Optic.nerve..Normal.appearing <- lm(Optic.nerve..Normal.appearing ~ PBD.group, 
                                   data = Optho3)
summary(Optho.model.Optic.nerve..Normal.appearing)

# runs sensemakr for sensitivity analysis
optho.sensitivity.Optic.nerve..Normal.appearing <- sensemakr(model = Optho.model.Optic.nerve..Normal.appearing, 
                                                  treatment = "PBD.group")
optho.sensitivity.Optic.nerve..Normal.appearing
summary(optho.sensitivity.Optic.nerve..Normal.appearing)
#-----------------------------------------------------------------
#Optic.nerve.atrophy
Optho.model.Optic.nerve.atrophy <- lm(Optic.nerve.atrophy  ~ PBD.group, 
                                         data = Optho3)
summary(Optho.model.Optic.nerve.atrophy)
#-----------------------------------------------------------------
#Peripheral.retina..Leopard.spots
Optho.model.Peripheral.retina..Leopard.spots <- lm(Peripheral.retina..Leopard.spots  ~ PBD.group, 
                                     data = Optho3)
summary(Optho.model.Peripheral.retina..Leopard.spots)
#-----------------------------------------------------------------
#Vessels..Attenuated.vessels
Optho.model.Vessels..Attenuated.vessels <- lm(Vessels..Attenuated.vessels ~ PBD.group, 
                                                data = Optho3)
summary(Optho.model.Vessels..Attenuated.vessels)
#-----------------------------------------------------------------
#Pupils.1..Equal
Optho.model.Pupils.1..Equal <- lm(Pupils.1..Equal ~ PBD.group, 
                                     data = Optho3)
summary(Optho.model.Pupils.1..Equal)
#-----------------------------------------------------------------
#Pupils.2..Reactive
Optho.model.Pupils.2..Reactive <- lm(Pupils.2..Reactive ~ PBD.group, 
                                  data = Optho3)
summary(Optho.model.Pupils.2..Reactive)
#-----------------------------------------------------------------
#Refraction_OD_R
Optho.model.Refraction_OD_R <- lm(Refraction_OD_R ~ PBD.group, 
                                  data = Optho3)
summary(Optho.model.Refraction_OD_R)
#-----------------------------------------------------------------
#Refraction_OS_L
Optho.model.Refraction_OS_L <- lm(Refraction_OS_L ~ PBD.group, 
                                  data = Optho3)
summary(Optho.model.Refraction_OS_L)
#-----------------------------------------------------------------


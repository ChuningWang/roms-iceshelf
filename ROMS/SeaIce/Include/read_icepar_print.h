# ifdef ICE_MODEL
          IF (Hout(idIuice,ng)) WRITE (out,70) Hout(idIuice,ng),        &
     &       'Hout(idIuice)',                                           &
     &       'Write out 2D ice momentum in the XI-direction.'
          IF (Hout(idIvice,ng)) WRITE (out,70) Hout(idIvice,ng),        &
     &       'Hout(idIvice)',                                           &
     &       'Write out 2D ice momentum in the ETA-direction.'
          IF (Hout(idIuiceE,ng)) WRITE (out,70) Hout(idIuiceE,ng),      &
     &       'Hout(idIuiceE)',                                          &
     &       'Write out 2D ice momentum in the E-direction.'
          IF (Hout(idIviceN,ng)) WRITE (out,70) Hout(idIviceN,ng),      &
     &       'Hout(idIviceN)',                                          &
     &       'Write out 2D ice momentum in the N-direction.'
          IF (Hout(idIaice,ng)) WRITE (out,70) Hout(idIaice,ng),        &
     &       'Hout(idIaice)',                                           &
     &       'Write out ice concentration.'
          IF (Hout(idIhice,ng)) WRITE (out,70) Hout(idIhice,ng),        &
     &       'Hout(idIhice)',                                           &
     &       'Write out ice average thickness.'
          IF (Hout(idIhsno,ng)) WRITE (out,70) Hout(idIhsno,ng),        &
     &       'Hout(idIhsno)',                                           &
     &       'Write out snow thickness.'
          IF (Hout(idIagei,ng)) WRITE (out,70) Hout(idIagei,ng),        &
     &       'Hout(idIagei)',                                           &
     &       'Write out ice age.'
          IF (Hout(idItice,ng)) WRITE (out,70) Hout(idItice,ng),        &
     &       'Hout(idItice)',                                           &
     &       'Write out temperature of ice/snow surface.'
          IF (Hout(idItmid,ng)) WRITE (out,70) Hout(idItmid,ng),        &
     &       'Hout(idItmid)',                                           &
     &       'Write out temperature of internal ice.'
          IF (Hout(idIt2ic,ng)) WRITE (out,70) Hout(idIt2ic,ng),        &
     &       'Hout(idIt2ic)',                                           &
     &       'Write out temperature of top ice.'
          IF (Hout(idIsg11,ng)) WRITE (out,70) Hout(idIsg11,ng),        &
     &       'Hout(idIsg11)',                                           &
     &       'Write out internal ice stress component 11.'
          IF (Hout(idIsg12,ng)) WRITE (out,70) Hout(idIsg12,ng),        &
     &       'Hout(idIsg12)',                                           &
     &       'Write out internal ice stress component 12.'
          IF (Hout(idIsg22,ng)) WRITE (out,70) Hout(idIsg22,ng),        &
     &       'Hout(idIsg22)',                                           &
     &       'Write out internal ice stress component 22.'
          IF (Hout(idIutau,ng)) WRITE (out,70) Hout(idIutau,ng),        &
     &       'Hout(idIutau)',                                           &
     &       'Write out ice-water friction velocity.'
          IF (Hout(idImchu,ng)) WRITE (out,70) Hout(idImchu,ng),        &
     &       'Hout(idImchu)',                                           &
     &       'Write out ice-water momentum transfer coef.'
          IF (Hout(idIt0mk,ng)) WRITE (out,70) Hout(idIt0mk,ng),        &
     &       'Hout(idIt0mk)',                                           &
     &       'Write out T of molecular sublayer under ice.'
          IF (Hout(idIs0mk,ng)) WRITE (out,70) Hout(idIs0mk,ng),        &
     &       'Hout(idIs0mk)',                                           &
     &       'Write out S of molecular sublayer under ice.'
          IF (Hout(idImflx,ng)) WRITE (out,70) Hout(idImflx,ng),        &
     &       'Hout(idImflx)',                                           &
     &       'Write out ice-ocean mass flux.'
          IF (Hout(idIwfr,ng)) WRITE (out,70) Hout(idIwfr,ng),          &
     &       'Hout(idIwfr)',                                            &
     &       'Write out ice frazil growth rate.'
          IF (Hout(idIwai,ng)) WRITE (out,70) Hout(idIwai,ng),          &
     &       'Hout(idIwai)',                                            &
     &       'Write out ice growth/melt rate of ice-air.'
          IF (Hout(idIwao,ng)) WRITE (out,70) Hout(idIwao,ng),          &
     &       'Hout(idIwao)',                                            &
     &       'Write out ice growth/melt rate of leads.'
          IF (Hout(idIwio,ng)) WRITE (out,70) Hout(idIwio,ng),          &
     &       'Hout(idIwio)',                                            &
     &       'Write out ice growth/melt rate of ice-ocean.'
          IF (Hout(idIwro,ng)) WRITE (out,70) Hout(idIwro,ng),          &
     &       'Hout(idIwro)',                                            &
     &       'Write out ice melt runoff rate.'
          IF (Hout(idIwdiv,ng)) WRITE (out,70) Hout(idIwdiv,ng),        &
     &       'Hout(idIwdiv)',                                           &
     &       'Write out ice divergence rate.'
#  ifdef ICE_DIAGS
#   ifdef ICE_ALBEDO
          IF (Hout(idIalbe,ng)) WRITE (out,70) Hout(idIalbe,ng),        &
     &       'Hout(idIalbe)',                                           &
     &       'Write out ocean albedo.'
          IF (Hout(idIalbI,ng)) WRITE (out,70) Hout(idIalbI,ng),        &
     &       'Hout(idIalbI)',                                           &
     &       'Write out ice albedo.'
#    ifdef ICE_ALB_CSIM
          IF (Hout(idIfrsw,ng)) WRITE (out,70) Hout(idIfrsw,ng),        &
     &       'Hout(idIfrsw)',                                           &
     &       'Write out fraction of SW penetrating into ice.'
#    endif
#   endif
          IF (Hout(idIrfac,ng)) WRITE (out,70) Hout(idIrfac,ng),        &
     &       'Hout(idIrfac)',                                           &
     &       'Write out reduction fac of near-IR SW under ice.'
          IF (Hout(idIlrdn,ng)) WRITE (out,70) Hout(idIlrdn,ng),        &
     &       'Hout(idIlrdn)',                                           &
     &       'Write out downward longwave radiation.'
          IF (Hout(idIsrdn,ng)) WRITE (out,70) Hout(idIsrdn,ng),        &
     &       'Hout(idIsrdn)',                                           &
     &       'Write out downward shortwave radiation.'
          IF (Hout(idIlatI,ng)) WRITE (out,70) Hout(idIlatI,ng),        &
     &       'Hout(idIlatI)',                                           &
     &       'Write out latent heat over ice.'
          IF (Hout(idIsenI,ng)) WRITE (out,70) Hout(idIsenI,ng),        &
     &       'Hout(idIsenI)',                                           &
     &       'Write out sensible heat over ice.'
          IF (Hout(idIlrup,ng)) WRITE (out,70) Hout(idIlrup,ng),        &
     &       'Hout(idIlrup)',                                           &
     &       'Write out upward longwave over ice.'
          IF (Hout(idIsrup,ng)) WRITE (out,70) Hout(idIsrup,ng),        &
     &       'Hout(idIsrup)',                                           &
     &       'Write out upward shortwave over ice.'
          IF (Hout(idIsrin,ng)) WRITE (out,70) Hout(idIsrin,ng),        &
     &       'Hout(idIsrin)',                                           &
     &       'Write out shortwave into ice.'
          IF (Hout(idIsrth,ng)) WRITE (out,70) Hout(idIsrth,ng),        &
     &       'Hout(idIsrth)',                                           &
     &       'Write out shortwave through ice.'
          IF (Hout(idIsfI,ng)) WRITE (out,70) Hout(idIsfI,ng),          &
     &       'Hout(idIsfI)',                                            &
     &       'Write out salt flux under ice.'
          IF (Hout(idIsfL,ng)) WRITE (out,70) Hout(idIsfL,ng),          &
     &       'Hout(idIsfL)',                                            &
     &       'Write out salt flux through leads.'
          IF (Hout(idIqao,ng)) WRITE (out,70) Hout(idIqao,ng),          &
     &       'Hout(idIqao)',                                            &
     &       'Write out heat flux through leads.'
          IF (Hout(idIqai,ng)) WRITE (out,70) Hout(idIqai,ng),          &
     &       'Hout(idIqai)',                                            &
     &       'Write out heat flux of ice-air.'
          IF (Hout(idIqio,ng)) WRITE (out,70) Hout(idIqio,ng),          &
     &       'Hout(idIqio)',                                            &
     &       'Write out heat flux of ice-ocean.'
          IF (Hout(idIqi2,ng)) WRITE (out,70) Hout(idIqi2,ng),          &
     &       'Hout(idIqi2)',                                            &
     &       'Write out ice heat flux.'
          IF (Hout(idIsnoi,ng)) WRITE (out,70) Hout(idIsnoi,ng),        &
     &       'Hout(idIsnoi)',                                           &
     &       'Write out snow-ice conversion.'
#  endif
# endif
# ifdef ICESHELF
          IF (Hout(idIsMr,ng)) WRITE (out,70) Hout(idIsMr,ng),          &
     &       'Hout(idIsMr)',                                            &
     &       'Write out iceshelf melt rate.'
#  ifdef ICESHELF_3EQN_VBC
          IF (Hout(idIsTs,ng)) WRITE (out,70) Hout(idIsTs,ng),          &
     &       'Hout(idIsTs)',                                            &
     &       'Write out iceshelf frictional temperature.'
          IF (Hout(idIsUs,ng)) WRITE (out,70) Hout(idIsUs,ng),          &
     &       'Hout(idIsUs)',                                            &
     &       'Write out iceshelf frictional velocity.'
          IF (Hout(idIsTb,ng)) WRITE (out,70) Hout(idIsTb,ng),          &
     &       'Hout(idIsTb)',                                            &
     &       'Write out iceshelf molecular sublayer T.'
          IF (Hout(idIsSb,ng)) WRITE (out,70) Hout(idIsSb,ng),          &
     &       'Hout(idIsSb)',                                            &
     &       'Write out iceshelf molecular sublayer S.'
#  endif
#  ifdef ICESHELF_MORPH
          IF (Hout(idIsDrft,ng)) WRITE (out,70) Hout(idIsDrft,ng),      &
     &       'Hout(idIsDrft)',                                          &
     &       'Write out time-varying ice draft.'
#  endif
#  ifdef ICESHELF_SOF
          IF (Hout(idIsSOFs,ng)) WRITE (out,70) Hout(idIsSOFs,ng),      &
     &       'Hout(idIsSOFs)',                                          &
     &       'Write out surface salt overflux.'
          IF (Hout(idIsSOFh,ng)) WRITE (out,70) Hout(idIsSOFh,ng),      &
     &       'Hout(idIsSOFh)',                                          &
     &       'Write out surface heat overflux.'
#  endif
# endif

# ifdef ICE_MODEL
          IF (Aout(idIuice,ng)) WRITE (out,70) Aout(idIuice,ng),        &
     &       'Aout(idIuice)',                                           &
     &       'Write out averaged 2D ice momentum in the XI-direction.'
          IF (Aout(idIvice,ng)) WRITE (out,70) Aout(idIvice,ng),        &
     &       'Aout(idIvice)',                                           &
     &       'Write out averaged 2D ice momentum in the ETA-direction.'
          IF (Aout(idIuiceE,ng)) WRITE (out,70) Aout(idIuiceE,ng),      &
     &       'Aout(idIuiceE)',                                          &
     &       'Write out averaged 2D ice momentum in the E-direction.'
          IF (Aout(idIviceN,ng)) WRITE (out,70) Aout(idIviceN,ng),      &
     &       'Aout(idIviceN)',                                          &
     &       'Write out averaged 2D ice momentum in the N-direction.'
          IF (Aout(idIaice,ng)) WRITE (out,70) Aout(idIaice,ng),        &
     &       'Aout(idIaice)',                                           &
     &       'Write out averaged ice concentration.'
          IF (Aout(idIhice,ng)) WRITE (out,70) Aout(idIhice,ng),        &
     &       'Aout(idIhice)',                                           &
     &       'Write out averaged ice average thickness.'
          IF (Aout(idIhsno,ng)) WRITE (out,70) Aout(idIhsno,ng),        &
     &       'Aout(idIhsno)',                                           &
     &       'Write out averaged snow thickness.'
          IF (Aout(idIagei,ng)) WRITE (out,70) Aout(idIagei,ng),        &
     &       'Aout(idIagei)',                                           &
     &       'Write out averaged ice age.'
          IF (Aout(idItice,ng)) WRITE (out,70) Aout(idItice,ng),        &
     &       'Aout(idItice)',                                           &
     &       'Write out averaged temperature of ice/snow surface.'
          IF (Aout(idItmid,ng)) WRITE (out,70) Aout(idItmid,ng),        &
     &       'Aout(idItmid)',                                           &
     &       'Write out averaged temperature of internal ice.'
          IF (Aout(idIt2ic,ng)) WRITE (out,70) Aout(idIt2ic,ng),        &
     &       'Aout(idIt2ic)',                                           &
     &       'Write out averaged temperature of top ice.'
          IF (Aout(idIsg11,ng)) WRITE (out,70) Aout(idIsg11,ng),        &
     &       'Aout(idIsg11)',                                           &
     &       'Write out averaged internal ice stress component 11.'
          IF (Aout(idIsg12,ng)) WRITE (out,70) Aout(idIsg12,ng),        &
     &       'Aout(idIsg12)',                                           &
     &       'Write out averaged internal ice stress component 12.'
          IF (Aout(idIsg22,ng)) WRITE (out,70) Aout(idIsg22,ng),        &
     &       'Aout(idIsg22)',                                           &
     &       'Write out averaged internal ice stress component 22.'
          IF (Aout(idIutau,ng)) WRITE (out,70) Aout(idIutau,ng),        &
     &       'Aout(idIutau)',                                           &
     &       'Write out averaged ice-water friction velocity.'
          IF (Aout(idImchu,ng)) WRITE (out,70) Aout(idImchu,ng),        &
     &       'Aout(idImchu)',                                           &
     &       'Write out averaged ice-water momentum transfer coef.'
          IF (Aout(idIt0mk,ng)) WRITE (out,70) Aout(idIt0mk,ng),        &
     &       'Aout(idIt0mk)',                                           &
     &       'Write out averaged T of molecular sublayer under ice.'
          IF (Aout(idIs0mk,ng)) WRITE (out,70) Aout(idIs0mk,ng),        &
     &       'Aout(idIs0mk)',                                           &
     &       'Write out averaged S of molecular sublayer under ice.'
          IF (Aout(idImflx,ng)) WRITE (out,70) Aout(idImflx,ng),        &
     &       'Aout(idImflx)',                                           &
     &       'Write out averaged ice-ocean mass flux.'
          IF (Aout(idIwfr,ng)) WRITE (out,70) Aout(idIwfr,ng),          &
     &       'Aout(idIwfr)',                                            &
     &       'Write out averaged ice frazil growth rate.'
          IF (Aout(idIwai,ng)) WRITE (out,70) Aout(idIwai,ng),          &
     &       'Aout(idIwai)',                                            &
     &       'Write out averaged ice growth/melt rate of ice-air.'
          IF (Aout(idIwao,ng)) WRITE (out,70) Aout(idIwao,ng),          &
     &       'Aout(idIwao)',                                            &
     &       'Write out averaged ice growth/melt rate of leads.'
          IF (Aout(idIwio,ng)) WRITE (out,70) Aout(idIwio,ng),          &
     &       'Aout(idIwio)',                                            &
     &       'Write out averaged ice growth/melt rate of ice-ocean.'
          IF (Aout(idIwro,ng)) WRITE (out,70) Aout(idIwro,ng),          &
     &       'Aout(idIwro)',                                            &
     &       'Write out averaged ice melt runoff rate.'
          IF (Aout(idIwdiv,ng)) WRITE (out,70) Aout(idIwdiv,ng),        &
     &       'Aout(idIwdiv)',                                           &
     &       'Write out averaged ice divergence rate.'
#  ifdef ICE_DIAGS
#   ifdef ICE_ALBEDO
          IF (Aout(idIalbe,ng)) WRITE (out,70) Aout(idIalbe,ng),        &
     &       'Aout(idIalbe)',                                           &
     &       'Write out averaged ocean albedo.'
          IF (Aout(idIalbI,ng)) WRITE (out,70) Aout(idIalbI,ng),        &
     &       'Aout(idIalbI)',                                           &
     &       'Write out averaged ice albedo.'
#    ifdef ICE_ALB_CSIM
          IF (Aout(idIfrsw,ng)) WRITE (out,70) Aout(idIfrsw,ng),        &
     &       'Aout(idIfrsw)',                                           &
     &       'Write out averaged fraction of SW penetrating into ice.'
#    endif
#   endif
          IF (Aout(idIrfac,ng)) WRITE (out,70) Aout(idIrfac,ng),        &
     &       'Aout(idIrfac)',                                           &
     &       'Write out averaged reduction fac of near-IR SW under ice.'
          IF (Aout(idIlrdn,ng)) WRITE (out,70) Aout(idIlrdn,ng),        &
     &       'Aout(idIlrdn)',                                           &
     &       'Write out averaged downward longwave radiation.'
          IF (Aout(idIsrdn,ng)) WRITE (out,70) Aout(idIsrdn,ng),        &
     &       'Aout(idIsrdn)',                                           &
     &       'Write out averaged downward shortwave radiation.'
          IF (Aout(idIlatI,ng)) WRITE (out,70) Aout(idIlatI,ng),        &
     &       'Aout(idIlatI)',                                           &
     &       'Write out averaged latent heat over ice.'
          IF (Aout(idIsenI,ng)) WRITE (out,70) Aout(idIsenI,ng),        &
     &       'Aout(idIsenI)',                                           &
     &       'Write out averaged sensible heat over ice.'
          IF (Aout(idIlrup,ng)) WRITE (out,70) Aout(idIlrup,ng),        &
     &       'Aout(idIlrup)',                                           &
     &       'Write out averaged upward longwave over ice.'
          IF (Aout(idIsrup,ng)) WRITE (out,70) Aout(idIsrup,ng),        &
     &       'Aout(idIsrup)',                                           &
     &       'Write out averaged upward shortwave over ice.'
          IF (Aout(idIsrin,ng)) WRITE (out,70) Aout(idIsrin,ng),        &
     &       'Aout(idIsrin)',                                           &
     &       'Write out averaged shortwave into ice.'
          IF (Aout(idIsrth,ng)) WRITE (out,70) Aout(idIsrth,ng),        &
     &       'Aout(idIsrth)',                                           &
     &       'Write out averaged shortwave through ice.'
          IF (Aout(idIsfI,ng)) WRITE (out,70) Aout(idIsfI,ng),          &
     &       'Aout(idIsfI)',                                            &
     &       'Write out averaged salt flux under ice.'
          IF (Aout(idIsfL,ng)) WRITE (out,70) Aout(idIsfL,ng),          &
     &       'Aout(idIsfL)',                                            &
     &       'Write out averaged salt flux through leads.'
          IF (Aout(idIqao,ng)) WRITE (out,70) Aout(idIqao,ng),          &
     &       'Aout(idIqao)',                                            &
     &       'Write out averaged heat flux through leads.'
          IF (Aout(idIqai,ng)) WRITE (out,70) Aout(idIqai,ng),          &
     &       'Aout(idIqai)',                                            &
     &       'Write out averaged heat flux of ice-air.'
          IF (Aout(idIqio,ng)) WRITE (out,70) Aout(idIqio,ng),          &
     &       'Aout(idIqio)',                                            &
     &       'Write out averaged heat flux of ice-ocean.'
          IF (Aout(idIqi2,ng)) WRITE (out,70) Aout(idIqi2,ng),          &
     &       'Aout(idIqi2)',                                            &
     &       'Write out averaged ice heat flux.'
          IF (Aout(idIsnoi,ng)) WRITE (out,70) Aout(idIsnoi,ng),        &
     &       'Aout(idIsnoi)',                                           &
     &       'Write out averaged snow-ice conversion.'
#  endif
# endif
# ifdef ICESHELF
          IF (Aout(idIsMr,ng)) WRITE (out,70) Aout(idIsMr,ng),          &
     &       'Aout(idIsMr)',                                            &
     &       'Write out averaged iceshelf melt rate.'
#  ifdef ICESHELF_3EQN_VBC
          IF (Aout(idIsTs,ng)) WRITE (out,70) Aout(idIsTs,ng),          &
     &       'Aout(idIsTs)',                                            &
     &       'Write out averaged iceshelf frictional temperature.'
          IF (Aout(idIsUs,ng)) WRITE (out,70) Aout(idIsUs,ng),          &
     &       'Aout(idIsUs)',                                            &
     &       'Write out averaged iceshelf frictional velocity.'
          IF (Aout(idIsTb,ng)) WRITE (out,70) Aout(idIsTb,ng),          &
     &       'Aout(idIsTb)',                                            &
     &       'Write out averaged iceshelf molecular sublayer T.'
          IF (Aout(idIsSb,ng)) WRITE (out,70) Aout(idIsSb,ng),          &
     &       'Aout(idIsSb)',                                            &
     &       'Write out averaged iceshelf molecular sublayer S.'
#  endif
#  ifdef ICESHELF_MORPH
          IF (Aout(idIsDrft,ng)) WRITE (out,70) Aout(idIsDrft,ng),      &
     &       'Aout(idIsDrft)',                                          &
     &       'Write out averaged time-varying ice draft.'
#  endif
#  ifdef ICESHELF_SOF
          IF (Aout(idIsSOFs,ng)) WRITE (out,70) Aout(idIsSOFs,ng),      &
     &       'Aout(idIsSOFs)',                                          &
     &       'Write out averaged surface salt overflux.'
          IF (Aout(idIsSOFh,ng)) WRITE (out,70) Aout(idIsSOFh,ng),      &
     &       'Aout(idIsSOFh)',                                          &
     &       'Write out averaged surface heat overflux.'
#  endif
# endif

# ifdef ICE_MODEL
          IF (Qout(idIuice,ng)) WRITE (out,70) Qout(idIuice,ng),        &
     &       'Qout(idIuice)',                                           &
     &       'Write out quick 2D ice momentum in the XI-direction.'
          IF (Qout(idIvice,ng)) WRITE (out,70) Qout(idIvice,ng),        &
     &       'Qout(idIvice)',                                           &
     &       'Write out quick 2D ice momentum in the ETA-direction.'
          IF (Qout(idIuiceE,ng)) WRITE (out,70) Qout(idIuiceE,ng),      &
     &       'Qout(idIuiceE)',                                          &
     &       'Write out quick 2D ice momentum in the E-direction.'
          IF (Qout(idIviceN,ng)) WRITE (out,70) Qout(idIviceN,ng),      &
     &       'Qout(idIviceN)',                                          &
     &       'Write out quick 2D ice momentum in the N-direction.'
          IF (Qout(idIaice,ng)) WRITE (out,70) Qout(idIaice,ng),        &
     &       'Qout(idIaice)',                                           &
     &       'Write out quick ice concentration.'
          IF (Qout(idIhice,ng)) WRITE (out,70) Qout(idIhice,ng),        &
     &       'Qout(idIhice)',                                           &
     &       'Write out quick ice average thickness.'
          IF (Qout(idIhsno,ng)) WRITE (out,70) Qout(idIhsno,ng),        &
     &       'Qout(idIhsno)',                                           &
     &       'Write out quick snow thickness.'
          IF (Qout(idIagei,ng)) WRITE (out,70) Qout(idIagei,ng),        &
     &       'Qout(idIagei)',                                           &
     &       'Write out quick ice age.'
          IF (Qout(idItice,ng)) WRITE (out,70) Qout(idItice,ng),        &
     &       'Qout(idItice)',                                           &
     &       'Write out quick temperature of ice/snow surface.'
          IF (Qout(idItmid,ng)) WRITE (out,70) Qout(idItmid,ng),        &
     &       'Qout(idItmid)',                                           &
     &       'Write out quick temperature of internal ice.'
          IF (Qout(idIt2ic,ng)) WRITE (out,70) Qout(idIt2ic,ng),        &
     &       'Qout(idIt2ic)',                                           &
     &       'Write out quick temperature of top ice.'
          IF (Qout(idIsg11,ng)) WRITE (out,70) Qout(idIsg11,ng),        &
     &       'Qout(idIsg11)',                                           &
     &       'Write out quick internal ice stress component 11.'
          IF (Qout(idIsg12,ng)) WRITE (out,70) Qout(idIsg12,ng),        &
     &       'Qout(idIsg12)',                                           &
     &       'Write out quick internal ice stress component 12.'
          IF (Qout(idIsg22,ng)) WRITE (out,70) Qout(idIsg22,ng),        &
     &       'Qout(idIsg22)',                                           &
     &       'Write out quick internal ice stress component 22.'
          IF (Qout(idIutau,ng)) WRITE (out,70) Qout(idIutau,ng),        &
     &       'Qout(idIutau)',                                           &
     &       'Write out quick ice-water friction velocity.'
          IF (Qout(idImchu,ng)) WRITE (out,70) Qout(idImchu,ng),        &
     &       'Qout(idImchu)',                                           &
     &       'Write out quick ice-water momentum transfer coef.'
          IF (Qout(idIt0mk,ng)) WRITE (out,70) Qout(idIt0mk,ng),        &
     &       'Qout(idIt0mk)',                                           &
     &       'Write out quick T of molecular sublayer under ice.'
          IF (Qout(idIs0mk,ng)) WRITE (out,70) Qout(idIs0mk,ng),        &
     &       'Qout(idIs0mk)',                                           &
     &       'Write out quick S of molecular sublayer under ice.'
          IF (Qout(idImflx,ng)) WRITE (out,70) Qout(idImflx,ng),        &
     &       'Qout(idImflx)',                                           &
     &       'Write out quick ice-ocean mass flux.'
          IF (Qout(idIwfr,ng)) WRITE (out,70) Qout(idIwfr,ng),          &
     &       'Qout(idIwfr)',                                            &
     &       'Write out quick ice frazil growth rate.'
          IF (Qout(idIwai,ng)) WRITE (out,70) Qout(idIwai,ng),          &
     &       'Qout(idIwai)',                                            &
     &       'Write out quick ice growth/melt rate of ice-air.'
          IF (Qout(idIwao,ng)) WRITE (out,70) Qout(idIwao,ng),          &
     &       'Qout(idIwao)',                                            &
     &       'Write out quick ice growth/melt rate of leads.'
          IF (Qout(idIwio,ng)) WRITE (out,70) Qout(idIwio,ng),          &
     &       'Qout(idIwio)',                                            &
     &       'Write out quick ice growth/melt rate of ice-ocean.'
          IF (Qout(idIwro,ng)) WRITE (out,70) Qout(idIwro,ng),          &
     &       'Qout(idIwro)',                                            &
     &       'Write out quick ice melt runoff rate.'
          IF (Qout(idIwdiv,ng)) WRITE (out,70) Qout(idIwdiv,ng),        &
     &       'Qout(idIwdiv)',                                           &
     &       'Write out quick ice divergence rate.'
#  ifdef ICE_DIAGS
#   ifdef ICE_ALBEDO
          IF (Qout(idIalbe,ng)) WRITE (out,70) Qout(idIalbe,ng),        &
     &       'Qout(idIalbe)',                                           &
     &       'Write out quick ocean albedo.'
          IF (Qout(idIalbI,ng)) WRITE (out,70) Qout(idIalbI,ng),        &
     &       'Qout(idIalbI)',                                           &
     &       'Write out quick ice albedo.'
#    ifdef ICE_ALB_CSIM
          IF (Qout(idIfrsw,ng)) WRITE (out,70) Qout(idIfrsw,ng),        &
     &       'Qout(idIfrsw)',                                           &
     &       'Write out quick fraction of SW penetrating into ice.'
#    endif
#   endif
          IF (Qout(idIrfac,ng)) WRITE (out,70) Qout(idIrfac,ng),        &
     &       'Qout(idIrfac)',                                           &
     &       'Write out quick reduction fac of near-IR SW under ice.'
          IF (Qout(idIlrdn,ng)) WRITE (out,70) Qout(idIlrdn,ng),        &
     &       'Qout(idIlrdn)',                                           &
     &       'Write out quick downward longwave radiation.'
          IF (Qout(idIsrdn,ng)) WRITE (out,70) Qout(idIsrdn,ng),        &
     &       'Qout(idIsrdn)',                                           &
     &       'Write out quick downward shortwave radiation.'
          IF (Qout(idIlatI,ng)) WRITE (out,70) Qout(idIlatI,ng),        &
     &       'Qout(idIlatI)',                                           &
     &       'Write out quick latent heat over ice.'
          IF (Qout(idIsenI,ng)) WRITE (out,70) Qout(idIsenI,ng),        &
     &       'Qout(idIsenI)',                                           &
     &       'Write out quick sensible heat over ice.'
          IF (Qout(idIlrup,ng)) WRITE (out,70) Qout(idIlrup,ng),        &
     &       'Qout(idIlrup)',                                           &
     &       'Write out quick upward longwave over ice.'
          IF (Qout(idIsrup,ng)) WRITE (out,70) Qout(idIsrup,ng),        &
     &       'Qout(idIsrup)',                                           &
     &       'Write out quick upward shortwave over ice.'
          IF (Qout(idIsrin,ng)) WRITE (out,70) Qout(idIsrin,ng),        &
     &       'Qout(idIsrin)',                                           &
     &       'Write out quick shortwave into ice.'
          IF (Qout(idIsrth,ng)) WRITE (out,70) Qout(idIsrth,ng),        &
     &       'Qout(idIsrth)',                                           &
     &       'Write out quick shortwave through ice.'
          IF (Qout(idIsfI,ng)) WRITE (out,70) Qout(idIsfI,ng),          &
     &       'Qout(idIsfI)',                                            &
     &       'Write out quick salt flux under ice.'
          IF (Qout(idIsfL,ng)) WRITE (out,70) Qout(idIsfL,ng),          &
     &       'Qout(idIsfL)',                                            &
     &       'Write out quick salt flux through leads.'
          IF (Qout(idIqao,ng)) WRITE (out,70) Qout(idIqao,ng),          &
     &       'Qout(idIqao)',                                            &
     &       'Write out quick heat flux through leads.'
          IF (Qout(idIqai,ng)) WRITE (out,70) Qout(idIqai,ng),          &
     &       'Qout(idIqai)',                                            &
     &       'Write out quick heat flux of ice-air.'
          IF (Qout(idIqio,ng)) WRITE (out,70) Qout(idIqio,ng),          &
     &       'Qout(idIqio)',                                            &
     &       'Write out quick heat flux of ice-ocean.'
          IF (Qout(idIqi2,ng)) WRITE (out,70) Qout(idIqi2,ng),          &
     &       'Qout(idIqi2)',                                            &
     &       'Write out quick ice heat flux.'
          IF (Qout(idIsnoi,ng)) WRITE (out,70) Qout(idIsnoi,ng),        &
     &       'Qout(idIsnoi)',                                           &
     &       'Write out quick snow-ice conversion.'
#  endif
# endif
# ifdef ICESHELF
          IF (Qout(idIsMr,ng)) WRITE (out,70) Qout(idIsMr,ng),          &
     &       'Qout(idIsMr)',                                            &
     &       'Write out quick iceshelf melt rate.'
#  ifdef ICESHELF_3EQN_VBC
          IF (Qout(idIsTs,ng)) WRITE (out,70) Qout(idIsTs,ng),          &
     &       'Qout(idIsTs)',                                            &
     &       'Write out quick iceshelf frictional temperature.'
          IF (Qout(idIsUs,ng)) WRITE (out,70) Qout(idIsUs,ng),          &
     &       'Qout(idIsUs)',                                            &
     &       'Write out quick iceshelf frictional velocity.'
          IF (Qout(idIsTb,ng)) WRITE (out,70) Qout(idIsTb,ng),          &
     &       'Qout(idIsTb)',                                            &
     &       'Write out quick iceshelf molecular sublayer T.'
          IF (Qout(idIsSb,ng)) WRITE (out,70) Qout(idIsSb,ng),          &
     &       'Qout(idIsSb)',                                            &
     &       'Write out quick iceshelf molecular sublayer S.'
#  endif
#  ifdef ICESHELF_MORPH
          IF (Qout(idIsDrft,ng)) WRITE (out,70) Qout(idIsDrft,ng),      &
     &       'Qout(idIsDrft)',                                          &
     &       'Write out quick time-varying ice draft.'
#  endif
#  ifdef ICESHELF_SOF
          IF (Qout(idIsSOFs,ng)) WRITE (out,70) Qout(idIsSOFs,ng),      &
     &       'Qout(idIsSOFs)',                                          &
     &       'Write out quick surface salt overflux.'
          IF (Qout(idIsSOFh,ng)) WRITE (out,70) Qout(idIsSOFh,ng),      &
     &       'Qout(idIsSOFh)',                                          &
     &       'Write out quick surface heat overflux.'
#  endif
# endif

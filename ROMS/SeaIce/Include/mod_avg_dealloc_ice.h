# ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
      IF (Aout(idIuice,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgui, MyFile,                 &
     &                   __LINE__, 'AVERAGE(ng)%avgui')) RETURN
      END IF
      IF (Aout(idIvice,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgvi, MyFile,                 &
     &                   __LINE__, 'AVERAGE(ng)%avgvi')) RETURN
      END IF
      IF (Aout(idIuiceE,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avguie, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avguie')) RETURN
      END IF
      IF (Aout(idIviceN,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgvin, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avgvin')) RETURN
      END IF
      IF (Aout(idIaice,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgai, MyFile,                 &
     &                   __LINE__, 'AVERAGE(ng)%avgai')) RETURN
      END IF
      IF (Aout(idIhice,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avghi, MyFile,                 &
     &                   __LINE__, 'AVERAGE(ng)%avghi')) RETURN
      END IF
      IF (Aout(idIhsno,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avghsn, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avghsn')) RETURN
      END IF
      IF (Aout(idIagei,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgageice, MyFile,             &
     &                   __LINE__, 'AVERAGE(ng)%avgageice')) RETURN
      END IF
      IF (Aout(idItice,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgtis, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avgtis')) RETURN
      END IF
      IF (Aout(idItmid,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgti, MyFile,                 &
     &                   __LINE__, 'AVERAGE(ng)%avgti')) RETURN
      END IF
      IF (Aout(idIt2ic,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgt2, MyFile,                 &
     &                   __LINE__, 'AVERAGE(ng)%avgt2')) RETURN
      END IF
      IF (Aout(idIsg11,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgsig11, MyFile,              &
     &                   __LINE__, 'AVERAGE(ng)%avgsig11')) RETURN
      END IF
      IF (Aout(idIsg12,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgsig12, MyFile,              &
     &                   __LINE__, 'AVERAGE(ng)%avgsig12')) RETURN
      END IF
      IF (Aout(idIsg22,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgsig22, MyFile,              &
     &                   __LINE__, 'AVERAGE(ng)%avgsig22')) RETURN
      END IF
      IF (Aout(idIutau,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgutau_iw, MyFile,            &
     &                   __LINE__, 'AVERAGE(ng)%avgutau_iw')) RETURN
      END IF
      IF (Aout(idImchu,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgchu_iw, MyFile,             &
     &                   __LINE__, 'AVERAGE(ng)%avgchu_iw')) RETURN
      END IF
      IF (Aout(idIt0mk,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgt0mk, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgt0mk')) RETURN
      END IF
      IF (Aout(idIs0mk,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgs0mk, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgs0mk')) RETURN
      END IF
      IF (Aout(idImflx,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgiomflx, MyFile,             &
     &                   __LINE__, 'AVERAGE(ng)%avgiomflx')) RETURN
      END IF
      IF (Aout(idIwfr,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgwfr, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avgwfr')) RETURN
      END IF
      IF (Aout(idIwai,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgwai, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avgwai')) RETURN
      END IF
      IF (Aout(idIwao,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgwao, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avgwao')) RETURN
      END IF
      IF (Aout(idIwio,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgwio, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avgwio')) RETURN
      END IF
      IF (Aout(idIwro,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgwro, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avgwro')) RETURN
      END IF
      IF (Aout(idIwdiv,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgwdiv, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgwdiv')) RETURN
      END IF
#  ifdef ICE_DIAGS
#   ifdef ICE_ALBEDO
      IF (Aout(idIalbO,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgalbo, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgalbo')) RETURN
      END IF
      IF (Aout(idIalbI,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgalbi, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgalbi')) RETURN
      END IF
#    ifdef ICE_ALB_CSIM
      IF (Aout(idIfrsw,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgfrswp, MyFile,              &
     &                   __LINE__, 'AVERAGE(ng)%avgfrswp')) RETURN
      END IF
#    endif
#   endif
      IF (Aout(idIrfac,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgrfaci, MyFile,              &
     &                   __LINE__, 'AVERAGE(ng)%avgrfaci')) RETURN
      END IF
      IF (Aout(idIqswl,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgqswl, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgqswl')) RETURN
      END IF
      IF (Aout(idIqlwl,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgqlwl, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgqlwl')) RETURN
      END IF
      IF (Aout(idIqlai,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgqlai, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgqlai')) RETURN
      END IF
      IF (Aout(idIqsei,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgqsei, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgqsei')) RETURN
      END IF
      IF (Aout(idIqlwa,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgqlwa, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgqlwa')) RETURN
      END IF
      IF (Aout(idIqswa,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgqswa, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgqswa')) RETURN
      END IF
      IF (Aout(idIqswi,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgqswi, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgqswi')) RETURN
      END IF
      IF (Aout(idIqswo,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgqswo, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgqswo')) RETURN
      END IF
      IF (Aout(idIsio,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgsio, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avgsio')) RETURN
      END IF
      IF (Aout(idIsao,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgsao, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avgsao')) RETURN
      END IF
      IF (Aout(idIqao,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgqao, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avgqao')) RETURN
      END IF
      IF (Aout(idIqai,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgqai, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avgqai')) RETURN
      END IF
      IF (Aout(idIqio,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgqio, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avgqio')) RETURN
      END IF
      IF (Aout(idIqi2,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgqi2, MyFile,                &
     &                   __LINE__, 'AVERAGE(ng)%avgqi2')) RETURN
      END IF
      IF (Aout(idIwsni,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgwsni, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgwsni')) RETURN
      END IF
#  endif
# endif
# ifdef ICESHELF
      IF (Aout(idIsMr,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgm, MyFile,                  &
     &                   __LINE__, 'AVERAGE(ng)%avgm')) RETURN
      END IF
#  ifdef ICESHELF_3EQN_VBC
      IF (Aout(idIsTs,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgTstar, MyFile,              &
     &                   __LINE__, 'AVERAGE(ng)%avgTstar')) RETURN
      END IF
      IF (Aout(idIsUs,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgUstar, MyFile,              &
     &                   __LINE__, 'AVERAGE(ng)%avgUstar')) RETURN
      END IF
      IF (Aout(idIsTb,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgTb, MyFile,                 &
     &                   __LINE__, 'AVERAGE(ng)%avgTb')) RETURN
      END IF
      IF (Aout(idIsSb,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgSb, MyFile,                 &
     &                   __LINE__, 'AVERAGE(ng)%avgSb')) RETURN
      END IF
#  endif
#  ifdef ICESHELF_MORPH
      IF (Aout(idIsDrft,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgzice, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgzice')) RETURN
      END IF
#  endif
#  ifdef ICESHELF_SOF
      IF (Aout(idIsSOFs,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgSOFs, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgSOFs')) RETURN
      END IF
      IF (Aout(idIsSOFh,ng)) THEN
        IF (.not.destroy(ng, AVERAGE(ng)%avgSOFh, MyFile,               &
     &                   __LINE__, 'AVERAGE(ng)%avgSOFh')) RETURN
      END IF
#  endif
# endif

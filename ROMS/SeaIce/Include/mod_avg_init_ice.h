# ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
      IF (Aout(idIuice,ng)) THEN
        DO j=JstrR,JendR
          DO i=Istr,IendR 
            AVERAGE(ng) % avgui(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIvice,ng)) THEN
        DO j=Jstr,JendR 
          DO i=IstrR,IendR
            AVERAGE(ng) % avgvi(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIuiceE,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avguie(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIviceN,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgvin(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIaice,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgai(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIhice,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avghi(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIhsno,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avghsn(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIagei,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgageice(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idItice,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgtis(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idItmid,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgti(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIt2ic,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgt2(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsg11,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgsig11(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsg12,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgsig12(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsg22,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgsig22(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIutau,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgutau_iw(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idImchu,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgchu_iw(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIt0mk,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgt0mk(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIs0mk,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgs0mk(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idImflx,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgiomflx(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIwfr,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgwfr(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIwai,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgwai(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIwao,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgwao(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIwio,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgwio(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIwro,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgwro(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIwdiv,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgwdiv(i,j) = IniVal
          END DO
        END DO
      END IF
#  ifdef ICE_DIAGS
#   ifdef ICE_ALBEDO
      IF (Aout(idIalbe,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgalb(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIalbI,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgalb_i(i,j) = IniVal
          END DO
        END DO
      END IF
#    ifdef ICE_ALB_CSIM
      IF (Aout(idIfrsw,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgfrswp(i,j) = IniVal
          END DO
        END DO
      END IF
#    endif
#   endif
      IF (Aout(idIrfac,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgrfaci(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIlrdn,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avglr_dn(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsrdn,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgsr_dn(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIlatI,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avglat_i(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsenI,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgsen_i(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIlrup,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avglr_up_i(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsrup,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgsr_up_i(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsrin,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgsr_in_i(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsrth,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgsr_th_i(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsfI,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgssflx_i(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsfL,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgssflx_l(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIqao,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgqao_n(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIqai,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgqai_n(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIqio,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgqio_n(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIqi2,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgqi2_n(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsnoi,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgsnoice(i,j) = IniVal
          END DO
        END DO
      END IF
#  endif
# endif
# ifdef ICESHELF
      IF (Aout(idIsMr,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgm(i,j) = IniVal
          END DO
        END DO
      END IF
#  ifdef ICESHELF_3EQN_VBC
      IF (Aout(idIsTs,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgTstar(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsUs,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgUstar(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsTb,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgTb(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsSb,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgSb(i,j) = IniVal
          END DO
        END DO
      END IF
#  endif
#  ifdef ICESHELF_MORPH
      IF (Aout(idIsDrft,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgzice(i,j) = IniVal
          END DO
        END DO
      END IF
#  endif
#  ifdef ICESHELF_SOF
      IF (Aout(idIsSOFs,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgSOFs(i,j) = IniVal
          END DO
        END DO
      END IF
      IF (Aout(idIsSOFh,ng)) THEN
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            AVERAGE(ng) % avgSOFh(i,j) = IniVal
          END DO
        END DO
      END IF
#  endif
# endif

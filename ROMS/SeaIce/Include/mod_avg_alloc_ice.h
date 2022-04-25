# ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
      IF (Aout(idIuice,ng)) THEN
        allocate ( AVERAGE(ng) % avgui(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIvice,ng)) THEN
        allocate ( AVERAGE(ng) % avgvi(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIuiceE,ng)) THEN
        allocate ( AVERAGE(ng) % avguie(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIviceN,ng)) THEN
        allocate ( AVERAGE(ng) % avgvin(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIaice,ng)) THEN
        allocate ( AVERAGE(ng) % avgai(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIhice,ng)) THEN
        allocate ( AVERAGE(ng) % avghi(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIhsno,ng)) THEN
        allocate ( AVERAGE(ng) % avghsn(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIagei,ng)) THEN
        allocate ( AVERAGE(ng) % avgageice(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idItice,ng)) THEN
        allocate ( AVERAGE(ng) % avgtis(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idItmid,ng)) THEN
        allocate ( AVERAGE(ng) % avgti(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIt2ic,ng)) THEN
        allocate ( AVERAGE(ng) % avgt2(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIsg11,ng)) THEN
        allocate ( AVERAGE(ng) % avgsig11(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIsg12,ng)) THEN
        allocate ( AVERAGE(ng) % avgsig12(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIsg22,ng)) THEN
        allocate ( AVERAGE(ng) % avgsig22(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIutau,ng)) THEN
        allocate ( AVERAGE(ng) % avgutau_iw(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idImchu,ng)) THEN
        allocate ( AVERAGE(ng) % avgchu_iw(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIt0mk,ng)) THEN
        allocate ( AVERAGE(ng) % avgt0mk(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIs0mk,ng)) THEN
        allocate ( AVERAGE(ng) % avgs0mk(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idImflx,ng)) THEN
        allocate ( AVERAGE(ng) % avgiomflx(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIwfr,ng)) THEN
        allocate ( AVERAGE(ng) % avgwfr(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIwai,ng)) THEN
        allocate ( AVERAGE(ng) % avgwai(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIwao,ng)) THEN
        allocate ( AVERAGE(ng) % avgwao(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIwio,ng)) THEN
        allocate ( AVERAGE(ng) % avgwio(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIwro,ng)) THEN
        allocate ( AVERAGE(ng) % avgwro(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIwdiv,ng)) THEN
        allocate ( AVERAGE(ng) % avgwdiv(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
#  ifdef ICE_DIAGS
#   ifdef ICE_ALBEDO
      IF (Aout(idIalbO,ng)) THEN
        allocate ( AVERAGE(ng) % avgalbo(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIalbI,ng)) THEN
        allocate ( AVERAGE(ng) % avgalbi(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
#    ifdef ICE_ALB_CSIM
      IF (Aout(idIfrsw,ng)) THEN
        allocate ( AVERAGE(ng) % avgfrswp(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
#    endif
#   endif
      IF (Aout(idIqswi,ng)) THEN
        allocate ( AVERAGE(ng) % avgqswi(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIqlwi,ng)) THEN
        allocate ( AVERAGE(ng) % avgqlwi(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIqlai,ng)) THEN
        allocate ( AVERAGE(ng) % avgqlai(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIqsei,ng)) THEN
        allocate ( AVERAGE(ng) % avgqsei(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIqswl,ng)) THEN
        allocate ( AVERAGE(ng) % avgqswl(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIqlwl,ng)) THEN
        allocate ( AVERAGE(ng) % avgqlwl(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIqlwa,ng)) THEN
        allocate ( AVERAGE(ng) % avgqlwa(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIqswa,ng)) THEN
        allocate ( AVERAGE(ng) % avgqswa(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIqswo,ng)) THEN
        allocate ( AVERAGE(ng) % avgqswo(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIqao,ng)) THEN
        allocate ( AVERAGE(ng) % avgqao(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIqai,ng)) THEN
        allocate ( AVERAGE(ng) % avgqai(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIqio,ng)) THEN
        allocate ( AVERAGE(ng) % avgqio(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIqi2,ng)) THEN
        allocate ( AVERAGE(ng) % avgqi2(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIsao,ng)) THEN
        allocate ( AVERAGE(ng) % avgsao(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIsio,ng)) THEN
        allocate ( AVERAGE(ng) % avgsio(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIwsni,ng)) THEN
        allocate ( AVERAGE(ng) % avgwsni(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
#  endif
# endif
# ifdef ICESHELF
      IF (Aout(idIsMr,ng)) THEN
        allocate ( AVERAGE(ng) % avgm(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
#  ifdef ICESHELF_3EQN_VBC
      IF (Aout(idIsTs,ng)) THEN
        allocate ( AVERAGE(ng) % avgTstar(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIsUs,ng)) THEN
        allocate ( AVERAGE(ng) % avgUstar(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIsTb,ng)) THEN
        allocate ( AVERAGE(ng) % avgTb(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIsSb,ng)) THEN
        allocate ( AVERAGE(ng) % avgSb(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
#  endif
#  ifdef ICESHELF_MORPH
      IF (Aout(idIsDrft,ng)) THEN
        allocate ( AVERAGE(ng) % avgzice(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
#  endif
#  ifdef ICESHELF_SOF
      IF (Aout(idIsSOFs,ng)) THEN
        allocate ( AVERAGE(ng) % avgSOFs(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
      IF (Aout(idIsSOFh,ng)) THEN
        allocate ( AVERAGE(ng) % avgSOFh(LBi:UBi,LBj:UBj) )
        Dmem(ng)=Dmem(ng)+size2d
      END IF
#  endif
# endif

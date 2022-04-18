# if defined ICE_MODEL || defined ICESHELF
!
!-----------------------------------------------------------------------
!  Initialize ice model variables.
!-----------------------------------------------------------------------
!
      IF (((iic(ng).gt.ntsAVG(ng)).and.                                 &
     &     (MOD(iic(ng)-1,nAVG(ng)).eq.1)).or.                          &
     &    ((iic(ng).ge.ntsAVG(ng)).and.(nAVG(ng).eq.1)).or.             &
     &    ((nrrec(ng).gt.0).and.(iic(ng).eq.ntstart(ng)))) THEN
!
!  Initialize state variables.
!
#  ifdef ICE_MODEL
        IF (Aout(idIuice,ng)) THEN
          DO j=JstrR,JendR
            DO i=Istr,IendR 
              AVERAGE(ng)%avgui(i,j)=ICE(ng)%ui(i,j,Iout)
#   ifdef WET_DRY
              AVERAGE(ng)%avgui(i,j)=AVERAGE(ng)%avgui(i,j)*            &
     &                               GRID(ng)%umask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIvice,ng)) THEN
          DO j=Jstr,JendR 
            DO i=IstrR,IendR
              AVERAGE(ng)%avgvi(i,j)=ICE(ng)%vi(i,j,Iout)
#   ifdef WET_DRY
              AVERAGE(ng)%avgvi(i,j)=AVERAGE(ng)%avgvi(i,j)*            &
     &                               GRID(ng)%vmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIuiceE,ng).and.Aout(idIviceN,ng)) THEN
          CALL uv_rotate2d (ng, tile, .FALSE., .FALSE.,                 &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      GRID(ng) % CosAngler,                       &
     &                      GRID(ng) % SinAngler,                       &
#   ifdef MASKING
     &                      GRID(ng)%rmask_full,                        &
#   endif
     &                      ICE(ng) % ui(:,:,Kout),                     &
     &                      ICE(ng) % vi(:,:,Kout),                     &
     &                      AVERAGE(ng)%avguie,                         &
     &                      AVERAGE(ng)%avgvin)
        END IF

        IF (Aout(idIaice,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgai(i,j)=ICE(ng)%ai(i,j,Iout)
#   ifdef WET_DRY
              AVERAGE(ng)%avgai(i,j)=AVERAGE(ng)%avgai(i,j)*            &
     &                               GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIhice,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avghi(i,j)=ICE(ng)%hi(i,j,Iout)
#   ifdef WET_DRY
              AVERAGE(ng)%avghi(i,j)=AVERAGE(ng)%avghi(i,j)*            &
     &                               GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIhsno,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avghsn(i,j)=ICE(ng)%hsn(i,j,Iout)
#   ifdef WET_DRY
              AVERAGE(ng)%avghsn(i,j)=AVERAGE(ng)%avghsn(i,j)*          &
     &                                GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIagei,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgageice(i,j)=ICE(ng)%ageice(i,j,Iout)
#   ifdef WET_DRY
              AVERAGE(ng)%avgageice(i,j)=AVERAGE(ng)%avgageice(i,j)*    &
     &                                   GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idItice,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgtis(i,j)=ICE(ng)%tis(i,j)
#   ifdef WET_DRY
              AVERAGE(ng)%avgtis(i,j)=AVERAGE(ng)%avgtis(i,j)*          &
     &                                GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idItmid,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgti(i,j)=ICE(ng)%ti(i,j,Iout)
#   ifdef WET_DRY
              AVERAGE(ng)%avgti(i,j)=AVERAGE(ng)%avgti(i,j)*            &
     &                               GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIt2ic,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgt2(i,j)=ICE(ng)%t2(i,j)
#   ifdef WET_DRY
              AVERAGE(ng)%avgt2(i,j)=AVERAGE(ng)%avgt2(i,j)*            &
     &                               GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIsg11,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsig11(i,j)=ICE(ng)%sig11(i,j,Iout)
#   ifdef WET_DRY
              AVERAGE(ng)%avgsig11(i,j)=AVERAGE(ng)%avgsig11(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIsg12,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsig12(i,j)=ICE(ng)%sig12(i,j,Iout)
#   ifdef WET_DRY
              AVERAGE(ng)%avgsig12(i,j)=AVERAGE(ng)%avgsig12(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIsg22,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsig22(i,j)=ICE(ng)%sig22(i,j,Iout)
#   ifdef WET_DRY
              AVERAGE(ng)%avgsig22(i,j)=AVERAGE(ng)%avgsig22(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIutau,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgutau_iw(i,j)=ICE(ng)%utau_iw(i,j)
#   ifdef WET_DRY
              AVERAGE(ng)%avgutau_iw(i,j)=AVERAGE(ng)%avgutau_iw(i,j)*  &
     &                                    GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idImchu,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgchu_iw(i,j)=ICE(ng)%chu_iw(i,j)
#   ifdef WET_DRY
              AVERAGE(ng)%avgchu_iw(i,j)=AVERAGE(ng)%avgchu_iw(i,j)*    &
     &                                   GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIt0mk,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgt0mk(i,j)=ICE(ng)%t0mk(i,j)
#   ifdef WET_DRY
              AVERAGE(ng)%avgt0mk(i,j)=AVERAGE(ng)%avgt0mk(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIs0mk,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgs0mk(i,j)=ICE(ng)%s0mk(i,j)
#   ifdef WET_DRY
              AVERAGE(ng)%avgs0mk(i,j)=AVERAGE(ng)%avgs0mk(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idImflx,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgiomflx(i,j)=ICE(ng)%iomflx(i,j)
#   ifdef WET_DRY
              AVERAGE(ng)%avgiomflx(i,j)=AVERAGE(ng)%avgiomflx(i,j)*    &
     &                                   GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIwfr,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwfr(i,j)=ICE(ng)%wfr(i,j)
#   ifdef WET_DRY
              AVERAGE(ng)%avgwfr(i,j)=AVERAGE(ng)%avgwfr(i,j)*          &
     &                                GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIwai,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwai(i,j)=ICE(ng)%wai(i,j)
#   ifdef WET_DRY
              AVERAGE(ng)%avgwai(i,j)=AVERAGE(ng)%avgwai(i,j)*          &
     &                                GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIwao,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwao(i,j)=ICE(ng)%wao(i,j)
#   ifdef WET_DRY
              AVERAGE(ng)%avgwao(i,j)=AVERAGE(ng)%avgwao(i,j)*          &
     &                                GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIwio,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwio(i,j)=ICE(ng)%wio(i,j)
#   ifdef WET_DRY
              AVERAGE(ng)%avgwio(i,j)=AVERAGE(ng)%avgwio(i,j)*          &
     &                                GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIwro,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwro(i,j)=ICE(ng)%wro(i,j)
#   ifdef WET_DRY
              AVERAGE(ng)%avgwro(i,j)=AVERAGE(ng)%avgwro(i,j)*          &
     &                                GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

        IF (Aout(idIwdiv,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwdiv(i,j)=ICE(ng)%wdiv(i,j)
#   ifdef WET_DRY
              AVERAGE(ng)%avgwdiv(i,j)=AVERAGE(ng)%avgwdiv(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

#   ifdef ICE_DIAGS
#    ifdef ICE_ALBEDO
        IF (Aout(idIalbO,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgalbo(i,j)=FORCES(ng)%albo(i,j)
#     ifdef WET_DRY
              AVERAGE(ng)%avgalbo(i,j)=AVERAGE(ng)%avgalbo(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#     endif
            END DO
          END DO
        END IF

        IF (Aout(idIalbI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgalbi(i,j)=FORCES(ng)%albi(i,j)
#     ifdef WET_DRY
              AVERAGE(ng)%avgalbi(i,j)=AVERAGE(ng)%avgalbi(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#     endif
            END DO
          END DO
        END IF

#     ifdef ICE_ALB_CSIM
        IF (Aout(idIfrsw,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgfrswp(i,j)=FORCES(ng)%frswp(i,j)
#      ifdef WET_DRY
              AVERAGE(ng)%avgfrswp(i,j)=AVERAGE(ng)%avgfrswp(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#      endif
            END DO
          END DO
        END IF

#     endif
#    endif
        IF (Aout(idIrfac,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgrfaci(i,j)=ICE(ng)%rfaci(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgrfaci(i,j)=AVERAGE(ng)%avgrfaci(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqswl,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqswl(i,j)=FORCES(ng)%qswl(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqswl(i,j)=AVERAGE(ng)%avgqswl(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqlwl,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqlwl(i,j)=FORCES(ng)%qlwl(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqlwl(i,j)=AVERAGE(ng)%avgqlwl(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqlai,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqlai(i,j)=FORCES(ng)%qlai(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqlai(i,j)=AVERAGE(ng)%avgqlai(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqsei,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqsei(i,j)=FORCES(ng)%qsei(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqsei(i,j)=AVERAGE(ng)%avgqsei(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqlwa,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqlwa(i,j)=FORCES(ng)%qlwa(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqlwa(i,j)=AVERAGE(ng)%avgqlwa(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqswa,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqswa(i,j)=FORCES(ng)%qswa(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqswa(i,j)=AVERAGE(ng)%avgqswa(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqswi,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqswi(i,j)=FORCES(ng)%qswi(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqswi(i,j)=AVERAGE(ng)%avgqswi(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqswo,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqswo(i,j)=FORCES(ng)%qswo(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqswo(i,j)=AVERAGE(ng)%avgqswo(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIsio,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsio(i,j)=FORCES(ng)%sio(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgsio(i,j)=AVERAGE(ng)%avgsio(i,j)*          &
     &                                GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIsao,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsao(i,j)=FORCES(ng)%sao(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgsao(i,j)=AVERAGE(ng)%avgsao(i,j)*          &
     &                                GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqao,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqao(i,j)=FORCES(ng)%qao(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqao(i,j)=AVERAGE(ng)%avgqao(i,j)*          &
     &                                GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqai,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqai(i,j)=FORCES(ng)%qai(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqai(i,j)=AVERAGE(ng)%avgqai(i,j)*          &
     &                                GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqio,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqio(i,j)=FORCES(ng)%qio(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqio(i,j)=AVERAGE(ng)%avgqio(i,j)*          &
     &                                GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqi2,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqi2(i,j)=FORCES(ng)%qi2(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqi2(i,j)=AVERAGE(ng)%avgqi2(i,j)*          &
     &                                GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIwsni,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwsni(i,j)=FORCES(ng)%wsni(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgwsni(i,j)=AVERAGE(ng)%avgwsni(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

#   endif
#  endif
#  ifdef ICESHELF
        IF (Aout(idIsMr,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgm(i,j)=ICESHELFVAR(ng)%m(i,j)
#   ifdef WET_DRY
              AVERAGE(ng)%avgm(i,j)=AVERAGE(ng)%avgm(i,j)*              &
     &                              GRID(ng)%rmask_full(i,j)
#   endif
            END DO
          END DO
        END IF

#   ifdef ICESHELF_3EQN_VBC
        IF (Aout(idIsTs,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgTstar(i,j)=ICESHELFVAR(ng)%Tstar(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgTstar(i,j)=AVERAGE(ng)%avgTstar(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIsUs,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgUstar(i,j)=ICESHELFVAR(ng)%Ustar(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgUstar(i,j)=AVERAGE(ng)%avgUstar(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIsTb,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgTb(i,j)=ICESHELFVAR(ng)%Tb(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgTb(i,j)=AVERAGE(ng)%avgTb(i,j)*            &
     &                               GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIsSb,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgSb(i,j)=ICESHELFVAR(ng)%Sb(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgSb(i,j)=AVERAGE(ng)%avgSb(i,j)*            &
     &                               GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

#   endif
#   ifdef ICESHELF_MORPH
        IF (Aout(idIsDrft,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgzice(i,j)=GRID(ng)%zice(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgzice(i,j)=AVERAGE(ng)%avgzice(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

#   endif
#   ifdef ICESHELF_SOF
        IF (Aout(idIsSOFs,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgSOFs(i,j)=ICESHELFVAR(ng)%SOFs(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgSOFs(i,j)=AVERAGE(ng)%avgSOFs(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIsSOFh,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgSOFh(i,j)=ICESHELFVAR(ng)%SOFh(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgSOFh(i,j)=AVERAGE(ng)%avgSOFh(i,j)*        &
     &                                 GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

#   endif
#  endif
!
!-----------------------------------------------------------------------
!  Accumulate ice model variables.
!-----------------------------------------------------------------------
!
      ELSE IF (iic(ng).gt.ntsAVG(ng)) THEN
#  ifdef ICE_MODEL
!
!  Accumulate state variables.
!
        IF (Aout(idIuice,ng)) THEN
          DO j=JstrR,JendR
            DO i=Istr,IendR 
              AVERAGE(ng)%avgui(i,j)=AVERAGE(ng)%avgui(i,j)+            &
#   ifdef WET_DRY
     &                               GRID(ng)%umask_full(i,j)*          &
#   endif
     &                               ICE(ng)%ui(i,j,Iout)
            END DO
          END DO
        END IF

        IF (Aout(idIvice,ng)) THEN
          DO j=Jstr,JendR 
            DO i=IstrR,IendR
              AVERAGE(ng)%avgvi(i,j)=AVERAGE(ng)%avgvi(i,j)+            &
#   ifdef WET_DRY
     &                               GRID(ng)%vmask_full(i,j)*          &
#   endif
     &                               ICE(ng)%vi(i,j,Iout)
            END DO
          END DO
        END IF

        IF (Aout(idIuiceE,ng).and.Aout(idIviceN,ng)) THEN
          CALL uv_rotate2d (ng, tile, .TRUE., .FALSE.,                  &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      GRID(ng) % CosAngler,                       &
     &                      GRID(ng) % SinAngler,                       &
#   ifdef MASKING
     &                      GRID(ng)%rmask_full,                        &
#   endif
     &                      ICE(ng) % ui(:,:,Kout),                     &
     &                      ICE(ng) % vi(:,:,Kout),                     &
     &                      AVERAGE(ng)%avguie,                         &
     &                      AVERAGE(ng)%avgvin)
        END IF

        IF (Aout(idIaice,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgai(i,j)=AVERAGE(ng)%avgai(i,j)+            &
#   ifdef WET_DRY
     &                               GRID(ng)%rmask_full(i,j)*          &
#   endif
     &                               ICE(ng)%ai(i,j,Iout)
            END DO
          END DO
        END IF

        IF (Aout(idIhice,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avghi(i,j)=AVERAGE(ng)%avghi(i,j)+            &
#   ifdef WET_DRY
     &                               GRID(ng)%rmask_full(i,j)*          &
#   endif
     &                               ICE(ng)%hi(i,j,Iout)
            END DO
          END DO
        END IF

        IF (Aout(idIhsno,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avghsn(i,j)=AVERAGE(ng)%avghsn(i,j)+          &
#   ifdef WET_DRY
     &                                GRID(ng)%rmask_full(i,j)*         &
#   endif
     &                                ICE(ng)%hsn(i,j,Iout)
            END DO
          END DO
        END IF

        IF (Aout(idIagei,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgageice(i,j)=AVERAGE(ng)%avgageice(i,j)+    &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%ageice(i,j,Iout)
            END DO
          END DO
        END IF

        IF (Aout(idItice,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgtis(i,j)=AVERAGE(ng)%avgtis(i,j)+          &
#   ifdef WET_DRY
     &                                GRID(ng)%rmask_full(i,j)*         &
#   endif
     &                                ICE(ng)%tis(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idItmid,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgti(i,j)=AVERAGE(ng)%avgti(i,j)+            &
#   ifdef WET_DRY
     &                               GRID(ng)%rmask_full(i,j)*          &
#   endif
     &                               ICE(ng)%ti(i,j,Iout)
            END DO
          END DO
        END IF

        IF (Aout(idIt2ic,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgt2(i,j)=AVERAGE(ng)%avgt2(i,j)+            &
#   ifdef WET_DRY
     &                               GRID(ng)%rmask_full(i,j)*          &
#   endif
     &                               ICE(ng)%t2(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsg11,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsig11(i,j)=AVERAGE(ng)%avgsig11(i,j)+      &
#   ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#   endif
     &                                  ICE(ng)%sig11(i,j,Iout)
            END DO
          END DO
        END IF

        IF (Aout(idIsg12,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsig12(i,j)=AVERAGE(ng)%avgsig12(i,j)+      &
#   ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#   endif
     &                                  ICE(ng)%sig12(i,j,Iout)
            END DO
          END DO
        END IF

        IF (Aout(idIsg22,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsig22(i,j)=AVERAGE(ng)%avgsig22(i,j)+      &
#   ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#   endif
     &                                  ICE(ng)%sig22(i,j,Iout)
            END DO
          END DO
        END IF

        IF (Aout(idIutau,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgutau_iw(i,j)=AVERAGE(ng)%avgutau_iw(i,j)+  &
#   ifdef WET_DRY
     &                                    GRID(ng)%rmask_full(i,j)*     &
#   endif
     &                                    ICE(ng)%utau_iw(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idImchu,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgchu_iw(i,j)=AVERAGE(ng)%avgchu_iw(i,j)+    &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%chu_iw(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIt0mk,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgt0mk(i,j)=AVERAGE(ng)%avgt0mk(i,j)+        &
#   ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#   endif
     &                                 ICE(ng)%t0mk(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIs0mk,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgs0mk(i,j)=AVERAGE(ng)%avgs0mk(i,j)+        &
#   ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#   endif
     &                                 ICE(ng)%s0mk(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idImflx,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgiomflx(i,j)=AVERAGE(ng)%avgiomflx(i,j)+    &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%iomflx(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIwfr,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwfr(i,j)=AVERAGE(ng)%avgwfr(i,j)+          &
#   ifdef WET_DRY
     &                                GRID(ng)%rmask_full(i,j)*         &
#   endif
     &                                ICE(ng)%wfr(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIwai,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwai(i,j)=AVERAGE(ng)%avgwai(i,j)+          &
#   ifdef WET_DRY
     &                                GRID(ng)%rmask_full(i,j)*         &
#   endif
     &                                ICE(ng)%wai(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIwao,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwao(i,j)=AVERAGE(ng)%avgwao(i,j)+          &
#   ifdef WET_DRY
     &                                GRID(ng)%rmask_full(i,j)*         &
#   endif
     &                                ICE(ng)%wao(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIwio,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwio(i,j)=AVERAGE(ng)%avgwio(i,j)+          &
#   ifdef WET_DRY
     &                                GRID(ng)%rmask_full(i,j)*         &
#   endif
     &                                ICE(ng)%wio(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIwro,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwro(i,j)=AVERAGE(ng)%avgwro(i,j)+          &
#   ifdef WET_DRY
     &                                GRID(ng)%rmask_full(i,j)*         &
#   endif
     &                                ICE(ng)%wro(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIwdiv,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwdiv(i,j)=AVERAGE(ng)%avgwdiv(i,j)+        &
#   ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#   endif
     &                                 ICE(ng)%wdiv(i,j)
            END DO
          END DO
        END IF

#   ifdef ICE_DIAGS
#    ifdef ICE_ALBEDO
        IF (Aout(idIalbO,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgalbo(i,j)=AVERAGE(ng)%avgalbo(i,j)+        &
#     ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#     endif
     &                                 FORCES(ng)%albo(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIalbI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgalbi(i,j)=AVERAGE(ng)%avgalbi(i,j)+        &
#     ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#     endif
     &                                 FORCES(ng)%albi(i,j)
            END DO
          END DO
        END IF

#     ifdef ICE_ALB_CSIM
        IF (Aout(idIfrsw,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgfrswp(i,j)=AVERAGE(ng)%avgfrswp(i,j)+      &
#      ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#      endif
     &                                  FORCES(ng)%frswp(i,j)
            END DO
          END DO
        END IF

#     endif
#    endif
        IF (Aout(idIrfac,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgrfaci(i,j)=AVERAGE(ng)%avgrfaci(i,j)+      &
#    ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#    endif
     &                                  ICE(ng)%rfaci(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqswl,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqswl(i,j)=AVERAGE(ng)%avgqswl(i,j)+        &
#    ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#    endif
     &                                 FORCES(ng)%qswl(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqlwl,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqlwl(i,j)=AVERAGE(ng)%avgqlwl(i,j)+        &
#    ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#    endif
     &                                 FORCES(ng)%qlwl(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqlai,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqlai(i,j)=AVERAGE(ng)%avgqlai(i,j)+        &
#    ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#    endif
     &                                 FORCES(ng)%qlai(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqsei,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqsei(i,j)=AVERAGE(ng)%avgqsei(i,j)+        &
#    ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#    endif
     &                                 FORCES(ng)%qsei(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqlwa,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqlwa(i,j)=AVERAGE(ng)%avgqlwa(i,j)+        &
#    ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#    endif
     &                                 FORCES(ng)%qlwa(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqswa,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqswa(i,j)=AVERAGE(ng)%avgqswa(i,j)+        &
#    ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#    endif
     &                                 FORCES(ng)%qswa(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqswi,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqswi(i,j)=AVERAGE(ng)%avgqswi(i,j)+        &
#    ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#    endif
     &                                 FORCES(ng)%qswi(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqswo,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqswo(i,j)=AVERAGE(ng)%avgqswo(i,j)+        &
#    ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#    endif
     &                                 FORCES(ng)%qswo(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsio,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsio(i,j)=AVERAGE(ng)%avgsio(i,j)+          &
#    ifdef WET_DRY
     &                                GRID(ng)%rmask_full(i,j)*         &
#    endif
     &                                FORCES(ng)%sio(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsao,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsao(i,j)=AVERAGE(ng)%avgsao(i,j)+          &
#    ifdef WET_DRY
     &                                GRID(ng)%rmask_full(i,j)*         &
#    endif
     &                                FORCES(ng)%sao(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqao,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqao(i,j)=AVERAGE(ng)%avgqao(i,j)+          &
#    ifdef WET_DRY
     &                                GRID(ng)%rmask_full(i,j)*         &
#    endif
     &                                FORCES(ng)%qao(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqai,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqai(i,j)=AVERAGE(ng)%avgqai(i,j)+          &
#    ifdef WET_DRY
     &                                GRID(ng)%rmask_full(i,j)*         &
#    endif
     &                                FORCES(ng)%qai(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqio,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqio(i,j)=AVERAGE(ng)%avgqio(i,j)+          &
#    ifdef WET_DRY
     &                                GRID(ng)%rmask_full(i,j)*         &
#    endif
     &                                FORCES(ng)%qio(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqi2,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqi2(i,j)=AVERAGE(ng)%avgqi2(i,j)+          &
#    ifdef WET_DRY
     &                                GRID(ng)%rmask_full(i,j)*         &
#    endif
     &                                FORCES(ng)%qi2(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIwsni,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwsni(i,j)=AVERAGE(ng)%avgwsni(i,j)+        &
#    ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#    endif
     &                                 FORCES(ng)%wsni(i,j)
            END DO
          END DO
        END IF

#   endif
#  endif
#  ifdef ICESHELF
        IF (Aout(idIsMr,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgm(i,j)=AVERAGE(ng)%avgm(i,j)+              &
#   ifdef WET_DRY
     &                              GRID(ng)%rmask_full(i,j)*           &
#   endif
     &                              ICESHELFVAR(ng)%m(i,j)
            END DO
          END DO
        END IF

#   ifdef ICESHELF_3EQN_VBC
        IF (Aout(idIsTs,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgTstar(i,j)=AVERAGE(ng)%avgTstar(i,j)+      &
#    ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#    endif
     &                                  ICESHELFVAR(ng)%Tstar(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsUs,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgUstar(i,j)=AVERAGE(ng)%avgUstar(i,j)+      &
#    ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#    endif
     &                                  ICESHELFVAR(ng)%Ustar(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsTb,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgTb(i,j)=AVERAGE(ng)%avgTb(i,j)+            &
#    ifdef WET_DRY
     &                               GRID(ng)%rmask_full(i,j)*          &
#    endif
     &                               ICESHELFVAR(ng)%Tb(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsSb,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgSb(i,j)=AVERAGE(ng)%avgSb(i,j)+            &
#    ifdef WET_DRY
     &                               GRID(ng)%rmask_full(i,j)*          &
#    endif
     &                               ICESHELFVAR(ng)%Sb(i,j)
            END DO
          END DO
        END IF

#   endif
#   ifdef ICESHELF_MORPH
        IF (Aout(idIsDrft,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgzice(i,j)=AVERAGE(ng)%avgzice(i,j)+        &
#    ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#    endif
     &                                 GRID(ng)%zice(i,j)
            END DO
          END DO
        END IF

#   endif
#   ifdef ICESHELF_SOF
        IF (Aout(idIsSOFs,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgSOFs(i,j)=AVERAGE(ng)%avgSOFs(i,j)+        &
#    ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#    endif
     &                                 ICESHELFVAR(ng)%SOFs(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsSOFh,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgSOFh(i,j)=AVERAGE(ng)%avgSOFh(i,j)+        &
#    ifdef WET_DRY
     &                                 GRID(ng)%rmask_full(i,j)*        &
#    endif
     &                                 ICESHELFVAR(ng)%SOFh(i,j)
            END DO
          END DO
        END IF

#   endif
#  endif
      END IF
!
!-----------------------------------------------------------------------
!  Covert accumulated sums into time-averages for ice variables.
!-----------------------------------------------------------------------
!
      IF (((iic(ng).gt.ntsAVG(ng)).and.                                 &
     &     (MOD(iic(ng)-1,nAVG(ng)).eq.0).and.                          &
     &     ((iic(ng).ne.ntstart(ng)).or.(nrrec(ng).eq.0))).or.          &
     &    ((iic(ng).ge.ntsAVG(ng)).and.(nAVG(ng).eq.1))) THEN
!
!  Process state variables.
!
#  ifdef ICE_MODEL
        IF (Aout(idIuice,ng)) THEN
          DO j=JstrR,JendR
            DO i=Istr,IendR 
              AVERAGE(ng)%avgui(i,j)=ufac(i,j)*                         &
     &                               AVERAGE(ng)%avgui(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_u2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgui)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgui)
#   endif
          END IF
        END IF

        IF (Aout(idIvice,ng)) THEN
          DO j=Jstr,JendR 
            DO i=IstrR,IendR
              AVERAGE(ng)%avgvi(i,j)=vfac(i,j)*                         &
     &                               AVERAGE(ng)%avgvi(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_v2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgvi)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgvi)
#   endif
          END IF
        END IF

        IF (Aout(idIuiceE,ng).and.Aout(idIviceN,ng)) THEN
          DO j=Jstr,Jend
            DO i=Istr,Iend
              AVERAGE(ng)%avguie(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avguie(i,j)
              AVERAGE(ng)%avgvin(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avgvin(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avguie)
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgvin)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 2,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avguie,                     &
     &                          AVERAGE(ng)%avgvin)
#   endif
          END IF
        END IF

        IF (Aout(idIaice,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgai(i,j)=rfac(i,j)*                         &
     &                               AVERAGE(ng)%avgai(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgai)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgai)
#   endif
          END IF
        END IF

        IF (Aout(idIhice,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avghi(i,j)=rfac(i,j)*                         &
     &                               AVERAGE(ng)%avghi(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avghi)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avghi)
#   endif
          END IF
        END IF

        IF (Aout(idIhsno,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avghsn(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avghsn(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avghsn)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avghsn)
#   endif
          END IF
        END IF

        IF (Aout(idIagei,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgageice(i,j)=rfac(i,j)*                     &
     &                                   AVERAGE(ng)%avgageice(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgageice)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgageice)
#   endif
          END IF
        END IF

        IF (Aout(idItice,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgtis(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avgtis(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgtis)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgtis)
#   endif
          END IF
        END IF

        IF (Aout(idItmid,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgti(i,j)=rfac(i,j)*                         &
     &                               AVERAGE(ng)%avgti(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgti)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgti)
#   endif
          END IF
        END IF

        IF (Aout(idIt2ic,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgt2(i,j)=rfac(i,j)*                         &
     &                               AVERAGE(ng)%avgt2(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgt2)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgt2)
#   endif
          END IF
        END IF

        IF (Aout(idIsg11,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsig11(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgsig11(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgsig11)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgsig11)
#   endif
          END IF
        END IF

        IF (Aout(idIsg12,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsig12(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgsig12(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgsig12)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgsig12)
#   endif
          END IF
        END IF

        IF (Aout(idIsg22,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsig22(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgsig22(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgsig22)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgsig22)
#   endif
          END IF
        END IF

        IF (Aout(idIutau,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgutau_iw(i,j)=rfac(i,j)*                    &
     &                                    AVERAGE(ng)%avgutau_iw(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgutau_iw)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgutau_iw)
#   endif
          END IF
        END IF

        IF (Aout(idImchu,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgchu_iw(i,j)=rfac(i,j)*                     &
     &                                   AVERAGE(ng)%avgchu_iw(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgchu_iw)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgchu_iw)
#   endif
          END IF
        END IF

        IF (Aout(idIt0mk,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgt0mk(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgt0mk(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgt0mk)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgt0mk)
#   endif
          END IF
        END IF

        IF (Aout(idIs0mk,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgs0mk(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgs0mk(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgs0mk)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgs0mk)
#   endif
          END IF
        END IF

        IF (Aout(idImflx,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgiomflx(i,j)=rfac(i,j)*                     &
     &                                   AVERAGE(ng)%avgiomflx(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgiomflx)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgiomflx)
#   endif
          END IF
        END IF

        IF (Aout(idIwfr,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwfr(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avgwfr(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgwfr)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgwfr)
#   endif
          END IF
        END IF

        IF (Aout(idIwai,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwai(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avgwai(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgwai)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgwai)
#   endif
          END IF
        END IF

        IF (Aout(idIwao,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwao(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avgwao(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgwao)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgwao)
#   endif
          END IF
        END IF

        IF (Aout(idIwio,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwio(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avgwio(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgwio)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgwio)
#   endif
          END IF
        END IF

        IF (Aout(idIwro,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwro(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avgwro(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgwro)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgwro)
#   endif
          END IF
        END IF

        IF (Aout(idIwdiv,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwdiv(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgwdiv(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgwdiv)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgwdiv)
#   endif
          END IF
        END IF

#   ifdef ICE_DIAGS
#    ifdef ICE_ALBEDO
        IF (Aout(idIalbO,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgalbo(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgalbo(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgalbo)
#     ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgalbo)
#     endif
          END IF
        END IF

        IF (Aout(idIalbI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgalbi(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgalbi(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgalbi)
#     ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgalbi)
#     endif
          END IF
        END IF

#     ifdef ICE_ALB_CSIM
        IF (Aout(idIfrsw,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgfrswp(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgfrswp(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgfrswp)
#      ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgfrswp)
#      endif
          END IF
        END IF

#     endif
#    endif
        IF (Aout(idIrfac,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgrfaci(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgrfaci(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgrfaci)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgrfaci)
#    endif
          END IF
        END IF

        IF (Aout(idIqswl,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqswl(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgqswl(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgqswl)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgqswl)
#    endif
          END IF
        END IF

        IF (Aout(idIqlwl,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqlwl(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgqlwl(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgqlwl)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgqlwl)
#    endif
          END IF
        END IF

        IF (Aout(idIqlai,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqlai(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgqlai(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgqlai)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgqlai)
#    endif
          END IF
        END IF

        IF (Aout(idIqsei,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqsei(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgqsei(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgqsei)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgqsei)
#    endif
          END IF
        END IF

        IF (Aout(idIqlwa,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqlwa(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgqlwa(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgqlwa)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgqlwa)
#    endif
          END IF
        END IF

        IF (Aout(idIqswa,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqswa(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgqswa(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgqswa)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgqswa)
#    endif
          END IF
        END IF

        IF (Aout(idIqswi,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqswi(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgqswi(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgqswi)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgqswi)
#    endif
          END IF
        END IF

        IF (Aout(idIqswo,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqswo(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgqswo(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgqswo)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgqswo)
#    endif
          END IF
        END IF

        IF (Aout(idIsio,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsio(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avgsio(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgsio)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgsio)
#    endif
          END IF
        END IF

        IF (Aout(idIsao,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsao(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avgsao(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgsao)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgsao)
#    endif
          END IF
        END IF

        IF (Aout(idIqao,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqao(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avgqao(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgqao)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgqao)
#    endif
          END IF
        END IF

        IF (Aout(idIqai,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqai(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avgqai(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgqai)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgqai)
#    endif
          END IF
        END IF

        IF (Aout(idIqio,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqio(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avgqio(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgqio)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgqio)
#    endif
          END IF
        END IF

        IF (Aout(idIqi2,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqi2(i,j)=rfac(i,j)*                        &
     &                                AVERAGE(ng)%avgqi2(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgqi2)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgqi2)
#    endif
          END IF
        END IF

        IF (Aout(idIwsni,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgwsni(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgwsni(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgwsni)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgwsni)
#    endif
          END IF
        END IF

#   endif
#  endif
#  ifdef ICESHELF
        IF (Aout(idIsMr,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgm(i,j)=rfac(i,j)*                          &
     &                              AVERAGE(ng)%avgm(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgm)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgm)
#   endif
          END IF
        END IF

#   ifdef ICESHELF_3EQN_VBC
        IF (Aout(idIsTs,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgTstar(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgTstar(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgTstar)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgTstar)
#    endif
          END IF
        END IF

        IF (Aout(idIsUs,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgUstar(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgUstar(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgUstar)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgUstar)
#    endif
          END IF
        END IF

        IF (Aout(idIsTb,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgTb(i,j)=rfac(i,j)*                         &
     &                               AVERAGE(ng)%avgTb(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgTb)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgTb)
#    endif
          END IF
        END IF

        IF (Aout(idIsSb,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgSb(i,j)=rfac(i,j)*                         &
     &                               AVERAGE(ng)%avgSb(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgSb)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgSb)
#    endif
          END IF
        END IF

#   endif
#   ifdef ICESHELF_MORPH
        IF (Aout(idIsDrft,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgzice(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgzice(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgzice)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgzice)
#    endif
          END IF
        END IF

#   endif
#   ifdef ICESHELF_SOF
        IF (Aout(idIsSOFs,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgSOFs(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgSOFs(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgSOFs)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgSOFs)
#    endif
          END IF
        END IF

        IF (Aout(idIsSOFh,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgSOFh(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgSOFh(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgSOFh)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgSOFh)
#    endif
          END IF
        END IF

#   endif
#  endif
      END IF
# endif

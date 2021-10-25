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
#  ifdef ICE_MODEL
        IF (Aout(idIuice,ng)) THEN
          DO j=JstrR,JendR
            DO i=Istr,IendR 
              AVERAGE(ng)%avgui(i,j)=ICE(ng)%ui(i,j,IOUT)
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
              AVERAGE(ng)%avgvi(i,j)=ICE(ng)%vi(i,j,IOUT)
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
              AVERAGE(ng)%avgai(i,j)=ICE(ng)%ai(i,j,IOUT)
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
              AVERAGE(ng)%avghi(i,j)=ICE(ng)%hi(i,j,IOUT)
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
              AVERAGE(ng)%avghsn(i,j)=ICE(ng)%hsn(i,j,IOUT)
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
              AVERAGE(ng)%avgageice(i,j)=ICE(ng)%ageice(i,j,IOUT)
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
              AVERAGE(ng)%avgti(i,j)=ICE(ng)%ti(i,j,IOUT)
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
              AVERAGE(ng)%avgsig11(i,j)=ICE(ng)%sig11(i,j,IOUT)
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
              AVERAGE(ng)%avgsig12(i,j)=ICE(ng)%sig12(i,j,IOUT)
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
              AVERAGE(ng)%avgsig22(i,j)=ICE(ng)%sig22(i,j,IOUT)
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
        IF (Aout(idIalbe,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgalb(i,j)=FORCES(ng)%alb(i,j)
#     ifdef WET_DRY
              AVERAGE(ng)%avgalb(i,j)=AVERAGE(ng)%avgalb(i,j)*          &
     &                                GRID(ng)%rmask_full(i,j)
#     endif
            END DO
          END DO
        END IF

        IF (Aout(idIalbI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgalb_i(i,j)=FORCES(ng)%alb_i(i,j)
#     ifdef WET_DRY
              AVERAGE(ng)%avgalb_i(i,j)=AVERAGE(ng)%avgalb_i(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
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

        IF (Aout(idIlrdn,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avglr_dn(i,j)=FORCES(ng)%lr_dn(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avglr_dn(i,j)=AVERAGE(ng)%avglr_dn(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIsrdn,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsr_dn(i,j)=FORCES(ng)%sr_dn(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgsr_dn(i,j)=AVERAGE(ng)%avgsr_dn(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIlatI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avglat_i(i,j)=FORCES(ng)%lat_i(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avglat_i(i,j)=AVERAGE(ng)%avglat_i(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIsenI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsen_i(i,j)=FORCES(ng)%sen_i(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgsen_i(i,j)=AVERAGE(ng)%avgsen_i(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIlrup,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avglr_up_i(i,j)=FORCES(ng)%lr_up_i(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avglr_up_i(i,j)=AVERAGE(ng)%avglr_up_i(i,j)*  &
     &                                    GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIsrup,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsr_up_i(i,j)=FORCES(ng)%sr_up_i(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgsr_up_i(i,j)=AVERAGE(ng)%avgsr_up_i(i,j)*  &
     &                                    GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIsrin,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsr_in_i(i,j)=FORCES(ng)%sr_in_i(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgsr_in_i(i,j)=AVERAGE(ng)%avgsr_in_i(i,j)*  &
     &                                    GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIsrth,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsr_th_i(i,j)=FORCES(ng)%sr_th_i(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgsr_th_i(i,j)=AVERAGE(ng)%avgsr_th_i(i,j)*  &
     &                                    GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIsfI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgssflx_i(i,j)=FORCES(ng)%ssflx_i(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgssflx_i(i,j)=AVERAGE(ng)%avgssflx_i(i,j)*  &
     &                                    GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIsfL,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgssflx_l(i,j)=FORCES(ng)%ssflx_l(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgssflx_l(i,j)=AVERAGE(ng)%avgssflx_l(i,j)*  &
     &                                    GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqao,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqao_n(i,j)=FORCES(ng)%qao_n(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqao_n(i,j)=AVERAGE(ng)%avgqao_n(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqai,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqai_n(i,j)=FORCES(ng)%qai_n(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqai_n(i,j)=AVERAGE(ng)%avgqai_n(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqio,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqio_n(i,j)=FORCES(ng)%qio_n(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqio_n(i,j)=AVERAGE(ng)%avgqio_n(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIqi2,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgqi2_n(i,j)=FORCES(ng)%qi2_n(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgqi2_n(i,j)=AVERAGE(ng)%avgqi2_n(i,j)*      &
     &                                  GRID(ng)%rmask_full(i,j)
#    endif
            END DO
          END DO
        END IF

        IF (Aout(idIsnoi,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgsnoice(i,j)=FORCES(ng)%snoice(i,j)
#    ifdef WET_DRY
              AVERAGE(ng)%avgsnoice(i,j)=AVERAGE(ng)%avgsnoice(i,j)*    &
     &                                   GRID(ng)%rmask_full(i,j)
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
        IF (Aout(idIuice,ng)) THEN
          DO j=JstrR,JendR
            DO i=Istr,IendR 
              AVERAGE(ng)%avgIuice(i,j)=AVERAGE(ng)%avgIuice(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%umask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%ui(i,j,IOUT)
            END DO
          END DO
        END IF

        IF (Aout(idIvice,ng)) THEN
          DO j=Jstr,JendR 
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIvice(i,j)=AVERAGE(ng)%avgIvice(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%vmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%vi(i,j,IOUT)
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
              AVERAGE(ng)%avgIaice(i,j)=AVERAGE(ng)%avgIaice(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%ai(i,j,IOUT)
            END DO
          END DO
        END IF

        IF (Aout(idIhice,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIhice(i,j)=AVERAGE(ng)%avgIhice(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%hi(i,j,IOUT)
            END DO
          END DO
        END IF

        IF (Aout(idIhsno,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIhsno(i,j)=AVERAGE(ng)%avgIhsno(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%hsn(i,j,IOUT)
            END DO
          END DO
        END IF

        IF (Aout(idIagei,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIagei(i,j)=AVERAGE(ng)%avgIagei(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%ageice(i,j,IOUT)
            END DO
          END DO
        END IF

        IF (Aout(idItice,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgItice(i,j)=AVERAGE(ng)%avgItice(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%tis(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idItmid,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgItmid(i,j)=AVERAGE(ng)%avgItmid(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%ti(i,j,IOUT)
            END DO
          END DO
        END IF

        IF (Aout(idIt2ic,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIt2ic(i,j)=AVERAGE(ng)%avgIt2ic(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%t2(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsg11,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsg11(i,j)=AVERAGE(ng)%avgIsg11(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%sig11(i,j,IOUT)
            END DO
          END DO
        END IF

        IF (Aout(idIsg12,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsg12(i,j)=AVERAGE(ng)%avgIsg12(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%sig12(i,j,IOUT)
            END DO
          END DO
        END IF

        IF (Aout(idIsg22,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsg22(i,j)=AVERAGE(ng)%avgIsg22(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%sig22(i,j,IOUT)
            END DO
          END DO
        END IF

        IF (Aout(idIutau,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIutau(i,j)=AVERAGE(ng)%avgIutau(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%utau_iw(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idImchu,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgImchu(i,j)=AVERAGE(ng)%avgImchu(i,j)+      &
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
              AVERAGE(ng)%avgIt0mk(i,j)=AVERAGE(ng)%avgIt0mk(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%t0mk(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIs0mk,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIs0mk(i,j)=AVERAGE(ng)%avgIs0mk(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%s0mk(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idImflx,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgImflx(i,j)=AVERAGE(ng)%avgImflx(i,j)+      &
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
              AVERAGE(ng)%avgIwfr(i,j)=AVERAGE(ng)%avgIwfr(i,j)+        &
#   ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#   endif
     &                                  ICE(ng)%wfr(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIwai,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIwai(i,j)=AVERAGE(ng)%avgIwai(i,j)+        &
#   ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#   endif
     &                                  ICE(ng)%wai(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIwao,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIwao(i,j)=AVERAGE(ng)%avgIwao(i,j)+        &
#   ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#   endif
     &                                  ICE(ng)%wao(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIwio,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIwio(i,j)=AVERAGE(ng)%avgIwio(i,j)+        &
#   ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#   endif
     &                                  ICE(ng)%wio(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIwro,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIwro(i,j)=AVERAGE(ng)%avgIwro(i,j)+        &
#   ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#   endif
     &                                  ICE(ng)%wro(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIwdiv,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIwdiv(i,j)=AVERAGE(ng)%avgIwdiv(i,j)+      &
#   ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#   endif
     &                                   ICE(ng)%wdiv(i,j)
            END DO
          END DO
        END IF

#   ifdef ICE_DIAGS
#    ifdef ICE_ALBEDO
        IF (Aout(idIalbe,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIalbe(i,j)=AVERAGE(ng)%avgIalbe(i,j)+      &
#     ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#     endif
     &                                   FORCES(ng)%alb(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIalbI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIalbI(i,j)=AVERAGE(ng)%avgIalbI(i,j)+      &
#     ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#     endif
     &                                   FORCES(ng)%alb_i(i,j)
            END DO
          END DO
        END IF

#     ifdef ICE_ALB_CSIM
        IF (Aout(idIfrsw,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIfrsw(i,j)=AVERAGE(ng)%avgIfrsw(i,j)+      &
#      ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#      endif
     &                                   FORCES(ng)%frswp(i,j)
            END DO
          END DO
        END IF

#     endif
#    endif
        IF (Aout(idIrfac,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIrfac(i,j)=AVERAGE(ng)%avgIrfac(i,j)+      &
#    ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#    endif
     &                                   ICE(ng)%rfaci(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIlrdn,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIlrdn(i,j)=AVERAGE(ng)%avgIlrdn(i,j)+      &
#    ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#    endif
     &                                   FORCES(ng)%lr_dn(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsrdn,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsrdn(i,j)=AVERAGE(ng)%avgIsrdn(i,j)+      &
#    ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#    endif
     &                                   FORCES(ng)%sr_dn(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIlatI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIlatI(i,j)=AVERAGE(ng)%avgIlatI(i,j)+      &
#    ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#    endif
     &                                   FORCES(ng)%lat_i(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsenI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsenI(i,j)=AVERAGE(ng)%avgIsenI(i,j)+      &
#    ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#    endif
     &                                   FORCES(ng)%sen_i(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIlrup,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIlrup(i,j)=AVERAGE(ng)%avgIlrup(i,j)+      &
#    ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#    endif
     &                                   FORCES(ng)%lr_up_i(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsrup,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsrup(i,j)=AVERAGE(ng)%avgIsrup(i,j)+      &
#    ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#    endif
     &                                   FORCES(ng)%sr_up_i(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsrin,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsrin(i,j)=AVERAGE(ng)%avgIsrin(i,j)+      &
#    ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#    endif
     &                                   FORCES(ng)%sr_in_i(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsrth,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsrth(i,j)=AVERAGE(ng)%avgIsrth(i,j)+      &
#    ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#    endif
     &                                   FORCES(ng)%sr_th_i(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsfI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsfI(i,j)=AVERAGE(ng)%avgIsfI(i,j)+        &
#    ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#    endif
     &                                  FORCES(ng)%ssflx_i(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsfL,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsfL(i,j)=AVERAGE(ng)%avgIsfL(i,j)+        &
#    ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#    endif
     &                                  FORCES(ng)%ssflx_l(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqao,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIqao(i,j)=AVERAGE(ng)%avgIqao(i,j)+        &
#    ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#    endif
     &                                  FORCES(ng)%qao_n(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqai,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIqai(i,j)=AVERAGE(ng)%avgIqai(i,j)+        &
#    ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#    endif
     &                                  FORCES(ng)%qai_n(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqio,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIqio(i,j)=AVERAGE(ng)%avgIqio(i,j)+        &
#    ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#    endif
     &                                  FORCES(ng)%qio_n(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIqi2,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIqi2(i,j)=AVERAGE(ng)%avgIqi2(i,j)+        &
#    ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#    endif
     &                                  FORCES(ng)%qi2_n(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsnoi,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsnoi(i,j)=AVERAGE(ng)%avgIsnoi(i,j)+      &
#    ifdef WET_DRY
     &                                   GRID(ng)%rmask_full(i,j)*      &
#    endif
     &                                   FORCES(ng)%snoice(i,j)
            END DO
          END DO
        END IF

#   endif
#  endif
#  ifdef ICESHELF
        IF (Aout(idIsMr,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsMr(i,j)=AVERAGE(ng)%avgIsMr(i,j)+        &
#   ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#   endif
     &                                  ICESHELFVAR(ng)%m(i,j)
            END DO
          END DO
        END IF

#   ifdef ICESHELF_3EQN_VBC
        IF (Aout(idIsTs,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsTs(i,j)=AVERAGE(ng)%avgIsTs(i,j)+        &
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
              AVERAGE(ng)%avgIsUs(i,j)=AVERAGE(ng)%avgIsUs(i,j)+        &
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
              AVERAGE(ng)%avgIsTb(i,j)=AVERAGE(ng)%avgIsTb(i,j)+        &
#    ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#    endif
     &                                  ICESHELFVAR(ng)%Tb(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsSb,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsSb(i,j)=AVERAGE(ng)%avgIsSb(i,j)+        &
#    ifdef WET_DRY
     &                                  GRID(ng)%rmask_full(i,j)*       &
#    endif
     &                                  ICESHELFVAR(ng)%Sb(i,j)
            END DO
          END DO
        END IF

#   endif
#   ifdef ICESHELF_MORPH
        IF (Aout(idIsDrft,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsDrft(i,j)=AVERAGE(ng)%avgIsDrft(i,j)+    &
#    ifdef WET_DRY
     &                                    GRID(ng)%rmask_full(i,j)*     &
#    endif
     &                                    GRID(ng)%zice(i,j)
            END DO
          END DO
        END IF

#   endif
#   ifdef ICESHELF_SOF
        IF (Aout(idIsSOFs,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsSOFs(i,j)=AVERAGE(ng)%avgIsSOFs(i,j)+    &
#    ifdef WET_DRY
     &                                    GRID(ng)%rmask_full(i,j)*     &
#    endif
     &                                    ICESHELFVAR(ng)%SOFs(i,j)
            END DO
          END DO
        END IF

        IF (Aout(idIsSOFh,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsSOFh(i,j)=AVERAGE(ng)%avgIsSOFh(i,j)+    &
#    ifdef WET_DRY
     &                                    GRID(ng)%rmask_full(i,j)*     &
#    endif
     &                                    ICESHELFVAR(ng)%SOFh(i,j)
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
#  ifdef ICE_MODEL
        IF (Aout(idIuice,ng)) THEN
          DO j=JstrR,JendR
            DO i=Istr,IendR 
              AVERAGE(ng)%avgIuice(i,j)=ufac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIuice(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_u2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIuice)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIuice)
#   endif
          END IF
        END IF

        IF (Aout(idIvice,ng)) THEN
          DO j=Jstr,JendR 
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIvice(i,j)=vfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIvice(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_v2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIvice)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIvice)
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
              AVERAGE(ng)%avgIaice(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIaice(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIaice)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIaice)
#   endif
          END IF
        END IF

        IF (Aout(idIhice,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIhice(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIhice(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIhice)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIhice)
#   endif
          END IF
        END IF

        IF (Aout(idIhsno,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIhsno(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIhsno(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIhsno)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIhsno)
#   endif
          END IF
        END IF

        IF (Aout(idIagei,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIagei(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIagei(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIagei)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIagei)
#   endif
          END IF
        END IF

        IF (Aout(idItice,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgItice(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgItice(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgItice)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgItice)
#   endif
          END IF
        END IF

        IF (Aout(idItmid,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgItmid(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgItmid(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgItmid)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgItmid)
#   endif
          END IF
        END IF

        IF (Aout(idIt2ic,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIt2ic(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIt2ic(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIt2ic)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIt2ic)
#   endif
          END IF
        END IF

        IF (Aout(idIsg11,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsg11(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIsg11(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsg11)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsg11)
#   endif
          END IF
        END IF

        IF (Aout(idIsg12,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsg12(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIsg12(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsg12)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsg12)
#   endif
          END IF
        END IF

        IF (Aout(idIsg22,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsg22(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIsg22(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsg22)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsg22)
#   endif
          END IF
        END IF

        IF (Aout(idIutau,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIutau(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIutau(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIutau)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIutau)
#   endif
          END IF
        END IF

        IF (Aout(idImchu,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgImchu(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgImchu(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgImchu)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgImchu)
#   endif
          END IF
        END IF

        IF (Aout(idIt0mk,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIt0mk(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIt0mk(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIt0mk)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIt0mk)
#   endif
          END IF
        END IF

        IF (Aout(idIs0mk,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIs0mk(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIs0mk(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIs0mk)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIs0mk)
#   endif
          END IF
        END IF

        IF (Aout(idImflx,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgImflx(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgImflx(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgImflx)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgImflx)
#   endif
          END IF
        END IF

        IF (Aout(idIwfr,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIwfr(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIwfr(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIwfr)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIwfr)
#   endif
          END IF
        END IF

        IF (Aout(idIwai,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIwai(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIwai(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIwai)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIwai)
#   endif
          END IF
        END IF

        IF (Aout(idIwao,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIwao(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIwao(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIwao)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIwao)
#   endif
          END IF
        END IF

        IF (Aout(idIwio,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIwio(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIwio(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIwio)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIwio)
#   endif
          END IF
        END IF

        IF (Aout(idIwro,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIwro(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIwro(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIwro)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIwro)
#   endif
          END IF
        END IF

        IF (Aout(idIwdiv,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIwdiv(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIwdiv(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIwdiv)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIwdiv)
#   endif
          END IF
        END IF

#   ifdef ICE_DIAGS
#    ifdef ICE_ALBEDO
        IF (Aout(idIalbe,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIalbe(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIalbe(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIalbe)
#     ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIalbe)
#     endif
          END IF
        END IF

        IF (Aout(idIalbI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIalbI(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIalbI(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIalbI)
#     ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIalbI)
#     endif
          END IF
        END IF

#     ifdef ICE_ALB_CSIM
        IF (Aout(idIfrsw,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIfrsw(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIfrsw(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIfrsw)
#      ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIfrsw)
#      endif
          END IF
        END IF

#     endif
#    endif
        IF (Aout(idIrfac,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIrfac(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIrfac(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIrfac)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIrfac)
#    endif
          END IF
        END IF

        IF (Aout(idIlrdn,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIlrdn(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIlrdn(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIlrdn)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIlrdn)
#    endif
          END IF
        END IF

        IF (Aout(idIsrdn,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsrdn(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIsrdn(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsrdn)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsrdn)
#    endif
          END IF
        END IF

        IF (Aout(idIlatI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIlatI(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIlatI(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIlatI)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIlatI)
#    endif
          END IF
        END IF

        IF (Aout(idIsenI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsenI(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIsenI(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsenI)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsenI)
#    endif
          END IF
        END IF

        IF (Aout(idIlrup,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIlrup(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIlrup(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIlrup)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIlrup)
#    endif
          END IF
        END IF

        IF (Aout(idIsrup,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsrup(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIsrup(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsrup)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsrup)
#    endif
          END IF
        END IF

        IF (Aout(idIsrin,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsrin(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIsrin(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsrin)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsrin)
#    endif
          END IF
        END IF

        IF (Aout(idIsrth,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsrth(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIsrth(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsrth)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsrth)
#    endif
          END IF
        END IF

        IF (Aout(idIsfI,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsfI(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIsfI(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsfI)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsfI)
#    endif
          END IF
        END IF

        IF (Aout(idIsfL,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsfL(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIsfL(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsfL)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsfL)
#    endif
          END IF
        END IF

        IF (Aout(idIqao,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIqao(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIqao(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIqao)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIqao)
#    endif
          END IF
        END IF

        IF (Aout(idIqai,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIqai(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIqai(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIqai)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIqai)
#    endif
          END IF
        END IF

        IF (Aout(idIqio,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIqio(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIqio(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIqio)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIqio)
#    endif
          END IF
        END IF

        IF (Aout(idIqi2,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIqi2(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIqi2(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIqi2)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIqi2)
#    endif
          END IF
        END IF

        IF (Aout(idIsnoi,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsnoi(i,j)=rfac(i,j)*                      &
     &                                  AVERAGE(ng)%avgIsnoi(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsnoi)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsnoi)
#    endif
          END IF
        END IF

#   endif
#  endif
#  ifdef ICESHELF
        IF (Aout(idIsMr,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsMr(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIsMr(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsMr)
#   ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsMr)
#   endif
          END IF
        END IF

#   ifdef ICESHELF_3EQN_VBC
        IF (Aout(idIsTs,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsTs(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIsTs(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsTs)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsTs)
#    endif
          END IF
        END IF

        IF (Aout(idIsUs,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsUs(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIsUs(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsUs)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsUs)
#    endif
          END IF
        END IF

        IF (Aout(idIsTb,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsTb(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIsTb(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsTb)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsTb)
#    endif
          END IF
        END IF

        IF (Aout(idIsSb,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsSb(i,j)=rfac(i,j)*                       &
     &                                 AVERAGE(ng)%avgIsSb(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsSb)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsSb)
#    endif
          END IF
        END IF

#   endif
#   ifdef ICESHELF_MORPH
        IF (Aout(idIsDrft,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsDrft(i,j)=rfac(i,j)*                     &
     &                                   AVERAGE(ng)%avgIsDrft(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsDrft)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsDrft)
#    endif
          END IF
        END IF

#   endif
#   ifdef ICESHELF_SOF
        IF (Aout(idIsSOFs,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsSOFs(i,j)=rfac(i,j)*                     &
     &                                   AVERAGE(ng)%avgIsSOFs(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsSOFs)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsSOFs)
#    endif
          END IF
        END IF

        IF (Aout(idIsSOFh,ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              AVERAGE(ng)%avgIsSOFh(i,j)=rfac(i,j)*                     &
     &                                   AVERAGE(ng)%avgIsSOFh(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              AVERAGE(ng)%avgIsSOFh)
#    ifdef DISTRIBUTE
            CALL mp_exchange2d (ng, tile, iNLM, 1,                      &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          AVERAGE(ng)%avgIsSOFh)
#    endif
          END IF
        END IF

#   endif
#  endif
      END IF
# endif

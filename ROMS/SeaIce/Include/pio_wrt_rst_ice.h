# ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
!  Write out 2D ice momentum in the XI-direction.
!
      scale=1.0_dp
#  ifdef PERFECT_RESTART
      IF (RST(ng)%pioVar(idIuice)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_rubar(ng)
      ELSE
        ioDesc => ioDesc_sp_rubar(ng)
      END IF
      status=nf_fwrite3d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIuice),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#   ifdef MASKING
     &                   GRID(ng) % umask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%ui,                                    &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%ui)
#   endif
#  else
      IF (RST(ng)%pioVar(idIuice)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_u2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_u2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIuice),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % umask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%ui(:,:,IOUT),                          &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%ui(:,:,IOUT))
#   endif
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIuice)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out 2D ice momentum in the ETA-direction.
!
      scale=1.0_dp
#  ifdef PERFECT_RESTART
      IF (RST(ng)%pioVar(idIvice)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_rvbar(ng)
      ELSE
        ioDesc => ioDesc_sp_rvbar(ng)
      END IF
      status=nf_fwrite3d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIvice),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#   ifdef MASKING
     &                   GRID(ng) % vmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%vi,                                    &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%vi)
#   endif
#  else
      IF (RST(ng)%pioVar(idIvice)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_v2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_v2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIvice),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % vmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%vi(:,:,IOUT),                          &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%vi(:,:,IOUT))
#   endif
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIvice)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out ice concentration.
!
      scale=1.0_dp
#  ifdef PERFECT_RESTART
      IF (RST(ng)%pioVar(idIaice)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_rzeta(ng)
      ELSE
        ioDesc => ioDesc_sp_rzeta(ng)
      END IF
      status=nf_fwrite3d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIaice),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%ai,                                    &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%ai)
#   endif
#  else
      IF (RST(ng)%pioVar(idIaice)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIaice),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%ai(:,:,IOUT),                          &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%ai(:,:,IOUT))
#   endif
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIaice)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out ice average thickness.
!
      scale=1.0_dp
#  ifdef PERFECT_RESTART
      IF (RST(ng)%pioVar(idIhice)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_rzeta(ng)
      ELSE
        ioDesc => ioDesc_sp_rzeta(ng)
      END IF
      status=nf_fwrite3d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIhice),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%hi,                                    &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%hi)
#   endif
#  else
      IF (RST(ng)%pioVar(idIhice)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIhice),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%hi(:,:,IOUT),                          &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%hi(:,:,IOUT))
#   endif
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIhice)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out snow thickness.
!
      scale=1.0_dp
#  ifdef PERFECT_RESTART
      IF (RST(ng)%pioVar(idIhsno)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_rzeta(ng)
      ELSE
        ioDesc => ioDesc_sp_rzeta(ng)
      END IF
      status=nf_fwrite3d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIhsno),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%hsn,                                   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%hsn)
#   endif
#  else
      IF (RST(ng)%pioVar(idIhsno)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIhsno),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%hsn(:,:,IOUT),                         &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%hsn(:,:,IOUT))
#   endif
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIhsno)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out ice age.
!
      scale=1.0_dp
#  ifdef PERFECT_RESTART
      IF (RST(ng)%pioVar(idIagei)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_rzeta(ng)
      ELSE
        ioDesc => ioDesc_sp_rzeta(ng)
      END IF
      status=nf_fwrite3d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIagei),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%ageice,                                &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%ageice)
#   endif
#  else
      IF (RST(ng)%pioVar(idIagei)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIagei),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%ageice(:,:,IOUT),                      &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%ageice(:,:,IOUT))
#   endif
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIagei)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out temperature of ice/snow surface.
!
      scale=1.0_dp
      IF (RST(ng)%pioVar(idItice)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idItice),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%tis,                                   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%tis)
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idItice)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out temperature of internal ice.
!
      scale=1.0_dp
#  ifdef PERFECT_RESTART
      IF (RST(ng)%pioVar(idItmid)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_rzeta(ng)
      ELSE
        ioDesc => ioDesc_sp_rzeta(ng)
      END IF
      status=nf_fwrite3d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idItmid),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%ti,                                    &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%ti)
#   endif
#  else
      IF (RST(ng)%pioVar(idItmid)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idItmid),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%ti(:,:,IOUT),                          &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%ti(:,:,IOUT))
#   endif
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idItmid)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out internal ice stress component 11.
!
      scale=1.0_dp
#  ifdef PERFECT_RESTART
      IF (RST(ng)%pioVar(idIsg11)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_rzeta(ng)
      ELSE
        ioDesc => ioDesc_sp_rzeta(ng)
      END IF
      status=nf_fwrite3d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIsg11),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%sig11,                                 &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%sig11)
#   endif
#  else
      IF (RST(ng)%pioVar(idIsg11)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIsg11),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%sig11(:,:,IOUT),                       &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%sig11(:,:,IOUT))
#   endif
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIsg11)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out internal ice stress component 12.
!
      scale=1.0_dp
#  ifdef PERFECT_RESTART
      IF (RST(ng)%pioVar(idIsg12)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_rzeta(ng)
      ELSE
        ioDesc => ioDesc_sp_rzeta(ng)
      END IF
      status=nf_fwrite3d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIsg12),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%sig12,                                 &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%sig12)
#   endif
#  else
      IF (RST(ng)%pioVar(idIsg12)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIsg12),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%sig12(:,:,IOUT),                       &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%sig12(:,:,IOUT))
#   endif
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIsg12)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out internal ice stress component 22.
!
      scale=1.0_dp
#  ifdef PERFECT_RESTART
      IF (RST(ng)%pioVar(idIsg22)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_rzeta(ng)
      ELSE
        ioDesc => ioDesc_sp_rzeta(ng)
      END IF
      status=nf_fwrite3d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIsg22),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%sig22,                                 &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%sig22)
#   endif
#  else
      IF (RST(ng)%pioVar(idIsg22)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIsg22),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICE(ng)%sig22(:,:,IOUT),                       &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICE(ng)%sig22(:,:,IOUT))
#   endif
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIsg22)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out ice-water friction velocity.
!
      scale=1.0_dp
      IF (RST(ng)%pioVar(idIutau)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIutau),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%utau_iw,                               &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%utau_iw)
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIutau)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out ice-water momentum transfer coefficient.
!
      scale=1.0_dp
      IF (RST(ng)%pioVar(idImchu)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idImchu),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%chu_iw,                                &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%chu_iw)
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idImchu)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out temperature of molecular sublayer under ice.
!
      scale=1.0_dp
      IF (RST(ng)%pioVar(idIt0mk)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIt0mk),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%t0mk,                                  &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%t0mk)
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIt0mk)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out salinity of molecular sublayer under ice.
!
      scale=1.0_dp
      IF (RST(ng)%pioVar(idIs0mk)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIs0mk),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%s0mk,                                  &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%s0mk)
#  endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIs0mk)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
#  ifdef PERFECT_RESTART
!
!  Write out surface active tracer fluxes.
!
      DO itrc=1,NAT
        scale=1.0_dp
        IF (RST(ng)%pioVar(idTsur(itrc))%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                   &
     &                     RST(ng)%pioVar(idTsur(itrc)),                &
     &                     RST(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
#   ifdef WET_DRY
     &                     FORCES(ng)%stflx_save(:,:,itrc),             &
     &                     SetFillVal = .FALSE.)
#   else
     &                     FORCES(ng)%stflx_save(:,:,itrc))
#   endif
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTsur(itrc))),             &
                              RST(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END DO
!
!  Write out surface U-momentum stress.
!
      scale=1.0_dp
      IF (RST(ng)%pioVar(idUsms)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idUsms),                        &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   FORCES(ng)%sustr,                              &
     &                   SetFillVal = .FALSE.)
#   else
     &                   FORCES(ng)%sustr)
#   endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idUsms)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out surface V-momentum stress.
!
      scale=1.0_dp
      IF (RST(ng)%pioVar(idVsms)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idVsms),                        &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   FORCES(ng)%svstr,                              &
     &                   SetFillVal = .FALSE.)
#   else
     &                   FORCES(ng)%svstr)
#   endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idVsms)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
#  endif
# endif
# ifdef ICESHELF
#  ifdef ICESHELF_MORPH
!
!  Write out time-varying ice draft.
!
      scale=1.0_dp
      IF (RST(ng)%pioVar(idIsDrft)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIsDrft),                      &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   GRID(ng)%zice,                                 &
     &                   SetFillVal = .FALSE.)
#   else
     &                   GRID(ng)%zice)
#   endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIsDrft)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
#  endif
#  ifdef ICESHELF_TRACER
!
!  Write out iceshelf volume (m).
!
      scale=1.0_dp
      IF (RST(ng)%pioVar(idIsVol)%dkind.eq.PIO_double) THEN
        ioDesc => ioDesc_dp_r2dvar(ng)
      ELSE
        ioDesc => ioDesc_sp_r2dvar(ng)
      END IF
      status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                     &
     &                   RST(ng)%pioVar(idIsVol),                       &
     &                   RST(ng)%Rindex,                                &
     &                   ioDesc,                                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#   endif
#   ifdef WET_DRY
     &                   ICESHELFVAR(ng)%isvol,                         &
     &                   SetFillVal = .FALSE.)
#   else
     &                   ICESHELFVAR(ng)%isvol)
#   endif
      IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIsVol)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out integrated tracer (m Tunits).
!
      DO itrc=NAT+1, NT(ng)
        scale=1.0_dp
        IF (RST(ng)%pioVar(idIsTrc(itrc))%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, RST(ng)%pioFile,                   &
     &                     RST(ng)%pioVar(idIsTrc(itrc)),               &
     &                     RST(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
#   ifdef WET_DRY
     &                     ICESHELFVAR(ng)%istrc,                       &
     &                     SetFillVal = .FALSE.)
#   else
     &                     ICESHELFVAR(ng)%istrc)
#   endif
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsTrc(itrc))),             &
                              RST(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END DO
#  endif
# endif

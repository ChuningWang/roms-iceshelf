#ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
!  Write out 2D ice momentum in the XI-direction.
!
      scale=1.0_dp
# ifdef PERFECT_RESTART
      gtype=gfactor*u3dvar
      status=nf_fwrite3d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIuice),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#  ifdef MASKING
     &                   GRID(ng) % umask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%ui,                                    &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%ui)
#  endif
# else
      gtype=gfactor*u2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIuice),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % umask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%ui(:,:,IOUT),                          &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%ui(:,:,IOUT))
#  endif
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
# ifdef PERFECT_RESTART
      gtype=gfactor*v3dvar
      status=nf_fwrite3d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIvice),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#  ifdef MASKING
     &                   GRID(ng) % vmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%vi,                                    &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%vi)
#  endif
# else
      gtype=gfactor*v2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIvice),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % vmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%vi(:,:,IOUT),                          &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%vi(:,:,IOUT))
#  endif
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
# ifdef PERFECT_RESTART
      gtype=gfactor*r3dvar
      status=nf_fwrite3d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIaice),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%ai,                                    &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%ai)
#  endif
# else
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIaice),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%ai(:,:,IOUT),                          &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%ai(:,:,IOUT))
#  endif
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
# ifdef PERFECT_RESTART
      gtype=gfactor*r3dvar
      status=nf_fwrite3d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIhice),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%hi,                                    &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%hi)
#  endif
# else
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIhice),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%hi(:,:,IOUT),                          &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%hi(:,:,IOUT))
#  endif
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
# ifdef PERFECT_RESTART
      gtype=gfactor*r3dvar
      status=nf_fwrite3d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIhsno),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%hsn,                                   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%hsn)
#  endif
# else
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIhsno),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%hsn(:,:,IOUT),                         &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%hsn(:,:,IOUT))
#  endif
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
# ifdef PERFECT_RESTART
      gtype=gfactor*r3dvar
      status=nf_fwrite3d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIagei),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%ageice,                                &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%ageice)
#  endif
# else
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIagei),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%ageice(:,:,IOUT),                      &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%ageice(:,:,IOUT))
#  endif
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idItice),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % rmask,                              &
# endif
# ifdef WET_DRY
     &                   ICE(ng)%tis,                                   &
     &                   SetFillVal = .FALSE.)
# else
     &                   ICE(ng)%tis)
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
# ifdef PERFECT_RESTART
      gtype=gfactor*r3dvar
      status=nf_fwrite3d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idItmid),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%ti,                                    &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%ti)
#  endif
# else
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idItmid),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%ti(:,:,IOUT),                          &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%ti(:,:,IOUT))
#  endif
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
# ifdef PERFECT_RESTART
      gtype=gfactor*r3dvar
      status=nf_fwrite3d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsg11),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%sig11,                                 &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%sig11)
#  endif
# else
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsg11),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%sig11(:,:,IOUT),                       &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%sig11(:,:,IOUT))
#  endif
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
# ifdef PERFECT_RESTART
      gtype=gfactor*r3dvar
      status=nf_fwrite3d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsg12),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%sig12,                                 &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%sig12)
#  endif
# else
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsg12),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%sig12(:,:,IOUT),                       &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%sig12(:,:,IOUT))
#  endif
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
# ifdef PERFECT_RESTART
      gtype=gfactor*r3dvar
      status=nf_fwrite3d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsg22),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, 2, scale,               &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%sig22,                                 &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%sig22)
#  endif
# else
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsg22),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICE(ng)%sig22(:,:,IOUT),                       &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICE(ng)%sig22(:,:,IOUT))
#  endif
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIutau),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % rmask,                              &
# endif
# ifdef WET_DRY
     &                   ICE(ng)%utau_iw,                               &
     &                   SetFillVal = .FALSE.)
# else
     &                   ICE(ng)%utau_iw)
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idImchu),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % rmask,                              &
# endif
# ifdef WET_DRY
     &                   ICE(ng)%chu_iw,                                &
     &                   SetFillVal = .FALSE.)
# else
     &                   ICE(ng)%chu_iw)
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIt0mk),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % rmask,                              &
# endif
# ifdef WET_DRY
     &                   ICE(ng)%t0mk,                                  &
     &                   SetFillVal = .FALSE.)
# else
     &                   ICE(ng)%t0mk)
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIs0mk),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % rmask,                              &
# endif
# ifdef WET_DRY
     &                   ICE(ng)%s0mk,                                  &
     &                   SetFillVal = .FALSE.)
# else
     &                   ICE(ng)%s0mk)
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIs0mk)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
# ifdef PERFECT_RESTART
!
!  Write out surface active tracer fluxes.
!
      DO itrc=1,NAT
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid,                      &
     &                     RST(ng)%Vid(idTsur(itrc)),                   &
     &                     RST(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%stflx_save,                       &
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idUsms),   &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   FORCES(ng)%sustr,                              &
     &                   SetFillVal = .FALSE.)
#  else
     &                   FORCES(ng)%sustr)
#  endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idVsms),   &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   FORCES(ng)%svstr,                              &
     &                   SetFillVal = .FALSE.)
#  else
     &                   FORCES(ng)%svstr)
#  endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idVsms)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
# endif
#endif
#ifdef ICESHELF
# ifdef ICESHELF_MORPH
!
!  Write out time-varying ice draft.
!
      scale=1.0_dp
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsDrft), &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   GRID(ng)%zice,                                 &
     &                   SetFillVal = .FALSE.)
#  else
     &                   GRID(ng)%zice)
#  endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idIsDrft)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
# endif
# ifdef ICESHELF_TRACER
!
!  Write out iceshelf volume (m).
!
      scale=1.0_dp
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsVol),  &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
#  ifdef WET_DRY
     &                   ICESHELFVAR(ng)%isvol,                         &
     &                   SetFillVal = .FALSE.)
#  else
     &                   ICESHELFVAR(ng)%isvol)
#  endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
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
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid,                      &
     &                     RST(ng)%Vid(idIsTrc(itrc)),                  &
     &                     RST(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICESHELFVAR(ng)%istrc,                       &
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsTrc(itrc))),             &
                              RST(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END DO
# endif
#endif

#  ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
!  Write out 2D ice momentum in the XI-direction.
!
      IF (Aout(idIuice,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIuice)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIuice),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % umask,                            &
#   endif
     &                     AVERAGE(ng) % avgui)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIuice)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D ice momentum in the ETA-direction.
!
      IF (Aout(idIvice,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIvice)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIvice),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#   endif
     &                     AVERAGE(ng) % avgvi)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIvice)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D ice momentum in the East-direction.
!
      IF (Aout(idIuiceE,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIuiceE)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIuiceE),                    &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avguie)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIuiceE)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D ice momentum in the North-direction.
!
      IF (Aout(idIviceN,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIviceN)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIviceN),                    &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgvin)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIviceN)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice concentration.
!
      IF (Aout(idIaice,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIaice)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIaice),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgai)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIaice)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice average thickness.
!
      IF (Aout(idIhice,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIhice)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIhice),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avghi)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIhice)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out snow thickness.
!
      IF (Aout(idIhsno,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIhsno)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIhsno),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avghsn)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIhsno)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice age.
!
      IF (Aout(idIagei,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIagei)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIagei),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgageice)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIagei)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out temperature of ice/snow surface.
!
      IF (Aout(idItice,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idItice)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idItice),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgtis)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idItice)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out temperature of internal ice.
!
      IF (Aout(idItmid,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idItmid)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idItmid),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgti)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idItmid)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out temperature of top ice.
!
      IF (Aout(idIt2ic,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIt2ic)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIt2ic),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgt2)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIt2ic)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 11.
!
      IF (Aout(idIsg11,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsg11)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsg11),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgsig11)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsg11)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 12.
!
      IF (Aout(idIsg12,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsg12)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsg12),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgsig12)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsg12)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 22.
!
      IF (Aout(idIsg22,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsg22)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsg22),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgsig22)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsg22)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-water friction velocity.
!
      IF (Aout(idIutau,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIutau)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIutau),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgutau_iw)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIutau)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-water momentum transfer coefficient.
!
      IF (Aout(idImchu,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idImchu)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idImchu),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgchu_iw)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idImchu)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out temperature of molecular sublayer under ice.
!
      IF (Aout(idIt0mk,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIt0mk)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIt0mk),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgt0mk)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIt0mk)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out salinity of molecular sublayer under ice.
!
      IF (Aout(idIs0mk,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIs0mk)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIs0mk),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgs0mk)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIs0mk)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-ocean mass flux.
!
      IF (Aout(idImflx,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idImflx)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idImflx),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgiomflx)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idImflx)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice frazil growth rate.
!
      IF (Aout(idIwfr,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIwfr)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIwfr),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgwfr)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwfr)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice growth/melt rate of ice-air.
!
      IF (Aout(idIwai,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIwai)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIwai),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgwai)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwai)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice growth/melt rate of leads.
!
      IF (Aout(idIwao,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIwao)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIwao),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgwao)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwao)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice growth/melt rate of ice-ocean.
!
      IF (Aout(idIwio,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIwio)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIwio),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgwio)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwio)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice melt runoff rate.
!
      IF (Aout(idIwro,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIwro)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIwro),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgwro)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwro)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice divergence rate.
!
      IF (Aout(idIwdiv,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIwdiv)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIwdiv),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgwdiv)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwdiv)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   ifdef ICE_DIAGS
#    ifdef ICE_ALBEDO
!
!  Write out ocean albedo.
!
      IF (Aout(idIalbe,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIalbe)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIalbe),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#     ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#     endif
     &                     AVERAGE(ng) % avgalb)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIalbe)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice albedo.
!
      IF (Aout(idIalbI,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIalbI)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIalbI),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#     ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#     endif
     &                     AVERAGE(ng) % avgalb_i)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIalbI)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#     ifdef ICE_ALB_CSIM
!
!  Write out fraction of shortwave penetrating into the ice.
!
      IF (Aout(idIfrsw,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIfrsw)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIfrsw),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#      ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#      endif
     &                     AVERAGE(ng) % avgfrswp)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIfrsw)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#     endif
#    endif
!
!  Write out reduction factor for near-IR fraction of SW under ice.
!
      IF (Aout(idIrfac,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIrfac)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIrfac),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgrfaci)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIrfac)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out downward longwave radiation.
!
      IF (Aout(idIlrdn,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIlrdn)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIlrdn),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avglr_dn)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIlrdn)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out downward shortwave radiation.
!
      IF (Aout(idIsrdn,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsrdn)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsrdn),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgsr_dn)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsrdn)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out latent heat over ice.
!
      IF (Aout(idIlatI,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIlatI)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIlatI),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avglat_i)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIlatI)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out sensible heat over ice.
!
      IF (Aout(idIsenI,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsenI)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsenI),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgsen_i)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsenI)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out upward longwave over ice.
!
      IF (Aout(idIlrup,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIlrup)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIlrup),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avglr_up_i)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIlrup)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out upward shortwave over ice.
!
      IF (Aout(idIsrup,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsrup)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsrup),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgsr_up_i)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsrup)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out shortwave into ice.
!
      IF (Aout(idIsrin,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsrin)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsrin),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgsr_in_i)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsrin)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out shortwave through ice.
!
      IF (Aout(idIsrth,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsrth)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsrth),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgsr_th_i)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsrth)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out salt flux under ice.
!
      IF (Aout(idIsfI,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsfI)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsfI),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgssflx_i)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsfI)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out salt flux through leads.
!
      IF (Aout(idIsfL,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsfL)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsfL),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgssflx_l)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsfL)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out heat flux through leads.
!
      IF (Aout(idIqao,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIqao)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIqao),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgqao_n)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqao)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out heat flux of ice-air.
!
      IF (Aout(idIqai,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIqai)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIqai),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgqai_n)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqai)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out heat flux of ice-ocean.
!
      IF (Aout(idIqio,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIqio)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIqio),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgqio_n)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqio)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice heat flux.
!
      IF (Aout(idIqi2,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIqi2)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIqi2),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgqi2_n)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqi2)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out snow-ice conversion.
!
      IF (Aout(idIsnoi,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsnoi)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsnoi),                     &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgsnoice)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsnoi)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#  endif
#  ifdef ICESHELF
!
!  Write out iceshelf melt rate.
!
      IF (Aout(idIsMr,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsMr)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsMr),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgm)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsMr)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   ifdef ICESHELF_3EQN_VBC
!
!  Write out iceshelf frictional temperature.
!
      IF (Aout(idIsTs,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsTs)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsTs),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgTstar)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsTs)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out iceshelf frictional velocity.
!
      IF (Aout(idIsUs,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsUs)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsUs),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgUstar)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsUs)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out iceshelf molecular sublayer temperature.
!
      IF (Aout(idIsTb,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsTb)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsTb),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgTb)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsTb)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out iceshelf molecular sublayer salinity.
!
      IF (Aout(idIsSb,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsSb)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsSb),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgSb)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsSb)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#   ifdef ICESHELF_MORPH
!
!  Write out time-varying ice draft.
!
      IF (Aout(idIsDrft,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsDrft)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsDrft),                    &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgzice)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsDrft)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#   ifdef ICESHELF_SOF
!
!  Write out surface salt overflux.
!
      IF (Aout(idIsSOFs,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsSOFs)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsSOFs),                    &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgSOFs)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsSOFs)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface heat overflux.
!
      IF (Aout(idIsSOFh,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idIsSOFh)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idIsSOFh),                    &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgSOFh)
        IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsSOFh)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#  endif

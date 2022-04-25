#ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
!  Write out 2D ice momentum in the XI-direction.
!
      IF (Qout(idIuice,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIuice),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % umask,                            &
# endif
     &                     ICE(ng) % ui(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIuice)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D ice momentum in the ETA-direction.
!
      IF (Qout(idIvice,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIvice),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % vmask,                            &
# endif
     &                     ICE(ng) % vi(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIvice)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D Eastward and Northward ice momentum components (m/s) at
!  RHO-points.
!
      IF (Qout(idIuiceE,ng).and.Qout(idIviceN,ng)) THEN
        IF (.not.allocated(Ur2d)) THEN
          allocate (Ur2d(LBi:UBi,LBj:UBj))
            Ur2d(LBi:UBi,LBj:UBj)=0.0_r8
        END IF
        IF (.not.allocated(Vr2d)) THEN
          allocate (Vr2d(LBi:UBi,LBj:UBj))
            Vr2d(LBi:UBi,LBj:UBj)=0.0_r8
        END IF
        CALL uv_rotate2d (ng, tile, .FALSE., .TRUE.,                    &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    GRID(ng) % CosAngler,                         &
     &                    GRID(ng) % SinAngler,                         &
# ifdef MASKING
     &                    GRID(ng) % rmask_full,                        &
# endif
     &                    ICE(ng) % ui(:,:,IUOUT),                      &
     &                    ICE(ng) % vi(:,:,IUOUT),                      &
     &                    Ur2d, Vr2d)

        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIuiceE),                       &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
# endif
     &                     Ur2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIuiceE)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF

        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIviceN),                       &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
# endif
     &                     Vr2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIviceN)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
        deallocate (Ur2d)
        deallocate (Vr2d)
      END IF
!
!  Write out ice concentration.
!
      IF (Qout(idIaice,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIaice),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % ai(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIaice)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice average thickness.
!
      IF (Qout(idIhice,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIhice),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % hi(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIhice)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out snow thickness.
!
      IF (Qout(idIhsno,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIhsno),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % hsn(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIhsno)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice age.
!
      IF (Qout(idIagei,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIagei),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % ageice(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIagei)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out temperature of ice/snow surface.
!
      IF (Qout(idItice,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idItice),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % tis)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idItice)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out temperature of internal ice.
!
      IF (Qout(idItmid,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idItmid),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % ti(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idItmid)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out temperature of top ice.
!
      IF (Qout(idIt2ic,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIt2ic),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % t2)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIt2ic)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 11.
!
      IF (Qout(idIsg11,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIsg11),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % sig11(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsg11)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 12.
!
      IF (Qout(idIsg12,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIsg12),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % sig12(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsg12)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 22.
!
      IF (Qout(idIsg22,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIsg22),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % sig22(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsg22)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-water friction velocity.
!
      IF (Qout(idIutau,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIutau),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % utau_iw)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIutau)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-water momentum transfer coefficient.
!
      IF (Qout(idImchu,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idImchu),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % chu_iw)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idImchu)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out temperature of molecular sublayer under ice.
!
      IF (Qout(idIt0mk,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIt0mk),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % t0mk)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIt0mk)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out salinity of molecular sublayer under ice.
!
      IF (Qout(idIs0mk,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIs0mk),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % s0mk)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIs0mk)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-ocean mass flux.
!
      IF (Qout(idImflx,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idImflx),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % iomflx)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idImflx)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice frazil growth rate.
!
      IF (Qout(idIwfr,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIwfr),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % wfr)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwfr)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice growth/melt rate of ice-air.
!
      IF (Qout(idIwai,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIwai),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % wai)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwai)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice growth/melt rate of leads.
!
      IF (Qout(idIwao,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIwao),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % wao)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwao)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice growth/melt rate of ice-ocean.
!
      IF (Qout(idIwio,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIwio),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % wio)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwio)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice melt runoff rate.
!
      IF (Qout(idIwro,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIwro),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % wro)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwro)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice divergence rate.
!
      IF (Qout(idIwdiv,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIwdiv),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng) % wdiv)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwdiv)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# ifdef ICE_DIAGS
#  ifdef ICE_ALBEDO
!
!  Write out ocean albedo.
!
      IF (Qout(idIalbO,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIalbO),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     FORCES(ng) % albo)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIalbO)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice albedo.
!
      IF (Qout(idIalbI,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIalbI),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     FORCES(ng) % albi)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIalbI)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   ifdef ICE_ALB_CSIM
!
!  Write out fraction of shortwave penetrating into the ice.
!
      IF (Qout(idIfrsw,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIfrsw),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     FORCES(ng) % frswp)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIfrsw)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#  endif
!
!  Write out shortwave radiation into ice.
!
      IF (Qout(idIqswi,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIqswi),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % qswi)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqswi)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out longwave radiation into ice.
!
      IF (Qout(idIqlwi,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIqlwi),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % qlwi)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqlwi)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out latent heat flux into ice.
!
      IF (Qout(idIqlai,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIqlai),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % qlai)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqlai)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out sensible heat flux into ice.
!
      IF (Qout(idIqsei,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIqsei),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % qsei)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqsei)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out downward shortwave radiation into leads.
!
      IF (Qout(idIqswl,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIqswl),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % qswl)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqswl)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out downward longwave radiation into leads.
!
      IF (Qout(idIqlwl,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIqlwl),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % qlwl)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqlwl)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out upward longwave into atmosphere.
!
      IF (Qout(idIqlwa,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIqlwa),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % qlwa)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqlwa)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out upward shortwave into atmosphere.
!
      IF (Qout(idIqswa,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIqswa),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % qswa)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqswa)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out shortwave radiation through ice.
!
      IF (Qout(idIqswo,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIqswo),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % qswo)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqswo)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out heat flux, air-ocean.
!
      IF (Qout(idIqao,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIqao),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % qao)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqao)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out heat flux, air-ice.
!
      IF (Qout(idIqai,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIqai),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % qai)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqai)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out heat flux, ice-ocean.
!
      IF (Qout(idIqio,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIqio),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % qio)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqio)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out heat flux, ice-snow.
!
      IF (Qout(idIqi2,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIqi2),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % qi2)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqi2)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out salt flux, air-ocean.
!
      IF (Qout(idIsao,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIsao),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % sao)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsao)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out salt flux, ice-ocean.
!
      IF (Qout(idIsio,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIsio),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % sio)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsio)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out snow-ice conversion.
!
      IF (Qout(idIwsni,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIwsni),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % wsni)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwsni)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
#endif
#ifdef ICESHELF
!
!  Write out iceshelf melt rate.
!
      IF (Qout(idIsMr,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIsMr),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICESHELFVAR(ng) % m)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsMr)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# ifdef ICESHELF_3EQN_VBC
!
!  Write out iceshelf frictional temperature.
!
      IF (Qout(idIsTs,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIsTs),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICESHELFVAR(ng) % Tstar)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsTs)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out iceshelf frictional velocity.
!
      IF (Qout(idIsUs,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIsUs),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICESHELFVAR(ng) % Ustar)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsUs)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out iceshelf molecular sublayer temperature.
!
      IF (Qout(idIsTb,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIsTb),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICESHELFVAR(ng) % Tb)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsTb)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out iceshelf molecular sublayer salinity.
!
      IF (Qout(idIsSb,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIsSb),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICESHELFVAR(ng) % Sb)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsSb)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
# ifdef ICESHELF_MORPH
!
!  Write out time-varying ice draft.
!
      IF (Qout(idIsDrft,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIsDrft),                       &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     GRID(ng) % zice)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsDrft)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
# ifdef ICESHELF_SOF
!
!  Write out surface salt overflux.
!
      IF (Qout(idIsSOFs,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIsSOFs),                       &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICESHELFVAR(ng) % SOFs)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsSOFs)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface heat overflux.
!
      IF (Qout(idIsSOFh,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIsSOFh),                       &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICESHELFVAR(ng) % SOFh)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsSOFh)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
#endif

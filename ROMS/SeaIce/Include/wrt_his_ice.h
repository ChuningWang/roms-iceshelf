#ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
!  Write out 2D ice momentum in the XI-direction.
!
      IF (Hout(idIuice,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIuice),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % umask,                            &
# endif
     &                     ICE(ng)%ui(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIuice)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D ice momentum in the ETA-direction.
!
      IF (Hout(idIvice,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIvice),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % vmask,                            &
# endif
     &                     ICE(ng)%vi(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIvice)), HIS(ng)%Rindex
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
      IF (Hout(idIuiceE,ng).and.Hout(idIviceN,ng)) THEN
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
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIuiceE),                       &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
# endif
     &                     Ur2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIuiceE)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF

        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
                           HIS(ng)%Vid(idIviceN),                       &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
# endif
     &                     Vr2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIviceN)), HIS(ng)%Rindex
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
      IF (Hout(idIaice,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIaice),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%ai(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIaice)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice average thickness.
!
      IF (Hout(idIhice,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIhice),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%hi(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIhice)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out snow thickness.
!
      IF (Hout(idIhsno,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIhsno),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%hsn(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIhsno)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice age.
!
      IF (Hout(idIagei,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIagei),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%ageice(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIagei)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out temperature of ice/snow surface.
!
      IF (Hout(idItice,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idItice),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%tis)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idItice)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out temperature of internal ice.
!
      IF (Hout(idItmid,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idItmid),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%ti(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idItmid)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out temperature of top ice.
!
      IF (Hout(idIt2ic,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIt2ic),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%t2)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIt2ic)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 11.
!
      IF (Hout(idIsg11,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIsg11),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%sig11(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsg11)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 12.
!
      IF (Hout(idIsg12,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIsg12),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%sig12(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsg12)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 22.
!
      IF (Hout(idIsg22,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIsg22),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%sig22(:,:,IOUT))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsg22)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-water friction velocity.
!
      IF (Hout(idIutau,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIutau),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%utau_iw)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIutau)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-water momentum transfer coefficient.
!
      IF (Hout(idImchu,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idImchu),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%chu_iw)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idImchu)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out temperature of molecular sublayer under ice.
!
      IF (Hout(idIt0mk,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIt0mk),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%t0mk)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIt0mk)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out salinity of molecular sublayer under ice.
!
      IF (Hout(idIs0mk,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIs0mk),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%s0mk)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIs0mk)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-ocean mass flux.
!
      IF (Hout(idImflx,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idImflx),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%iomflx)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idImflx)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice frazil growth rate.
!
      IF (Hout(idIwfr,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIwfr),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%wfr)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwfr)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice growth/melt rate of ice-air.
!
      IF (Hout(idIwai,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIwai),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%wai)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwai)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice growth/melt rate of leads.
!
      IF (Hout(idIwao,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIwao),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%wao)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwao)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice growth/melt rate of ice-ocean.
!
      IF (Hout(idIwio,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIwio),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%wio)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwio)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice melt runoff rate.
!
      IF (Hout(idIwro,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIwro),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%wro)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwro)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice divergence rate.
!
      IF (Hout(idIwdiv,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIwdiv),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICE(ng)%wdiv)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwdiv)), HIS(ng)%Rindex
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
      IF (Hout(idIalbO,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIalbO),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     FORCES(ng)%albo)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIalbO)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice albedo.
!
      IF (Hout(idIalbI,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIalbI),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     FORCES(ng)%albi)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIalbI)), HIS(ng)%Rindex
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
      IF (Hout(idIfrsw,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIfrsw),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     FORCES(ng)%frswp)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIfrsw)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#  endif
!
!  Write out reduction factor for near-IR fraction of SW under ice.
!
      IF (Hout(idIrfac,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIrfac),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICE(ng)%rfaci)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIrfac)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out downward shortwave radiation.
!
      IF (Hout(idIqswl,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIqswl),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%qswl)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqswl)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out downward longwave radiation.
!
      IF (Hout(idIqlwl,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIqlwl),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%qlwl)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqlwl)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out latent heat over ice.
!
      IF (Hout(idIqlai,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIqlai),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%qlai)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqlai)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out sensible heat over ice.
!
      IF (Hout(idIqsei,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIqsei),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%qsei)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqsei)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out upward longwave over ice.
!
      IF (Hout(idIqlwa,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIqlwa),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%qlwa)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqlwa)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out upward shortwave over ice.
!
      IF (Hout(idIqswa,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIqswa),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%qswa)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqswa)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out shortwave into ice.
!
      IF (Hout(idIqswi,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIqswi),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%qswi)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqswi)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out shortwave through ice.
!
      IF (Hout(idIqswo,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIqswo),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%qswo)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqswo)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out salt flux under ice.
!
      IF (Hout(idIsio,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIsio),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%sio)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsio)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out salt flux through leads.
!
      IF (Hout(idIsao,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIsao),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%sao)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsao)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out heat flux through leads.
!
      IF (Hout(idIqao,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIqao),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%qao)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqao)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out heat flux of ice-air.
!
      IF (Hout(idIqai,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIqai),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%qai)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqai)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out heat flux of ice-ocean.
!
      IF (Hout(idIqio,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIqio),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%qio)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqio)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice heat flux.
!
      IF (Hout(idIqi2,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIqi2),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%qi2)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIqi2)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out snow-ice conversion.
!
      IF (Hout(idIwsni,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIwsni),                        &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng)%wsni)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIwsni)), HIS(ng)%Rindex
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
      IF (Hout(idIsMr,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIsMr),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     ICESHELFVAR(ng)%m)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsMr)), HIS(ng)%Rindex
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
      IF (Hout(idIsTs,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIsTs),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICESHELFVAR(ng)%Tstar)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsTs)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out iceshelf frictional velocity.
!
      IF (Hout(idIsUs,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIsUs),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICESHELFVAR(ng)%Ustar)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsUs)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out iceshelf molecular sublayer temperature.
!
      IF (Hout(idIsTb,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIsTb),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICESHELFVAR(ng)%Tb)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsTb)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out iceshelf molecular sublayer salinity.
!
      IF (Hout(idIsSb,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIsSb),                         &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICESHELFVAR(ng)%Sb)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsSb)), HIS(ng)%Rindex
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
      IF (Hout(idIsDrft,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIsDrft),                       &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     GRID(ng)%zice)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsDrft)), HIS(ng)%Rindex
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
      IF (Hout(idIsSOFs,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIsSOFs),                       &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICESHELFVAR(ng)%SOFs)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsSOFs)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface heat overflux.
!
      IF (Hout(idIsSOFh,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idIsSOFh),                       &
     &                     HIS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICESHELFVAR(ng)%SOFh)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIsSOFh)), HIS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
#endif

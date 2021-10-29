#  ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
!  Write out 2D ice momentum in the XI-direction.
!
      IF (Sout(idIuice,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIuice, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%ui(:,:,IOUT),                &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIuice)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIuice)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out 2D ice momentum in the ETA-direction.
!
      IF (Sout(idIvice,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIvice, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%vi(:,:,IOUT),                &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIvice)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIvice)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out 2D Eastward and Northward ice momentum components (m/s) at
!  RHO-points.
!
      IF (Sout(idIuiceE,ng).and.Sout(idIviceN,ng)) THEN
        IF (.not.allocated(Ur2d)) THEN
          allocate (Ur2d(LBi:UBi,LBj:UBj))
            Ur2d(LBi:UBi,LBj:UBj)=0.0_r8
        END IF
        IF (.not.allocated(Vr2d)) THEN
          allocate (Vr2d(LBi:UBi,LBj:UBj))
            Vr2d(LBi:UBi,LBj:UBj)=0.0_r8
        END IF
#   ifdef DISTRIBUTE
        tile=MyRank
#   else
        tile=-1
#   endif
        CALL uv_rotate2d (ng, TILE, .FALSE., .TRUE.,                    &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    GRID(ng) % CosAngler,                         &
     &                    GRID(ng) % SinAngler,                         &
#   ifdef MASKING
     &                    GRID(ng) % rmask_full,                        &
#   endif
     &                    ICE(ng) % ui(:,:,IUOUT),                      &
     &                    ICE(ng) % vi(:,:,IUOUT),                      &
     &                    Ur2d, Vr2d)

        scale=1.0_r8
        CALL extract_sta2d (ng, iNLM, Cgrid, idIuiceE, r2dvar,          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, Ur2d,                                &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIuiceE)), psta,        &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIuiceE)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

        CALL extract_sta2d (ng, iNLM, Cgrid, idIviceN, r2dvar,          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, Vr2d,                                &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIviceN)), psta,        &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIviceN)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

        deallocate (Ur2d)
        deallocate (Vr2d)
      END IF
!
!  Write out ice concentration.
!
      IF (Sout(idIaice,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIaice, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%ai(:,:,IOUT),                &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIaice)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIaice)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out ice average thickness.
!
      IF (Sout(idIhice,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIhice, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%hi(:,:,IOUT),                &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIhice)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIhice)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out snow thickness.
!
      IF (Sout(idIhsno,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIhsno, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%hsn(:,:,IOUT),               &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIhsno)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIhsno)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out ice age.
!
      IF (Sout(idIagei,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIagei, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%ageice(:,:,IOUT),            &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIagei)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIagei)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out temperature of ice/snow surface.
!
      IF (Sout(idItice,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idItice, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%tis,                         &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idItice)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idItice)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out temperature of internal ice.
!
      IF (Sout(idItmid,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idItmid, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%ti(:,:,IOUT),                &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idItmid)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idItmid)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out temperature of top ice.
!
      IF (Sout(idIt2ic,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIt2ic, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%t2,                          &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIt2ic)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIt2ic)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out internal ice stress component 11.
!
      IF (Sout(idIsg11,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsg11, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%sig11(:,:,IOUT),             &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsg11)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsg11)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out internal ice stress component 12.
!
      IF (Sout(idIsg12,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsg12, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%sig12(:,:,IOUT),             &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsg12)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsg12)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out internal ice stress component 22.
!
      IF (Sout(idIsg22,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsg22, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%sig22(:,:,IOUT),             &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsg22)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsg22)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out ice-water friction velocity.
!
      IF (Sout(idIutau,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIutau, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%utau_iw,                     &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIutau)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIutau)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out ice-water momentum transfer coefficient.
!
      IF (Sout(idImchu,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idImchu, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%chu_iw,                      &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idImchu)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idImchu)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out temperature of molecular sublayer under ice.
!
      IF (Sout(idIt0mk,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIt0mk, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%t0mk,                        &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIt0mk)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIt0mk)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out salinity of molecular sublayer under ice.
!
      IF (Sout(idIs0mk,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIs0mk, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%s0mk,                        &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIs0mk)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIs0mk)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out ice-ocean mass flux.
!
      IF (Sout(idImflx,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idImflx, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%iomflx,                      &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idImflx)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idImflx)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out ice frazil growth rate.
!
      IF (Sout(idIwfr,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIwfr, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%wfr,                         &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIwfr)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIwfr)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out ice growth/melt rate of ice-air.
!
      IF (Sout(idIwai,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIwai, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%wai,                         &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIwai)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIwai)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out ice growth/melt rate of leads.
!
      IF (Sout(idIwao,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIwao, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%wao,                         &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIwao)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIwao)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out ice growth/melt rate of ice-ocean.
!
      IF (Sout(idIwio,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIwio, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%wio,                         &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIwio)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIwio)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out ice melt runoff rate.
!
      IF (Sout(idIwro,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIwro, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%wro,                         &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIwro)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIwro)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out ice divergence rate.
!
      IF (Sout(idIwdiv,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIwdiv, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%wdiv,                        &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIwdiv)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIwdiv)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
#   ifdef ICE_DIAGS
#    ifdef ICE_ALBEDO
!
!  Write out ocean albedo.
!
      IF (Sout(idIalbe,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIalbe, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%alb,                      &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIalbe)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIalbe)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out ice albedo.
!
      IF (Sout(idIalbI,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIalbI, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%alb_i,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIalbI)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIalbI)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
#     ifdef ICE_ALB_CSIM
!
!  Write out fraction of shortwave penetrating into the ice.
!
      IF (Sout(idIfrsw,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIfrsw, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%frswp,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIfrsw)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIfrsw)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
#     endif
#    endif
!
!  Write out reduction factor for near-IR fraction of SW under ice.
!
      IF (Sout(idIrfac,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIrfac, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICE(ng)%rfaci,                       &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIrfac)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIrfac)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out downward longwave radiation.
!
      IF (Sout(idIlrdn,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIlrdn, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%lr_dn,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIlrdn)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIlrdn)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out downward shortwave radiation.
!
      IF (Sout(idIsrdn,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsrdn, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%sr_dn,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsrdn)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsrdn)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out latent heat over ice.
!
      IF (Sout(idIlatI,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIlatI, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%lat_i,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIlatI)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIlatI)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out sensible heat over ice.
!
      IF (Sout(idIsenI,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsenI, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%sen_i,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsenI)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsenI)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out upward longwave over ice.
!
      IF (Sout(idIlrup,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIlrup, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%lr_up_i,                  &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIlrup)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIlrup)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out upward shortwave over ice.
!
      IF (Sout(idIsrup,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsrup, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%sr_up_i,                  &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsrup)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsrup)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out shortwave into ice.
!
      IF (Sout(idIsrin,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsrin, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%sr_in_i,                  &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsrin)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsrin)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out shortwave through ice.
!
      IF (Sout(idIsrth,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsrth, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%sr_th_i,                  &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsrth)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsrth)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out salt flux under ice.
!
      IF (Sout(idIsfI,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsfI, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%ssflx_i,                  &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsfI)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsfI)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out salt flux through leads.
!
      IF (Sout(idIsfL,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsfL, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%ssflx_l,                  &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsfL)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsfL)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out heat flux through leads.
!
      IF (Sout(idIqao,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIqao, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%qao_n,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIqao)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIqao)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out heat flux of ice-air.
!
      IF (Sout(idIqai,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIqai, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%qai_n,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIqai)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIqai)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out heat flux of ice-ocean.
!
      IF (Sout(idIqio,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIqio, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%qio_n,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIqio)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIqio)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out ice heat flux.
!
      IF (Sout(idIqi2,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIqi2, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%qi2_n,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIqi2)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIqi2)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out snow-ice conversion.
!
      IF (Sout(idIsnoi,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsnoi, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%snoice,                   &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsnoi)), psta,         &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsnoi)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
#   endif
#  endif
#  ifdef ICESHELF
!
!  Write out iceshelf melt rate.
!
      IF (Sout(idIsMr,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsMr, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICESHELFVAR(ng)%m,                   &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsMr)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsMr)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
#   ifdef ICESHELF_3EQN_VBC
!
!  Write out iceshelf frictional temperature.
!
      IF (Sout(idIsTs,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsTs, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICESHELFVAR(ng)%Tstar,               &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsTs)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsTs)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out iceshelf frictional velocity.
!
      IF (Sout(idIsUs,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsUs, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICESHELFVAR(ng)%Ustar,               &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsUs)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsUs)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out iceshelf molecular sublayer temperature.
!
      IF (Sout(idIsTb,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsTb, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICESHELFVAR(ng)%Tb,                  &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsTb)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsTb)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out iceshelf molecular sublayer salinity.
!
      IF (Sout(idIsSb,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsSb, r2dvar,            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICESHELFVAR(ng)%Sb,                  &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsSb)), psta,          &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsSb)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
#   endif
#   ifdef ICESHELF_MORPH
!
!  Write out time-varying ice draft.
!
      IF (Sout(idIsDrft,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsDrft, r2dvar,          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, GRID(ng)%zice,                       &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsDrft)), psta,        &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsDrft)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
#   endif
#   ifdef ICESHELF_SOF
!
!  Write out surface salt overflux.
!
      IF (Sout(idIsSOFs,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsSOFs, r2dvar,          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICESHELFVAR(ng)%SOFs,                &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsSOFs)), psta,        &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsSOFs)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
!
!  Write out surface heat overflux.
!
      IF (Sout(idIsSOFh,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, iNLM, Cgrid, idIsSOFh, r2dvar,          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, ICESHELFVAR(ng)%SOFh,                &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL pio_netcdf_put_fvar (ng, iNLM, STA(ng)%name,               &
     &                            TRIM(Vname(1,idIsSOFh)), psta,        &
     &                            (/1,STA(ng)%Rindex/),                 &
     &                            (/Nstation(ng),1/),                   &
     &                            pioFile = STA(ng)%pioFile,            &
     &                            pioVar = STA(ng)%pioVar(idIsSOFh)%vd)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
#   endif
#  endif

#  ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
!  Define 2D ice momentum in the XI-direction.
!
        IF (Aout(idIuice,ng)) THEN
          Vinfo( 1)=Vname(1,idIuice)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIuice))
          Vinfo( 3)=Vname(3,idIuice)
          Vinfo(14)=Vname(4,idIuice)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_u'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIuice,ng),r8)
          AVG(ng)%pioVar(idIuice)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIuice)%gtype=u2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIuice)%vd,                    &
     &                   PIO_FOUT, nvd3, u2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define 2D ice momentum in the ETA-direction.
!
        IF (Aout(idIvice,ng)) THEN
          Vinfo( 1)=Vname(1,idIvice)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIvice))
          Vinfo( 3)=Vname(3,idIvice)
          Vinfo(14)=Vname(4,idIvice)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_v'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIvice,ng),r8)
          AVG(ng)%pioVar(idIvice)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIvice)%gtype=v2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIvice)%vd,                    &
     &                   PIO_FOUT, nvd3, v2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define 2D ice momentum in the East-direction.
!
        IF (Aout(idIuiceE,ng)) THEN
          Vinfo( 1)=Vname(1,idIuiceE)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIuiceE))
          Vinfo( 3)=Vname(3,idIuiceE)
          Vinfo(14)=Vname(4,idIuiceE)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIuiceE,ng),r8)
          AVG(ng)%pioVar(idIuiceE)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIuiceE)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIuiceE)%vd,                   &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define 2D ice momentum in the North-direction.
!
        IF (Aout(idIviceN,ng)) THEN
          Vinfo( 1)=Vname(1,idIviceN)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIviceN))
          Vinfo( 3)=Vname(3,idIviceN)
          Vinfo(14)=Vname(4,idIviceN)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIviceN,ng),r8)
          AVG(ng)%pioVar(idIviceN)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIviceN)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIviceN)%vd,                   &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice concentration.
!
        IF (Aout(idIaice,ng)) THEN
          Vinfo( 1)=Vname(1,idIaice)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIaice))
          Vinfo( 3)=Vname(3,idIaice)
          Vinfo(14)=Vname(4,idIaice)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIaice,ng),r8)
          AVG(ng)%pioVar(idIaice)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIaice)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIaice)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice average thickness.
!
        IF (Aout(idIhice,ng)) THEN
          Vinfo( 1)=Vname(1,idIhice)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIhice))
          Vinfo( 3)=Vname(3,idIhice)
          Vinfo(14)=Vname(4,idIhice)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIhice,ng),r8)
          AVG(ng)%pioVar(idIhice)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIhice)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIhice)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define snow thickness.
!
        IF (Aout(idIhsno,ng)) THEN
          Vinfo( 1)=Vname(1,idIhsno)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIhsno))
          Vinfo( 3)=Vname(3,idIhsno)
          Vinfo(14)=Vname(4,idIhsno)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIhsno,ng),r8)
          AVG(ng)%pioVar(idIhsno)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIhsno)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIhsno)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice age.
!
        IF (Aout(idIagei,ng)) THEN
          Vinfo( 1)=Vname(1,idIagei)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIagei))
          Vinfo( 3)=Vname(3,idIagei)
          Vinfo(14)=Vname(4,idIagei)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIagei,ng),r8)
          AVG(ng)%pioVar(idIagei)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIagei)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIagei)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define temperature of ice/snow surface.
!
        IF (Aout(idItice,ng)) THEN
          Vinfo( 1)=Vname(1,idItice)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idItice))
          Vinfo( 3)=Vname(3,idItice)
          Vinfo(14)=Vname(4,idItice)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idItice,ng),r8)
          AVG(ng)%pioVar(idItice)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idItice)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idItice)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define temperature of internal ice.
!
        IF (Aout(idItmid,ng)) THEN
          Vinfo( 1)=Vname(1,idItmid)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idItmid))
          Vinfo( 3)=Vname(3,idItmid)
          Vinfo(14)=Vname(4,idItmid)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idItmid,ng),r8)
          AVG(ng)%pioVar(idItmid)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idItmid)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idItmid)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define temperature of top ice.
!
        IF (Aout(idIt2ic,ng)) THEN
          Vinfo( 1)=Vname(1,idIt2ic)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIt2ic))
          Vinfo( 3)=Vname(3,idIt2ic)
          Vinfo(14)=Vname(4,idIt2ic)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIt2ic,ng),r8)
          AVG(ng)%pioVar(idIt2ic)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIt2ic)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIt2ic)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define internal ice stress component 11.
!
        IF (Aout(idIsg11,ng)) THEN
          Vinfo( 1)=Vname(1,idIsg11)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsg11))
          Vinfo( 3)=Vname(3,idIsg11)
          Vinfo(14)=Vname(4,idIsg11)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsg11,ng),r8)
          AVG(ng)%pioVar(idIsg11)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsg11)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsg11)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define internal ice stress component 12.
!
        IF (Aout(idIsg12,ng)) THEN
          Vinfo( 1)=Vname(1,idIsg12)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsg12))
          Vinfo( 3)=Vname(3,idIsg12)
          Vinfo(14)=Vname(4,idIsg12)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsg12,ng),r8)
          AVG(ng)%pioVar(idIsg12)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsg12)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsg12)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define internal ice stress component 22.
!
        IF (Aout(idIsg22,ng)) THEN
          Vinfo( 1)=Vname(1,idIsg22)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsg22))
          Vinfo( 3)=Vname(3,idIsg22)
          Vinfo(14)=Vname(4,idIsg22)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsg22,ng),r8)
          AVG(ng)%pioVar(idIsg22)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsg22)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsg22)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice-water friction velocity.
!
        IF (Aout(idIutau,ng)) THEN
          Vinfo( 1)=Vname(1,idIutau)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIutau))
          Vinfo( 3)=Vname(3,idIutau)
          Vinfo(14)=Vname(4,idIutau)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIutau,ng),r8)
          AVG(ng)%pioVar(idIutau)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIutau)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIutau)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice-water momentum transfer coefficient.
!
        IF (Aout(idImchu,ng)) THEN
          Vinfo( 1)=Vname(1,idImchu)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idImchu))
          Vinfo( 3)=Vname(3,idImchu)
          Vinfo(14)=Vname(4,idImchu)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idImchu,ng),r8)
          AVG(ng)%pioVar(idImchu)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idImchu)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idImchu)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define temperature of molecular sublayer under ice.
!
        IF (Aout(idIt0mk,ng)) THEN
          Vinfo( 1)=Vname(1,idIt0mk)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIt0mk))
          Vinfo( 3)=Vname(3,idIt0mk)
          Vinfo(14)=Vname(4,idIt0mk)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIt0mk,ng),r8)
          AVG(ng)%pioVar(idIt0mk)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIt0mk)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIt0mk)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define salinity of molecular sublayer under ice.
!
        IF (Aout(idIs0mk,ng)) THEN
          Vinfo( 1)=Vname(1,idIs0mk)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIs0mk))
          Vinfo( 3)=Vname(3,idIs0mk)
          Vinfo(14)=Vname(4,idIs0mk)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIs0mk,ng),r8)
          AVG(ng)%pioVar(idIs0mk)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIs0mk)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIs0mk)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice-ocean mass flux.
!
        IF (Aout(idImflx,ng)) THEN
          Vinfo( 1)=Vname(1,idImflx)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idImflx))
          Vinfo( 3)=Vname(3,idImflx)
          Vinfo(14)=Vname(4,idImflx)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idImflx,ng),r8)
          AVG(ng)%pioVar(idImflx)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idImflx)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idImflx)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice frazil growth rate.
!
        IF (Aout(idIwfr,ng)) THEN
          Vinfo( 1)=Vname(1,idIwfr)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIwfr))
          Vinfo( 3)=Vname(3,idIwfr)
          Vinfo(14)=Vname(4,idIwfr)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIwfr,ng),r8)
          AVG(ng)%pioVar(idIwfr)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIwfr)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIwfr)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice groth/melt rate of ice-air.
!
        IF (Aout(idIwai,ng)) THEN
          Vinfo( 1)=Vname(1,idIwai)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIwai))
          Vinfo( 3)=Vname(3,idIwai)
          Vinfo(14)=Vname(4,idIwai)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIwai,ng),r8)
          AVG(ng)%pioVar(idIwai)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIwai)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIwai)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice groth/melt rate of leads.
!
        IF (Aout(idIwao,ng)) THEN
          Vinfo( 1)=Vname(1,idIwao)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIwao))
          Vinfo( 3)=Vname(3,idIwao)
          Vinfo(14)=Vname(4,idIwao)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIwao,ng),r8)
          AVG(ng)%pioVar(idIwao)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIwao)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIwao)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice groth/melt rate of ice-ocean.
!
        IF (Aout(idIwio,ng)) THEN
          Vinfo( 1)=Vname(1,idIwio)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIwio))
          Vinfo( 3)=Vname(3,idIwio)
          Vinfo(14)=Vname(4,idIwio)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIwio,ng),r8)
          AVG(ng)%pioVar(idIwio)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIwio)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIwio)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice melt runoff rate.
!
        IF (Aout(idIwro,ng)) THEN
          Vinfo( 1)=Vname(1,idIwro)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIwro))
          Vinfo( 3)=Vname(3,idIwro)
          Vinfo(14)=Vname(4,idIwro)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIwro,ng),r8)
          AVG(ng)%pioVar(idIwro)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIwro)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIwro)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice divergence rate.
!
        IF (Aout(idIwdiv,ng)) THEN
          Vinfo( 1)=Vname(1,idIwdiv)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIwdiv))
          Vinfo( 3)=Vname(3,idIwdiv)
          Vinfo(14)=Vname(4,idIwdiv)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIwdiv,ng),r8)
          AVG(ng)%pioVar(idIwdiv)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIwdiv)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIwdiv)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#   ifdef ICE_DIAGS
#    ifdef ICE_ALBEDO
!
!  Define ocean albedo.
!
        IF (Aout(idIalbe,ng)) THEN
          Vinfo( 1)=Vname(1,idIalbe)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIalbe))
          Vinfo( 3)=Vname(3,idIalbe)
          Vinfo(14)=Vname(4,idIalbe)
          Vinfo(16)=Vname(1,idtime)
#     if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#     endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIalbe,ng),r8)
          AVG(ng)%pioVar(idIalbe)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIalbe)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIalbe)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice albedo.
!
        IF (Aout(idIalbI,ng)) THEN
          Vinfo( 1)=Vname(1,idIalbI)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIalbI))
          Vinfo( 3)=Vname(3,idIalbI)
          Vinfo(14)=Vname(4,idIalbI)
          Vinfo(16)=Vname(1,idtime)
#     if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#     endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIalbI,ng),r8)
          AVG(ng)%pioVar(idIalbI)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIalbI)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIalbI)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#     ifdef ICE_ALB_CSIM
!
!  Define fraction of shortwave penetrating into the ice.
!
        IF (Aout(idIfrsw,ng)) THEN
          Vinfo( 1)=Vname(1,idIfrsw)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIfrsw))
          Vinfo( 3)=Vname(3,idIfrsw)
          Vinfo(14)=Vname(4,idIfrsw)
          Vinfo(16)=Vname(1,idtime)
#      if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#      endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIfrsw,ng),r8)
          AVG(ng)%pioVar(idIfrsw)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIfrsw)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIfrsw)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#     endif
#    endif
!
!  Define reduction factor for near-IR fraction of SW under ice.
!
        IF (Aout(idIrfac,ng)) THEN
          Vinfo( 1)=Vname(1,idIrfac)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIrfac))
          Vinfo( 3)=Vname(3,idIrfac)
          Vinfo(14)=Vname(4,idIrfac)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIrfac,ng),r8)
          AVG(ng)%pioVar(idIrfac)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIrfac)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIrfac)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define downward longwave radiation.
!
        IF (Aout(idIlrdn,ng)) THEN
          Vinfo( 1)=Vname(1,idIlrdn)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIlrdn))
          Vinfo( 3)=Vname(3,idIlrdn)
          Vinfo(14)=Vname(4,idIlrdn)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIlrdn,ng),r8)
          AVG(ng)%pioVar(idIlrdn)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIlrdn)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIlrdn)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define downward shortwave radiation.
!
        IF (Aout(idIsrdn,ng)) THEN
          Vinfo( 1)=Vname(1,idIsrdn)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsrdn))
          Vinfo( 3)=Vname(3,idIsrdn)
          Vinfo(14)=Vname(4,idIsrdn)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsrdn,ng),r8)
          AVG(ng)%pioVar(idIsrdn)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsrdn)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsrdn)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define latent heat over ice.
!
        IF (Aout(idIlatI,ng)) THEN
          Vinfo( 1)=Vname(1,idIlatI)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIlatI))
          Vinfo( 3)=Vname(3,idIlatI)
          Vinfo(14)=Vname(4,idIlatI)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIlatI,ng),r8)
          AVG(ng)%pioVar(idIlatI)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIlatI)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIlatI)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define sensible heat over ice.
!
        IF (Aout(idIsenI,ng)) THEN
          Vinfo( 1)=Vname(1,idIsenI)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsenI))
          Vinfo( 3)=Vname(3,idIsenI)
          Vinfo(14)=Vname(4,idIsenI)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsenI,ng),r8)
          AVG(ng)%pioVar(idIsenI)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsenI)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsenI)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define upward longwave over ice.
!
        IF (Aout(idIlrup,ng)) THEN
          Vinfo( 1)=Vname(1,idIlrup)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIlrup))
          Vinfo( 3)=Vname(3,idIlrup)
          Vinfo(14)=Vname(4,idIlrup)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIlrup,ng),r8)
          AVG(ng)%pioVar(idIlrup)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIlrup)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIlrup)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define upward shortwave over ice.
!
        IF (Aout(idIsrup,ng)) THEN
          Vinfo( 1)=Vname(1,idIsrup)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsrup))
          Vinfo( 3)=Vname(3,idIsrup)
          Vinfo(14)=Vname(4,idIsrup)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsrup,ng),r8)
          AVG(ng)%pioVar(idIsrup)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsrup)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsrup)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define shortwave into ice.
!
        IF (Aout(idIsrin,ng)) THEN
          Vinfo( 1)=Vname(1,idIsrin)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsrin))
          Vinfo( 3)=Vname(3,idIsrin)
          Vinfo(14)=Vname(4,idIsrin)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsrin,ng),r8)
          AVG(ng)%pioVar(idIsrin)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsrin)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsrin)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define shortwave through ice.
!
        IF (Aout(idIsrth,ng)) THEN
          Vinfo( 1)=Vname(1,idIsrth)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsrth))
          Vinfo( 3)=Vname(3,idIsrth)
          Vinfo(14)=Vname(4,idIsrth)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsrth,ng),r8)
          AVG(ng)%pioVar(idIsrth)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsrth)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsrth)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define salt flux under ice.
!
        IF (Aout(idIsfI,ng)) THEN
          Vinfo( 1)=Vname(1,idIsfI)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsfI))
          Vinfo( 3)=Vname(3,idIsfI)
          Vinfo(14)=Vname(4,idIsfI)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsfI,ng),r8)
          AVG(ng)%pioVar(idIsfI)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsfI)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsfI)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define salt flux through leads.
!
        IF (Aout(idIsfL,ng)) THEN
          Vinfo( 1)=Vname(1,idIsfL)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsfL))
          Vinfo( 3)=Vname(3,idIsfL)
          Vinfo(14)=Vname(4,idIsfL)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsfL,ng),r8)
          AVG(ng)%pioVar(idIsfL)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsfL)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsfL)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux through leads.
!
        IF (Aout(idIqao,ng)) THEN
          Vinfo( 1)=Vname(1,idIqao)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIqao))
          Vinfo( 3)=Vname(3,idIqao)
          Vinfo(14)=Vname(4,idIqao)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqao,ng),r8)
          AVG(ng)%pioVar(idIqao)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIqao)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIqao)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux of ice-air.
!
        IF (Aout(idIqai,ng)) THEN
          Vinfo( 1)=Vname(1,idIqai)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIqai))
          Vinfo( 3)=Vname(3,idIqai)
          Vinfo(14)=Vname(4,idIqai)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqai,ng),r8)
          AVG(ng)%pioVar(idIqai)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIqai)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIqai)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux of ice-ocean.
!
        IF (Aout(idIqio,ng)) THEN
          Vinfo( 1)=Vname(1,idIqio)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIqio))
          Vinfo( 3)=Vname(3,idIqio)
          Vinfo(14)=Vname(4,idIqio)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqio,ng),r8)
          AVG(ng)%pioVar(idIqio)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIqio)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIqio)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice heat flux.
!
        IF (Aout(idIqi2,ng)) THEN
          Vinfo( 1)=Vname(1,idIqi2)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIqi2))
          Vinfo( 3)=Vname(3,idIqi2)
          Vinfo(14)=Vname(4,idIqi2)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqi2,ng),r8)
          AVG(ng)%pioVar(idIqi2)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIqi2)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIqi2)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define snow-ice conversion.
!
        IF (Aout(idIsnoi,ng)) THEN
          Vinfo( 1)=Vname(1,idIsnoi)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsnoi))
          Vinfo( 3)=Vname(3,idIsnoi)
          Vinfo(14)=Vname(4,idIsnoi)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsnoi,ng),r8)
          AVG(ng)%pioVar(idIsnoi)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsnoi)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsnoi)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#   endif
#  endif
#  ifdef ICESHELF
!
!  Define iceshelf melt rate.
!
        IF (Aout(idIsMr,ng)) THEN
          Vinfo( 1)=Vname(1,idIsMr)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsMr))
          Vinfo( 3)=Vname(3,idIsMr)
          Vinfo(14)=Vname(4,idIsMr)
          Vinfo(16)=Vname(1,idtime)
#   if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsMr,ng),r8)
          AVG(ng)%pioVar(idIsMr)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsMr)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsMr)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#   ifdef ICESHELF_3EQN_VBC
!
!  Define iceshelf frictional temperature.
!
        IF (Aout(idIsTs,ng)) THEN
          Vinfo( 1)=Vname(1,idIsTs)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsTs))
          Vinfo( 3)=Vname(3,idIsTs)
          Vinfo(14)=Vname(4,idIsTs)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsTs,ng),r8)
          AVG(ng)%pioVar(idIsTs)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsTs)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsTs)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define iceshelf frictional velocity.
!
        IF (Aout(idIsUs,ng)) THEN
          Vinfo( 1)=Vname(1,idIsUs)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsUs))
          Vinfo( 3)=Vname(3,idIsUs)
          Vinfo(14)=Vname(4,idIsUs)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsUs,ng),r8)
          AVG(ng)%pioVar(idIsUs)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsUs)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsUs)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define iceshelf molecular sublayer temperature.
!
        IF (Aout(idIsTb,ng)) THEN
          Vinfo( 1)=Vname(1,idIsTb)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsTb))
          Vinfo( 3)=Vname(3,idIsTb)
          Vinfo(14)=Vname(4,idIsTb)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsTb,ng),r8)
          AVG(ng)%pioVar(idIsTb)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsTb)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsTb)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define iceshelf molecular sublayer salinity.
!
        IF (Aout(idIsSb,ng)) THEN
          Vinfo( 1)=Vname(1,idIsSb)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsSb))
          Vinfo( 3)=Vname(3,idIsSb)
          Vinfo(14)=Vname(4,idIsSb)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsSb,ng),r8)
          AVG(ng)%pioVar(idIsSb)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsSb)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsSb)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#   endif
#   ifdef ICESHELF_MORPH
!
!  Define time-varying ice draft.
!
        IF (Aout(idIsDrft,ng)) THEN
          Vinfo( 1)=Vname(1,idIsDrft)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsDrft))
          Vinfo( 3)=Vname(3,idIsDrft)
          Vinfo(14)=Vname(4,idIsDrft)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsDrft,ng),r8)
          AVG(ng)%pioVar(idIsDrft)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsDrft)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsDrft)%vd,                   &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#   endif
#   ifdef ICESHELF_SOF
!
!  Define surface salt overflux.
!
        IF (Aout(idIsSOFs,ng)) THEN
          Vinfo( 1)=Vname(1,idIsSOFs)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsSOFs))
          Vinfo( 3)=Vname(3,idIsSOFs)
          Vinfo(14)=Vname(4,idIsSOFs)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsSOFs,ng),r8)
          AVG(ng)%pioVar(idIsSOFs)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsSOFs)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsSOFs)%vd,                   &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define surface heat overflux.
!
        IF (Aout(idIsSOFh,ng)) THEN
          Vinfo( 1)=Vname(1,idIsSOFh)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsSOFh))
          Vinfo( 3)=Vname(3,idIsSOFh)
          Vinfo(14)=Vname(4,idIsSOFh)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsSOFh,ng),r8)
          AVG(ng)%pioVar(idIsSOFh)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsSOFh)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsSOFh)%vd,                   &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#   endif
#  endif

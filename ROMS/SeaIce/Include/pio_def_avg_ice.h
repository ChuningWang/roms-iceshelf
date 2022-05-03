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
          Vinfo(21)=Vname(6,idIuice)
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
          Vinfo(21)=Vname(6,idIvice)
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
          Vinfo(21)=Vname(6,idIuiceE)
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
          Vinfo(21)=Vname(6,idIviceN)
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
          Vinfo(21)=Vname(6,idIaice)
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
          Vinfo(21)=Vname(6,idIhice)
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
          Vinfo(21)=Vname(6,idIhsno)
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
          Vinfo(21)=Vname(6,idIagei)
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
          Vinfo(21)=Vname(6,idItice)
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
          Vinfo(21)=Vname(6,idItmid)
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
          Vinfo(21)=Vname(6,idIt2ic)
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
          Vinfo(21)=Vname(6,idIsg11)
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
          Vinfo(21)=Vname(6,idIsg12)
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
          Vinfo(21)=Vname(6,idIsg22)
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
          Vinfo(21)=Vname(6,idIutau)
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
          Vinfo(21)=Vname(6,idImchu)
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
          Vinfo(21)=Vname(6,idIt0mk)
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
          Vinfo(21)=Vname(6,idIs0mk)
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
          Vinfo(21)=Vname(6,idImflx)
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
          Vinfo(21)=Vname(6,idIwfr)
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
!  Define ice growth/melt rate of ice-air.
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
          Vinfo(21)=Vname(6,idIwai)
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
!  Define ice growth/melt rate of leads.
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
          Vinfo(21)=Vname(6,idIwao)
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
!  Define ice growth/melt rate of ice-ocean.
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
          Vinfo(21)=Vname(6,idIwio)
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
          Vinfo(21)=Vname(6,idIwro)
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
          Vinfo(21)=Vname(6,idIwdiv)
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
        IF (Aout(idIalbO,ng)) THEN
          Vinfo( 1)=Vname(1,idIalbO)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIalbO))
          Vinfo( 3)=Vname(3,idIalbO)
          Vinfo(14)=Vname(4,idIalbO)
          Vinfo(16)=Vname(1,idtime)
#     if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#     endif
          Vinfo(21)=Vname(6,idIalbO)
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIalbO,ng),r8)
          AVG(ng)%pioVar(idIalbO)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIalbO)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIalbO)%vd,                    &
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
          Vinfo(21)=Vname(6,idIalbI)
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
          Vinfo(21)=Vname(6,idIfrsw)
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
!  Define shortwave radiation into ice.
!
        IF (Aout(idIqswi,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswi)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIqswi))
          Vinfo( 3)=Vname(3,idIqswi)
          Vinfo(14)=Vname(4,idIqswi)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(21)=Vname(6,idIqswi)
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqswi,ng),r8)
          AVG(ng)%pioVar(idIqswi)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIqswi)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIqswi)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define longwave radiation into ice.
!
        IF (Aout(idIqlwi,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlwi)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIqlwi))
          Vinfo( 3)=Vname(3,idIqlwi)
          Vinfo(14)=Vname(4,idIqlwi)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(21)=Vname(6,idIqlwi)
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqlwi,ng),r8)
          AVG(ng)%pioVar(idIqlwi)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIqlwi)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIqlwi)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define latent heat flux into ice.
!
        IF (Aout(idIqlai,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlai)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIqlai))
          Vinfo( 3)=Vname(3,idIqlai)
          Vinfo(14)=Vname(4,idIqlai)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(21)=Vname(6,idIqlai)
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqlai,ng),r8)
          AVG(ng)%pioVar(idIqlai)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIqlai)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIqlai)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define sensible heat flux into ice.
!
        IF (Aout(idIqsei,ng)) THEN
          Vinfo( 1)=Vname(1,idIqsei)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIqsei))
          Vinfo( 3)=Vname(3,idIqsei)
          Vinfo(14)=Vname(4,idIqsei)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(21)=Vname(6,idIqsei)
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqsei,ng),r8)
          AVG(ng)%pioVar(idIqsei)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIqsei)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIqsei)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define downward shortwave radiation into leads.
!
        IF (Aout(idIqswl,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswl)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIqswl))
          Vinfo( 3)=Vname(3,idIqswl)
          Vinfo(14)=Vname(4,idIqswl)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(21)=Vname(6,idIqswl)
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqswl,ng),r8)
          AVG(ng)%pioVar(idIqswl)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIqswl)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIqswl)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define downward longwave radiation into leads.
!
        IF (Aout(idIqlwl,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlwl)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIqlwl))
          Vinfo( 3)=Vname(3,idIqlwl)
          Vinfo(14)=Vname(4,idIqlwl)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(21)=Vname(6,idIqlwl)
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqlwl,ng),r8)
          AVG(ng)%pioVar(idIqlwl)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIqlwl)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIqlwl)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define upward longwave into atmosphere.
!
        IF (Aout(idIqlwa,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlwa)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIqlwa))
          Vinfo( 3)=Vname(3,idIqlwa)
          Vinfo(14)=Vname(4,idIqlwa)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(21)=Vname(6,idIqlwa)
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqlwa,ng),r8)
          AVG(ng)%pioVar(idIqlwa)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIqlwa)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIqlwa)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define upward shortwave into atmosphere.
!
        IF (Aout(idIqswa,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswa)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIqswa))
          Vinfo( 3)=Vname(3,idIqswa)
          Vinfo(14)=Vname(4,idIqswa)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(21)=Vname(6,idIqswa)
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqswa,ng),r8)
          AVG(ng)%pioVar(idIqswa)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIqswa)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIqswa)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define shortwave radiation through ice.
!
        IF (Aout(idIqswo,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswo)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIqswo))
          Vinfo( 3)=Vname(3,idIqswo)
          Vinfo(14)=Vname(4,idIqswo)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(21)=Vname(6,idIqswo)
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqswo,ng),r8)
          AVG(ng)%pioVar(idIqswo)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIqswo)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIqswo)%vd,                    &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux, air-ocean.
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
          Vinfo(21)=Vname(6,idIqao)
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
!  Define heat flux, air-ice.
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
          Vinfo(21)=Vname(6,idIqai)
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
!  Define heat flux, ice-ocean.
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
          Vinfo(21)=Vname(6,idIqio)
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
!  Define heat flux, ice-snow.
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
          Vinfo(21)=Vname(6,idIqi2)
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
!  Define salt flux, air-ocean.
!
        IF (Aout(idIsao,ng)) THEN
          Vinfo( 1)=Vname(1,idIsao)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsao))
          Vinfo( 3)=Vname(3,idIsao)
          Vinfo(14)=Vname(4,idIsao)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(21)=Vname(6,idIsao)
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsao,ng),r8)
          AVG(ng)%pioVar(idIsao)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsao)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsao)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define salt flux, ice-ocean.
!
        IF (Aout(idIsio,ng)) THEN
          Vinfo( 1)=Vname(1,idIsio)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIsio))
          Vinfo( 3)=Vname(3,idIsio)
          Vinfo(14)=Vname(4,idIsio)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(21)=Vname(6,idIsio)
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsio,ng),r8)
          AVG(ng)%pioVar(idIsio)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIsio)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIsio)%vd,                     &
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define snow-ice conversion.
!
        IF (Aout(idIwsni,ng)) THEN
          Vinfo( 1)=Vname(1,idIwsni)
          WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,idIwsni))
          Vinfo( 3)=Vname(3,idIwsni)
          Vinfo(14)=Vname(4,idIwsni)
          Vinfo(16)=Vname(1,idtime)
#    if !defined WET_DRY && (defined WRITE_WATER && defined MASKING)
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(21)=Vname(6,idIwsni)
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIwsni,ng),r8)
          AVG(ng)%pioVar(idIwsni)%dkind=PIO_FOUT
          AVG(ng)%pioVar(idIwsni)%gtype=r2dvar
!
          status=def_var(ng, iNLM, AVG(ng)%pioFile,                     &
     &                   AVG(ng)%pioVar(idIwsni)%vd,                    &
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
          Vinfo(21)=Vname(6,idIsMr)
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
          Vinfo(21)=Vname(6,idIsTs)
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
          Vinfo(21)=Vname(6,idIsUs)
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
          Vinfo(21)=Vname(6,idIsTb)
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
          Vinfo(21)=Vname(6,idIsSb)
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
          Vinfo(21)=Vname(6,idIsDrft)
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
          Vinfo(21)=Vname(6,idIsSOFs)
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
          Vinfo(21)=Vname(6,idIsSOFh)
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

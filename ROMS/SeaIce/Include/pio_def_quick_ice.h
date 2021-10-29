# ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
!  Define 2D ice momentum in the XI-direction.
!
        IF (Qout(idIuice,ng)) THEN
          Vinfo( 1)=Vname(1,idIuice)
          Vinfo( 2)=Vname(2,idIuice)
          Vinfo( 3)=Vname(3,idIuice)
          Vinfo(14)=Vname(4,idIuice)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_u'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIuice,ng),r8)
          QCK(ng)%pioVar(idIuice)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIuice)%gtype=u2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIuice)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, u2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, u2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define 2D ice momentum in the ETA-direction.
!
        IF (Qout(idIvice,ng)) THEN
          Vinfo( 1)=Vname(1,idIvice)
          Vinfo( 2)=Vname(2,idIvice)
          Vinfo( 3)=Vname(3,idIvice)
          Vinfo(14)=Vname(4,idIvice)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_v'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIvice,ng),r8)
          QCK(ng)%pioVar(idIvice)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIvice)%gtype=v2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIvice)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, v2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, v2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define 2D ice momentum in the East-direction.
!
        IF (Qout(idIuiceE,ng)) THEN
          Vinfo( 1)=Vname(1,idIuiceE)
          Vinfo( 2)=Vname(2,idIuiceE)
          Vinfo( 3)=Vname(3,idIuiceE)
          Vinfo(14)=Vname(4,idIuiceE)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIuiceE,ng),r8)
          QCK(ng)%pioVar(idIuiceE)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIuiceE)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIuiceE)%vd,                   &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define 2D ice momentum in the North-direction.
!
        IF (Qout(idIviceN,ng)) THEN
          Vinfo( 1)=Vname(1,idIviceN)
          Vinfo( 2)=Vname(2,idIviceN)
          Vinfo( 3)=Vname(3,idIviceN)
          Vinfo(14)=Vname(4,idIviceN)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIviceN,ng),r8)
          QCK(ng)%pioVar(idIviceN)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIviceN)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIviceN)%vd,                   &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice concentration.
!
        IF (Qout(idIaice,ng)) THEN
          Vinfo( 1)=Vname(1,idIaice)
          Vinfo( 2)=Vname(2,idIaice)
          Vinfo( 3)=Vname(3,idIaice)
          Vinfo(14)=Vname(4,idIaice)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIaice,ng),r8)
          QCK(ng)%pioVar(idIaice)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIaice)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIaice)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice average thickness.
!
        IF (Qout(idIhice,ng)) THEN
          Vinfo( 1)=Vname(1,idIhice)
          Vinfo( 2)=Vname(2,idIhice)
          Vinfo( 3)=Vname(3,idIhice)
          Vinfo(14)=Vname(4,idIhice)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIhice,ng),r8)
          QCK(ng)%pioVar(idIhice)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIhice)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIhice)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define snow thickness.
!
        IF (Qout(idIhsno,ng)) THEN
          Vinfo( 1)=Vname(1,idIhsno)
          Vinfo( 2)=Vname(2,idIhsno)
          Vinfo( 3)=Vname(3,idIhsno)
          Vinfo(14)=Vname(4,idIhsno)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIhsno,ng),r8)
          QCK(ng)%pioVar(idIhsno)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIhsno)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIhsno)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice age.
!
        IF (Qout(idIagei,ng)) THEN
          Vinfo( 1)=Vname(1,idIagei)
          Vinfo( 2)=Vname(2,idIagei)
          Vinfo( 3)=Vname(3,idIagei)
          Vinfo(14)=Vname(4,idIagei)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIagei,ng),r8)
          QCK(ng)%pioVar(idIagei)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIagei)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIagei)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define temperature of ice/snow surface.
!
        IF (Qout(idItice,ng)) THEN
          Vinfo( 1)=Vname(1,idItice)
          Vinfo( 2)=Vname(2,idItice)
          Vinfo( 3)=Vname(3,idItice)
          Vinfo(14)=Vname(4,idItice)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idItice,ng),r8)
          QCK(ng)%pioVar(idItice)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idItice)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idItice)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define temperature of internal ice.
!
        IF (Qout(idItmid,ng)) THEN
          Vinfo( 1)=Vname(1,idItmid)
          Vinfo( 2)=Vname(2,idItmid)
          Vinfo( 3)=Vname(3,idItmid)
          Vinfo(14)=Vname(4,idItmid)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idItmid,ng),r8)
          QCK(ng)%pioVar(idItmid)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idItmid)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idItmid)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define temperature of top ice.
!
        IF (Qout(idIt2ic,ng)) THEN
          Vinfo( 1)=Vname(1,idIt2ic)
          Vinfo( 2)=Vname(2,idIt2ic)
          Vinfo( 3)=Vname(3,idIt2ic)
          Vinfo(14)=Vname(4,idIt2ic)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIt2ic,ng),r8)
          QCK(ng)%pioVar(idIt2ic)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIt2ic)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIt2ic)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define internal ice stress component 11.
!
        IF (Qout(idIsg11,ng)) THEN
          Vinfo( 1)=Vname(1,idIsg11)
          Vinfo( 2)=Vname(2,idIsg11)
          Vinfo( 3)=Vname(3,idIsg11)
          Vinfo(14)=Vname(4,idIsg11)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsg11,ng),r8)
          QCK(ng)%pioVar(idIsg11)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsg11)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsg11)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define internal ice stress component 12.
!
        IF (Qout(idIsg12,ng)) THEN
          Vinfo( 1)=Vname(1,idIsg12)
          Vinfo( 2)=Vname(2,idIsg12)
          Vinfo( 3)=Vname(3,idIsg12)
          Vinfo(14)=Vname(4,idIsg12)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsg12,ng),r8)
          QCK(ng)%pioVar(idIsg12)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsg12)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsg12)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define internal ice stress component 22.
!
        IF (Qout(idIsg22,ng)) THEN
          Vinfo( 1)=Vname(1,idIsg22)
          Vinfo( 2)=Vname(2,idIsg22)
          Vinfo( 3)=Vname(3,idIsg22)
          Vinfo(14)=Vname(4,idIsg22)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsg22,ng),r8)
          QCK(ng)%pioVar(idIsg22)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsg22)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsg22)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice-water friction velocity.
!
        IF (Qout(idIutau,ng)) THEN
          Vinfo( 1)=Vname(1,idIutau)
          Vinfo( 2)=Vname(2,idIutau)
          Vinfo( 3)=Vname(3,idIutau)
          Vinfo(14)=Vname(4,idIutau)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIutau,ng),r8)
          QCK(ng)%pioVar(idIutau)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIutau)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIutau)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice-water momentum transfer coefficient.
!
        IF (Qout(idImchu,ng)) THEN
          Vinfo( 1)=Vname(1,idImchu)
          Vinfo( 2)=Vname(2,idImchu)
          Vinfo( 3)=Vname(3,idImchu)
          Vinfo(14)=Vname(4,idImchu)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idImchu,ng),r8)
          QCK(ng)%pioVar(idImchu)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idImchu)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idImchu)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define temperature of molecular sublayer under ice.
!
        IF (Qout(idIt0mk,ng)) THEN
          Vinfo( 1)=Vname(1,idIt0mk)
          Vinfo( 2)=Vname(2,idIt0mk)
          Vinfo( 3)=Vname(3,idIt0mk)
          Vinfo(14)=Vname(4,idIt0mk)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIt0mk,ng),r8)
          QCK(ng)%pioVar(idIt0mk)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIt0mk)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIt0mk)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define salinity of molecular sublayer under ice.
!
        IF (Qout(idIs0mk,ng)) THEN
          Vinfo( 1)=Vname(1,idIs0mk)
          Vinfo( 2)=Vname(2,idIs0mk)
          Vinfo( 3)=Vname(3,idIs0mk)
          Vinfo(14)=Vname(4,idIs0mk)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIs0mk,ng),r8)
          QCK(ng)%pioVar(idIs0mk)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIs0mk)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIs0mk)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice-ocean mass flux.
!
        IF (Qout(idImflx,ng)) THEN
          Vinfo( 1)=Vname(1,idImflx)
          Vinfo( 2)=Vname(2,idImflx)
          Vinfo( 3)=Vname(3,idImflx)
          Vinfo(14)=Vname(4,idImflx)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idImflx,ng),r8)
          QCK(ng)%pioVar(idImflx)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idImflx)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idImflx)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice frazil growth rate.
!
        IF (Qout(idIwfr,ng)) THEN
          Vinfo( 1)=Vname(1,idIwfr)
          Vinfo( 2)=Vname(2,idIwfr)
          Vinfo( 3)=Vname(3,idIwfr)
          Vinfo(14)=Vname(4,idIwfr)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIwfr,ng),r8)
          QCK(ng)%pioVar(idIwfr)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIwfr)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIwfr)%vd,                     &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice growth/melt rate of ice-air.
!
        IF (Qout(idIwai,ng)) THEN
          Vinfo( 1)=Vname(1,idIwai)
          Vinfo( 2)=Vname(2,idIwai)
          Vinfo( 3)=Vname(3,idIwai)
          Vinfo(14)=Vname(4,idIwai)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIwai,ng),r8)
          QCK(ng)%pioVar(idIwai)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIwai)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIwai)%vd,                     &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice growth/melt rate of leads.
!
        IF (Qout(idIwao,ng)) THEN
          Vinfo( 1)=Vname(1,idIwao)
          Vinfo( 2)=Vname(2,idIwao)
          Vinfo( 3)=Vname(3,idIwao)
          Vinfo(14)=Vname(4,idIwao)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIwao,ng),r8)
          QCK(ng)%pioVar(idIwao)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIwao)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIwao)%vd,                     &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice growth/melt rate of ice-ocean.
!
        IF (Qout(idIwio,ng)) THEN
          Vinfo( 1)=Vname(1,idIwio)
          Vinfo( 2)=Vname(2,idIwio)
          Vinfo( 3)=Vname(3,idIwio)
          Vinfo(14)=Vname(4,idIwio)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIwio,ng),r8)
          QCK(ng)%pioVar(idIwio)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIwio)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIwio)%vd,                     &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice melt runoff rate.
!
        IF (Qout(idIwro,ng)) THEN
          Vinfo( 1)=Vname(1,idIwro)
          Vinfo( 2)=Vname(2,idIwro)
          Vinfo( 3)=Vname(3,idIwro)
          Vinfo(14)=Vname(4,idIwro)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIwro,ng),r8)
          QCK(ng)%pioVar(idIwro)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIwro)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIwro)%vd,                     &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice divergence rate.
!
        IF (Qout(idIwdiv,ng)) THEN
          Vinfo( 1)=Vname(1,idIwdiv)
          Vinfo( 2)=Vname(2,idIwdiv)
          Vinfo( 3)=Vname(3,idIwdiv)
          Vinfo(14)=Vname(4,idIwdiv)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIwdiv,ng),r8)
          QCK(ng)%pioVar(idIwdiv)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIwdiv)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIwdiv)%vd,                    &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  ifdef ICE_DIAGS
#   ifdef ICE_ALBEDO
!
!  Define ocean albedo.
!
        IF (Qout(idIalbe,ng)) THEN
          Vinfo( 1)=Vname(1,idIalbe)
          Vinfo( 2)=Vname(2,idIalbe)
          Vinfo( 3)=Vname(3,idIalbe)
          Vinfo(14)=Vname(4,idIalbe)
          Vinfo(16)=Vname(1,idtime)
#    if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIalbe,ng),r8)
          QCK(ng)%pioVar(idIalbe)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIalbe)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIalbe)%vd,                    &
#    ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#    else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#    endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice albedo.
!
        IF (Qout(idIalbI,ng)) THEN
          Vinfo( 1)=Vname(1,idIalbI)
          Vinfo( 2)=Vname(2,idIalbI)
          Vinfo( 3)=Vname(3,idIalbI)
          Vinfo(14)=Vname(4,idIalbI)
          Vinfo(16)=Vname(1,idtime)
#    if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIalbI,ng),r8)
          QCK(ng)%pioVar(idIalbI)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIalbI)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIalbI)%vd,                    &
#    ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#    else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#    endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#    ifdef ICE_ALB_CSIM
!
!  Define fraction of shortwave penetrating into the ice.
!
        IF (Qout(idIfrsw,ng)) THEN
          Vinfo( 1)=Vname(1,idIfrsw)
          Vinfo( 2)=Vname(2,idIfrsw)
          Vinfo( 3)=Vname(3,idIfrsw)
          Vinfo(14)=Vname(4,idIfrsw)
          Vinfo(16)=Vname(1,idtime)
#     if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#     endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIfrsw,ng),r8)
          QCK(ng)%pioVar(idIfrsw)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIfrsw)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIfrsw)%vd,                    &
#     ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#     else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#     endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#    endif
#   endif
!
!  Define reduction factor for near-IR fraction of SW under ice.
!
        IF (Qout(idIrfac,ng)) THEN
          Vinfo( 1)=Vname(1,idIrfac)
          Vinfo( 2)=Vname(2,idIrfac)
          Vinfo( 3)=Vname(3,idIrfac)
          Vinfo(14)=Vname(4,idIrfac)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIrfac,ng),r8)
          QCK(ng)%pioVar(idIrfac)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIrfac)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIrfac)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define downward longwave radiation.
!
        IF (Qout(idIlrdn,ng)) THEN
          Vinfo( 1)=Vname(1,idIlrdn)
          Vinfo( 2)=Vname(2,idIlrdn)
          Vinfo( 3)=Vname(3,idIlrdn)
          Vinfo(14)=Vname(4,idIlrdn)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIlrdn,ng),r8)
          QCK(ng)%pioVar(idIlrdn)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIlrdn)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIlrdn)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define downward shortwave radiation.
!
        IF (Qout(idIsrdn,ng)) THEN
          Vinfo( 1)=Vname(1,idIsrdn)
          Vinfo( 2)=Vname(2,idIsrdn)
          Vinfo( 3)=Vname(3,idIsrdn)
          Vinfo(14)=Vname(4,idIsrdn)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsrdn,ng),r8)
          QCK(ng)%pioVar(idIsrdn)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsrdn)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsrdn)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define latent heat over ice.
!
        IF (Qout(idIlatI,ng)) THEN
          Vinfo( 1)=Vname(1,idIlatI)
          Vinfo( 2)=Vname(2,idIlatI)
          Vinfo( 3)=Vname(3,idIlatI)
          Vinfo(14)=Vname(4,idIlatI)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIlatI,ng),r8)
          QCK(ng)%pioVar(idIlatI)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIlatI)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIlatI)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define sensible heat over ice.
!
        IF (Qout(idIsenI,ng)) THEN
          Vinfo( 1)=Vname(1,idIsenI)
          Vinfo( 2)=Vname(2,idIsenI)
          Vinfo( 3)=Vname(3,idIsenI)
          Vinfo(14)=Vname(4,idIsenI)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsenI,ng),r8)
          QCK(ng)%pioVar(idIsenI)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsenI)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsenI)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define upward longwave over ice.
!
        IF (Qout(idIlrup,ng)) THEN
          Vinfo( 1)=Vname(1,idIlrup)
          Vinfo( 2)=Vname(2,idIlrup)
          Vinfo( 3)=Vname(3,idIlrup)
          Vinfo(14)=Vname(4,idIlrup)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIlrup,ng),r8)
          QCK(ng)%pioVar(idIlrup)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIlrup)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIlrup)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define upward shortwave over ice.
!
        IF (Qout(idIsrup,ng)) THEN
          Vinfo( 1)=Vname(1,idIsrup)
          Vinfo( 2)=Vname(2,idIsrup)
          Vinfo( 3)=Vname(3,idIsrup)
          Vinfo(14)=Vname(4,idIsrup)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsrup,ng),r8)
          QCK(ng)%pioVar(idIsrup)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsrup)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsrup)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define shortwave into ice.
!
        IF (Qout(idIsrin,ng)) THEN
          Vinfo( 1)=Vname(1,idIsrin)
          Vinfo( 2)=Vname(2,idIsrin)
          Vinfo( 3)=Vname(3,idIsrin)
          Vinfo(14)=Vname(4,idIsrin)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsrin,ng),r8)
          QCK(ng)%pioVar(idIsrin)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsrin)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsrin)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define shortwave through ice.
!
        IF (Qout(idIsrth,ng)) THEN
          Vinfo( 1)=Vname(1,idIsrth)
          Vinfo( 2)=Vname(2,idIsrth)
          Vinfo( 3)=Vname(3,idIsrth)
          Vinfo(14)=Vname(4,idIsrth)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsrth,ng),r8)
          QCK(ng)%pioVar(idIsrth)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsrth)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsrth)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define salt flux under ice.
!
        IF (Qout(idIsfI,ng)) THEN
          Vinfo( 1)=Vname(1,idIsfI)
          Vinfo( 2)=Vname(2,idIsfI)
          Vinfo( 3)=Vname(3,idIsfI)
          Vinfo(14)=Vname(4,idIsfI)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsfI,ng),r8)
          QCK(ng)%pioVar(idIsfI)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsfI)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsfI)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define salt flux through leads.
!
        IF (Qout(idIsfL,ng)) THEN
          Vinfo( 1)=Vname(1,idIsfL)
          Vinfo( 2)=Vname(2,idIsfL)
          Vinfo( 3)=Vname(3,idIsfL)
          Vinfo(14)=Vname(4,idIsfL)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsfL,ng),r8)
          QCK(ng)%pioVar(idIsfL)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsfL)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsfL)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux through leads.
!
        IF (Qout(idIqao,ng)) THEN
          Vinfo( 1)=Vname(1,idIqao)
          Vinfo( 2)=Vname(2,idIqao)
          Vinfo( 3)=Vname(3,idIqao)
          Vinfo(14)=Vname(4,idIqao)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqao,ng),r8)
          QCK(ng)%pioVar(idIqao)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIqao)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIqao)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux of ice-air.
!
        IF (Qout(idIqai,ng)) THEN
          Vinfo( 1)=Vname(1,idIqai)
          Vinfo( 2)=Vname(2,idIqai)
          Vinfo( 3)=Vname(3,idIqai)
          Vinfo(14)=Vname(4,idIqai)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqai,ng),r8)
          QCK(ng)%pioVar(idIqai)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIqai)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIqai)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux of ice-ocean.
!
        IF (Qout(idIqio,ng)) THEN
          Vinfo( 1)=Vname(1,idIqio)
          Vinfo( 2)=Vname(2,idIqio)
          Vinfo( 3)=Vname(3,idIqio)
          Vinfo(14)=Vname(4,idIqio)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqio,ng),r8)
          QCK(ng)%pioVar(idIqio)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIqio)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIqio)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice heat flux.
!
        IF (Qout(idIqi2,ng)) THEN
          Vinfo( 1)=Vname(1,idIqi2)
          Vinfo( 2)=Vname(2,idIqi2)
          Vinfo( 3)=Vname(3,idIqi2)
          Vinfo(14)=Vname(4,idIqi2)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqi2,ng),r8)
          QCK(ng)%pioVar(idIqi2)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIqi2)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIqi2)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define snow-ice conversion.
!
        IF (Qout(idIsnoi,ng)) THEN
          Vinfo( 1)=Vname(1,idIsnoi)
          Vinfo( 2)=Vname(2,idIsnoi)
          Vinfo( 3)=Vname(3,idIsnoi)
          Vinfo(14)=Vname(4,idIsnoi)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsnoi,ng),r8)
          QCK(ng)%pioVar(idIsnoi)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsnoi)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsnoi)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  endif
# endif
# ifdef ICESHELF
!
!  Define iceshelf melt rate.
!
        IF (Qout(idIsMr,ng)) THEN
          Vinfo( 1)=Vname(1,idIsMr)
          Vinfo( 2)=Vname(2,idIsMr)
          Vinfo( 3)=Vname(3,idIsMr)
          Vinfo(14)=Vname(4,idIsMr)
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsMr,ng),r8)
          QCK(ng)%pioVar(idIsMr)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsMr)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsMr)%vd,                     &
#  ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#  else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#  endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  ifdef ICESHELF_3EQN_VBC
!
!  Define iceshelf frictional temperature.
!
        IF (Qout(idIsTs,ng)) THEN
          Vinfo( 1)=Vname(1,idIsTs)
          Vinfo( 2)=Vname(2,idIsTs)
          Vinfo( 3)=Vname(3,idIsTs)
          Vinfo(14)=Vname(4,idIsTs)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsTs,ng),r8)
          QCK(ng)%pioVar(idIsTs)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsTs)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsTs)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define iceshelf frictional velocity.
!
        IF (Qout(idIsUs,ng)) THEN
          Vinfo( 1)=Vname(1,idIsUs)
          Vinfo( 2)=Vname(2,idIsUs)
          Vinfo( 3)=Vname(3,idIsUs)
          Vinfo(14)=Vname(4,idIsUs)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsUs,ng),r8)
          QCK(ng)%pioVar(idIsUs)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsUs)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsUs)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define iceshelf molecular sublayer temperature.
!
        IF (Qout(idIsTb,ng)) THEN
          Vinfo( 1)=Vname(1,idIsTb)
          Vinfo( 2)=Vname(2,idIsTb)
          Vinfo( 3)=Vname(3,idIsTb)
          Vinfo(14)=Vname(4,idIsTb)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsTb,ng),r8)
          QCK(ng)%pioVar(idIsTb)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsTb)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsTb)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define iceshelf molecular sublayer salinity.
!
        IF (Qout(idIsSb,ng)) THEN
          Vinfo( 1)=Vname(1,idIsSb)
          Vinfo( 2)=Vname(2,idIsSb)
          Vinfo( 3)=Vname(3,idIsSb)
          Vinfo(14)=Vname(4,idIsSb)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsSb,ng),r8)
          QCK(ng)%pioVar(idIsSb)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsSb)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsSb)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  endif
#  ifdef ICESHELF_MORPH
!
!  Define time-varying ice draft.
!
        IF (Qout(idIsDrft,ng)) THEN
          Vinfo( 1)=Vname(1,idIsDrft)
          Vinfo( 2)=Vname(2,idIsDrft)
          Vinfo( 3)=Vname(3,idIsDrft)
          Vinfo(14)=Vname(4,idIsDrft)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsDrft,ng),r8)
          QCK(ng)%pioVar(idIsDrft)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsDrft)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsDrft)%vd,                   &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  endif
#  ifdef ICESHELF_SOF
!
!  Define surface salt overflux.
!
        IF (Qout(idIsSOFs,ng)) THEN
          Vinfo( 1)=Vname(1,idIsSOFs)
          Vinfo( 2)=Vname(2,idIsSOFs)
          Vinfo( 3)=Vname(3,idIsSOFs)
          Vinfo(14)=Vname(4,idIsSOFs)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsSOFs,ng),r8)
          QCK(ng)%pioVar(idIsSOFs)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsSOFs)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsSOFs)%vd,                   &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define surface heat overflux.
!
        IF (Qout(idIsSOFh,ng)) THEN
          Vinfo( 1)=Vname(1,idIsSOFh)
          Vinfo( 2)=Vname(2,idIsSOFh)
          Vinfo( 3)=Vname(3,idIsSOFh)
          Vinfo(14)=Vname(4,idIsSOFh)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsSOFh,ng),r8)
          QCK(ng)%pioVar(idIsSOFh)%dkind=PIO_FOUT
          QCK(ng)%pioVar(idIsSOFh)%gtype=r2dvar
!
          status=def_var(ng, iNLM, QCK(ng)%pioFile,                     &
     &                   QCK(ng)%pioVar(idIsSOFh)%vd,                   &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  endif
# endif

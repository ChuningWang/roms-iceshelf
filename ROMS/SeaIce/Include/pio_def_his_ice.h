# ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
!  Define 2D ice momentum in the XI-direction.
!
        IF (Hout(idIuice,ng)) THEN
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
          HIS(ng)%pioVar(idIuice)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIuice)%gtype=u2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIuice)%vd,                    &
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
        IF (Hout(idIvice,ng)) THEN
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
          HIS(ng)%pioVar(idIvice)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIvice)%gtype=v2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIvice)%vd,                    &
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
        IF (Hout(idIuiceE,ng)) THEN
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
          HIS(ng)%pioVar(idIuiceE)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIuiceE)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIuiceE)%vd,                   &
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
        IF (Hout(idIviceN,ng)) THEN
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
          HIS(ng)%pioVar(idIviceN)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIviceN)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIviceN)%vd,                   &
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
        IF (Hout(idIaice,ng)) THEN
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
          HIS(ng)%pioVar(idIaice)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIaice)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIaice)%vd,                    &
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
        IF (Hout(idIhice,ng)) THEN
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
          HIS(ng)%pioVar(idIhice)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIhice)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIhice)%vd,                    &
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
        IF (Hout(idIhsno,ng)) THEN
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
          HIS(ng)%pioVar(idIhsno)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIhsno)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIhsno)%vd,                    &
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
        IF (Hout(idIagei,ng)) THEN
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
          HIS(ng)%pioVar(idIagei)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIagei)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIagei)%vd,                    &
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
        IF (Hout(idItice,ng)) THEN
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
          HIS(ng)%pioVar(idItice)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idItice)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idItice)%vd,                    &
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
        IF (Hout(idItmid,ng)) THEN
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
          HIS(ng)%pioVar(idItmid)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idItmid)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idItmid)%vd,                    &
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
        IF (Hout(idIt2ic,ng)) THEN
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
          HIS(ng)%pioVar(idIt2ic)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIt2ic)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIt2ic)%vd,                    &
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
        IF (Hout(idIsg11,ng)) THEN
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
          HIS(ng)%pioVar(idIsg11)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIsg11)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIsg11)%vd,                    &
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
        IF (Hout(idIsg12,ng)) THEN
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
          HIS(ng)%pioVar(idIsg12)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIsg12)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIsg12)%vd,                    &
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
        IF (Hout(idIsg22,ng)) THEN
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
          HIS(ng)%pioVar(idIsg22)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIsg22)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIsg22)%vd,                    &
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
        IF (Hout(idIutau,ng)) THEN
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
          HIS(ng)%pioVar(idIutau)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIutau)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIutau)%vd,                    &
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
        IF (Hout(idImchu,ng)) THEN
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
          HIS(ng)%pioVar(idImchu)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idImchu)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idImchu)%vd,                    &
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
        IF (Hout(idIt0mk,ng)) THEN
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
          HIS(ng)%pioVar(idIt0mk)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIt0mk)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIt0mk)%vd,                    &
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
        IF (Hout(idIs0mk,ng)) THEN
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
          HIS(ng)%pioVar(idIs0mk)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIs0mk)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIs0mk)%vd,                    &
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
        IF (Hout(idImflx,ng)) THEN
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
          HIS(ng)%pioVar(idImflx)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idImflx)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idImflx)%vd,                    &
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
        IF (Hout(idIwfr,ng)) THEN
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
          HIS(ng)%pioVar(idIwfr)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIwfr)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIwfr)%vd,                     &
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
        IF (Hout(idIwai,ng)) THEN
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
          HIS(ng)%pioVar(idIwai)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIwai)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIwai)%vd,                     &
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
        IF (Hout(idIwao,ng)) THEN
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
          HIS(ng)%pioVar(idIwao)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIwao)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIwao)%vd,                     &
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
        IF (Hout(idIwio,ng)) THEN
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
          HIS(ng)%pioVar(idIwio)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIwio)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIwio)%vd,                     &
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
        IF (Hout(idIwro,ng)) THEN
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
          HIS(ng)%pioVar(idIwro)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIwro)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIwro)%vd,                     &
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
        IF (Hout(idIwdiv,ng)) THEN
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
          HIS(ng)%pioVar(idIwdiv)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIwdiv)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIwdiv)%vd,                    &
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
        IF (Hout(idIalbO,ng)) THEN
          Vinfo( 1)=Vname(1,idIalbO)
          Vinfo( 2)=Vname(2,idIalbO)
          Vinfo( 3)=Vname(3,idIalbO)
          Vinfo(14)=Vname(4,idIalbO)
          Vinfo(16)=Vname(1,idtime)
#    if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#    endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIalbO,ng),r8)
          HIS(ng)%pioVar(idIalbO)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIalbO)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIalbO)%vd,                    &
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
        IF (Hout(idIalbI,ng)) THEN
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
          HIS(ng)%pioVar(idIalbI)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIalbI)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIalbI)%vd,                    &
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
        IF (Hout(idIfrsw,ng)) THEN
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
          HIS(ng)%pioVar(idIfrsw)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIfrsw)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIfrsw)%vd,                    &
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
!  Define shortwave radiation into ice.
!
        IF (Hout(idIqswi,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswi)
          Vinfo( 2)=Vname(2,idIqswi)
          Vinfo( 3)=Vname(3,idIqswi)
          Vinfo(14)=Vname(4,idIqswi)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqswi,ng),r8)
          HIS(ng)%pioVar(idIqswi)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIqswi)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIqswi)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define longwave radiation into ice.
!
        IF (Hout(idIqlwi,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlwi)
          Vinfo( 2)=Vname(2,idIqlwi)
          Vinfo( 3)=Vname(3,idIqlwi)
          Vinfo(14)=Vname(4,idIqlwi)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqlwi,ng),r8)
          HIS(ng)%pioVar(idIqlwi)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIqlwi)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIqlwi)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define latent heat flux into ice.
!
        IF (Hout(idIqlai,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlai)
          Vinfo( 2)=Vname(2,idIqlai)
          Vinfo( 3)=Vname(3,idIqlai)
          Vinfo(14)=Vname(4,idIqlai)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqlai,ng),r8)
          HIS(ng)%pioVar(idIqlai)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIqlai)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIqlai)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define sensible heat flux into ice.
!
        IF (Hout(idIqsei,ng)) THEN
          Vinfo( 1)=Vname(1,idIqsei)
          Vinfo( 2)=Vname(2,idIqsei)
          Vinfo( 3)=Vname(3,idIqsei)
          Vinfo(14)=Vname(4,idIqsei)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqsei,ng),r8)
          HIS(ng)%pioVar(idIqsei)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIqsei)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIqsei)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define downward shortwave radiation into leads.
!
        IF (Hout(idIqswl,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswl)
          Vinfo( 2)=Vname(2,idIqswl)
          Vinfo( 3)=Vname(3,idIqswl)
          Vinfo(14)=Vname(4,idIqswl)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqswl,ng),r8)
          HIS(ng)%pioVar(idIqswl)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIqswl)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIqswl)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define downward longwave radiation into leads.
!
        IF (Hout(idIqlwl,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlwl)
          Vinfo( 2)=Vname(2,idIqlwl)
          Vinfo( 3)=Vname(3,idIqlwl)
          Vinfo(14)=Vname(4,idIqlwl)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqlwl,ng),r8)
          HIS(ng)%pioVar(idIqlwl)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIqlwl)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIqlwl)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define upward longwave into atmosphere.
!
        IF (Hout(idIqlwa,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlwa)
          Vinfo( 2)=Vname(2,idIqlwa)
          Vinfo( 3)=Vname(3,idIqlwa)
          Vinfo(14)=Vname(4,idIqlwa)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqlwa,ng),r8)
          HIS(ng)%pioVar(idIqlwa)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIqlwa)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIqlwa)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define upward shortwave into atmosphere.
!
        IF (Hout(idIqswa,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswa)
          Vinfo( 2)=Vname(2,idIqswa)
          Vinfo( 3)=Vname(3,idIqswa)
          Vinfo(14)=Vname(4,idIqswa)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqswa,ng),r8)
          HIS(ng)%pioVar(idIqswa)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIqswa)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIqswa)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define shortwave radiation through ice.
!
        IF (Hout(idIqswo,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswo)
          Vinfo( 2)=Vname(2,idIqswo)
          Vinfo( 3)=Vname(3,idIqswo)
          Vinfo(14)=Vname(4,idIqswo)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIqswo,ng),r8)
          HIS(ng)%pioVar(idIqswo)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIqswo)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIqswo)%vd,                    &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux, air-ocean.
!
        IF (Hout(idIqao,ng)) THEN
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
          HIS(ng)%pioVar(idIqao)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIqao)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIqao)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux, air-ice.
!
        IF (Hout(idIqai,ng)) THEN
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
          HIS(ng)%pioVar(idIqai)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIqai)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIqai)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux, ice-ocean.
!
        IF (Hout(idIqio,ng)) THEN
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
          HIS(ng)%pioVar(idIqio)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIqio)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIqio)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux, ice-snow.
!
        IF (Hout(idIqi2,ng)) THEN
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
          HIS(ng)%pioVar(idIqi2)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIqi2)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIqi2)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define salt flux, air-ocean.
!
        IF (Hout(idIsao,ng)) THEN
          Vinfo( 1)=Vname(1,idIsao)
          Vinfo( 2)=Vname(2,idIsao)
          Vinfo( 3)=Vname(3,idIsao)
          Vinfo(14)=Vname(4,idIsao)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsao,ng),r8)
          HIS(ng)%pioVar(idIsao)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIsao)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIsao)%vd,                     &
#   ifdef WET_DRY
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname,   &
     &                   SetFillVal = .FALSE.)
#   else
     &                   PIO_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
#   endif
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define salt flux, ice-ocean.
!
        IF (Hout(idIsio,ng)) THEN
          Vinfo( 1)=Vname(1,idIsio)
          Vinfo( 2)=Vname(2,idIsio)
          Vinfo( 3)=Vname(3,idIsio)
          Vinfo(14)=Vname(4,idIsio)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsio,ng),r8)
          HIS(ng)%pioVar(idIsio)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIsio)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIsio)%vd,                     &
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
        IF (Hout(idIwsni,ng)) THEN
          Vinfo( 1)=Vname(1,idIwsni)
          Vinfo( 2)=Vname(2,idIwsni)
          Vinfo( 3)=Vname(3,idIwsni)
          Vinfo(14)=Vname(4,idIwsni)
          Vinfo(16)=Vname(1,idtime)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIwsni,ng),r8)
          HIS(ng)%pioVar(idIwsni)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIwsni)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIwsni)%vd,                    &
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
        IF (Hout(idIsMr,ng)) THEN
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
          HIS(ng)%pioVar(idIsMr)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIsMr)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIsMr)%vd,                     &
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
        IF (Hout(idIsTs,ng)) THEN
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
          HIS(ng)%pioVar(idIsTs)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIsTs)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIsTs)%vd,                     &
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
        IF (Hout(idIsUs,ng)) THEN
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
          HIS(ng)%pioVar(idIsUs)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIsUs)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIsUs)%vd,                     &
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
        IF (Hout(idIsTb,ng)) THEN
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
          HIS(ng)%pioVar(idIsTb)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIsTb)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIsTb)%vd,                     &
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
        IF (Hout(idIsSb,ng)) THEN
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
          HIS(ng)%pioVar(idIsSb)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIsSb)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIsSb)%vd,                     &
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
        IF (Hout(idIsDrft,ng)) THEN
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
          HIS(ng)%pioVar(idIsDrft)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIsDrft)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIsDrft)%vd,                   &
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
        IF (Hout(idIsSOFs,ng)) THEN
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
          HIS(ng)%pioVar(idIsSOFs)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIsSOFs)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIsSOFs)%vd,                   &
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
        IF (Hout(idIsSOFh,ng)) THEN
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
          HIS(ng)%pioVar(idIsSOFh)%dkind=PIO_FOUT
          HIS(ng)%pioVar(idIsSOFh)%gtype=r2dvar
!
          status=def_var(ng, iNLM, HIS(ng)%pioFile,                     &
     &                   HIS(ng)%pioVar(idIsSOFh)%vd,                   &
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

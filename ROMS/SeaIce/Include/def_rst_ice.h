#ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
!  Define 2D ice momentum in the XI-direction.
!
        Vinfo( 1)=Vname(1,idIuice)
        Vinfo( 2)=Vname(2,idIuice)
        Vinfo( 3)=Vname(3,idIuice)
        Vinfo(14)=Vname(4,idIuice)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idIuice,ng),r8)
# ifdef PERFECT_RESTART
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIuice),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd4, u2dgrd, Aval, Vinfo, ncname,      &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd4, u2dgrd, Aval, Vinfo, ncname)
#  endif
# else
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIuice),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd3, su2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd3, su2dgrd, Aval, Vinfo, ncname)

#  endif
# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define 2D ice momentum in the ETA-direction.
!
        Vinfo( 1)=Vname(1,idIvice)
        Vinfo( 2)=Vname(2,idIvice)
        Vinfo( 3)=Vname(3,idIvice)
        Vinfo(14)=Vname(4,idIvice)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idIvice,ng),r8)
# ifdef PERFECT_RESTART
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIvice),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd4, v2dgrd, Aval, Vinfo, ncname,      &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd4, v2dgrd, Aval, Vinfo, ncname)
#  endif
# else
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIvice),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd3, sv2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd3, sv2dgrd, Aval, Vinfo, ncname)

#  endif
# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define ice concentration.
!
        Vinfo( 1)=Vname(1,idIaice)
        Vinfo( 2)=Vname(2,idIaice)
        Vinfo( 3)=Vname(3,idIaice)
        Vinfo(14)=Vname(4,idIaice)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idIaice,ng),r8)
# ifdef PERFECT_RESTART
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIaice),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname,      &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname)
#  endif
# else
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIaice),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

#  endif
# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define ice average thickness.
!
        Vinfo( 1)=Vname(1,idIhice)
        Vinfo( 2)=Vname(2,idIhice)
        Vinfo( 3)=Vname(3,idIhice)
        Vinfo(14)=Vname(4,idIhice)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idIhice,ng),r8)
# ifdef PERFECT_RESTART
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIhice),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname,      &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname)
#  endif
# else
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIhice),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

#  endif
# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define snow thickness.
!
        Vinfo( 1)=Vname(1,idIhsno)
        Vinfo( 2)=Vname(2,idIhsno)
        Vinfo( 3)=Vname(3,idIhsno)
        Vinfo(14)=Vname(4,idIhsno)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idIhsno,ng),r8)
# ifdef PERFECT_RESTART
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIhsno),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname,      &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname)
#  endif
# else
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIhsno),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

#  endif
# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define ice age.
!
        Vinfo( 1)=Vname(1,idIagei)
        Vinfo( 2)=Vname(2,idIagei)
        Vinfo( 3)=Vname(3,idIagei)
        Vinfo(14)=Vname(4,idIagei)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idIagei,ng),r8)
# ifdef PERFECT_RESTART
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIagei),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname,      &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname)
#  endif
# else
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIagei),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

#  endif
# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define temperature of ice/snow surface.
!
        Vinfo( 1)=Vname(1,idItice)
        Vinfo( 2)=Vname(2,idItice)
        Vinfo( 3)=Vname(3,idItice)
        Vinfo(14)=Vname(4,idItice)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idItice,ng),r8)
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idItice),    &
# ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
# else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define temperature of internal ice.
!
        Vinfo( 1)=Vname(1,idItmid)
        Vinfo( 2)=Vname(2,idItmid)
        Vinfo( 3)=Vname(3,idItmid)
        Vinfo(14)=Vname(4,idItmid)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idItmid,ng),r8)
# ifdef PERFECT_RESTART
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idItmid),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname,      &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname)
#  endif
# else
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idItmid),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

#  endif
# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define internal ice stress component 11.
!
        Vinfo( 1)=Vname(1,idIsg11)
        Vinfo( 2)=Vname(2,idIsg11)
        Vinfo( 3)=Vname(3,idIsg11)
        Vinfo(14)=Vname(4,idIsg11)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idIsg11,ng),r8)
# ifdef PERFECT_RESTART
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsg11),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname,      &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname)
#  endif
# else
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsg11),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

#  endif
# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define internal ice stress component 12.
!
        Vinfo( 1)=Vname(1,idIsg12)
        Vinfo( 2)=Vname(2,idIsg12)
        Vinfo( 3)=Vname(3,idIsg12)
        Vinfo(14)=Vname(4,idIsg12)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idIsg12,ng),r8)
# ifdef PERFECT_RESTART
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsg12),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname,      &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname)
#  endif
# else
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsg12),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

#  endif
# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define internal ice stress component 22.
!
        Vinfo( 1)=Vname(1,idIsg22)
        Vinfo( 2)=Vname(2,idIsg22)
        Vinfo( 3)=Vname(3,idIsg22)
        Vinfo(14)=Vname(4,idIsg22)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idIsg22,ng),r8)
# ifdef PERFECT_RESTART
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsg22),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname,      &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd4, t2dgrd, Aval, Vinfo, ncname)
#  endif
# else
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsg22),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

#  endif
# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define ice-water friction velocity.
!
        Vinfo( 1)=Vname(1,idIutau)
        Vinfo( 2)=Vname(2,idIutau)
        Vinfo( 3)=Vname(3,idIutau)
        Vinfo(14)=Vname(4,idIutau)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idIutau,ng),r8)
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIutau),    &
# ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
# else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define ice-water momentum transfer coefficient.
!
        Vinfo( 1)=Vname(1,idImchu)
        Vinfo( 2)=Vname(2,idImchu)
        Vinfo( 3)=Vname(3,idImchu)
        Vinfo(14)=Vname(4,idImchu)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idImchu,ng),r8)
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idImchu),    &
# ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
# else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define temperature of molecular sublayer under ice.
!
        Vinfo( 1)=Vname(1,idIt0mk)
        Vinfo( 2)=Vname(2,idIt0mk)
        Vinfo( 3)=Vname(3,idIt0mk)
        Vinfo(14)=Vname(4,idIt0mk)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idIt0mk,ng),r8)
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIt0mk),    &
# ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
# else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define salinity of molecular sublayer under ice.
!
        Vinfo( 1)=Vname(1,idIs0mk)
        Vinfo( 2)=Vname(2,idIs0mk)
        Vinfo( 3)=Vname(3,idIs0mk)
        Vinfo(14)=Vname(4,idIs0mk)
        Vinfo(16)=Vname(1,idtime)
# if defined WRITE_WATER && defined MASKING
#  if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#  else
        Vinfo(20)='mask_rho'
#  endif
# endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idIs0mk,ng),r8)
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIs0mk),    &
# ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
# else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

# endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

# ifdef PERFECT_RESTART
        DO itrc=1,NAT
          Vinfo( 1)=Vname(1,idTsur(itrc))
          Vinfo( 2)=Vname(2,idTsur(itrc))
          Vinfo( 3)=Vname(3,idTsur(itrc))
          Vinfo(14)=Vname(4,idTsur(itrc))
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
#   if defined PERFECT_RESTART
          Vinfo(24)='_FillValue'
          Aval(6)=spval
#   else
          Vinfo(20)='mask_rho'
#   endif
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idTsur(itrc),ng),r8)
          status=def_var(ng, iNLM, RST(ng)%ncid,                        &
     &                   RST(ng)%Vid(idTsur(itrc)),                     &
     &                   NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END DO

!
!  Define surface U-momentum stress.
!
        Vinfo( 1)=Vname(1,idUsms)
        Vinfo( 2)=Vname(2,idUsms)
        Vinfo( 3)=Vname(3,idUsms)
        Vinfo(14)=Vname(4,idUsms)
        Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
#   if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#   else
        Vinfo(20)='mask_rho'
#   endif
#  endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idUsms,ng),r8)
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idUsms),     &
#  ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

#  endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

!
!  Define surface V-momentum stress.
!
        Vinfo( 1)=Vname(1,idVsms)
        Vinfo( 2)=Vname(2,idVsms)
        Vinfo( 3)=Vname(3,idVsms)
        Vinfo(14)=Vname(4,idVsms)
        Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
#   if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#   else
        Vinfo(20)='mask_rho'
#   endif
#  endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idVsms,ng),r8)
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idVsms),     &
#  ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

#  endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

# endif
#endif
#ifdef ICESHELF
# ifdef ICESHELF_MORPH
!
!  Define time-varying ice draft.
!
        Vinfo( 1)=Vname(1,idIsDrft)
        Vinfo( 2)=Vname(2,idIsDrft)
        Vinfo( 3)=Vname(3,idIsDrft)
        Vinfo(14)=Vname(4,idIsDrft)
        Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
#   if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#   else
        Vinfo(20)='mask_rho'
#   endif
#  endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idIsDrft,ng),r8)
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsDrft),   &
#  ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

#  endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

# endif
# ifdef ICESHELF_TRACER
!
!  Define iceshelf volume (m).
!
        Vinfo( 1)=Vname(1,idIsVol)
        Vinfo( 2)=Vname(2,idIsVol)
        Vinfo( 3)=Vname(3,idIsVol)
        Vinfo(14)=Vname(4,idIsVol)
        Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
#   if !defined WET_DRY && defined PERFECT_RESTART
        Vinfo(24)='_FillValue'
        Aval(6)=spval
#   else
        Vinfo(20)='mask_rho'
#   endif
#  endif
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idIsVol,ng),r8)
        status=def_var(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idIsVol),    &
#  ifdef WET_DRY
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname,     &
     &                 SetFillVal = .FALSE.)
#  else
     &                 NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)

#  endif
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

        DO itrc=NAT+1, NT(ng)
          Vinfo( 1)=Vname(1,idIsTrc(itrc))
          Vinfo( 2)=Vname(2,idIsTrc(itrc))
          Vinfo( 3)=Vname(3,idIsTrc(itrc))
          Vinfo(14)=Vname(4,idIsTrc(itrc))
          Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
#   if defined PERFECT_RESTART
          Vinfo(24)='_FillValue'
          Aval(6)=spval
#   else
          Vinfo(20)='mask_rho'
#   endif
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idIsTrc(itrc),ng),r8)
          status=def_var(ng, iNLM, RST(ng)%ncid,                        &
     &                   RST(ng)%Vid(idIsTrc(itrc)),                    &
     &                   NF_FRST, nvd3, sr2dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END DO

# endif
#endif

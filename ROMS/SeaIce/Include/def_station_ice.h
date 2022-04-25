# ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
!  Define 2D ice momentum in the XI-direction.
!
        IF (Sout(idIuice,ng)) THEN
          Vinfo( 1)=Vname(1,idIuice)
          Vinfo( 2)=Vname(2,idIuice)
          Vinfo( 3)=Vname(3,idIuice)
          Vinfo(14)=Vname(4,idIuice)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIuice),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define 2D ice momentum in the ETA-direction.
!
        IF (Sout(idIvice,ng)) THEN
          Vinfo( 1)=Vname(1,idIvice)
          Vinfo( 2)=Vname(2,idIvice)
          Vinfo( 3)=Vname(3,idIvice)
          Vinfo(14)=Vname(4,idIvice)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIvice),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define 2D ice momentum in the East-direction.
!
        IF (Sout(idIuiceE,ng)) THEN
          Vinfo( 1)=Vname(1,idIuiceE)
          Vinfo( 2)=Vname(2,idIuiceE)
          Vinfo( 3)=Vname(3,idIuiceE)
          Vinfo(14)=Vname(4,idIuiceE)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIuiceE), &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define 2D ice momentum in the North-direction.
!
        IF (Sout(idIviceN,ng)) THEN
          Vinfo( 1)=Vname(1,idIviceN)
          Vinfo( 2)=Vname(2,idIviceN)
          Vinfo( 3)=Vname(3,idIviceN)
          Vinfo(14)=Vname(4,idIviceN)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIviceN), &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice concentration.
!
        IF (Sout(idIaice,ng)) THEN
          Vinfo( 1)=Vname(1,idIaice)
          Vinfo( 2)=Vname(2,idIaice)
          Vinfo( 3)=Vname(3,idIaice)
          Vinfo(14)=Vname(4,idIaice)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIaice),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice average thickness.
!
        IF (Sout(idIhice,ng)) THEN
          Vinfo( 1)=Vname(1,idIhice)
          Vinfo( 2)=Vname(2,idIhice)
          Vinfo( 3)=Vname(3,idIhice)
          Vinfo(14)=Vname(4,idIhice)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIhice),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define snow thickness.
!
        IF (Sout(idIhsno,ng)) THEN
          Vinfo( 1)=Vname(1,idIhsno)
          Vinfo( 2)=Vname(2,idIhsno)
          Vinfo( 3)=Vname(3,idIhsno)
          Vinfo(14)=Vname(4,idIhsno)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIhsno),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice age.
!
        IF (Sout(idIagei,ng)) THEN
          Vinfo( 1)=Vname(1,idIagei)
          Vinfo( 2)=Vname(2,idIagei)
          Vinfo( 3)=Vname(3,idIagei)
          Vinfo(14)=Vname(4,idIagei)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIagei),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define temperature of ice/snow surface.
!
        IF (Sout(idItice,ng)) THEN
          Vinfo( 1)=Vname(1,idItice)
          Vinfo( 2)=Vname(2,idItice)
          Vinfo( 3)=Vname(3,idItice)
          Vinfo(14)=Vname(4,idItice)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idItice),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define temperature of internal ice.
!
        IF (Sout(idItmid,ng)) THEN
          Vinfo( 1)=Vname(1,idItmid)
          Vinfo( 2)=Vname(2,idItmid)
          Vinfo( 3)=Vname(3,idItmid)
          Vinfo(14)=Vname(4,idItmid)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idItmid),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define temperature of top ice.
!
        IF (Sout(idIt2ic,ng)) THEN
          Vinfo( 1)=Vname(1,idIt2ic)
          Vinfo( 2)=Vname(2,idIt2ic)
          Vinfo( 3)=Vname(3,idIt2ic)
          Vinfo(14)=Vname(4,idIt2ic)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIt2ic),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define internal ice stress component 11.
!
        IF (Sout(idIsg11,ng)) THEN
          Vinfo( 1)=Vname(1,idIsg11)
          Vinfo( 2)=Vname(2,idIsg11)
          Vinfo( 3)=Vname(3,idIsg11)
          Vinfo(14)=Vname(4,idIsg11)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIsg11),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define internal ice stress component 12.
!
        IF (Sout(idIsg12,ng)) THEN
          Vinfo( 1)=Vname(1,idIsg12)
          Vinfo( 2)=Vname(2,idIsg12)
          Vinfo( 3)=Vname(3,idIsg12)
          Vinfo(14)=Vname(4,idIsg12)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIsg12),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define internal ice stress component 22.
!
        IF (Sout(idIsg22,ng)) THEN
          Vinfo( 1)=Vname(1,idIsg22)
          Vinfo( 2)=Vname(2,idIsg22)
          Vinfo( 3)=Vname(3,idIsg22)
          Vinfo(14)=Vname(4,idIsg22)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIsg22),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice-water friction velocity.
!
        IF (Sout(idIutau,ng)) THEN
          Vinfo( 1)=Vname(1,idIutau)
          Vinfo( 2)=Vname(2,idIutau)
          Vinfo( 3)=Vname(3,idIutau)
          Vinfo(14)=Vname(4,idIutau)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIutau),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice-water momentum transfer coefficient.
!
        IF (Sout(idImchu,ng)) THEN
          Vinfo( 1)=Vname(1,idImchu)
          Vinfo( 2)=Vname(2,idImchu)
          Vinfo( 3)=Vname(3,idImchu)
          Vinfo(14)=Vname(4,idImchu)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idImchu),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define temperature of molecular sublayer under ice.
!
        IF (Sout(idIt0mk,ng)) THEN
          Vinfo( 1)=Vname(1,idIt0mk)
          Vinfo( 2)=Vname(2,idIt0mk)
          Vinfo( 3)=Vname(3,idIt0mk)
          Vinfo(14)=Vname(4,idIt0mk)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIt0mk),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define salinity of molecular sublayer under ice.
!
        IF (Sout(idIs0mk,ng)) THEN
          Vinfo( 1)=Vname(1,idIs0mk)
          Vinfo( 2)=Vname(2,idIs0mk)
          Vinfo( 3)=Vname(3,idIs0mk)
          Vinfo(14)=Vname(4,idIs0mk)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIs0mk),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice-ocean mass flux.
!
        IF (Sout(idImflx,ng)) THEN
          Vinfo( 1)=Vname(1,idImflx)
          Vinfo( 2)=Vname(2,idImflx)
          Vinfo( 3)=Vname(3,idImflx)
          Vinfo(14)=Vname(4,idImflx)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idImflx),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice frazil growth rate.
!
        IF (Sout(idIwfr,ng)) THEN
          Vinfo( 1)=Vname(1,idIwfr)
          Vinfo( 2)=Vname(2,idIwfr)
          Vinfo( 3)=Vname(3,idIwfr)
          Vinfo(14)=Vname(4,idIwfr)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIwfr),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice growth/melt rate of ice-air.
!
        IF (Sout(idIwai,ng)) THEN
          Vinfo( 1)=Vname(1,idIwai)
          Vinfo( 2)=Vname(2,idIwai)
          Vinfo( 3)=Vname(3,idIwai)
          Vinfo(14)=Vname(4,idIwai)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIwai),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice growth/melt rate of leads.
!
        IF (Sout(idIwao,ng)) THEN
          Vinfo( 1)=Vname(1,idIwao)
          Vinfo( 2)=Vname(2,idIwao)
          Vinfo( 3)=Vname(3,idIwao)
          Vinfo(14)=Vname(4,idIwao)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIwao),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice growth/melt rate of ice-ocean.
!
        IF (Sout(idIwio,ng)) THEN
          Vinfo( 1)=Vname(1,idIwio)
          Vinfo( 2)=Vname(2,idIwio)
          Vinfo( 3)=Vname(3,idIwio)
          Vinfo(14)=Vname(4,idIwio)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIwio),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice melt runoff rate.
!
        IF (Sout(idIwro,ng)) THEN
          Vinfo( 1)=Vname(1,idIwro)
          Vinfo( 2)=Vname(2,idIwro)
          Vinfo( 3)=Vname(3,idIwro)
          Vinfo(14)=Vname(4,idIwro)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIwro),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice divergence rate.
!
        IF (Sout(idIwdiv,ng)) THEN
          Vinfo( 1)=Vname(1,idIwdiv)
          Vinfo( 2)=Vname(2,idIwdiv)
          Vinfo( 3)=Vname(3,idIwdiv)
          Vinfo(14)=Vname(4,idIwdiv)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIwdiv),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  ifdef ICE_DIAGS
#   ifdef ICE_ALBEDO
!
!  Define ocean albedo.
!
        IF (Sout(idIalbO,ng)) THEN
          Vinfo( 1)=Vname(1,idIalbO)
          Vinfo( 2)=Vname(2,idIalbO)
          Vinfo( 3)=Vname(3,idIalbO)
          Vinfo(14)=Vname(4,idIalbO)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIalbO),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice albedo.
!
        IF (Sout(idIalbI,ng)) THEN
          Vinfo( 1)=Vname(1,idIalbI)
          Vinfo( 2)=Vname(2,idIalbI)
          Vinfo( 3)=Vname(3,idIalbI)
          Vinfo(14)=Vname(4,idIalbI)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIalbI),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#    ifdef ICE_ALB_CSIM
!
!  Define fraction of shortwave penetrating into the ice.
!
        IF (Sout(idIfrsw,ng)) THEN
          Vinfo( 1)=Vname(1,idIfrsw)
          Vinfo( 2)=Vname(2,idIfrsw)
          Vinfo( 3)=Vname(3,idIfrsw)
          Vinfo(14)=Vname(4,idIfrsw)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIfrsw),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#     ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#     else
     &                   SetFillVal = .TRUE.,                           &
#     endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#    endif
#   endif
!
!  Define shortwave radiation into ice.
!
        IF (Sout(idIqswi,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswi)
          Vinfo( 2)=Vname(2,idIqswi)
          Vinfo( 3)=Vname(3,idIqswi)
          Vinfo(14)=Vname(4,idIqswi)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIqswi),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define longwave radiation into ice.
!
        IF (Sout(idIqlwi,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlwi)
          Vinfo( 2)=Vname(2,idIqlwi)
          Vinfo( 3)=Vname(3,idIqlwi)
          Vinfo(14)=Vname(4,idIqlwi)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIqlwi),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define latent heat flux into ice.
!
        IF (Sout(idIqlai,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlai)
          Vinfo( 2)=Vname(2,idIqlai)
          Vinfo( 3)=Vname(3,idIqlai)
          Vinfo(14)=Vname(4,idIqlai)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIqlai),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define sensible heat flux into ice.
!
        IF (Sout(idIqsei,ng)) THEN
          Vinfo( 1)=Vname(1,idIqsei)
          Vinfo( 2)=Vname(2,idIqsei)
          Vinfo( 3)=Vname(3,idIqsei)
          Vinfo(14)=Vname(4,idIqsei)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIqsei),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define downward shortwave radiation into leads.
!
        IF (Sout(idIqswl,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswl)
          Vinfo( 2)=Vname(2,idIqswl)
          Vinfo( 3)=Vname(3,idIqswl)
          Vinfo(14)=Vname(4,idIqswl)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIqswl),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define downward longwave radiation into leads.
!
        IF (Sout(idIqlwl,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlwl)
          Vinfo( 2)=Vname(2,idIqlwl)
          Vinfo( 3)=Vname(3,idIqlwl)
          Vinfo(14)=Vname(4,idIqlwl)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIqlwl),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define upward longwave into atmosphere.
!
        IF (Sout(idIqlwa,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlwa)
          Vinfo( 2)=Vname(2,idIqlwa)
          Vinfo( 3)=Vname(3,idIqlwa)
          Vinfo(14)=Vname(4,idIqlwa)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIqlwa),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define upward shortwave into atmosphere.
!
        IF (Sout(idIqswa,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswa)
          Vinfo( 2)=Vname(2,idIqswa)
          Vinfo( 3)=Vname(3,idIqswa)
          Vinfo(14)=Vname(4,idIqswa)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIqswa),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define shortwave radiation through ice.
!
        IF (Sout(idIqswo,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswo)
          Vinfo( 2)=Vname(2,idIqswo)
          Vinfo( 3)=Vname(3,idIqswo)
          Vinfo(14)=Vname(4,idIqswo)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIqswo),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux, air-ocean.
!
        IF (Sout(idIqao,ng)) THEN
          Vinfo( 1)=Vname(1,idIqao)
          Vinfo( 2)=Vname(2,idIqao)
          Vinfo( 3)=Vname(3,idIqao)
          Vinfo(14)=Vname(4,idIqao)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIqao),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux, air-ice.
!
        IF (Sout(idIqai,ng)) THEN
          Vinfo( 1)=Vname(1,idIqai)
          Vinfo( 2)=Vname(2,idIqai)
          Vinfo( 3)=Vname(3,idIqai)
          Vinfo(14)=Vname(4,idIqai)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIqai),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux, ice-ocean.
!
        IF (Sout(idIqio,ng)) THEN
          Vinfo( 1)=Vname(1,idIqio)
          Vinfo( 2)=Vname(2,idIqio)
          Vinfo( 3)=Vname(3,idIqio)
          Vinfo(14)=Vname(4,idIqio)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIqio),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux, ice-snow.
!
        IF (Sout(idIqi2,ng)) THEN
          Vinfo( 1)=Vname(1,idIqi2)
          Vinfo( 2)=Vname(2,idIqi2)
          Vinfo( 3)=Vname(3,idIqi2)
          Vinfo(14)=Vname(4,idIqi2)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIqi2),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define salt flux, air-ocean.
!
        IF (Sout(idIsao,ng)) THEN
          Vinfo( 1)=Vname(1,idIsao)
          Vinfo( 2)=Vname(2,idIsao)
          Vinfo( 3)=Vname(3,idIsao)
          Vinfo(14)=Vname(4,idIsao)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIsao),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define salt flux, ice-ocean.
!
        IF (Sout(idIsio,ng)) THEN
          Vinfo( 1)=Vname(1,idIsio)
          Vinfo( 2)=Vname(2,idIsio)
          Vinfo( 3)=Vname(3,idIsio)
          Vinfo(14)=Vname(4,idIsio)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIsio),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define snow-ice conversion.
!
        IF (Sout(idIwsni,ng)) THEN
          Vinfo( 1)=Vname(1,idIwsni)
          Vinfo( 2)=Vname(2,idIwsni)
          Vinfo( 3)=Vname(3,idIwsni)
          Vinfo(14)=Vname(4,idIwsni)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIwsni),  &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  endif
# endif
# ifdef ICESHELF
!
!  Define iceshelf melt rate.
!
        IF (Sout(idIsMr,ng)) THEN
          Vinfo( 1)=Vname(1,idIsMr)
          Vinfo( 2)=Vname(2,idIsMr)
          Vinfo( 3)=Vname(3,idIsMr)
          Vinfo(14)=Vname(4,idIsMr)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIsMr),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#  ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#  else
     &                   SetFillVal = .TRUE.,                           &
#  endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  ifdef ICESHELF_3EQN_VBC
!
!  Define iceshelf frictional temperature.
!
        IF (Sout(idIsTs,ng)) THEN
          Vinfo( 1)=Vname(1,idIsTs)
          Vinfo( 2)=Vname(2,idIsTs)
          Vinfo( 3)=Vname(3,idIsTs)
          Vinfo(14)=Vname(4,idIsTs)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIsTs),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define iceshelf frictional velocity.
!
        IF (Sout(idIsUs,ng)) THEN
          Vinfo( 1)=Vname(1,idIsUs)
          Vinfo( 2)=Vname(2,idIsUs)
          Vinfo( 3)=Vname(3,idIsUs)
          Vinfo(14)=Vname(4,idIsUs)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIsUs),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define iceshelf molecular sublayer temperature.
!
        IF (Sout(idIsTb,ng)) THEN
          Vinfo( 1)=Vname(1,idIsTb)
          Vinfo( 2)=Vname(2,idIsTb)
          Vinfo( 3)=Vname(3,idIsTb)
          Vinfo(14)=Vname(4,idIsTb)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIsTb),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define iceshelf molecular sublayer salinity.
!
        IF (Sout(idIsSb,ng)) THEN
          Vinfo( 1)=Vname(1,idIsSb)
          Vinfo( 2)=Vname(2,idIsSb)
          Vinfo( 3)=Vname(3,idIsSb)
          Vinfo(14)=Vname(4,idIsSb)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIsSb),   &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  endif
#  ifdef ICESHELF_MORPH
!
!  Define time-varying ice draft.
!
        IF (Sout(idIsDrft,ng)) THEN
          Vinfo( 1)=Vname(1,idIsDrft)
          Vinfo( 2)=Vname(2,idIsDrft)
          Vinfo( 3)=Vname(3,idIsDrft)
          Vinfo(14)=Vname(4,idIsDrft)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIsDrft), &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  endif
#  ifdef ICESHELF_SOF
!
!  Define surface salt overflux.
!
        IF (Sout(idIsSOFs,ng)) THEN
          Vinfo( 1)=Vname(1,idIsSOFs)
          Vinfo( 2)=Vname(2,idIsSOFs)
          Vinfo( 3)=Vname(3,idIsSOFs)
          Vinfo(14)=Vname(4,idIsSOFs)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIsSOFs), &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define surface heat overflux.
!
        IF (Sout(idIsSOFh,ng)) THEN
          Vinfo( 1)=Vname(1,idIsSOFh)
          Vinfo( 2)=Vname(2,idIsSOFh)
          Vinfo( 3)=Vname(3,idIsSOFh)
          Vinfo(14)=Vname(4,idIsSOFh)
          Vinfo(16)=Vname(1,idtime)
          status=def_var(ng, iNLM, STA(ng)%ncid, STA(ng)%Vid(idIsSOFh), &
     &                   NF_FOUT, 2, pgrd, Aval, Vinfo, ncname,         &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  endif
# endif

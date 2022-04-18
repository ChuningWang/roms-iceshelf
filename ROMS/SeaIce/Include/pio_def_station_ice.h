#  ifdef ICE_MODEL
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
          STA(ng)%pioVar(idIuice)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIuice)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIuice)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIvice)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIvice)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIvice)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIuiceE)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIuiceE)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIuiceE)%vd,                   &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIviceN)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIviceN)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIviceN)%vd,                   &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIaice)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIaice)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIaice)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIhice)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIhice)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIhice)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIhsno)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIhsno)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIhsno)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIagei)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIagei)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIagei)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idItice)%dkind=PIO_FOUT
          STA(ng)%pioVar(idItice)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idItice)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idItmid)%dkind=PIO_FOUT
          STA(ng)%pioVar(idItmid)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idItmid)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIt2ic)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIt2ic)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIt2ic)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIsg11)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIsg11)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIsg11)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIsg12)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIsg12)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIsg12)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIsg22)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIsg22)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIsg22)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIutau)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIutau)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIutau)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idImchu)%dkind=PIO_FOUT
          STA(ng)%pioVar(idImchu)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idImchu)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIt0mk)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIt0mk)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIt0mk)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIs0mk)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIs0mk)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIs0mk)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idImflx)%dkind=PIO_FOUT
          STA(ng)%pioVar(idImflx)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idImflx)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIwfr)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIwfr)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIwfr)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIwai)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIwai)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIwai)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIwao)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIwao)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIwao)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIwio)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIwio)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIwio)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIwro)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIwro)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIwro)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
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
          STA(ng)%pioVar(idIwdiv)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIwdiv)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIwdiv)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#   ifdef ICE_DIAGS
#    ifdef ICE_ALBEDO
!
!  Define ocean albedo.
!
        IF (Sout(idIalbe,ng)) THEN
          Vinfo( 1)=Vname(1,idIalbe)
          Vinfo( 2)=Vname(2,idIalbe)
          Vinfo( 3)=Vname(3,idIalbe)
          Vinfo(14)=Vname(4,idIalbe)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIalbe)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIalbe)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIalbe)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#     ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#     else
     &                   SetFillVal = .TRUE.,                           &
#     endif
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
          STA(ng)%pioVar(idIalbI)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIalbI)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIalbI)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#     ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#     else
     &                   SetFillVal = .TRUE.,                           &
#     endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#     ifdef ICE_ALB_CSIM
!
!  Define fraction of shortwave penetrating into the ice.
!
        IF (Sout(idIfrsw,ng)) THEN
          Vinfo( 1)=Vname(1,idIfrsw)
          Vinfo( 2)=Vname(2,idIfrsw)
          Vinfo( 3)=Vname(3,idIfrsw)
          Vinfo(14)=Vname(4,idIfrsw)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIfrsw)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIfrsw)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIfrsw)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#      ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#      else
     &                   SetFillVal = .TRUE.,                           &
#      endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#     endif
#    endif
!
!  Define reduction factor for near-IR fraction of SW under ice.
!
        IF (Sout(idIrfac,ng)) THEN
          Vinfo( 1)=Vname(1,idIrfac)
          Vinfo( 2)=Vname(2,idIrfac)
          Vinfo( 3)=Vname(3,idIrfac)
          Vinfo(14)=Vname(4,idIrfac)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIrfac)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIrfac)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIrfac)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define downward shortwave radiation.
!
        IF (Sout(idIqswl,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswl)
          Vinfo( 2)=Vname(2,idIqswl)
          Vinfo( 3)=Vname(3,idIqswl)
          Vinfo(14)=Vname(4,idIqswl)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIqswl)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIqswl)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIqswl)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define downward longwave radiation.
!
        IF (Sout(idIqlwl,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlwl)
          Vinfo( 2)=Vname(2,idIqlwl)
          Vinfo( 3)=Vname(3,idIqlwl)
          Vinfo(14)=Vname(4,idIqlwl)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIqlwl)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIqlwl)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIqlwl)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define latent heat over ice.
!
        IF (Sout(idIqlai,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlai)
          Vinfo( 2)=Vname(2,idIqlai)
          Vinfo( 3)=Vname(3,idIqlai)
          Vinfo(14)=Vname(4,idIqlai)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIqlai)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIqlai)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIqlai)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define sensible heat over ice.
!
        IF (Sout(idIqsei,ng)) THEN
          Vinfo( 1)=Vname(1,idIqsei)
          Vinfo( 2)=Vname(2,idIqsei)
          Vinfo( 3)=Vname(3,idIqsei)
          Vinfo(14)=Vname(4,idIqsei)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIqsei)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIqsei)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIqsei)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define upward longwave over ice.
!
        IF (Sout(idIqlwa,ng)) THEN
          Vinfo( 1)=Vname(1,idIqlwa)
          Vinfo( 2)=Vname(2,idIqlwa)
          Vinfo( 3)=Vname(3,idIqlwa)
          Vinfo(14)=Vname(4,idIqlwa)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIqlwa)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIqlwa)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIqlwa)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define upward shortwave over ice.
!
        IF (Sout(idIqswa,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswa)
          Vinfo( 2)=Vname(2,idIqswa)
          Vinfo( 3)=Vname(3,idIqswa)
          Vinfo(14)=Vname(4,idIqswa)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIqswa)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIqswa)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIqswa)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define shortwave into ice.
!
        IF (Sout(idIqswi,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswi)
          Vinfo( 2)=Vname(2,idIqswi)
          Vinfo( 3)=Vname(3,idIqswi)
          Vinfo(14)=Vname(4,idIqswi)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIqswi)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIqswi)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIqswi)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define shortwave through ice.
!
        IF (Sout(idIqswo,ng)) THEN
          Vinfo( 1)=Vname(1,idIqswo)
          Vinfo( 2)=Vname(2,idIqswo)
          Vinfo( 3)=Vname(3,idIqswo)
          Vinfo(14)=Vname(4,idIqswo)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIqswo)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIqswo)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIqswo)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define salt flux under ice.
!
        IF (Sout(idIsio,ng)) THEN
          Vinfo( 1)=Vname(1,idIsio)
          Vinfo( 2)=Vname(2,idIsio)
          Vinfo( 3)=Vname(3,idIsio)
          Vinfo(14)=Vname(4,idIsio)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIsio)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIsio)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIsio)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define salt flux through leads.
!
        IF (Sout(idIsao,ng)) THEN
          Vinfo( 1)=Vname(1,idIsao)
          Vinfo( 2)=Vname(2,idIsao)
          Vinfo( 3)=Vname(3,idIsao)
          Vinfo(14)=Vname(4,idIsao)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIsao)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIsao)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIsao)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux through leads.
!
        IF (Sout(idIqao,ng)) THEN
          Vinfo( 1)=Vname(1,idIqao)
          Vinfo( 2)=Vname(2,idIqao)
          Vinfo( 3)=Vname(3,idIqao)
          Vinfo(14)=Vname(4,idIqao)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIqao)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIqao)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIqao)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux of ice-air.
!
        IF (Sout(idIqai,ng)) THEN
          Vinfo( 1)=Vname(1,idIqai)
          Vinfo( 2)=Vname(2,idIqai)
          Vinfo( 3)=Vname(3,idIqai)
          Vinfo(14)=Vname(4,idIqai)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIqai)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIqai)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIqai)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define heat flux of ice-ocean.
!
        IF (Sout(idIqio,ng)) THEN
          Vinfo( 1)=Vname(1,idIqio)
          Vinfo( 2)=Vname(2,idIqio)
          Vinfo( 3)=Vname(3,idIqio)
          Vinfo(14)=Vname(4,idIqio)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIqio)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIqio)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIqio)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define ice heat flux.
!
        IF (Sout(idIqi2,ng)) THEN
          Vinfo( 1)=Vname(1,idIqi2)
          Vinfo( 2)=Vname(2,idIqi2)
          Vinfo( 3)=Vname(3,idIqi2)
          Vinfo(14)=Vname(4,idIqi2)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIqi2)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIqi2)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIqi2)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
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
          STA(ng)%pioVar(idIwsni)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIwsni)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIwsni)%vd,                    &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#   endif
#  endif
#  ifdef ICESHELF
!
!  Define iceshelf melt rate.
!
        IF (Sout(idIsMr,ng)) THEN
          Vinfo( 1)=Vname(1,idIsMr)
          Vinfo( 2)=Vname(2,idIsMr)
          Vinfo( 3)=Vname(3,idIsMr)
          Vinfo(14)=Vname(4,idIsMr)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIsMr)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIsMr)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIsMr)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#   ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#   else
     &                   SetFillVal = .TRUE.,                           &
#   endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#   ifdef ICESHELF_3EQN_VBC
!
!  Define iceshelf frictional temperature.
!
        IF (Sout(idIsTs,ng)) THEN
          Vinfo( 1)=Vname(1,idIsTs)
          Vinfo( 2)=Vname(2,idIsTs)
          Vinfo( 3)=Vname(3,idIsTs)
          Vinfo(14)=Vname(4,idIsTs)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIsTs)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIsTs)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIsTs)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
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
          STA(ng)%pioVar(idIsUs)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIsUs)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIsUs)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
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
          STA(ng)%pioVar(idIsTb)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIsTb)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIsTb)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
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
          STA(ng)%pioVar(idIsSb)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIsSb)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIsSb)%vd,                     &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#   endif
#   ifdef ICESHELF_MORPH
!
!  Define time-varying ice draft.
!
        IF (Sout(idIsDrft,ng)) THEN
          Vinfo( 1)=Vname(1,idIsDrft)
          Vinfo( 2)=Vname(2,idIsDrft)
          Vinfo( 3)=Vname(3,idIsDrft)
          Vinfo(14)=Vname(4,idIsDrft)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIsDrft)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIsDrft)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIsDrft)%vd,                   &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#   endif
#   ifdef ICESHELF_SOF
!
!  Define surface salt overflux.
!
        IF (Sout(idIsSOFs,ng)) THEN
          Vinfo( 1)=Vname(1,idIsSOFs)
          Vinfo( 2)=Vname(2,idIsSOFs)
          Vinfo( 3)=Vname(3,idIsSOFs)
          Vinfo(14)=Vname(4,idIsSOFs)
          Vinfo(16)=Vname(1,idtime)
          STA(ng)%pioVar(idIsSOFs)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIsSOFs)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIsSOFs)%vd,                   &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
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
          STA(ng)%pioVar(idIsSOFh)%dkind=PIO_FOUT
          STA(ng)%pioVar(idIsSOFh)%gtype=0
!
          status=def_var(ng, iNLM, STA(ng)%pioFile,                     &
     &                   STA(ng)%pioVar(idIsSOFh)%vd,                   &
     &                   PIO_FOUT, 2, pgrd, Aval, Vinfo, ncname,        &
#    ifdef WET_DRY
     &                   SetFillVal = .FALSE.,                          &
#    else
     &                   SetFillVal = .TRUE.,                           &
#    endif
     &                   SetParAccess = .TRUE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#   endif
#  endif

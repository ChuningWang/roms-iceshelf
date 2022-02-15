# if defined ICE_MODEL || defined ICESHELF
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
        DO i=1,n_var
          IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idtime))) THEN
            got_var(idtime)=.TRUE.
#  ifdef ICE_MODEL
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIuice))) THEN
            got_var(idIuice)=.TRUE.
            RST(ng)%pioVar(idIuice)%vd=var_desc(i)
            RST(ng)%pioVar(idIuice)%dkind=PIO_FRST
            RST(ng)%pioVar(idIuice)%gtype=u2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIvice))) THEN
            got_var(idIvice)=.TRUE.
            RST(ng)%pioVar(idIvice)%vd=var_desc(i)
            RST(ng)%pioVar(idIvice)%dkind=PIO_FRST
            RST(ng)%pioVar(idIvice)%gtype=v2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIaice))) THEN
            got_var(idIaice)=.TRUE.
            RST(ng)%pioVar(idIaice)%vd=var_desc(i)
            RST(ng)%pioVar(idIaice)%dkind=PIO_FRST
            RST(ng)%pioVar(idIaice)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIhice))) THEN
            got_var(idIhice)=.TRUE.
            RST(ng)%pioVar(idIhice)%vd=var_desc(i)
            RST(ng)%pioVar(idIhice)%dkind=PIO_FRST
            RST(ng)%pioVar(idIhice)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIhsno))) THEN
            got_var(idIhsno)=.TRUE.
            RST(ng)%pioVar(idIhsno)%vd=var_desc(i)
            RST(ng)%pioVar(idIhsno)%dkind=PIO_FRST
            RST(ng)%pioVar(idIhsno)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIagei))) THEN
            got_var(idIagei)=.TRUE.
            RST(ng)%pioVar(idIagei)%vd=var_desc(i)
            RST(ng)%pioVar(idIagei)%dkind=PIO_FRST
            RST(ng)%pioVar(idIagei)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idItice))) THEN
            got_var(idItice)=.TRUE.
            RST(ng)%pioVar(idItice)%vd=var_desc(i)
            RST(ng)%pioVar(idItice)%dkind=PIO_FRST
            RST(ng)%pioVar(idItice)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idItmid))) THEN
            got_var(idItmid)=.TRUE.
            RST(ng)%pioVar(idItmid)%vd=var_desc(i)
            RST(ng)%pioVar(idItmid)%dkind=PIO_FRST
            RST(ng)%pioVar(idItmid)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg11))) THEN
            got_var(idIsg11)=.TRUE.
            RST(ng)%pioVar(idIsg11)%vd=var_desc(i)
            RST(ng)%pioVar(idIsg11)%dkind=PIO_FRST
            RST(ng)%pioVar(idIsg11)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg12))) THEN
            got_var(idIsg12)=.TRUE.
            RST(ng)%pioVar(idIsg12)%vd=var_desc(i)
            RST(ng)%pioVar(idIsg12)%dkind=PIO_FRST
            RST(ng)%pioVar(idIsg12)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg22))) THEN
            got_var(idIsg22)=.TRUE.
            RST(ng)%pioVar(idIsg22)%vd=var_desc(i)
            RST(ng)%pioVar(idIsg22)%dkind=PIO_FRST
            RST(ng)%pioVar(idIsg22)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIutau))) THEN
            got_var(idIutau)=.TRUE.
            RST(ng)%pioVar(idIutau)%vd=var_desc(i)
            RST(ng)%pioVar(idIutau)%dkind=PIO_FRST
            RST(ng)%pioVar(idIutau)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idImchu))) THEN
            got_var(idImchu)=.TRUE.
            RST(ng)%pioVar(idImchu)%vd=var_desc(i)
            RST(ng)%pioVar(idImchu)%dkind=PIO_FRST
            RST(ng)%pioVar(idImchu)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIt0mk))) THEN
            got_var(idIt0mk)=.TRUE.
            RST(ng)%pioVar(idIt0mk)%vd=var_desc(i)
            RST(ng)%pioVar(idIt0mk)%dkind=PIO_FRST
            RST(ng)%pioVar(idIt0mk)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIs0mk))) THEN
            got_var(idIs0mk)=.TRUE.
            RST(ng)%pioVar(idIs0mk)%vd=var_desc(i)
            RST(ng)%pioVar(idIs0mk)%dkind=PIO_FRST
            RST(ng)%pioVar(idIs0mk)%gtype=r2dvar
#   ifdef PERFECT_RESTART
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUsms))) THEN
            got_var(idUsms)=.TRUE.
            RST(ng)%pioVar(idUsms)%vd=var_desc(i)
            RST(ng)%pioVar(idUsms)%dkind=PIO_FRST
            RST(ng)%pioVar(idUsms)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVsms))) THEN
            got_var(idVsms)=.TRUE.
            RST(ng)%pioVar(idVsms)%vd=var_desc(i)
            RST(ng)%pioVar(idVsms)%dkind=PIO_FRST
            RST(ng)%pioVar(idVsms)%gtype=r2dvar
#   endif
#  endif
#  ifdef ICESHELF
#   ifdef ICESHELF_MORPH
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsDrft))) THEN
            got_var(idIsDrft)=.TRUE.
            RST(ng)%pioVar(idIsDrft)%vd=var_desc(i)
            RST(ng)%pioVar(idIsDrft)%dkind=PIO_FRST
            RST(ng)%pioVar(idIsDrft)%gtype=r2dvar
#   endif
#   ifdef ICESHELF_TRACER
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsVol))) THEN
            got_var(idIsVol)=.TRUE.
            RST(ng)%pioVar(idIsVol)%vd=var_desc(i)
            RST(ng)%pioVar(idIsVol)%dkind=PIO_FRST
            RST(ng)%pioVar(idIsVol)%gtype=r2dvar
#   endif
#  endif
          END IF
#  if defined ICE_MODEL && defined PERFECT_RESTART
          DO itrc=1,NAT
            IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idTsur(itrc)))) THEN
              got_var(idTsur(itrc))=.TRUE.
              RST(ng)%pioVar(idTsur(itrc))%vd=var_desc(i)
              RST(ng)%pioVar(idTsur(itrc))%dkind=PIO_FRST
              RST(ng)%pioVar(idTsur(itrc))%gtype=r2dvar
            END IF
          END DO
#  endif
#  if defined ICESHELF && defined ICESHELF_TRACER
          DO itrc=NAT+1,NT(ng)
            IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsTrc(itrc)))) THEN
              got_var(idIsTrc(itrc))=.TRUE.
              RST(ng)%pioVar(idIsTrc(itrc))%vd=var_desc(i)
              RST(ng)%pioVar(idIsTrc(itrc))%dkind=PIO_FRST
              RST(ng)%pioVar(idIsTrc(itrc))%gtype=r2dvar
            END IF
          END DO
#  endif
        END DO
!
!  Check if variables are available in input NetCDF file.
!
#  ifdef ICE_MODEL
        IF (.not.got_var(idIuice)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIuice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIvice)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIvice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIaice)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIaice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIhice)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIhice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIhsno)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIhsno)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIagei)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIagei)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idItice)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idItice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idItmid)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idItmid)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg11)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg11)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg12)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg12)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg22)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg22)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIutau)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIutau)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idImchu)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idImchu)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIt0mk)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIt0mk)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIs0mk)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIs0mk)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   ifdef PERFECT_RESTART
        DO itrc=1,NAT
          IF (.not.got_var(idTsur(itrc))) THEN
            IF (Master) WRITE (stdout,60) TRIM(Vname(1,idTsur(itrc))),  &
     &                                    TRIM(ncname)
            exit_flag=3
            RETURN
          END IF
        END DO
        IF (.not.got_var(idUsms)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idUsms)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idVsms)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idVsms)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   endif
#  endif
#  ifdef ICESHELF
#   ifdef ICESHELF_MORPH
        IF (.not.got_var(idIsDrft)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsDrft)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   endif
#   ifdef ICESHELF_TRACER
        IF (.not.got_var(idIsVol)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsVol)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        DO itrc=NAT+1, NT(ng)
          IF (.not.got_var(idIsTrc(itrc))) THEN
            IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsTrc(itrc))), &
     &                                    TRIM(ncname)
            exit_flag=3
            RETURN
          END IF
        END DO
#   endif
#  endif
# endif

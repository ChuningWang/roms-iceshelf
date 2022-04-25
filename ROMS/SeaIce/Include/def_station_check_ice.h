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
            STA(ng)%Vid(idIuice)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIvice))) THEN
            got_var(idIvice)=.TRUE.
            STA(ng)%Vid(idIvice)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIuiceE))) THEN
            got_var(idIuiceE)=.TRUE.
            STA(ng)%Vid(idIuiceE)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIviceN))) THEN
            got_var(idIviceN)=.TRUE.
            STA(ng)%Vid(idIviceN)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIaice))) THEN
            got_var(idIaice)=.TRUE.
            STA(ng)%Vid(idIaice)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIhice))) THEN
            got_var(idIhice)=.TRUE.
            STA(ng)%Vid(idIhice)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIhsno))) THEN
            got_var(idIhsno)=.TRUE.
            STA(ng)%Vid(idIhsno)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIagei))) THEN
            got_var(idIagei)=.TRUE.
            STA(ng)%Vid(idIagei)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idItice))) THEN
            got_var(idItice)=.TRUE.
            STA(ng)%Vid(idItice)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idItmid))) THEN
            got_var(idItmid)=.TRUE.
            STA(ng)%Vid(idItmid)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIt2ic))) THEN
            got_var(idIt2ic)=.TRUE.
            STA(ng)%Vid(idIt2ic)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg11))) THEN
            got_var(idIsg11)=.TRUE.
            STA(ng)%Vid(idIsg11)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg12))) THEN
            got_var(idIsg12)=.TRUE.
            STA(ng)%Vid(idIsg12)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg22))) THEN
            got_var(idIsg22)=.TRUE.
            STA(ng)%Vid(idIsg22)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIutau))) THEN
            got_var(idIutau)=.TRUE.
            STA(ng)%Vid(idIutau)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idImchu))) THEN
            got_var(idImchu)=.TRUE.
            STA(ng)%Vid(idImchu)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIt0mk))) THEN
            got_var(idIt0mk)=.TRUE.
            STA(ng)%Vid(idIt0mk)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIs0mk))) THEN
            got_var(idIs0mk)=.TRUE.
            STA(ng)%Vid(idIs0mk)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idImflx))) THEN
            got_var(idImflx)=.TRUE.
            STA(ng)%Vid(idImflx)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwfr))) THEN
            got_var(idIwfr)=.TRUE.
            STA(ng)%Vid(idIwfr)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwai))) THEN
            got_var(idIwai)=.TRUE.
            STA(ng)%Vid(idIwai)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwao))) THEN
            got_var(idIwao)=.TRUE.
            STA(ng)%Vid(idIwao)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwio))) THEN
            got_var(idIwio)=.TRUE.
            STA(ng)%Vid(idIwio)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwro))) THEN
            got_var(idIwro)=.TRUE.
            STA(ng)%Vid(idIwro)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwdiv))) THEN
            got_var(idIwdiv)=.TRUE.
            STA(ng)%Vid(idIwdiv)=var_id(i)
#   ifdef ICE_DIAGS
#    ifdef ICE_ALBEDO
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIalbO))) THEN
            got_var(idIalbO)=.TRUE.
            STA(ng)%Vid(idIalbO)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIalbI))) THEN
            got_var(idIalbI)=.TRUE.
            STA(ng)%Vid(idIalbI)=var_id(i)
#     ifdef ICE_ALB_CSIM
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIfrsw))) THEN
            got_var(idIfrsw)=.TRUE.
            STA(ng)%Vid(idIfrsw)=var_id(i)
#     endif
#    endif
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqswi))) THEN
            got_var(idIqswi)=.TRUE.
            STA(ng)%Vid(idIqswi)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqlwi))) THEN
            got_var(idIqlwi)=.TRUE.
            STA(ng)%Vid(idIqlwi)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqlai))) THEN
            got_var(idIqlai)=.TRUE.
            STA(ng)%Vid(idIqlai)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqsei))) THEN
            got_var(idIqsei)=.TRUE.
            STA(ng)%Vid(idIqsei)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqswl))) THEN
            got_var(idIqswl)=.TRUE.
            STA(ng)%Vid(idIqswl)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqlwl))) THEN
            got_var(idIqlwl)=.TRUE.
            STA(ng)%Vid(idIqlwl)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqlwa))) THEN
            got_var(idIqlwa)=.TRUE.
            STA(ng)%Vid(idIqlwa)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqswa))) THEN
            got_var(idIqswa)=.TRUE.
            STA(ng)%Vid(idIqswa)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqswo))) THEN
            got_var(idIqswo)=.TRUE.
            STA(ng)%Vid(idIqswo)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqao))) THEN
            got_var(idIqao)=.TRUE.
            STA(ng)%Vid(idIqao)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqai))) THEN
            got_var(idIqai)=.TRUE.
            STA(ng)%Vid(idIqai)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqio))) THEN
            got_var(idIqio)=.TRUE.
            STA(ng)%Vid(idIqio)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqi2))) THEN
            got_var(idIqi2)=.TRUE.
            STA(ng)%Vid(idIqi2)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsao))) THEN
            got_var(idIsao)=.TRUE.
            STA(ng)%Vid(idIsao)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsio))) THEN
            got_var(idIsio)=.TRUE.
            STA(ng)%Vid(idIsio)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwsni))) THEN
            got_var(idIwsni)=.TRUE.
            STA(ng)%Vid(idIwsni)=var_id(i)
#   endif
#  endif
#  ifdef ICESHELF
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsMr))) THEN
            got_var(idIsMr)=.TRUE.
            STA(ng)%Vid(idIsMr)=var_id(i)
#   ifdef ICESHELF_3EQN_VBC
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsTs))) THEN
            got_var(idIsTs)=.TRUE.
            STA(ng)%Vid(idIsTs)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsUs))) THEN
            got_var(idIsUs)=.TRUE.
            STA(ng)%Vid(idIsUs)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsTb))) THEN
            got_var(idIsTb)=.TRUE.
            STA(ng)%Vid(idIsTb)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSb))) THEN
            got_var(idIsSb)=.TRUE.
            STA(ng)%Vid(idIsSb)=var_id(i)
#   endif
#   ifdef ICESHELF_MORPH
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsDrft))) THEN
            got_var(idIsDrft)=.TRUE.
            STA(ng)%Vid(idIsDrft)=var_id(i)
#   endif
#   ifdef ICESHELF_SOF
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSOFs))) THEN
            got_var(idIsSOFs)=.TRUE.
            STA(ng)%Vid(idIsSOFs)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSOFh))) THEN
            got_var(idIsSOFh)=.TRUE.
            STA(ng)%Vid(idIsSOFh)=var_id(i)
#   endif
#  endif
          END IF
        END DO
!
!  Check if station variables are available in input NetCDF file.
!
#  ifdef ICE_MODEL
        IF (.not.got_var(idIuice).and.Sout(idIuice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIuice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIvice).and.Sout(idIvice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIvice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIuiceE).and.Sout(idIuiceE,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIuiceE)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIviceN).and.Sout(idIviceN,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIviceN)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIaice).and.Sout(idIaice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIaice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIhice).and.Sout(idIhice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIhice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIhsno).and.Sout(idIhsno,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIhsno)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIagei).and.Sout(idIagei,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIagei)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idItice).and.Sout(idItice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idItice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idItmid).and.Sout(idItmid,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idItmid)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIt2ic).and.Sout(idIt2ic,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIt2ic)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg11).and.Sout(idIsg11,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg11)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg12).and.Sout(idIsg12,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg12)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg22).and.Sout(idIsg22,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg22)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIutau).and.Sout(idIutau,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIutau)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idImchu).and.Sout(idImchu,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idImchu)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIt0mk).and.Sout(idIt0mk,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIt0mk)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIs0mk).and.Sout(idIs0mk,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIs0mk)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idImflx).and.Sout(idImflx,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idImflx)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwfr).and.Sout(idIwfr,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwfr)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwai).and.Sout(idIwai,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwai)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwao).and.Sout(idIwao,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwao)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwio).and.Sout(idIwio,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwio)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwro).and.Sout(idIwro,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwro)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwdiv).and.Sout(idIwdiv,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwdiv)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   ifdef ICE_DIAGS
#    ifdef ICE_ALBEDO
        IF (.not.got_var(idIalbO).and.Sout(idIalbO,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIalbO)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIalbI).and.Sout(idIalbI,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIalbI)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#     ifdef ICE_ALB_CSIM
        IF (.not.got_var(idIfrsw).and.Sout(idIfrsw,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIfrsw)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#     endif
#    endif
        IF (.not.got_var(idIqswi).and.Sout(idIqswi,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqswi)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqlwi).and.Sout(idIqlwi,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqlwi)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqlai).and.Sout(idIqlai,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqlai)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqsei).and.Sout(idIqsei,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqsei)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqswl).and.Sout(idIqswl,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqswl)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqlwl).and.Sout(idIqlwl,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqlwl)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqlwa).and.Sout(idIqlwa,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqlwa)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqswa).and.Sout(idIqswa,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqswa)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqswo).and.Sout(idIqswo,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqswo)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqao).and.Sout(idIqao,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqao)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqai).and.Sout(idIqai,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqai)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqio).and.Sout(idIqio,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqio)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqi2).and.Sout(idIqi2,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqi2)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsao).and.Sout(idIsao,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsao)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsio).and.Sout(idIsio,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsio)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwsni).and.Sout(idIwsni,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwsni)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   endif
#  endif
#  ifdef ICESHELF
        IF (.not.got_var(idIsMr).and.Sout(idIsMr,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsMr)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   ifdef ICESHELF_3EQN_VBC
        IF (.not.got_var(idIsTs).and.Sout(idIsTs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsTs)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsUs).and.Sout(idIsUs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsUs)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsTb).and.Sout(idIsTb,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsTb)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsSb).and.Sout(idIsSb,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSb)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   endif
#   ifdef ICESHELF_MORPH
        IF (.not.got_var(idIsDrft).and.Sout(idIsDrft,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsDrft)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   endif
#   ifdef ICESHELF_SOF
        IF (.not.got_var(idIsSOFs).and.Sout(idIsSOFs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSOFs)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsSOFh).and.Sout(idIsSOFh,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSOFh)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   endif
#  endif
# endif

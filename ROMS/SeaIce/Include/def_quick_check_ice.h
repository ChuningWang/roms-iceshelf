!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
        DO i=1,n_var
          IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idtime))) THEN
            got_var(idtime)=.TRUE.
#ifdef ICE_MODEL
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIuice))) THEN
            got_var(idIuice)=.TRUE.
            QCK(ng)%Vid(idIuice)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIvice))) THEN
            got_var(idIvice)=.TRUE.
            QCK(ng)%Vid(idIvice)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIuiceE))) THEN
            got_var(idIuiceE)=.TRUE.
            QCK(ng)%Vid(idIuiceE)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIviceN))) THEN
            got_var(idIviceN)=.TRUE.
            QCK(ng)%Vid(idIviceN)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIaice))) THEN
            got_var(idIaice)=.TRUE.
            QCK(ng)%Vid(idIaice)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIhice))) THEN
            got_var(idIhice)=.TRUE.
            QCK(ng)%Vid(idIhice)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIhsno))) THEN
            got_var(idIhsno)=.TRUE.
            QCK(ng)%Vid(idIhsno)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIagei))) THEN
            got_var(idIagei)=.TRUE.
            QCK(ng)%Vid(idIagei)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idItice))) THEN
            got_var(idItice)=.TRUE.
            QCK(ng)%Vid(idItice)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idItmid))) THEN
            got_var(idItmid)=.TRUE.
            QCK(ng)%Vid(idItmid)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIt2ic))) THEN
            got_var(idIt2ic)=.TRUE.
            QCK(ng)%Vid(idIt2ic)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg11))) THEN
            got_var(idIsg11)=.TRUE.
            QCK(ng)%Vid(idIsg11)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg12))) THEN
            got_var(idIsg12)=.TRUE.
            QCK(ng)%Vid(idIsg12)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg22))) THEN
            got_var(idIsg22)=.TRUE.
            QCK(ng)%Vid(idIsg22)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIutau))) THEN
            got_var(idIutau)=.TRUE.
            QCK(ng)%Vid(idIutau)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idImchu))) THEN
            got_var(idImchu)=.TRUE.
            QCK(ng)%Vid(idImchu)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIt0mk))) THEN
            got_var(idIt0mk)=.TRUE.
            QCK(ng)%Vid(idIt0mk)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIs0mk))) THEN
            got_var(idIs0mk)=.TRUE.
            QCK(ng)%Vid(idIs0mk)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idImflx))) THEN
            got_var(idImflx)=.TRUE.
            QCK(ng)%Vid(idImflx)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwfr))) THEN
            got_var(idIwfr)=.TRUE.
            QCK(ng)%Vid(idIwfr)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwai))) THEN
            got_var(idIwai)=.TRUE.
            QCK(ng)%Vid(idIwai)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwao))) THEN
            got_var(idIwao)=.TRUE.
            QCK(ng)%Vid(idIwao)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwio))) THEN
            got_var(idIwio)=.TRUE.
            QCK(ng)%Vid(idIwio)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwro))) THEN
            got_var(idIwro)=.TRUE.
            QCK(ng)%Vid(idIwro)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwdiv))) THEN
            got_var(idIwdiv)=.TRUE.
            QCK(ng)%Vid(idIwdiv)=var_id(i)
# ifdef ICE_DIAGS
#  ifdef ICE_ALBEDO
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIalbO))) THEN
            got_var(idIalbO)=.TRUE.
            QCK(ng)%Vid(idIalbO)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIalbI))) THEN
            got_var(idIalbI)=.TRUE.
            QCK(ng)%Vid(idIalbI)=var_id(i)
#   ifdef ICE_ALB_CSIM
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIfrsw))) THEN
            got_var(idIfrsw)=.TRUE.
            QCK(ng)%Vid(idIfrsw)=var_id(i)
#   endif
#  endif
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqswi))) THEN
            got_var(idIqswi)=.TRUE.
            QCK(ng)%Vid(idIqswi)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqlwi))) THEN
            got_var(idIqlwi)=.TRUE.
            QCK(ng)%Vid(idIqlwi)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqlai))) THEN
            got_var(idIqlai)=.TRUE.
            QCK(ng)%Vid(idIqlai)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqsei))) THEN
            got_var(idIqsei)=.TRUE.
            QCK(ng)%Vid(idIqsei)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqswl))) THEN
            got_var(idIqswl)=.TRUE.
            QCK(ng)%Vid(idIqswl)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqlwl))) THEN
            got_var(idIqlwl)=.TRUE.
            QCK(ng)%Vid(idIqlwl)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqlwa))) THEN
            got_var(idIqlwa)=.TRUE.
            QCK(ng)%Vid(idIqlwa)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqswa))) THEN
            got_var(idIqswa)=.TRUE.
            QCK(ng)%Vid(idIqswa)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqswo))) THEN
            got_var(idIqswo)=.TRUE.
            QCK(ng)%Vid(idIqswo)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqao))) THEN
            got_var(idIqao)=.TRUE.
            QCK(ng)%Vid(idIqao)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqai))) THEN
            got_var(idIqai)=.TRUE.
            QCK(ng)%Vid(idIqai)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqio))) THEN
            got_var(idIqio)=.TRUE.
            QCK(ng)%Vid(idIqio)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqi2))) THEN
            got_var(idIqi2)=.TRUE.
            QCK(ng)%Vid(idIqi2)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsao))) THEN
            got_var(idIsao)=.TRUE.
            QCK(ng)%Vid(idIsao)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsio))) THEN
            got_var(idIsio)=.TRUE.
            QCK(ng)%Vid(idIsio)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwsni))) THEN
            got_var(idIwsni)=.TRUE.
            QCK(ng)%Vid(idIwsni)=var_id(i)
# endif
#endif
#ifdef ICESHELF
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsMr))) THEN
            got_var(idIsMr)=.TRUE.
            QCK(ng)%Vid(idIsMr)=var_id(i)
# ifdef ICESHELF_3EQN_VBC
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsTs))) THEN
            got_var(idIsTs)=.TRUE.
            QCK(ng)%Vid(idIsTs)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsUs))) THEN
            got_var(idIsUs)=.TRUE.
            QCK(ng)%Vid(idIsUs)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsTb))) THEN
            got_var(idIsTb)=.TRUE.
            QCK(ng)%Vid(idIsTb)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSb))) THEN
            got_var(idIsSb)=.TRUE.
            QCK(ng)%Vid(idIsSb)=var_id(i)
# endif
# ifdef ICESHELF_MORPH
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsDrft))) THEN
            got_var(idIsDrft)=.TRUE.
            QCK(ng)%Vid(idIsDrft)=var_id(i)
# endif
# ifdef ICESHELF_SOF
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSOFs))) THEN
            got_var(idIsSOFs)=.TRUE.
            QCK(ng)%Vid(idIsSOFs)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSOFh))) THEN
            got_var(idIsSOFh)=.TRUE.
            QCK(ng)%Vid(idIsSOFh)=var_id(i)
# endif
#endif
          END IF
        END DO
!
!  Check if variables are available in input NetCDF file.
!
#ifdef ICE_MODEL
        IF (.not.got_var(idIuice).and.Qout(idIuice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIuice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIvice).and.Qout(idIvice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIvice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIuiceE).and.Qout(idIuiceE,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIuiceE)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIviceN).and.Qout(idIviceN,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIviceN)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIaice).and.Qout(idIaice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIaice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIhice).and.Qout(idIhice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIhice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIhsno).and.Qout(idIhsno,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIhsno)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIagei).and.Qout(idIagei,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIagei)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idItice).and.Qout(idItice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idItice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idItmid).and.Qout(idItmid,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idItmid)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIt2ic).and.Qout(idIt2ic,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIt2ic)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg11).and.Qout(idIsg11,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg11)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg12).and.Qout(idIsg12,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg12)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg22).and.Qout(idIsg22,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg22)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIutau).and.Qout(idIutau,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIutau)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idImchu).and.Qout(idImchu,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idImchu)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIt0mk).and.Qout(idIt0mk,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIt0mk)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIs0mk).and.Qout(idIs0mk,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIs0mk)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idImflx).and.Qout(idImflx,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idImflx)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwfr).and.Qout(idIwfr,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwfr)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwai).and.Qout(idIwai,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwai)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwao).and.Qout(idIwao,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwao)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwio).and.Qout(idIwio,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwio)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwro).and.Qout(idIwro,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwro)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwdiv).and.Qout(idIwdiv,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwdiv)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
# ifdef ICE_DIAGS
#  ifdef ICE_ALBEDO
        IF (.not.got_var(idIalbO).and.Qout(idIalbO,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIalbO)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIalbI).and.Qout(idIalbI,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIalbI)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   ifdef ICE_ALB_CSIM
        IF (.not.got_var(idIfrsw).and.Qout(idIfrsw,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIfrsw)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   endif
#  endif
        IF (.not.got_var(idIqswi).and.Qout(idIqswi,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqswi)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqlwi).and.Qout(idIqlwi,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqlwi)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqlai).and.Qout(idIqlai,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqlai)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqsei).and.Qout(idIqsei,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqsei)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqswl).and.Qout(idIqswl,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqswl)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqlwl).and.Qout(idIqlwl,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqlwl)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqlwa).and.Qout(idIqlwa,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqlwa)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqswa).and.Qout(idIqswa,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqswa)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqswo).and.Qout(idIqswo,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqswo)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqao).and.Qout(idIqao,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqao)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqai).and.Qout(idIqai,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqai)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqio).and.Qout(idIqio,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqio)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqi2).and.Qout(idIqi2,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqi2)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsao).and.Qout(idIsao,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsao)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsio).and.Qout(idIsio,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsio)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwsni).and.Qout(idIwsni,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwsni)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
# endif
#endif
#ifdef ICESHELF
        IF (.not.got_var(idIsMr).and.Qout(idIsMr,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsMr)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
# ifdef ICESHELF_3EQN_VBC
        IF (.not.got_var(idIsTs).and.Qout(idIsTs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsTs)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsUs).and.Qout(idIsUs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsUs)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsTb).and.Qout(idIsTb,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsTb)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsSb).and.Qout(idIsSb,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSb)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
# endif
# ifdef ICESHELF_MORPH
        IF (.not.got_var(idIsDrft).and.Qout(idIsDrft,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsDrft)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
# endif
# ifdef ICESHELF_SOF
        IF (.not.got_var(idIsSOFs).and.Qout(idIsSOFs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSOFs)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsSOFh).and.Qout(idIsSOFh,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSOFh)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
# endif
#endif

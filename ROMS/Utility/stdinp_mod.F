#include "cppdefs.h"
      MODULE stdinp_mod
!
!git $Id$
!svn $Id: stdinp_mod.F 1023 2020-05-13 04:30:35Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2020 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This module contains several routines to read requested KeyWord     !
!  parameter from ROMS standard input file. It is used to process      !
!  specific parameters before the normal call to read all of them.     !
!                                                                      !
!  getpar_i      Reads requested integer parameter.                    !
!                                                                      !
!  getpar_l      Reads requested logical parameter.                    !
!                                                                      !
!  getpar_r      Reads requested floating-point (real) parameter.      !
!                                                                      !
!  stdinp_unit   Determines input unit. If distributed-memory, it      !
!                  opens standard input file for reading.              !
!                                                                      !
!=======================================================================
!
      USE mod_kinds
      USE inp_decode_mod
!
      INTERFACE getpar_i
        MODULE PROCEDURE getpar_0d_i
        MODULE PROCEDURE getpar_1d_i
      END INTERFACE getpar_i

      INTERFACE getpar_l
        MODULE PROCEDURE getpar_0d_l
        MODULE PROCEDURE getpar_1d_l
      END INTERFACE getpar_l

      INTERFACE getpar_r
        MODULE PROCEDURE getpar_0d_r
        MODULE PROCEDURE getpar_1d_r
      END INTERFACE getpar_r
!
      CONTAINS
!
      FUNCTION stdinp_unit (localPET, GotFile) RESULT (InpUnit)
!
!***********************************************************************
!                                                                      !
!  This function determines ROMS standard input unit to process its    !
!  parameters. If distributed-memory, it gets standard input filename  !
!  and open it for processing.                                         !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     localPET   Local Persistent Execution Thread (integer)           !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     GotFile    Standard input filename is known (logical)            !
!                                                                      !
!***********************************************************************
!
      USE mod_iounits,  ONLY : Iname, SourceFile, stdinp, stdout
      USE mod_scalars,  ONLY : exit_flag

#ifdef DISTRIBUTE
!
      USE distribute_mod, ONLY : mp_bcasts
#endif
!
!  Imported variable declarations.
!
      logical, intent(out) :: GotFile
      integer, intent(in)  :: localPET
!
!  Local variable declararions
!
      integer :: InpUnit
!
      SourceFile=__FILE__// ", stdout_unit"
!
!-----------------------------------------------------------------------
!  Determine ROMS standard input unit.
!-----------------------------------------------------------------------
#ifdef DISTRIBUTE
!
!  In distributed-memory configurations, the input physical parameters
!  script is opened as a regular file.  It is read and processed by all
!  parallel nodes.  This is to avoid a very complex broadcasting of the
!  input parameters to all nodes.
!
      InpUnit=1
      IF (localPET.eq.0) CALL my_getarg (1, Iname)
      CALL mp_bcasts (1, 1, Iname)
      OPEN (InpUnit, FILE=TRIM(Iname), FORM='formatted', STATUS='old',  &
     &      ERR=10)
      GotFile=.TRUE.
      GO TO 30
 10   IF (localPET.eq.0) WRITE (stdout,20)
      exit_flag=2
      RETURN
 20   FORMAT (/,' STDINP_UNIT - Unable to open ROMS/TOMS input script', &
     &                        ' file.',                                 &
     &        /,11x,'In distributed-memory applications, the input',    &
     &        /,11x,'script file is processed in parallel. The Unix',   &
     &        /,11x,'routine GETARG is used to get script file name.',  &
     &        /,11x,'For example, in MPI applications make sure that',  &
     &        /,11x,'command line is something like:',/,                &
     &        /,11x,'mpirun -np 4 romsM roms.in',/,/,11x,'and not',/,   &
     &        /,11x,'mpirun -np 4 romsM < roms.in',/)
 30   CONTINUE
#else
      InpUnit=stdinp
      GotFile=.FALSE.
#endif
      END FUNCTION stdinp_unit
!
      SUBROUTINE getpar_0d_i (localPET, KeyWord, Value)
!
!***********************************************************************
!                                                                      !
!  Reads a scalar integer parameter from ROMS standard input file.     !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     localPET   Local Persistent Execution Thread (integer)           !
!     KeyWord    Keyword associated with input parameter (string)      !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Value      Standard input parameter value (integer)              !
!                                                                      !
!***********************************************************************
!
      USE mod_iounits, ONLY : stdout
      USE mod_scalars, ONLY : exit_flag

!
      USE strings_mod, ONLY : FoundError
!
!  Imported variable declarations.
!
      integer, intent(in)  :: localPET
      integer, intent(out) :: Value
!
      character (len=*), intent(in) :: KeyWord
!
!  Local variable declarations.
!
      logical :: foundit, GotFile

      integer :: InpUnit, Npts, Nval, status
      integer :: Ivalue(1)

      real(dp), dimension(nRval) :: Rval
!
      character (len= 40) :: string
      character (len=256) :: line
      character (len=256), dimension(nCval) :: Cval
!
!-----------------------------------------------------------------------
!  Read requested ROMS standard input integer parameter.
!-----------------------------------------------------------------------
!
!  Get standard input unit. If distribute-memory, open standard input
!  file for reading.
!
      InpUnit=stdinp_unit(localPET, GotFile)
!
!  Process requested parameter.
!
      foundit=.FALSE.
      DO WHILE (.TRUE.)
        READ (InpUnit,'(a)',ERR=10,END=30) line
        status=decode_line(line, string, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          IF (TRIM(string).eq.TRIM(KeyWord)) THEN
            Npts=load_i(Nval, Rval, 1, Ivalue)
            Value=Ivalue(1)
            foundit=.TRUE.
          END IF
        END IF
      END DO
  10  IF (localPET.eq.0) THEN
        WRITE (stdout,20) line
  20    FORMAT (/,' GETPAR_0D_I - Error while processing line: ',/,a)
      END IF
      exit_flag=4
  30  CONTINUE
      IF (.not.foundit) THEN
        IF (localPET.eq.0) THEN
          WRITE (stdout,40) TRIM(KeyWord)
  40    FORMAT (/,' GETPAR_0D_I - unable to find KeyWord: ',a,          &
     &          /,15x,'in ROMS standard input file.')
        END IF
        exit_flag=5
      END IF
      IF (GotFile) THEN
        CLOSE (InpUnit)
      END IF
!
      RETURN
      END SUBROUTINE getpar_0d_i
!
      SUBROUTINE getpar_1d_i (localPET, KeyWord, Ndim, Value)
!
!***********************************************************************
!                                                                      !
!  Reads a 1D integer parameter from ROMS standard input file.         !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     localPET   Local Persistent Execution Thread (integer)           !
!     KeyWord    Keyword associated with input parameter (string)      !
!     Ndim       Size integer variable dimension                       !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Value      Standard input parameter value (integer 1D array)     !
!                                                                      !
!***********************************************************************
!
      USE mod_iounits, ONLY : stdout
      USE mod_scalars, ONLY : exit_flag
!
      USE strings_mod, ONLY : FoundError
!
!  Imported variable declarations.
!
      integer, intent(in)  :: localPET
      integer, intent(in)  :: Ndim
      integer, intent(out) :: Value(:)
!
      character (len=*), intent(in) :: KeyWord
!
!  Local variable declarations.
!
      logical :: foundit, GotFile

      integer :: InpUnit, Npts, Nval, status

      real(dp), dimension(nRval) :: Rval
!
      character (len= 40) :: string
      character (len=256) :: line
      character (len=256), dimension(nCval) :: Cval
!
!-----------------------------------------------------------------------
!  Read requested ROMS standard input 1D integer parameter.
!-----------------------------------------------------------------------
!
!  Get standard input unit. If distribute-memory, open standard input
!  file for reading.
!
      InpUnit=stdinp_unit(localPET, GotFile)
!
!  Process requested parameter.
!
      foundit=.FALSE.
      DO WHILE (.TRUE.)
        READ (InpUnit,'(a)',ERR=10,END=30) line
        status=decode_line(line, string, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          IF (TRIM(string).eq.TRIM(KeyWord)) THEN
            Npts=load_i(Nval, Rval, Ndim, Value)
            foundit=.TRUE.
          END IF
        END IF
      END DO
  10  IF (localPET.eq.0) THEN
        WRITE (stdout,20) line
  20    FORMAT (/,' GETPAR_1D_I - Error while processing line: ',/,a)
      END IF
      exit_flag=4
  30  CONTINUE
      IF (.not.foundit) THEN
        IF (localPET.eq.0) THEN
          WRITE (stdout,40) TRIM(KeyWord)
  40    FORMAT (/,' GETPAR_1D_I - unable to find KeyWord: ',a,          &
     &          /,15x,'in ROMS standard input file.')
        END IF
        exit_flag=5
      END IF
      IF (GotFile) THEN
        CLOSE (InpUnit)
      END IF
!
      RETURN
      END SUBROUTINE getpar_1d_i
!
      SUBROUTINE getpar_0d_l (localPET, KeyWord, Value)
!
!***********************************************************************
!                                                                      !
!  Reads a scalar logical parameter from ROMS standard input file.     !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     localPET   Local Persistent Execution Thread (integer)           !
!     KeyWord    Keyword associated with input parameter (string)      !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Value      Standard input parameter value (logical)              !
!                                                                      !
!***********************************************************************
!
      USE mod_iounits, ONLY : stdout
      USE mod_scalars, ONLY : exit_flag
!
      USE strings_mod, ONLY : FoundError
!
!  Imported variable declarations.
!
      integer, intent(in)  :: localPET
      logical, intent(out) :: Value
!
      character (len=*), intent(in) :: KeyWord
!
!  Local variable declarations.
!
      logical :: foundit, GotFile
      logical :: Lvalue(1)

      integer :: InpUnit, Npts, Nval, status

      real(dp), dimension(nRval) :: Rval
!
      character (len= 40) :: string
      character (len=256) :: line
      character (len=256), dimension(nCval) :: Cval
!
!-----------------------------------------------------------------------
!  Read requested ROMS standard input logical parameter.
!-----------------------------------------------------------------------
!
!  Get standard input unit. If distribute-memory, open standard input
!  file for reading.
!
      InpUnit=stdinp_unit(localPET, GotFile)
!
!  Process requested parameter.
!
      foundit=.FALSE.
      DO WHILE (.TRUE.)
        READ (InpUnit,'(a)',ERR=10,END=30) line
        status=decode_line(line, string, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          IF (TRIM(string).eq.TRIM(KeyWord)) THEN
            Npts=load_l(Nval, Cval, 1, Lvalue)
            Value=Lvalue(1)
            foundit=.TRUE.
          END IF
        END IF
      END DO
  10  IF (localPET.eq.0) THEN
        WRITE (stdout,20) line
  20    FORMAT (/,' GETPAR_0D_L - Error while processing line: ',/,a)
      END IF
      exit_flag=4
  30  CONTINUE
      IF (.not.foundit) THEN
        IF (localPET.eq.0) THEN
          WRITE (stdout,40) TRIM(KeyWord)
  40    FORMAT (/,' GETPAR_0D_L - unable to find KeyWord: ',a,          &
     &          /,15x,'in ROMS standard input file.')
        END IF
        exit_flag=5
      END IF
      IF (GotFile) THEN
        CLOSE (InpUnit)
      END IF
!
      RETURN
      END SUBROUTINE getpar_0d_l
!
      SUBROUTINE getpar_1d_l (localPET, KeyWord, Ndim, Value)
!
!***********************************************************************
!                                                                      !
!  Reads a 1D logical parameter from ROMS standard input file.         !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     localPET   Local Persistent Execution Thread (integer)           !
!     KeyWord    Keyword associated with input parameter (string)      !
!     Ndim       Size logical variable dimension                       !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Value      Standard input parameter value (logical 1D array)     !
!                                                                      !
!***********************************************************************
!
      USE mod_iounits, ONLY : stdout
      USE mod_scalars, ONLY : exit_flag
!
      USE strings_mod, ONLY : FoundError
!
!  Imported variable declarations.
!
      logical, intent(out) :: Value(:)
      integer, intent(in)  :: localPET
      integer, intent(in)  :: Ndim
!
      character (len=*), intent(in) :: KeyWord
!
!  Local variable declarations.
!
      logical :: foundit, GotFile

      integer :: InpUnit, Npts, Nval, status

      real(dp), dimension(nRval) :: Rval
!
      character (len= 40) :: string
      character (len=256) :: line
      character (len=256), dimension(nCval) :: Cval
!
!-----------------------------------------------------------------------
!  Read requested ROMS standard input 1D logical parameter.
!-----------------------------------------------------------------------
!
!  Get standard input unit. If distribute-memory, open standard input
!  file for reading.
!
      InpUnit=stdinp_unit(localPET, GotFile)
!
!  Process requested parameter.
!
      foundit=.FALSE.
      DO WHILE (.TRUE.)
        READ (InpUnit,'(a)',ERR=10,END=30) line
        status=decode_line(line, string, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          IF (TRIM(string).eq.TRIM(KeyWord)) THEN
            Npts=load_l(Nval, Cval, Ndim, Value)
            foundit=.TRUE.
          END IF
        END IF
      END DO
  10  IF (localPET.eq.0) THEN
        WRITE (stdout,20) line
  20    FORMAT (/,' GETPAR_1D_L - Error while processing line: ',/,a)
      END IF
      exit_flag=4
  30  CONTINUE
      IF (.not.foundit) THEN
        IF (localPET.eq.0) THEN
          WRITE (stdout,40) TRIM(KeyWord)
  40    FORMAT (/,' GETPAR_1D_L - unable to find KeyWord: ',a,          &
     &          /,15x,'in ROMS standard input file.')
        END IF
        exit_flag=5
      END IF
      IF (GotFile) THEN
        CLOSE (InpUnit)
      END IF
!
      RETURN
      END SUBROUTINE getpar_1d_l
!
      SUBROUTINE getpar_0d_r (localPET, KeyWord, Value)
!
!***********************************************************************
!                                                                      !
!  Reads a scalar floating-point parameter from ROMS standard input    !
!  file.                                                               !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     localPET   Local Persistent Execution Thread (integer)           !
!     KeyWord    Keyword associated with input parameter (string)      !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Value      Standard input parameter value (real)                 !
!                                                                      !
!***********************************************************************
!
      USE mod_iounits, ONLY : stdout
      USE mod_scalars, ONLY : exit_flag
!
      USE strings_mod, ONLY : FoundError
!
!  Imported variable declarations.
!
      integer, intent(in)  :: localPET
      real(r8), intent(out) :: Value
!
      character (len=*), intent(in) :: KeyWord
!
!  Local variable declarations.
!
      logical :: foundit, GotFile

      integer :: InpUnit, Npts, Nval, status

      real(r8) :: Rvalue(1)

      real(dp), dimension(nRval) :: Rval
!
      character (len= 40) :: string
      character (len=256) :: line
      character (len=256), dimension(nCval) :: Cval
!
!-----------------------------------------------------------------------
!  Read requested ROMS standard input floating-point parameter.
!-----------------------------------------------------------------------
!
!  Get standard input unit. If distribute-memory, open standard input
!  file for reading.
!
      InpUnit=stdinp_unit(localPET, GotFile)
!
!  Process requested parameter.
!
      foundit=.FALSE.
      DO WHILE (.TRUE.)
        READ (InpUnit,'(a)',ERR=10,END=30) line
        status=decode_line(line, string, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          IF (TRIM(string).eq.TRIM(KeyWord)) THEN
            Npts=load_r(Nval, Rval, 1, Rvalue)
            Value=Rvalue(1)
            foundit=.TRUE.
          END IF
        END IF
      END DO
  10  IF (localPET.eq.0) THEN
        WRITE (stdout,20) line
  20    FORMAT (/,' GETPAR_0D_R - Error while processing line: ',/,a)
      END IF
      exit_flag=4
  30  CONTINUE
      IF (.not.foundit) THEN
        IF (localPET.eq.0) THEN
          WRITE (stdout,40) TRIM(KeyWord)
  40    FORMAT (/,' GETPAR_0D_R - unable to find KeyWord: ',a,          &
     &          /,15x,'in ROMS standard input file.')
        END IF
        exit_flag=5
      END IF
      IF (GotFile) THEN
        CLOSE (InpUnit)
      END IF
!
      RETURN
      END SUBROUTINE getpar_0d_r
!
      SUBROUTINE getpar_1d_r (localPET, KeyWord, Ndim, Value)
!
!***********************************************************************
!                                                                      !
!  Reads a 1D floating-point parameter from ROMS standard input file.  !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     localPET   Local Persistent Execution Thread (integer)           !
!     KeyWord    Keyword associated with input parameter (string)      !
!     Ndim       Size integer variable dimension                       !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Value      Standard input parameter value (real 1D array)        !
!                                                                      !
!***********************************************************************
!
      USE mod_iounits, ONLY : stdout
      USE mod_scalars, ONLY : exit_flag
!
      USE strings_mod, ONLY : FoundError
!
!  Imported variable declarations.
!
      integer, intent(in)  :: Ndim

      real(r8), intent(out) :: Value(:)
!
      character (len=*), intent(in) :: KeyWord
!
!  Local variable declarations.
!
      logical :: foundit, GotFile

      integer :: InpUnit, Npts, Nval, status

      real(dp), dimension(nRval) :: Rval
!
      character (len= 40) :: string
      character (len=256) :: line
      character (len=256), dimension(nCval) :: Cval
!
!-----------------------------------------------------------------------
!  Read requested ROMS standard input 1D floating-point parameter.
!-----------------------------------------------------------------------
!
!  Get standard input unit. If distribute-memory, open standard input
!  file for reading.
!
      InpUnit=stdinp_unit(localPET, GotFile)
!
!  Process requested parameter.
!
      foundit=.FALSE.
      DO WHILE (.TRUE.)
        READ (InpUnit,'(a)',ERR=10,END=30) line
        status=decode_line(line, string, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          IF (TRIM(string).eq.TRIM(KeyWord)) THEN
            Npts=load_r(Nval, Rval, Ndim, Value)
            foundit=.TRUE.
          END IF
        END IF
      END DO
  10  IF (localPET.eq.0) THEN
        WRITE (stdout,20) line
  20    FORMAT (/,' GETPAR_1D_R - Error while processing line: ',/,a)
      END IF
      exit_flag=4
  30  CONTINUE
      IF (.not.foundit) THEN
        IF (localPET.eq.0) THEN
          WRITE (stdout,40) TRIM(KeyWord)
  40    FORMAT (/,' GETPAR_1D_R - unable to find KeyWord: ',a,          &
     &          /,15x,'in ROMS standard input file.')
        END IF
        exit_flag=5
      END IF
      IF (GotFile) THEN
        CLOSE (InpUnit)
      END IF
!
      RETURN
      END SUBROUTINE getpar_1d_r
!
      END MODULE stdinp_mod
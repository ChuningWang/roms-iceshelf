/*
** svn $Id$
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2021 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
******************************************* Benjamin K. Galton-Fenzi ***
**                                                                    **
**  Defines iceshelf model input parameters in output NetCDF files.   **
**  It is used in routine "def_info.F".                               **
**                                                                    **
************************************************************************
*/

!
!  Define iceshelf model parameters.
!
      Vinfo( 1)='zice'
      Vinfo( 2)='ice draft'
      Vinfo( 3)='meter'
      status=def_var(ng, model, pioFile, pioVar, PIO_TYPE,              &
     &               1, (/iceshelfdim/), Aval, Vinfo, ncname,           &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

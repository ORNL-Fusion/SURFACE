!*******************************************************************************
!>  @file surface.f
!>  @brief Contains the main routines for SURFACE
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  SURFACE is a code for computing the vector potential on an arbitray wall
!>  structure. This uses the biot-savart law to compute the vector potential.
!>
!>  @author Mark Cianciosa
!*******************************************************************************
!*******************************************************************************
!  MAIN PROGRAM
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief SURFACE main program.
!>
!>  Highest level SURFACE routine.
!-------------------------------------------------------------------------------
      PROGRAM surface
      USE surface_context
      USE surface_commandline_parser
      USE bmw_parallel_context
      USE, INTRINSIC :: iso_fortran_env, Only : output_unit
      USE safe_open_mod

      IMPLICIT NONE

!  local variables
      CLASS (bmw_parallel_context_class), POINTER :: parallel => null()
      TYPE (surface_context_class), POINTER       :: context => null()
      CLASS (surface_commandline_parser_class), POINTER ::                     &
     &   cl_parser => null()
      INTEGER                                     :: io_unit
      INTEGER                                     :: status
      REAL (rprec)                                :: start_time

!  Start of executable code
#if defined (MPI_OPT)
      CALL MPI_INIT(status)
#endif
      CALL profiler_construct

      start_time = profiler_get_start_time()

      parallel => bmw_parallel_context_class(                                  &
#if defined (MPI_OPT)
     &                                       MPI_COMM_WORLD                    &
#endif
     &                                      )

      cl_parser => surface_commandline_parser_class(parallel)

!  Check if the required flags are set.
      IF (.not.cl_parser%is_flag_set('-woutf')) THEN
         WRITE (*,1001) '-woutf'
         CALL surface_commandline_parser_print_help
      END IF
      IF (.not.cl_parser%is_flag_set('-surff')) THEN
         WRITE (*,1001) '-surff'
         CALL surface_commandline_parser_print_help
      END IF

      CALL parallel%set_threads(cl_parser%get_integer('-para', 1))

      io_unit = output_unit
      IF (cl_parser%is_flag_set('-logf')) THEN
         CALL safe_open(io_unit, status, cl_parser%get('-logf'),               &
     &                  'replace', 'formatted', delim_in='none')
      END IF


      IF (parallel%offset .eq. 0) THEN
         WRITE (io_unit,*)
         WRITE (io_unit,1000) 1
         WRITE (io_unit,*)
      END IF
      CALL parallel%report(io_unit)

      context => surface_context_class(cl_parser%get('-woutf'),                &
     &                                 cl_parser%get('-surff'),                &
     &                                 parallel, io_unit)
      CALL context%set_vector_potential(io_unit, parallel)
      CALL context%write(cl_parser%get('-surff'), io_unit, parallel)

      DEALLOCATE(context)
      DEALLOCATE(cl_parser)

      CALL profiler_set_stop_time('surface_main', start_time)
      IF (parallel%offset .eq. 0) THEN
         CALL profiler_write(io_unit)
      END IF
      CALL profiler_destruct

      CLOSE(io_unit)

      DEALLOCATE(parallel)

1000  FORMAT('SURFACE verson ',i4,'.')
1001  FORMAT('Required flag ',a,' not set.')

      END PROGRAM

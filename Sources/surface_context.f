!*******************************************************************************
!>  @file surface_context.f
!>  @brief Contains module @ref surface_context.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref surface_context_class. This module
!>  contains all the code necessary define a surface for cariddi to read a
!>  virtual surface from.
!*******************************************************************************

      MODULE surface_context
      USE stel_kinds, ONLY: rprec
      USE vmec_file

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) surface context class.
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a surface context. This contains all memory needed
!>  to operate surface.
!-------------------------------------------------------------------------------
      TYPE :: surface_context_class
!>  Array of radial points on the current surface.
         REAL (rprec), DIMENSION(:), POINTER :: x
!>  Array of phi points on the current surface.
         REAL (rprec), DIMENSION(:), POINTER :: y
!>  Array of z points on the current surface.
         REAL (rprec), DIMENSION(:), POINTER :: z
!>  Virtual Surface current in the r direction.
         REAL (rprec), DIMENSION(:), POINTER :: a_x
!>  Virtual Surface current in the phi direction.
         REAL (rprec), DIMENSION(:), POINTER :: a_y
!>  Virtual Surface current in the z direction.
         REAL (rprec), DIMENSION(:), POINTER :: a_z
!>  VMEC equilibrium object.
         CLASS (vmec_file_class), POINTER    :: vmec => null()
      CONTAINS
         PROCEDURE, PASS :: set_vector_potential =>                            &
     &      surface_context_set_vector_potential
         PROCEDURE, PASS :: write => surface_context_write
         FINAL :: surface_context_destruct
      END TYPE

!-------------------------------------------------------------------------------
!>  Interface to constructors.
!-------------------------------------------------------------------------------
      INTERFACE surface_context_class
         MODULE PROCEDURE surface_context_construct
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref surface_context_class object.
!>
!>  Allocates memory and initializes a @ref surface_context_class object.
!>
!>  @param[in] wout_file_name    Path and name of the wout file.
!>  @param[in] surface_file_name Path defining the surface to compute fields to.
!>  @param[in] parallel          @ref bmw_parallel_context_class object
!>                               instance.
!>  @param[in] io_unit           Unit number to write messages to.
!>  @returns A pointer to a constructed @ref bmw_context_class object.
!-------------------------------------------------------------------------------
      FUNCTION surface_context_construct(wout_file_name,                       &
     &                                   surface_file_name, parallel,          &
     &                                   io_unit)
      USE bmw_parallel_context
      USE ezcdf

      IMPLICIT NONE

!  Declare Arguments
      CLASS (surface_context_class), POINTER ::                                &
     &   surface_context_construct
      CHARACTER (len=*), INTENT(in)          :: wout_file_name
      CHARACTER (len=*), INTENT(in)          :: surface_file_name
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel
      INTEGER, INTENT(in)                    :: io_unit

!  local variables
      REAL (rprec)                           :: start_time
      INTEGER                                :: status
      INTEGER                                :: iou
      INTEGER                                :: size
      INTEGER                                :: i
      INTEGER                                :: ncid
      INTEGER                                :: size_dim
      INTEGER                                :: xid
      INTEGER                                :: yid
      INTEGER                                :: zid
      INTEGER                                :: xnid
      INTEGER                                :: ynid
      INTEGER                                :: znid

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(surface_context_construct)

      surface_context_construct%vmec =>                                        &
     &   vmec_file_class(TRIM(wout_file_name))

!  Parse the surface file.
      status = NF90_OPEN(TRIM(surface_file_name), NF90_NOWRITE, ncid)

      status = NF90_INQ_DIMID(ncid, 'ng', size_dim)
      status = NF90_INQUIRE_DIMENSION(ncid, size_dim, len=size)

      ALLOCATE(surface_context_construct%x(size))
      ALLOCATE(surface_context_construct%y(size))
      ALLOCATE(surface_context_construct%z(size))

      ALLOCATE(surface_context_construct%a_x(size))
      ALLOCATE(surface_context_construct%a_y(size))
      ALLOCATE(surface_context_construct%a_z(size))

      status = NF90_INQ_VARID(ncid, 'x', xid)
      status = NF90_INQ_VARID(ncid, 'y', yid)
      status = NF90_INQ_VARID(ncid, 'z', zid)

      status = NF90_GET_VAR(ncid, xid, surface_context_construct%x)
      status = NF90_GET_VAR(ncid, yid, surface_context_construct%y)
      status = NF90_GET_VAR(ncid, zid, surface_context_construct%z)

      status = NF90_CLOSE(ncid)

      CALL profiler_set_stop_time('surface_context_construct',                 &
     &                            start_time)

1000  FORMAT(a,' is an invalid wout file.')
1001  FORMAT(a,' is not a free boundary wout file.')

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref bmw_context_class object.
!>
!>  Deallocates memory and uninitializes a @ref bmw_context_class object.
!>
!>  @param[inout] this A @ref bmw_context_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE surface_context_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (surface_context_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%x)) THEN
         DEALLOCATE(this%x)
         this%x => null()
      END IF

      IF (ASSOCIATED(this%y)) THEN
         DEALLOCATE(this%y)
         this%y => null()
      END IF

      IF (ASSOCIATED(this%z)) THEN
         DEALLOCATE(this%z)
         this%z => null()
      END IF

      IF (ASSOCIATED(this%a_x)) THEN
         DEALLOCATE(this%a_x)
         this%a_x => null()
      END IF

      IF (ASSOCIATED(this%a_y)) THEN
         DEALLOCATE(this%a_y)
         this%a_y => null()
      END IF

      IF (ASSOCIATED(this%a_z)) THEN
         DEALLOCATE(this%a_z)
         this%a_z => null()
      END IF

      IF (ASSOCIATED(this%vmec)) THEN
         DEALLOCATE(this%vmec)
         this%vmec => null()
      END IF

      END SUBROUTINE

!*******************************************************************************
!  SETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Set the vector potential on the surface.
!>
!>  This uses the biot-savart to set a vector potential on an arbitrary
!>  surface. At each point in the surface, the plasma
!>  current in volume integrated.
!>
!>    b = mu0/4pi Int Int Int (Jsupu*esubu + Jsupv*esubv) /
!>        |x - xp|*signJ*J*da*du*dv                                          (1)
!>
!>  Where the primed (p) variables are the values on the last closed flux
!>  surface grid.
!>
!>  @param[inout] this     A @ref surface_context_class instance.
!>  @param[in]    io_unit  Unit number to write messages to.
!>  @param[in]    parallel @ref bmw_parallel_context_class object instance.
!>  @returns A pointer to a constructed @ref bmw_context_class object.
!-------------------------------------------------------------------------------
      SUBROUTINE surface_context_set_vector_potential(this, io_unit,           &
     &                                                parallel)
      USE bmw_parallel_context
      USE stel_constants, ONLY: twopi, mu0
      USE bmw_state_flags, ONLY: clear_screen, progress,                       &
     &                           bmw_state_flags_off
      USE, INTRINSIC :: iso_fortran_env, Only : output_unit
      USE primed_grid
!$    USE omp_lib

!  Declare Arguments
      CLASS (surface_context_class), INTENT(inout)   :: this
      INTEGER, INTENT(in)                            :: io_unit
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel

!  local variables
      REAL (rprec)                                   :: start_time
      REAL (rprec)                                   :: total
      REAL (rprec)                                   :: done
      REAL (rprec)                                   :: current
      REAL (rprec), DIMENSION(:,:,:), ALLOCATABLE    :: lx
      REAL (rprec), DIMENSION(:,:,:), ALLOCATABLE    :: ly
      REAL (rprec), DIMENSION(:,:,:), ALLOCATABLE    :: lz
      REAL (rprec), DIMENSION(:,:,:), ALLOCATABLE    :: l
      INTEGER                                        :: i
      CLASS (primed_grid_class), POINTER             :: p_grid

!  Start of executable code
      start_time = profiler_get_start_time()

      p_grid => primed_grid_class(101, bmw_state_flags_off,                    &
     &                            this%vmec, '', '', parallel, io_unit)

      total = parallel%end(SIZE(this%x))
      total = CEILING(total/parallel%num_threads)
      current = 0.0

!$OMP PARALLEL
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i, lx, ly, lz, l)

      ALLOCATE(lx(SIZE(p_grid%x, 1),                                           &
     &            SIZE(p_grid%x, 2),                                           &
     &            SIZE(p_grid%x, 3)))
      ALLOCATE(ly(SIZE(p_grid%x, 1),                                           &
     &            SIZE(p_grid%x, 2),                                           &
     &            SIZE(p_grid%x, 3)))
      ALLOCATE(lz(SIZE(p_grid%x, 1),                                           &
     &            SIZE(p_grid%x, 2),                                           &
     &            SIZE(p_grid%x, 3)))
      ALLOCATE(l(SIZE(p_grid%x, 1),                                            &
     &           SIZE(p_grid%x, 2),                                            &
     &           SIZE(p_grid%x, 3)))

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(SIZE(this%x)), parallel%end(SIZE(this%x))
         lx = this%x(i) - p_grid%x
         ly = this%y(i) - p_grid%y
         lz = this%z(i) - p_grid%z

         l = SQRT(lx*lx + ly*ly + lz*lz)

         this%a_x(i) = SUM(p_grid%j_x/l)*p_grid%dvol
         this%a_y(i) = SUM(p_grid%j_y/l)*p_grid%dvol
         this%a_z(i) = SUM(p_grid%j_z/l)*p_grid%dvol

!  Progress Reporting
         IF (parallel%offset .eq. 0) THEN
!$          IF (OMP_GET_THREAD_NUM() .eq. 0) THEN
               current = current + 1.0
               done = 100.0*current/total

               WRITE (io_unit,1000,ADVANCE='NO')                               &
     &            clear_screen, progress(MOD(INT(current),4)), done

               IF (io_unit .ne. output_unit) THEN
                  BACKSPACE (io_unit)
               END IF
!$          END IF
         END IF
      END DO
!$OMP END DO

      DEALLOCATE(lx)
      DEALLOCATE(ly)
      DEALLOCATE(lz)
      DEALLOCATE(l)

!$OMP END PARALLEL

      IF (parallel%stride .gt. 1) THEN
         CALL parallel%reduce(this%a_x)
         CALL parallel%reduce(this%a_y)
         CALL parallel%reduce(this%a_z)
      END IF

      DEALLOCATE(p_grid)
      p_grid => null()

      IF (parallel%offset .eq. 0) THEN
         WRITE (io_unit,1001) clear_screen
      END IF

      CALL profiler_set_stop_time('surface_context_set_current',               &
     &                            start_time)

1000  FORMAT(a,a,f6.2,' % Finished')
1001  FORMAT(a,'Coupling Grid Finished')

      END SUBROUTINE

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
      SUBROUTINE surface_context_write(this, output_file, io_unit,             &
     &                                 parallel)
      USE bmw_parallel_context
      USE ezcdf

      IMPLICIT NONE

!  Declare Arguments
      CLASS (surface_context_class), INTENT(inout)   :: this
      CHARACTER (len=*), INTENT(in)                  :: output_file
      INTEGER, INTENT(in)                            :: io_unit
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel

!  local variables
      INTEGER                                        :: iou
      INTEGER                                        :: i
      INTEGER                                        :: status
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: ncid
      INTEGER                                        :: size_dim
      INTEGER                                        :: axid
      INTEGER                                        :: ayid
      INTEGER                                        :: azid

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (parallel%offset .eq. 0) THEN
         status = NF90_OPEN(TRIM(output_file), NF90_WRITE, ncid)

         status = NF90_INQ_DIMID(ncid, 'ng', size_dim)

         status = NF90_INQ_VARID(ncid, 'ax', axid)
         IF (status .ne. NF90_NOERR) THEN
            status = NF90_REDEF(ncid)
            status = NF90_DEF_VAR(ncid, 'ax', NF90_DOUBLE,                     &
     &                            dimids=size_dim, varid=axid)
            status = NF90_ENDDEF(ncid)
         END IF
         status = NF90_INQ_VARID(ncid, 'ay', ayid)
         IF (status .ne. NF90_NOERR) THEN
            status = NF90_REDEF(ncid)
            status = NF90_DEF_VAR(ncid, 'ay', NF90_DOUBLE,                     &
     &                            dimids=size_dim, varid=ayid)
            status = NF90_ENDDEF(ncid)
         END IF
         status = NF90_INQ_VARID(ncid, 'az', azid)
         IF (status .ne. NF90_NOERR) THEN
            status = NF90_REDEF(ncid)
            status = NF90_DEF_VAR(ncid, 'az', NF90_DOUBLE,                     &
     &                            dimids=size_dim, varid=azid)
            status = NF90_ENDDEF(ncid)
         END IF

         status = NF90_PUT_VAR(ncid, axid, this%a_x)
         status = NF90_PUT_VAR(ncid, ayid, this%a_y)
         status = NF90_PUT_VAR(ncid, azid, this%a_z)

         status = NF90_CLOSE(ncid)
      END IF

      CALL profiler_set_stop_time('surface_context_write', start_time)

      END SUBROUTINE

      END MODULE

!-------------------------------------------------------------------------------
!  The @header2, @begin_table, @item3 and @end_table commands are custom defined
!  commands in Doxygen.in. They are defined under ALIASES. For the page created
!  here, the 80 column limit is exceeded. Arguments of aliases are separated by
!  ','. If you intended ',' to be a string you must use an escaped comma '\,'.
!
!>  @page surface_cl_parsing_sec Command Line Arguments
!>
!>  @tableofcontents
!>
!>  @section surface_cl_parsing_intro Introduction
!>  This contains a description of the command line arguments. All arguments
!>  take the form of
!>
!>  @fixed_width{-arg=value}
!>
!>  @section surface_cl_parsing_arg_sec Command Line Arguments
!>  @header2{Argument, Takes Value, Discription}
!>  @begin_table
!>     @item3{@fixed_width{-h},       N, Displays the help text and exits the program.}
!>     @item3{@fixed_width{-woutf},   Y, Specify the wout input file name.}
!>     @item3{@fixed_width{-surff},   Y, Specify the surface file name.}
!>     @item3{@fixed_width{-logf},    Y, Write screen output to a log file.}
!>     @item3{@fixed_width{-para},    Y, Configures openmp parallelism. The value of this flag sets the maximum
!>                                       number of threads to use. A value of @fixed_width{-1} uses the default.}
!>  @end_table
!>
!>  @note
!>  OpenMP and MPI support must be configured at compile time to use. A simple way to check if support has been
!>  included is to check the command line help @fixed_width{-h}.
!>
!>  @section surface_cl_pasring_prog_ref_sec Programmers Reference
!>  Reference material for the coding to implement command line parsing is found
!>  in the @ref surface_commandline_parser module.
!-------------------------------------------------------------------------------
!*******************************************************************************
!>  @file surface_commandline_parser.f
!>  @brief Contains module @ref surface_commandline_parser
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref surface_commandline_parser_class.
!*******************************************************************************

      MODULE surface_commandline_parser
      USE stel_kinds
      USE file_opts, only: path_length
      USE profiler
      USE bmw_parallel_context

      IMPLICIT NONE

!*******************************************************************************
!  surface_commandline parser module parameters
!*******************************************************************************
!>  Maximum length of the argument including the '-' character.
      INTEGER, PARAMETER, PRIVATE :: max_arg_length = 8
!>  Maximum length of the complete flag. All command line flags take the form of
!>  '-flag=value'.
      INTEGER, PARAMETER, PRIVATE :: max_length = path_length                  &
     &                                          + max_arg_length + 1

!  Commandline parser error codes.
!>  Commandline argument not found.
      INTEGER, PARAMETER :: surface_commandline_parser_no_error = 0
!>  Commandline argument not found.
      INTEGER, PARAMETER ::                                                    &
     &   surface_commandline_parser_arg_not_found = -1

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) surface_commandline parser base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class containing a parsed surface_commandline.
!-------------------------------------------------------------------------------
      TYPE :: surface_commandline_parser_class
!>  Command name of current process.
         CHARACTER (len=max_length) :: command
!>  Array of arguments. An argument is the form of -flag
         CHARACTER (len=max_arg_length), DIMENSION(:), POINTER ::              &
     &      arg => null()
!>  Array of value corresponding to the flag. A value is everything after the
!>  '=' character. The value maybe blank indicating there was no value provided.
         CHARACTER (len=path_length), DIMENSION(:), POINTER ::                 &
     &      value => null()
      CONTAINS
         PROCEDURE, PASS ::                                                    &
     &      get_string => surface_commandline_parser_get_string
         PROCEDURE, PASS ::                                                    &
     &      get_integer => surface_commandline_parser_get_integer
         PROCEDURE, PASS ::                                                    &
     &      get_real => surface_commandline_parser_get_real
         GENERIC         :: get => get_string, get_integer, get_real
         PROCEDURE, PASS ::                                                    &
     &      is_flag_set => surface_commandline_parser_is_flag_set
         PROCEDURE, PASS :: flag_requires_value =>                             &
     &      surface_commandline_parser_flag_requires_value
         FINAL           :: surface_commandline_parser_destruct
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the bmw_commandline_parser constructor.
!-------------------------------------------------------------------------------
      INTERFACE surface_commandline_parser_class
         MODULE PROCEDURE surface_commandline_parser_construct
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref surface_commandline_parser_class object.
!>
!>  Allocates memory and initializes a @ref surface_commandline_parser_class
!>  object. The command line is parsed assuming the the flags take the form of
!>  -flag=value. If the '=' is missing, it is assumed there is no value. If -h
!>  is found, the help text is printed out and the program terminated.
!>
!>  @param[in] parallel @ref bmw_parallel_context_class object instance.
!>  @returns A pointer to a constructed @ref surface_commandline_parser_class
!>           object.
!-------------------------------------------------------------------------------
      FUNCTION surface_commandline_parser_construct(parallel)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (surface_commandline_parser_class), POINTER ::                     &
     &   surface_commandline_parser_construct
      CLASS (bmw_parallel_context_class), INTENT(in)    :: parallel

!  Local Variables
      CHARACTER (len=max_length) :: temp
      INTEGER                    :: num_args
      INTEGER                    :: i
      INTEGER                    :: value_index
      REAL (rprec)               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      temp = ''
      num_args = 0
      value_index = 0
      ALLOCATE(surface_commandline_parser_construct)

!  Read the zeroith arg to get the number of arguments. This should also be the
!  command name.
      CALL getcarg(0, surface_commandline_parser_construct%command,            &
     &             num_args)

!  Allocate the arrays and
      ALLOCATE(surface_commandline_parser_construct%arg(num_args))
      ALLOCATE(surface_commandline_parser_construct%value(num_args))

!  Loop through the command line arguments, and setup the argument and value
!  arrays
      DO i = 1, num_args
         CALL getcarg(i, temp, num_args)

!  Check for a - as the first character.
         IF (temp(1:1) .eq. '-') THEN
            value_index = INDEX(temp, '=')
            IF (value_index .eq. 0) THEN
!  Check for help command.
               IF (TRIM(temp) .eq. '-h' .and.                                  &
     &             parallel%offset .eq. 0) THEN
                  CALL surface_commandline_parser_print_help
               END IF

               surface_commandline_parser_construct%arg(i) = TRIM(temp)
               surface_commandline_parser_construct%value(i) = ''
            ELSE
               surface_commandline_parser_construct%arg(i) =                   &
     &            temp(1:value_index - 1)
               surface_commandline_parser_construct%value(i) =                 &
     &            temp(value_index + 1:LEN_TRIM(temp))
            END IF
          END IF

          CALL surface_commandline_parser_flag_requires_value(                 &
     &            surface_commandline_parser_construct, i)
      END DO

      CALL profiler_set_stop_time(                                             &
     &        'surface_commandline_parser_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref surface_commandline_parser_class object.
!>
!>  Deallocates memory and uninitializes a @ref surface_commandline_parser_class
!>  object.
!>
!>  @param[inout] this A @ref surface_commandline_parser_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE surface_commandline_parser_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (surface_commandline_parser_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%arg)) THEN
         DEALLOCATE(this%arg)
         this%arg => null()
      END IF

      IF (ASSOCIATED(this%value)) THEN
         DEALLOCATE(this%value)
         this%value => null()
      END IF

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Get the value of an argument as a string.
!>
!>  Searches the parsed command line flags for a matching command like argument.
!>  If no matching argument is found or no value was set, a blank string is
!>  returned. command line arguments take the form of -flag.
!>
!>  @param[in] this A @ref surface_commandline_parser_class instance.
!>  @param[in] arg  A argument string to search for.
!>  @returns The argument value.
!-------------------------------------------------------------------------------
      FUNCTION surface_commandline_parser_get_string(this, arg)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=path_length) ::                                           &
     &   surface_commandline_parser_get_string
      CLASS (surface_commandline_parser_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)                        :: arg

!  Local arguments
      INTEGER                                              :: i
      REAL (rprec)                                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Loop through the arguments until the correct arg is found.
      surface_commandline_parser_get_string = ''

      IF (ASSOCIATED(this%arg)) THEN
         DO i = 1, SIZE(this%arg)
            IF (TRIM(this%arg(i)) .eq. TRIM(arg)) THEN
               surface_commandline_parser_get_string = this%value(i)

               CALL profiler_set_stop_time(                                    &
     &                 'surface_commandline_parser_get_string',                &
     &                 start_time)

               RETURN
            END IF
         END DO
      END IF

      CALL profiler_set_stop_time(                                             &
     &        'surface_commandline_parser_get_string', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the value of an argument as an integer.
!>
!>  Searches the parsed command line flags for a matching command like argument.
!>  If no matching argument is found, no value was set or the value cannot be
!>  converted to an integer, the default value is returned. Command line
!>  arguments take the form of -flag.
!>
!>  @param[in] this          A @ref surface_commandline_parser_class instance.
!>  @param[in] arg           A argument string to search for.
!>  @param[in] default_value Default value in case flag is not set.
!>  @returns The argument value.
!-------------------------------------------------------------------------------
      FUNCTION surface_commandline_parser_get_integer(this, arg,               &
     &                                                default_value)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: surface_commandline_parser_get_integer
      CLASS (surface_commandline_parser_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in) :: arg
      INTEGER, INTENT(in)           :: default_value

!  Local arguments
      CHARACTER (len=path_length)   :: value
      INTEGER                       :: status
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      value = this%get(arg)

      IF (TRIM(value) .eq. '') THEN
         surface_commandline_parser_get_integer = default_value

         CALL profiler_set_stop_time(                                          &
     &           'surface_commandline_parser_get_integer', start_time)

         RETURN
      END IF

      READ (value,1000,iostat=status)                                          &
     &   surface_commandline_parser_get_integer

      IF (status .ne. 0) THEN
         surface_commandline_parser_get_integer = default_value
      END IF

      CALL profiler_set_stop_time(                                             &
     &   'surface_commandline_parser_get_integer', start_time)

1000  FORMAT(i20)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the value of an argument as a Real.
!>
!>  Searches the parsed command line flags for a matching command like argument.
!>  If no matching argument is found, no value was set or the value cannot be
!>  converted to an integer, the default value is returned. Command line
!>  arguments take the form of -flag.
!>
!>  @param[in] this          A @ref surface_commandline_parser_class instance.
!>  @param[in] arg           A argument string to search for.
!>  @param[in] default_value Default value in case flag is not set.
!>  @returns The argument value.
!-------------------------------------------------------------------------------
      FUNCTION surface_commandline_parser_get_real(this, arg,                  &
     &                                             default_value)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: surface_commandline_parser_get_real
      CLASS (surface_commandline_parser_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in) :: arg
      REAL (rprec), INTENT(in)      :: default_value

!  Local arguments
      CHARACTER (len=path_length)   :: value
      INTEGER                       :: status
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      value = this%get(arg)

      IF (TRIM(value) .eq. '') THEN
         surface_commandline_parser_get_real = default_value

         CALL profiler_set_stop_time(                                          &
     &           'surface_commandline_parser_get_real', start_time)

         RETURN
      END IF

      READ (value,*,iostat=status) surface_commandline_parser_get_real

      IF (status .ne. 0) THEN
         surface_commandline_parser_get_real = default_value
      END IF

      CALL profiler_set_stop_time(                                             &
     &        'surface_commandline_parser_get_real', start_time)

      END FUNCTION

!*******************************************************************************
!  QUERY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Check if a command line argument was set.
!>
!>  Searches the parsed command line flags for a matching command line argument.
!>  If no matching argument is found the flag was not set.
!>
!>  @param[in] this A @ref surface_commandline_parser_class instance.
!>  @param[in] arg  A argument string to search for.
!>  @returns True if the argument was found, false if it was not found.
!-------------------------------------------------------------------------------
      FUNCTION surface_commandline_parser_is_flag_set(this, arg)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL :: surface_commandline_parser_is_flag_set
      CLASS (surface_commandline_parser_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in) :: arg

!  Local arguments
      INTEGER                       :: i
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Loop through the arguments until the correct arg is found.
      surface_commandline_parser_is_flag_set = .false.

      IF (ASSOCIATED(this%arg)) THEN
         DO i = 1, SIZE(this%arg)
            IF (TRIM(this%arg(i)) .eq. TRIM(arg)) THEN
               surface_commandline_parser_is_flag_set = .true.

               CALL profiler_set_stop_time(                                    &
     &                 'surface_commandline_parser_is_flag_set',               &
     &                 start_time)

               RETURN
            END IF
         END DO
      END IF

      CALL profiler_set_stop_time(                                             &
     &        'surface_commandline_parser_is_flag_set', start_time)

      END FUNCTION

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Check if a command line argument requires a value.
!>
!>  Command line arguments for
!>
!>    * -woutf
!>    * -surff
!>    * -logf
!>
!>  require a value to be set.
!>
!>  @param[in] this A @ref surface_commandline_parser_class instance.
!>  @param[in] index  A argument string to search for.
!>  @returns True if the argument has a value.
!-------------------------------------------------------------------------------
      SUBROUTINE surface_commandline_parser_flag_requires_value(this,          &
     &                                                          index)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (surface_commandline_parser_class), INTENT(in) :: this
      INTEGER, INTENT(in)                                  :: index

!  Local arguments
      REAL (rprec)                                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (TRIM(this%arg(index)))

         CASE ('-woutf','-surff','-logf')
            IF (TRIM(this%value(index)) .eq. '') THEN
               WRITE (*,1000) TRIM(this%arg(index)),                           &
     &                        TRIM(this%arg(index))

               CALL surface_commandline_parser_print_help
            END IF

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'surface_commandline_parser_flag_requires_value',                &
     &        start_time)

1000  FORMAT(a,' flag requires value. Usage: ',a,'=value')

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Print out help text.
!>
!>  Command line help text should take the form of '-flag' that is maximum
!>  length of @ref max_arg_length followed by a space. A Y or N indicating the
!>  flag takes a value or not followed by a space. A short message describing
!>  the flag. A reference for all the command line arguments can be found at
!>  @ref cl_parsing_sec.
!-------------------------------------------------------------------------------
      SUBROUTINE surface_commandline_parser_print_help

      IMPLICIT NONE

!  Start of executable code
!  All command line messages need to fit within this width.
!                '           s              c                          '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '                       SURFACE                       '
      WRITE(*,*) '                                                     '
      WRITE(*,*) 'Usage: xsurface [-arg][=option] ...                  '
      WRITE(*,*) '                                                     '
      WRITE(*,*) 'Options:                                             '
      WRITE(*,*) 'All options are displayed as [arg][takesoption][text]'
      WRITE(*,*) '  -h       N Display this information                '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -woutf   Y Specify the wout file name.             '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -surff   Y Specify the surface file name.          '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -logf    Y Write screen output to a log file.      '
!$    WRITE(*,*) '                                                     '
!$    WRITE(*,*) '  -para    Y Determines number of threads to run     '
!$    WRITE(*,*) '             with. A value of -1 means use the       '
!$    WRITE(*,*) '             default number of threads.              '
!$    WRITE(*,*) '                                                     '

      CALL bmw_parallel_context_abort(0)

      END SUBROUTINE

      END MODULE


module fynth__app

	! TODO: split to app.f90

	use fynth
	use fynth__utils

	implicit none

	type args_t

		character(len = :), allocatable :: file1

		double precision :: sine_freq, sine_len
		double precision :: square_freq, square_len

		logical :: &
			has_file1 = .false., &
			version   = .false., &
			sine      = .false., &
			square    = .false., &
			help      = .false.

	end type args_t

contains

!===============================================================================

subroutine get_next_arg(i, argv)
	integer, intent(inout) :: i
	character(len = :), allocatable, intent(out) :: argv
	!********
	character(len = :), allocatable, save :: argv0
	character(len = 1024) :: buffer
	integer, parameter :: STAT_TRUNC = -1
	integer :: io, argc
	logical, save :: first = .true.

	if (first) then
		first = .false.
		call get_command_argument(0, buffer)
		argv0 = trim(buffer)
	end if

	i = i + 1
	argc = command_argument_count()
	if (i > argc) then
		call panic("missing required argument after """//argv0//"""")
	end if

	call get_command_argument(i, buffer, status = io)
	if (io == STAT_TRUNC) then
		! Could make buffer allocatable and automatically try resizing
		call panic("command argument too long after """//argv0//"""")

	else if (io /= EXIT_SUCCESS) then
		call panic("cannot get command argument after """//argv0//"""")

	end if
	argv = trim(buffer)
	!print *, "argv = ", argv

	argv0 = argv

end subroutine get_next_arg

!===============================================================================

function read_args() result(args)

	! This argument parser is based on http://docopt.org/
	!
	! c.f. github.com/jeffirwin/cali and syntran

	type(args_t) :: args

	!********

	character(len = :), allocatable :: argv, str, url, version

	integer :: i, io, argc, ipos

	logical :: error = .false.

	!! Defaults
	!args%maxerr = maxerr_def

	argc = command_argument_count()
	!print *, "argc = ", argc

	i = 0
	ipos = 0
	do while (i < argc)
		call get_next_arg(i, argv)

		select case (argv)

		case ("-h", "--help", "-help")
			args%help    = .true.

		!case ("--fmax-errors")
		!	call get_next_arg(i, str)
		!	read(str, *, iostat = io) args%maxerr
		!	if (io /= exit_success) then
		!		write(*,*) ERROR_STR//"--fmax-errors "//str &
		!			//" is not a valid integer"
		!		error = .true.
		!	end if

		case ("--version")
			args%version = .true.

		case ("--sin", "--sine")
			args%sine = .true.

			call get_next_arg(i, str)
			read(str, *, iostat = io) args%sine_freq
			if (io /= 0) then
				write(*,*) ERROR_STR//argv//" frequency """ &
					//str//""" is not a valid number"
				error = .true.
			end if

			call get_next_arg(i, str)
			read(str, *, iostat = io) args%sine_len
			if (io /= 0) then
				write(*,*) ERROR_STR//argv//" length """ &
					//str//""" is not a valid number"
				error = .true.
			end if

		case ("--squ", "--square")
			args%square = .true.

			call get_next_arg(i, str)
			read(str, *, iostat = io) args%square_freq
			if (io /= 0) then
				write(*,*) ERROR_STR//argv//" frequency """ &
					//str//""" is not a valid number"
				error = .true.
			end if

			call get_next_arg(i, str)
			read(str, *, iostat = io) args%square_len
			if (io /= 0) then
				write(*,*) ERROR_STR//argv//" length """ &
					//str//""" is not a valid number"
				error = .true.
			end if

		case default

			! Positional arg
			ipos = ipos + 1

			if (ipos == 1) then
				args%has_file1 = .true.
				args%file1 = argv

			!else if (ipos == 2) then
			!	args%lout_file = .true.
			!	args%out_file  = argv

			else
				write(*,*) ERROR_STR//"bad argument `"//argv//"`"
				error = .true.

			end if

		end select

	end do

	!if (ipos < 1 .and. .not. (args%help .or. args%version)) then
	!	write(*,*) ERROR_STR//"input file not defined"
	!	error = .true.
	!end if
	if (args%sine .and. .not. args%has_file1) then
		write(*,*) ERROR_STR//"output file arg not defined for --sine"
		error = .true.
	end if

	if (args%square .and. .not. args%has_file1) then
		write(*,*) ERROR_STR//"output file arg not defined for --square"
		error = .true.
	end if

	url = "https://github.com/JeffIrwin/fynth"

	version = &
		to_str(FYNTH_MAJOR)//"."// &
		to_str(FYNTH_MINOR)//"."// &
		to_str(FYNTH_PATCH)

	if (.not. error) then
		write(*,*)
		write(*,*) fg_bright_magenta//"fynth "//version//color_reset
		write(*,*) fg_bright_magenta//url//color_reset
		write(*,*)
	end if

	!if (args%has_file1) print *, "file1 = """, args%file1, """"

	if (error .or. args%help) then

		write(*,*) fg_bold//"Usage:"//color_reset
		write(*,*) "	fynth -h | --help"
		write(*,*) "	fynth --version"
		write(*,*) "	fynth out-file.wav (--sine|--square) <frequency> <length>"
		write(*,*)
		write(*,*) fg_bold//"Options:"//color_reset
		write(*,*) "	-h --help        Show this help"
		write(*,*) "	--version        Show version"
		write(*,*)

		if (.not. args%help) call fynth_exit(EXIT_FAILURE)
	end if

end function read_args

!===============================================================================

end module fynth__app

!===============================================================================

program main

	use fynth
	use fynth__app

	implicit none

	type(args_t)  :: args

	!********

	args = read_args()
	if (args%help .or. args%version) then
		call fynth_exit(EXIT_SUCCESS)
	end if

	if (args%sine) then
		call write_wav_sine(args%file1, args%sine_freq, args%sine_len)
		call fynth_exit(EXIT_SUCCESS)
	end if

	if (args%square) then
		call write_wav_square(args%file1, args%square_freq, args%square_len)
		call fynth_exit(EXIT_SUCCESS)
	end if

	call write_wav_test("test.wav")
	call write_wav_licc("licc.wav")
	call fynth_exit(EXIT_SUCCESS)

end program main


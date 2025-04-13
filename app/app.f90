
module fynth__app

	! TODO: split to app.f90

	use fynth
	use fynth__utils

	implicit none

	type args_t

		! File arguments can be input or output files, depending on other
		! arguments
		character(len = :), allocatable :: file1, file2

		double precision :: sine_freq, sine_len
		double precision :: square_freq, square_len

		logical :: &
			has_file1 = .false., &
			has_file2 = .false., &
			sine      = .false., &
			square    = .false., &
			licc      = .false., &
			version   = .false., &
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
	! c.f. github.com/jeffirwin/cali, syntran, ribbit, etc.

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

		case ("--version")
			args%version = .true.

		case ("--licc")
			args%licc = .true.

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

			else if (ipos == 2) then
				args%has_file2 = .true.
				args%file2 = argv

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

	if (args%licc .and. .not. args%has_file1) then
		write(*,*) ERROR_STR//"output file arg not defined for --licc"
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
		write(*,*) "	fynth <out.wav> (--sine|--square) <frequency> <length>"
		write(*,*) "	fynth <out.wav> --licc"
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



module fynth__app

	use fynth
	use fynth__audio
	use fynth__utils

	implicit none

	type args_t

		! File arguments can be input or output files, depending on other
		! arguments
		character(len = :), allocatable :: file1, file2

		double precision :: freq, len_

		double precision :: low_pass_freq
		double precision :: two_pole_cutoff !, two_pole_resonance

		logical :: &
			has_file1    = .false., &
			has_file2    = .false., &
			sine         = .false., &
			square       = .false., &
			triangle     = .false., &
			sawtooth     = .false., &
			noise        = .false., &
			has_waveform = .false., &
			has_env      = .false., &
			has_fenv     = .false., &
			two_pole     = .false., &
			low_pass     = .false., &
			fft          = .false., &
			licc         = .false., &
			version      = .false., &
			help         = .false.

		type(env_t) :: env, fenv

		type(waveform_t) :: waveform

	end type args_t

contains

!===============================================================================

function read_args() result(args)

	! This argument parser is based on http://docopt.org/
	!
	! c.f. github.com/jeffirwin/cali, syntran, ribbit, etc.

	type(args_t) :: args

	!********

	character(len = :), allocatable :: argv, url, version

	integer :: i, argc, ipos

	logical :: error = .false.

	! Defaults
	args%env  = env_t(a = 0, d = 0, s = 1, r = 0)
	args%fenv = env_t(a = 0, d = 0, s = 0, r = 0)
	args%two_pole_cutoff = 0.1d0 * huge(0.d0)

	argc = command_argument_count()
	!print *, "argc = ", argc

	i = 0
	ipos = 0
	do while (i < argc)
		argv = get_next_arg(i)

		select case (argv)

		case ("-h", "--help", "-help")
			args%help    = .true.

		case ("--version")
			args%version = .true.

		case ("--licc")
			args%licc = .true.

		case ("--fft")
			args%fft = .true.

		case ("--sin", "--sine")
			args%sine = .true.
			args%has_waveform = .true.

			args%freq = get_next_double_arg(i, argv, "frequency", error)
			args%len_ = get_next_double_arg(i, argv, "length", error)

		case ("--squ", "--square")
			args%square = .true.
			args%has_waveform = .true.

			args%freq = get_next_double_arg(i, argv, "frequency", error)
			args%len_ = get_next_double_arg(i, argv, "length", error)

		case ("--tri", "--triangle")
			args%triangle = .true.
			args%has_waveform = .true.

			args%freq = get_next_double_arg(i, argv, "frequency", error)
			args%len_ = get_next_double_arg(i, argv, "length", error)

		case ("--saw", "--sawtooth")
			args%sawtooth = .true.
			args%has_waveform = .true.

			args%freq = get_next_double_arg(i, argv, "frequency", error)
			args%len_ = get_next_double_arg(i, argv, "length", error)

		case ("--noi", "--noise")
			args%noise = .true.
			args%has_waveform = .true.

			args%freq = get_next_double_arg(i, argv, "frequency", error)
			args%len_ = get_next_double_arg(i, argv, "length", error)

		case ("--low", "--low-pass")
			args%low_pass = .true.

			args%low_pass_freq = get_next_double_arg(i, argv, "low pass frequency", error)

		case ("--adsr")
			args%has_env = .true.

			args%env%a = get_next_double_arg(i, argv, "attack" , error)
			args%env%d = get_next_double_arg(i, argv, "decay"  , error)
			args%env%s = get_next_double_arg(i, argv, "sustain", error)
			args%env%r = get_next_double_arg(i, argv, "release", error)
			!print *, "env = ", args%env

		case ("--fadsr", "--filter-adsr")
			args%has_fenv = .true.

			args%fenv%a = get_next_double_arg(i, argv, "attack" , error)
			args%fenv%d = get_next_double_arg(i, argv, "decay"  , error)
			args%fenv%s = get_next_double_arg(i, argv, "sustain", error)
			args%fenv%r = get_next_double_arg(i, argv, "release", error)
			!print *, "fenv = ", args%fenv

		case ("--2pole", "--two-pole")
			args%two_pole = .true.

			args%two_pole_cutoff    = get_next_double_arg(i, argv, "cutoff"   , error)
			!args%two_pole_resonance = get_next_double_arg(i, argv, "resonance", error)

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

	if (args%triangle .and. .not. args%has_file1) then
		write(*,*) ERROR_STR//"output file arg not defined for --triangle"
		error = .true.
	end if

	if (args%sawtooth .and. .not. args%has_file1) then
		write(*,*) ERROR_STR//"output file arg not defined for --sawtooth"
		error = .true.
	end if

	if (args%noise .and. .not. args%has_file1) then
		write(*,*) ERROR_STR//"output file arg not defined for --noise"
		error = .true.
	end if

	if (args%licc .and. .not. args%has_file1) then
		write(*,*) ERROR_STR//"output file arg not defined for --licc"
		error = .true.
	end if

	if (args%fft .and. .not. args%has_file1) then
		write(*,*) ERROR_STR//"input file arg not defined for --fft"
		error = .true.
	end if
	if (args%fft .and. .not. args%has_file2) then
		write(*,*) ERROR_STR//"output file arg not defined for --fft"
		error = .true.
	end if

	if (args%low_pass .and. .not. args%has_file1) then
		write(*,*) ERROR_STR//"input file arg not defined for --low-pass"
		error = .true.
	end if
	if (args%low_pass .and. .not. args%has_file2) then
		write(*,*) ERROR_STR//"output file arg not defined for --low-pass"
		error = .true.
	end if

	if (count([args%square, args%triangle, args%sawtooth, args%sine, &
			args%noise]) > 1) then
		! TODO: allow adding noise to any other waveform
		write(*,*) ERROR_STR//"cannot combine multiple waveforms"
		error = .true.
	end if
	if (args%square) then
		args%waveform = WAVEFORM_SQUARE
	else if (args%triangle) then
		args%waveform = WAVEFORM_TRIANGLE
	else if (args%sawtooth) then
		args%waveform = WAVEFORM_SAWTOOTH
	else if (args%sine) then
		args%waveform = WAVEFORM_SINE
	else if (args%noise) then
		args%waveform = WAVEFORM_NOISE
	end if

	! TODO: warn if file2 is given with args that don't use it?

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
		write(*,*) "    fynth -h | --help"
		write(*,*) "    fynth --version"

		! This is probably ok as a single line now.  If it gets too long I might
		! want to break it up (including no-filter echo input to output)
		write(*,*) "    fynth <in.wav> <out.wav> [--fft] [(--low-pass|--low) <frequency>]"
		!write(*,*) "    fynth <in.wav> <out.csv> [--fft]"
		!write(*,*) "    fynth <in.wav> <out.wav> (--low-pass|--low) <frequency>"

		write(*,*) "    fynth <out.wav> (--square|--triangle|--sawtooth|--sine|--noise) <frequency> <length>"
		write(*,*) "        [--adsr <attack> <decay> <sustain> <release>]"
		write(*,*) "        [(--filter-adsr|--fadsr) <attack> <decay> <sustain> <release>]"
		write(*,*) "        [(--two-pole|--2pole) <cutoff-frequency>]"
		!write(*,*) "    fynth <out.wav> --noise <length>"
		write(*,*) "    fynth <out.wav> --licc"
		write(*,*)
		write(*,*) fg_bold//"Options:"//color_reset
		write(*,*) "    -h --help        Show this help"
		write(*,*) "    --version        Show version"
		write(*,*)

		if (.not. args%help) call fynth_exit(EXIT_FAILURE)
	end if

end function read_args

!===============================================================================

end module fynth__app



!===============================================================================

program main

	use fynth__app

	implicit none

	double precision :: cutoff

	character(len = :), allocatable :: ext

	type(args_t)  :: args
	type(audio_t) :: audio
	type(env_t)   :: amp_env, filter_env

	procedure(fn_f64_to_f64), pointer :: waveform_fn

	!********

	args = read_args()
	if (args%help .or. args%version) then
		call fynth_exit(EXIT_SUCCESS)
	end if

	if (args%has_waveform) then
		select case (args%waveform%i)
		case (WAVEFORM_SQUARE%i)
			waveform_fn => square_wave

		case (WAVEFORM_TRIANGLE%i)
			waveform_fn => triangle_wave

		case (WAVEFORM_SAWTOOTH%i)
			waveform_fn => sawtooth_wave

		case (WAVEFORM_SINE%i)
			waveform_fn => sine_wave

		case (WAVEFORM_NOISE%i)
			waveform_fn => noise_wave

		case default
			call panic("bad waveform enum")
		end select
	end if

	! TODO: set defaults in read_args()
	if (args%has_env) then
		amp_env = args%env
	else
		! Set default null ADSR envelope
		amp_env = env_t(a = 0, d = 0, s = 1, r = 0)
	end if

	if (args%two_pole) then
		cutoff = args%two_pole_cutoff
	else
		! Defaut null low pass cutoff
		cutoff = 0.1d0 * huge(cutoff)
	end if

	if (args%has_fenv) then
		filter_env = args%fenv
	else
		!filter_env = env_t(a = 0, d = 0, s = 1, r = 0)
		filter_env = env_t(a = 0, d = 0, s = 0, r = 0)
	end if

	if (args%has_waveform) then

		call write_waveform &
		( &
			args%file1, &
			waveform_fn, args%freq, args%len_, &
			amp_env, &
			cutoff, &
			filter_env &
		)

		call fynth_exit(EXIT_SUCCESS)
	end if

	if (args%licc) then
		call write_wav_licc(args%file1)
		call fynth_exit(EXIT_SUCCESS)
	end if

	if (args%has_file1) then
		audio = read_wav(args%file1)

		if (args%has_file2) then

			! TODO: refactor out of main

			if (args%low_pass) then
				call low_pass_filter(audio, args%low_pass_freq)
			end if

			ext = get_file_extension(args%file2)
			select case (ext)
			case (".wav", ".WAV")
				call write_wav(args%file2, audio)
			case (".csv", ".CSV")
				if (args%fft) then
					call write_csv_fft  (args%file2, audio)
				else
					call write_csv_audio(args%file2, audio)
				end if
			case default
				call panic("Cannot write file type for extension """//ext//"""")
			end select

		end if

		call fynth_exit(EXIT_SUCCESS)
	end if

	!! TODO: remove write_wav_test() or move to test.f90
	!call write_wav_test("test.wav")

	! With no args, just write the licc by default.  This may change
	write(*,*) WARN_STR//"no command arguments given"
	write(*,*) "         Writing the licc by default"
	write(*,*) "         To see options, run:  fynth --help"
	write(*,*)
	call write_wav_licc("licc-sample.wav")

	call fynth_exit(EXIT_SUCCESS)

end program main


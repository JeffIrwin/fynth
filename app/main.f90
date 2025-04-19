
!===============================================================================

program main

	use fynth__app

	implicit none

	character(len = :), allocatable :: ext

	type(args_t)  :: args
	type(audio_t) :: audio

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

		if (args%adsr) then

			if (args%two_pole) then
				call write_wav_square_two_pole &
				( &
					args%file1, args%square_freq, args%square_len, &
					env = args%env, &
					cutoff = args%two_pole_cutoff &
				)
			else
				call write_wav_square(args%file1, args%square_freq, args%square_len, env = args%env)
			end if

		else
			call write_wav_square(args%file1, args%square_freq, args%square_len)
		end if

		call fynth_exit(EXIT_SUCCESS)
	end if

	if (args%noise) then
		call write_wav_noise(args%file1, args%noise_len)
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


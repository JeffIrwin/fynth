
!===============================================================================

program main

	use fynth__app

	implicit none

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
		call write_wav_square(args%file1, args%square_freq, args%square_len)
		call fynth_exit(EXIT_SUCCESS)
	end if

	if (args%has_file1) then
		audio = read_wav(args%file1)

		if (args%has_file2) then
			! TODO: switch case on out file extension -- wav or csv
			call write_wav(args%file2, audio)
		end if

		call fynth_exit(EXIT_SUCCESS)
	end if

	! TODO: add --licc arg for licc sample.  Remove write_wav_test() or move to
	! test.f90
	call write_wav_test("test.wav")
	call write_wav_licc("licc.wav")

	call fynth_exit(EXIT_SUCCESS)

end program main


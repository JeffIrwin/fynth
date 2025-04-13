
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

	if (args%licc) then
		call write_wav_licc(args%file1)
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

	!! TODO: remove write_wav_test() or move to test.f90
	!call write_wav_test("test.wav")

	! With no args, just write the licc by default.  This may change
	write(*,*) WARN_STR//"no command arguments given"
	write(*,*) "         Writing the licc by default"
	write(*,*) "         To see other options, try:  fynth --help"
	write(*,*)
	call write_wav_licc("licc-sample.wav")

	call fynth_exit(EXIT_SUCCESS)

end program main


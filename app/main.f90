
!===============================================================================

program main

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


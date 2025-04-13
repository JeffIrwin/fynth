
program main

	use fynth
	!use exercises_m
	implicit none

	!integer :: io

	call write_wav_test("test.wav")
	call write_wav_licc("licc.wav")
	!io = chapter_2_banded()  ! checking dependencies

end program main


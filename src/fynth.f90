
module fynth

	use fynth__io
	use fynth__utils

	implicit none

	! TODO:
	!
	! - parse args -- input file, output file, --licc sample, -y to quietly
	!   overwrite, fft option, filtering
	!   * see ribbit/src/main.f90 for template
	! - add read_wav() fn
	! - convert input wav to output csv for plotting in gnuplot
	! - extend for stereo, 8-bit, float formats
	!   * i have a few wav samples in my music folder if test read data is
	!     needed.  could also save from audacity or just try writing and see if
	!     it will play (although, windows media player is fault tolerant -- it
	!     doesn't care e.g. if actual wav data is shorter than dlength from
	!     header)
	! - play notes with different wave forms (square, triangle, sawtooth),
	!   envelopes (ADSR), filtering (e.g. low pass), etc.
	!   * LFO? this is a slippery slope to an entire modular digital synth
	!   * could generate white noise and plot fft to test filters
	!   * two-pole filter:  https://www.dsprelated.com/freebooks/filters/Two_Pole.html
	!     + ref describes two parameters -- cutoff frequency theta_c (i
	!       think?) and resonance R. this matches up with the actual knobs on my
	!       prophet.  maybe b0 is 1?
	! - make some tests
	!   * generate sample wav files and compare sha256 to expected files
	!   * round trip read/write
	! - parse some kind of human-writeable music notation format? doing well
	!   with the human-writeable part might be hard, at least if i want to do
	!   more than just one voice/track

	integer, parameter :: &
		FYNTH_MAJOR = 0, &
		FYNTH_MINOR = 1, &
		FYNTH_PATCH = 0

contains

!===============================================================================

subroutine panic(msg)
	character(len = *), intent(in) :: msg
	write(*,*) ERROR_STR//msg
	call fynth_exit(EXIT_FAILURE)
end subroutine panic

!===============================================================================

subroutine fynth_exit(exit_code)
	integer, intent(in) :: exit_code
	if (exit_code == EXIT_SUCCESS) write(*,*) fg_bright_green//"finished fynth"//color_reset
	call exit(exit_code)
end subroutine fynth_exit

!===============================================================================

end module fynth


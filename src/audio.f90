
module fynth__audio

	implicit none

	type audio_t

		! Could later add multiple tracks, stereo, etc.

		double precision, allocatable :: channel(:,:)  ! rename track?
		integer(kind = 4) :: sample_rate

	end type audio_t

	type env_t
		! ADSR (attack, decay, sustain, release) envelope
		!
		! Attack, decay, and release times are in seconds
		!
		! Sustain level is a fraction of the max amplitude, in the range [0.0, 1.0]
		double precision :: a, d, s, r
	end type env_t

end module fynth__audio



module fynth__audio

	implicit none

	type audio_t

		! Could later add multiple tracks, stereo, etc.

		double precision, allocatable :: channel(:,:)  ! rename track?
		integer(kind = 4) :: sample_rate

	end type audio_t

end module fynth__audio


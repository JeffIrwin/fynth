
module fynth__audio

	use fynth__utils
	implicit none

	type audio_t

		! Could later add multiple tracks, stereo, etc.

		double precision, allocatable :: channel(:,:)  ! rename track?
		integer(kind = 4) :: sample_rate

		integer(kind = 4) :: len_, cap  ! could also include num_chans for convenience
		!! Could also include num_chans for convenience
		!integer(kind = 4) :: len_ = 0
		!integer(kind = 4) :: cap  = 0

	end type audio_t

	type env_t
		! ADSR (attack, decay, sustain, release) envelope
		!
		! Attack, decay, and release times are in seconds
		!
		! Sustain level is a fraction of the max amplitude, in the range
		! [0.0, 1.0], or related to the cutoff frequency for filter envelopes
		double precision :: a, d, s, r
	end type env_t

	abstract interface
		! This is a function interface for passing callbacks
		function fn_f64_to_f64(x) result(fx)
			double precision, intent(in) :: x
			double precision :: fx
		end function
	end interface

	type synth_t
		! This type describes the sound of a synth instrument, like all the knob
		! positions on a physical synth
		double precision :: cutoff = 0.1d0 * huge(0.d0)
		type(env_t) ::  env = env_t(a = 0, d = 0, s = 1, r = 0)
		type(env_t) :: fenv = env_t(a = 0, d = 0, s = 0, r = 0)
		procedure(fn_f64_to_f64), pointer, nopass :: wave => square_wave
	end type synth_t

contains

!===============================================================================

double precision function square_wave(t) result(x)
	! Unit amplitude square wave with unit period
	double precision, intent(in) :: t

	! True square wave
	x = 4.d0 * floor(t) - 2.d0 * floor(2 * t) + 1.d0

	!! A pulse wave can be created by subtracting a sawtooth wave from a
	!! phase-shifted version of itself
	!!
	!! TODO: add pm arg for waveform fns (except true_saw_wave())
	!double precision, parameter :: pm = 0.2d0
	!x = true_saw_wave(t) - true_saw_wave(t + pm) + 2 * pm - 1.d0

end function square_wave

double precision function triangle_wave(t) result(x)
	double precision, intent(in) :: t

	!x = 4.d0 * abs(t - floor(t + 0.75d0) + 0.25d0) - 1.d0

	!double precision, parameter :: pm = 0.8d0  ! 0 for true tri wave

	!!double precision :: pm1, pm2
	!!pm2 = 2.d0 * pm - 1.d0
	!!pm1 = 1.d0 - pm2

	x = 4.d0 * abs(t - floor(t + 0.75d0) + 0.25d0) - 1.d0  ! in [-1, 1]

	!x = 0.5d0 * (x + 1.d0)  ! in [0, 1]
	!x = max(x, pm)          ! in [pm, 1]
	!x = x - pm              ! in [0, 1-pm]
	!x = x / (1.d0 - pm)     ! in [0, 1]
	!x = 2.d0 * x - 1.d0     ! in [-1, 1]

	!!x = max(x, pm2) * 2.d0 / pm1
	!!!x = (x - (pm2 + 1.d0)) * 2.d0 / pm1
	!!!x = (x - (pm2 + 1.d0))

end function triangle_wave

double precision function true_saw_wave(t) result(x)
	double precision, intent(in) :: t
	x = 2.d0 * (t - floor(t + 0.5d0))
end function true_saw_wave

double precision function sawtooth_wave(t) result(x)
	double precision, intent(in) :: t

	!double precision, parameter :: pm = 0.8d0  ! 0 for true sawtooth wave

	x = 2.d0 * (t - floor(t + 0.5d0))

	!x = 0.5d0 * (x + 1.d0)  ! in [0, 1]
	!x = max(x, pm)          ! in [pm, 1]
	!x = x - pm              ! in [0, 1-pm]
	!x = x / (1.d0 - pm)     ! in [0, 1]
	!x = 2.d0 * x - 1.d0     ! in [-1, 1]

end function sawtooth_wave

double precision function sine_wave(t) result(x)
	double precision, intent(in) :: t
	x = sin(2 * PI * t)
end function sine_wave

double precision function noise_wave(t) result(x)
	double precision, intent(in) :: t
	!! TODO:  seed rng once at beginning.  Or not depending on an arg?
	!call random_seed(size = nrng)
	!call random_seed(put = [(0, i = 1, nrng)])

	!associate(t => t) ; end associate
	if (.false.) print *, t  ! quiet unused dummy arg warning

	call random_number(x)  ! in [0, 1)
	x = 2.d0 * x - 1.d0
end function noise_wave

end module fynth__audio


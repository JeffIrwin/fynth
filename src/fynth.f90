
module fynth

	use fynth__audio
	use fynth__io
	use fynth__notes
	use fynth__utils

	implicit none

	! TODO:
	!
	! - more tests
	!   * round trip read/write
	! - add some of the generated gnuplot pngs to readme and discuss
	! - parse more args
	!   * -y to quietly overwrite a la ffmpeg.  otherwise just panic or prompt?
	!   * do i really want args for everything or is it time for input files?
	!   * more filtering options:
	!     + delay
	!     + max cutoff frequency
	!     + key amount (changing cutoff based on note freq)
	!   * concat wavs, crop start/end time, amplify, mix wavs
	! - extend for stereo, 8-bit, float formats
	!   * i think basic stereo io is working
	!     + needs tests
	!     + csv, fft output need generalized for stereo
	!   * i have a few wav samples in my music folder if test read data is
	!     needed.  could also save from audacity or just try writing and see if
	!     it will play (although, windows media player is fault tolerant -- it
	!     doesn't care e.g. if actual wav data is shorter than dlength from
	!     header)
	! - play notes with different wave forms envelopes (ADSR), filtering (e.g.
	!   low pass), etc.
	!   * wave forms:
	!     + saw+tri
	!     + pulse width option.  default 0.5, range (0, 1)
	!     + i'm clear on square and i guess triangle pulse mod/width.  what does
	!       it do for a sawtooth?
	!       > i think it's just the duty cycle
	!   * filters:
	!     + fft-based low-pass filter with hard cutoff done
	!     + tbd: high-pass, rolling cutoffs (units dB per octave?)
	!     + can't find much definitive info on four-pole digital filters
	!   * LFO? this is a slippery slope to an entire modular digital synth
	! - parse some kind of human-writeable music notation format? doing well
	!   with the human-writeable part might be hard, at least if i want to do
	!   more than just one voice/track
	!   * i was thinking of something vaguely similar to https://strudel.cc/

	integer, parameter :: &
		FYNTH_MAJOR = 0, &
		FYNTH_MINOR = 1, &
		FYNTH_PATCH = 0

	character(len = *), parameter :: program_name = "fynth"

	integer, parameter :: AMP_EXP = 3

	! Enum type
	type waveform_t
		integer :: i
	end type waveform_t
	type(waveform_t), parameter :: &
		WAVEFORM_SAWTOOTH = waveform_t(5), &
		WAVEFORM_TRIANGLE = waveform_t(4), &
		WAVEFORM_NOISE    = waveform_t(3), &
		WAVEFORM_SINE     = waveform_t(2), &
		WAVEFORM_SQUARE   = waveform_t(1)

	abstract interface
		! This is a function interface for passing callbacks
		function fn_f64_to_f64(x) result(fx)
			double precision, intent(in) :: x
			double precision :: fx
		end function
	end interface

contains

!===============================================================================

subroutine get_filter_coefs(cutoff, sample_rate, a1, a2, b0, b1, b2)

	double precision, intent(in) :: cutoff
	integer, intent(in) :: sample_rate
	double precision, intent(out) :: a1, a2, b0, b1, b2

	!double precision :: cc, dd, n0, n1, n2, d0, d1, d2, q, qq, theta, k, w, alpha
	double precision :: cc, dd, q, qq

	if (cutoff * 2 > sample_rate) then
		! Nyquist fail.  Apply no filter
		b0 = 1.d0
		b1 = 0.d0
		b2 = 0.d0
		a1 = 0.d0
		a2 = 0.d0
		return
	end if

	q = 2.d0
	q = 1.d0 / sqrt(2.d0) ! same as no resonance

	qq = sqrt(2.d0) * q
	cc = tan(2.d0 * PI * cutoff / (2.d0 * sample_rate))

	!****************
	! Source:  https://stackoverflow.com/a/52764064/4347028

	!! Without resonance, as in source
	!dd =  1.d0 + sqrt(2.d0) * cc + cc ** 2
	!a1 = 2.d0 * (cc ** 2.d0 - 1.d0) / dd
	!a2 = (1.d0 - sqrt(2.d0) * cc + cc ** 2) / dd
	!b0 = cc ** 2 / dd
	!b1 = 2.d0 * b0
	!b2 = b0

	! With resonance
	dd =  1.d0 + sqrt(2.d0) * cc / qq + cc ** 2
	a1 = 2.d0 * (cc ** 2.d0 - 1.d0) / dd
	a2 = (1.d0 - sqrt(2.d0) * cc / qq + cc ** 2) / dd
	b0 = cc ** 2 / dd
	b1 = 2.d0 * b0
	b2 = b0

	!!****************
	!! Source:  https://apicsllc.com/apics/Sr_3/Sr_3.htm
	!!
	!! Equivalent to stackoverflow source

	!n0 = 1.d0
	!n1 = 2.d0
	!n2 = 1.d0
	!d0 = cc ** 2 + sqrt(2.d0) * cc + 1.d0
	!d1 = 2.d0 * (cc ** 2 - 1.d0)  ! opposite sign as reference
	!d2 = cc ** 2 - sqrt(2.d0) * cc + 1.d0

	!b0 = cc ** 2 * n0 / d0
	!b1 = cc ** 2 * n1 / d0
	!b2 = cc ** 2 * n2 / d0
	!a1 = d1 / d0
	!a2 = d2 / d0

	!!****************
	!! Second-order filter with resonance `q`:
	!!
	!!     https://www.st.com/resource/en/application_note/an2874-bqd-filter-design-equations-stmicroelectronics.pdf
	!theta = 2.d0 * PI * cutoff / sample_rate
	!k = tan(theta / 2.d0)  ! aka `cc` above
	!w = k ** 2
	!alpha = 1.d0 + k/q + w

	!a1 = 2.d0 * (w - 1.d0) / alpha
	!a2 = (1.d0 - k/q + w) / alpha
	!b0 = w / alpha
	!b1 = 2 * w / alpha
	!b2 = b0

	!****************

	!a1 = -a1  ! wrong
	!a2 = -a2

	!print *, "b012 = ", b0, b1, b2
	!print *, "a12 = ", a1, a2
	!print *, "sum = ", b0 + b1 + b2 + a1 + a2
	!stop

end subroutine get_filter_coefs

!===============================================================================

function get_env_tab(env, len_, ymin, ysus, ymax) result(table)

	type(env_t), intent(in) :: env
	double precision, intent(in) :: len_, ymin, ysus, ymax
	double precision, allocatable :: table(:,:)

	!********

	double precision :: a, ad, ads, adsr

	! cumsum of envelope segment durations
	a = env%a
	ad = a + env%d
	ads = len_
	adsr = ads + env%r

	! Envelope lookup table:  left column is time, right column is
	! the variable being enveloped, e.g. amplitude volume or filter cutoff
	! frequency
	table = reshape( &
		[ &
			0.d0, ymin , &
			a   , ymax , &
			ad  , ysus , &
			ads , ysus , &
			adsr, ymin   &
		], [2, 5] &
	)

	if (len_ < a) then
		! Release begins during attack

		!print *, "table = "
		!print "(2es16.6)", table

		table(2, 2) = plerp(table, len_)
		table(1, 2) = len_

		table(1, 3)  = len_ + env%r
		table(2, 3:) = ymin

		!print *, "table = "
		!print "(2es16.6)", table
		!print *, ""

	else if (len_ < ad) then
		! Release begins during decay
		!
		! These edge cases could be generalized with a loop that could cover
		! things like DADSR or other higher-segment envelopes

		!print *, "table = "
		!print "(2es16.6)", table

		table(2, 3) = plerp(table, len_)
		table(1, 3) = len_

		table(1, 4)  = len_ + env%r
		table(2, 4:) = ymin

		!table(1, 5:) = 1.1d0 * table(1, 4)

		!print *, "table = "
		!print "(2es16.6)", table
		!print *, ""
	end if

	!print *, "table = "
	!print "(2es16.6)", table
	!print *, ""

end function get_env_tab

!===============================================================================

subroutine write_waveform(filename, waveform_fn, freq, len_, env, &
		cutoff, fenv)

	! TODO:
	!   - add more args:
	!     * amp :  max amplitude/volume
	!     * legato:  fraction of how long note is held out of `len_`.  name?  this
	!       is a spectrum from 0 == staccato to 1 == legato
	!     * start time
	!     * stereo pan
	!   - maybe generalize to play onto an audio track instead of directly
	!     writing to file.  that could be done separately

	character(len = *), intent(in) :: filename
	procedure(fn_f64_to_f64) :: waveform_fn
	double precision, intent(in) :: freq, len_
	type(env_t), intent(in) :: env
	double precision, intent(in) :: cutoff
	type(env_t), intent(in) :: fenv

	!********

	type(audio_t) :: audio
	!type(vec_f64_t) :: wave

	!********

	audio = new_audio(num_chans = 1, sample_rate = 44100)

	!call play_note(audio, wave, tr*A1J , wn, t, env, cutoff, fenv)
	call play_note(audio, waveform_fn, freq, len_, 0.d0, env, cutoff, fenv)

	call write_wav(filename, audio)

end subroutine write_waveform

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

!===============================================================================

function new_audio(num_chans, sample_rate)

	integer(kind = 4), intent(in) :: num_chans, sample_rate
	type(audio_t) :: new_audio

	!********

	double precision, allocatable :: channel(:,:)

	allocate(channel(num_chans, 0))

	new_audio = audio_t(channel, sample_rate)

end function new_audio

!===============================================================================

subroutine write_wav_licc(filename)

	character(len = *), intent(in) :: filename

	!********

	double precision :: bpm, quarter_note, eigth_note, en, qn, f, t, len_, cutoff
	double precision, allocatable :: notes(:), duras(:)

	integer :: ii!, it

	type(audio_t) :: audio
	type(env_t) :: env, fenv
	!type(vec_f64_t) :: wave

	!********

	! Beats per minute
	bpm = 120.d0

	! Durations in seconds
	quarter_note = 60.d0 / bpm
	eigth_note = quarter_note / 2.d0

	! Aliases
	qn = quarter_note
	en = eigth_note

	! The licc
	notes = [D3, E3, F3, G3, E3, C3, D3]
	duras = [en, en, en, en, qn, en, qn]

	!audio = audio_t([0.d0], 44100)
	audio = new_audio(num_chans = 1, sample_rate = 44100)

	!cutoff = 0.1d0 * huge(cutoff)
	!cutoff = 1500.d0
	cutoff = 300.d0

	!env  = env_t(a = 0, d = 0, s = 1, r = 0)
	env  = env_t(a = 0.02, d = 0.1, s = 0.5, r = 0.5)
	!env  = env_t(a = 0, d = 0.1, s = 0.5, r = 0.5)

	!fenv = env_t(a = 0, d = 0, s = 0, r = 0)
	!fenv = env_t(a = 0, d = 0.2, s = 0, r = 0)
	fenv = env_t(a = 0.2, d = 0.3, s = 0, r = 0)

	!wave = new_vec_f64()
	t = 0.d0
	do ii = 1, size(notes)

		! Play frequency `notes(ii)` for duration `duras(ii)`
		f = notes(ii)
		len_ = duras(ii)

		call play_note(audio, square_wave, f, len_, t, env, cutoff, fenv)
		!call play_note(audio, triangle_wave, f, len_, t, env, cutoff, fenv)

		t = t + len_

	end do

	call write_wav(filename, audio)

end subroutine write_wav_licc

!===============================================================================

subroutine play_note(audio, waveform_fn, freq, len_, t0, env, &
		cutoff, fenv)

	! TODO:
	!   - encapsulate waveform_fn, env, cutoff, fenv, etc. into new `synth_t` or
	!     `instrument_t` type.  but not freq, len_, legato etc.
	!   - add more args:
	!     * amp :  max amplitude/volume
	!     * legato:  fraction of how long note is held out of `len_`.  name?  this
	!       is a spectrum from 0 == staccato to 1 == legato
	!     * track index?
	!     * stereo pan, or leave until mixing?

	!character(len = *), intent(in) :: filename
	type(audio_t), intent(inout) :: audio

	procedure(fn_f64_to_f64) :: waveform_fn
	double precision, intent(in) :: freq, len_, t0
	type(env_t), intent(in) :: env
	double precision, intent(in) :: cutoff
	type(env_t), intent(in) :: fenv

	!********

	double precision, parameter :: amp = 1.d0  ! could be an arg later
	double precision :: f, t, ampi, ampl, b0, b1, b2, a1, a2, x, &
		y, y0, y00, yout, x0, x00, cutoffl, sampd, fsus, rand
	double precision, allocatable :: amp_tab(:,:), ftab(:,:), tmp(:,:)

	integer :: n, it, itl, it0, it_end
	integer(kind = 4) :: sample_rate! = audio%sample_rate

	!type(vec_f64_t) :: wave

	!********

	sample_rate = audio%sample_rate

	amp_tab = get_env_tab(env, len_, 0.d0, env%s, 1.d0)

	! In get_filter_coefs(), filter doesn't kick in until half the sample_rate
	sampd = 0.501d0 * dble(sample_rate)
	!sampd = 2250.d0

	fsus = lerp(cutoff, sampd, fenv%s)  ! TODO: linear in octaves?
	!print *, "fenv%s = ", fenv%s
	!print *, "cutoff = ", cutoff
	!print *, "sampd  = ", sampd
	!print *, "fsus   = ", fsus

	ftab = get_env_tab(fenv, len_, cutoff, fsus, sampd)

	ftab(2,:) = log(ftab(2,:)) / log(2.d0)

	call random_number(rand)
	rand = 2.d0 * rand - 1.d0

	!f = freq
	!f = freq * (1.d0 + 0.003d0 * rand)  ! slop
	f = freq * (1.d0 + 0.d0 * rand)

	! TODO: consider using arrays for previous signals for generalization from
	! two-pole to four-pole filters
	x = 0.d0
	x0 = 0.d0
	x00 = 0.d0

	y = 0.d0
	y0 = 0.d0
	y00 = 0.d0

	n = int((len_ + env%r) * sample_rate)
	it0 = int(t0 * sample_rate)

	!it_end = it0 + n + 1
	it_end = it0 + n

	!print *, "size channel = ", size(audio%channel, 1), size(audio%channel, 2)

	if (it_end > size(audio%channel, 2)) then
		! Resize
		!
		! TODO: grow by 2*it_end to reduce amortization.  Track length somewhere
		! and trim in caller
		call move_alloc(audio%channel, tmp)

		allocate(audio%channel( size(tmp,1), it_end ))
		audio%channel(:, 1: size(tmp,2)) = tmp
		audio%channel(:, size(tmp,2) + 1:) = 0

	end if

	do it = 1, n
		t = 1.d0 * it / sample_rate

		ampi = plerp(amp_tab, t)
		ampl = amp * ampi ** AMP_EXP

		! Update
		y00 = y0
		y0 = y

		x00 = x0
		x0 = x

		! Filter input signal is `x`
		x = ampl * waveform_fn(f * t)

		cutoffl = 2 ** plerp(ftab, t)
		call get_filter_coefs(cutoffl, sample_rate, a1, a2, b0, b1, b2)
		!print *, "cutoffl = ", cutoffl

		! Note the negative a* terms
		y = b0 * x + b1 * x0 + b2 * x00 - a1 * y0 - a2 * y00

		! Clamp because filter overshoots would otherwise squash down the volume
		! of the rest of the track?  Probably not, because this interferes with
		! the filter.  Maybe there should be a separate gain/clip arg
		yout = y
		!yout = max(-1.d0, min(1.d0, y))

		! TODO: probably don't want to use `push()` here due to release.
		! Release of one note can overlap with start of next note.  Instead of
		! pushing, resize once per note.  Then add sample to previous value
		! instead of (re) setting.  Fix release segment below too

		itl = it + it0
		!audio%channel(1, itl) = yout
		audio%channel(1, itl) = audio%channel(1, itl) + yout

		!print *, "t, x, yout = ", t, x, yout

	end do

end subroutine play_note

!===============================================================================

subroutine write_wav_licc_basic(filename)

	! This is a more flexible example that plays some notes from an array by
	! pushing their waveforms to a dynamic double `wave` vector

	character(len = *), intent(in) :: filename

	!********

	double precision :: bpm, quarter_note, eigth_note, en, qn, f, t
	double precision, allocatable :: notes(:), duras(:)

	integer :: ii, it
	integer(kind = 4) :: sample_rate

	type(vec_f64_t) :: wave

	!********

	! TODO: should default much higher (44.1 kHz or twice that?)
	sample_rate = 8000
	!sample_rate = 44100

	! Beats per minute
	bpm = 120.d0

	! Durations in seconds
	quarter_note = 60.d0 / bpm
	eigth_note = quarter_note / 2.d0

	! Aliases
	qn = quarter_note
	en = eigth_note

	! The licc
	notes = [D3, E3, F3, G3, E3, C3, D3]
	duras = [en, en, en, en, qn, en, qn]

	wave = new_vec_f64()
	do ii = 1, size(notes)

		! Play frequency `notes(ii)` for duration `duras(ii)`
		f = notes(ii)
		do it = 1, int(duras(ii) * sample_rate)
			t = 1.d0 * it / sample_rate
			!call wave%push( sin(2.d0 * PI * f * t) )
			call wave%push( 2.d0 * (2 * floor(f * t) - floor(2 * f * t)) + 1 )  ! square wave
		end do

	end do
	call wave%trim()

	call write_wav(filename, audio_t(reshape(wave%v, [1, wave%len_]), sample_rate))

end subroutine write_wav_licc_basic

!===============================================================================

subroutine write_wav_test(filename)

	! This is a simple and direct example of writing a wav file, using a
	! static-size integer wave `buffer` array which is written immediately

	character(len = *), intent(in) :: filename

	!********

	double precision :: duration_seconds

	integer :: i, fid, io, buffer_size, header_length
	integer(kind = 2), allocatable :: buffer(:)

	type(wav_header_t) :: wavh

	!********

	wavh%sample_rate = 8000
	duration_seconds = 3.d0
	buffer_size = int(wavh%sample_rate * duration_seconds)

	allocate(buffer(buffer_size))

	header_length = storage_size(wavh) / BITS_PER_BYTE  ! fortran's sizeof()
	print *, "header_length = ", header_length

	wavh%chunk_size = 16
	wavh%format_tag = 1
	wavh%num_chans = 1
	wavh%bits_per_samp = 16
	wavh%bytes_per_sec = wavh%sample_rate * wavh%bits_per_samp / BITS_PER_BYTE * wavh%num_chans
	wavh%bytes_per_samp = int(wavh%bits_per_samp / BITS_PER_BYTE * wavh%num_chans, 2)

	do i = 1, buffer_size
		if (i < buffer_size / 4) then
			buffer(i) = int(sin(2 * PI * C4 * i / wavh%sample_rate) * 32000, 2)
		else if (i < buffer_size / 2) then
			buffer(i) = int(sin(2 * PI * D4 * i / wavh%sample_rate) * 32000, 2)
		else if (i < buffer_size * 3 / 4) then
			buffer(i) = int(sin(2 * PI * E4 * i / wavh%sample_rate) * 32000, 2)
		else
			buffer(i) = int(sin(2 * PI * G4 * i / wavh%sample_rate) * 32000, 2)
		end if
	end do

	wavh%dlength = buffer_size * wavh%bytes_per_samp
	wavh%flength = wavh%dlength + header_length

	print *, "dlength        = ", wavh%dlength
	print *, "flength        = ", wavh%flength
	print *, "bytes_per_sec  = ", wavh%bytes_per_sec
	print *, "bytes_per_samp = ", wavh%bytes_per_samp

	! Remove old file first, or junk will be left over at end
	io = rm_file(filename)
	open(file = filename, newunit = fid, form = "unformatted", &
		access = "stream", iostat = io)
	if (io /= 0) call panic("cannot open file for writing: """//filename//"""")

	! Holy fucking bingle.  Today I learned you can just write a whole struct to
	! a binary file all at once
	write(fid) wavh

	write(fid) buffer

	close(fid)
	write(*,*) "Finished writing file """, filename, """"

end subroutine write_wav_test

!===============================================================================

subroutine low_pass_filter(audio, freq)

	! Run an FFT, filter with a hard cutoff frequency `freq`, then do an inverse
	! FFT and modify the `audio`

	use numa, only: fft, ifft

	type(audio_t), intent(inout) :: audio
	double precision, intent(in) :: freq

	!********

	double complex, allocatable :: xx(:)

	double precision :: df

	integer :: i, ic, nt, nf, if_cutoff

	write(*,*) "Applying low-pass filter with cutoff frequency " &
		//to_str(freq)//" Hz"

	nt = size(audio%channel, 2)

	do ic = 1, size(audio%channel, 1)

		xx = fft(cmplx(audio%channel(ic,:), kind = 8))

		! Frequency resolution
		df = 1.d0 * audio%sample_rate / size(xx)
		nf = size(xx)

		! Imagine your FFT is extremely low-resolution and has frequencies [0, 1, 2]
		! at indices [1, 2, 3] and your cutoff is 1.5.  This will allow low-pass
		! frequency 1 Hz at index 2, but filter out frequency 2 Hz at index 3.  So
		! we do ceiling plus one -- ceiling(1.5) + 1 == 2 + 1 == 3
		if_cutoff = max(1, min(nf, ceiling(freq / df) + 1))

		! The upper bound is like this because the FFT is a two-sided FFT, with
		! negative frequencies in the second half of the array
		do i = if_cutoff, nf - if_cutoff + 1
			xx(i) = 0.d0
		end do

		! Invert the FFT, re-using the same array
		xx = ifft(xx)

		audio%channel(ic,:) = xx(1: nt)%re  ! trim.  TODO: abs? I think %re is correct but complex numbers hurt brain

	end do

end subroutine low_pass_filter

!===============================================================================

end module fynth


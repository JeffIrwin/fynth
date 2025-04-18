
module fynth

	use fynth__audio
	use fynth__io
	use fynth__notes
	use fynth__utils

	implicit none

	! TODO:
	!
	! - make some tests
	!   * generate sample wav files and compare sha256 to expected files
	!   * round trip read/write
	! - add some of the generated gnuplot pngs to readme and discuss
	! - parse more args
	!   * -y to quietly overwrite a la ffmpeg.  otherwise just panic or prompt?
	!   * filtering
	!   * envelopes
	!   * more waveforms
	!   * concat wavs, crop start/end time, amplify, mix wavs
	! - extend for stereo, 8-bit, float formats
	!   * i have a few wav samples in my music folder if test read data is
	!     needed.  could also save from audacity or just try writing and see if
	!     it will play (although, windows media player is fault tolerant -- it
	!     doesn't care e.g. if actual wav data is shorter than dlength from
	!     header)
	! - play notes with different wave forms envelopes (ADSR), filtering (e.g.
	!   low pass), etc.
	!   * wave forms:
	!     + sine and square wave done
	!     + tbd:  triangle, sawtooth
	!     + pulse width option for square.  default 0.5, range (0, 1)
	!   * filters:
	!     + fft-based low-pass filter with hard cutoff done
	!     + tbd: high-pass, rolling cutoffs (units dB per octave?)
	!     + two-pole filter:  https://www.dsprelated.com/freebooks/filters/Two_Pole.html
	!       > ref describes two parameters -- cutoff frequency theta_c (i
	!         think?) and resonance R. this matches up with the actual knobs on my
	!         prophet.  maybe b0 is 1?
	!     + can't find much definitive info on four-pole digital filters
	!   * LFO? this is a slippery slope to an entire modular digital synth
	! - parse some kind of human-writeable music notation format? doing well
	!   with the human-writeable part might be hard, at least if i want to do
	!   more than just one voice/track

	integer, parameter :: &
		FYNTH_MAJOR = 0, &
		FYNTH_MINOR = 1, &
		FYNTH_PATCH = 0

	character(len = *), parameter :: program_name = "fynth"

	integer, parameter :: AMP_EXP = 3

contains

!===============================================================================

subroutine write_wav_sine(filename, freq, len_)

	character(len = *), intent(in) :: filename
	double precision, intent(in) :: freq, len_

	!********

	double precision :: f, t
	double precision, allocatable :: notes(:), duras(:)

	integer :: ii, it
	integer(kind = 4) :: sample_rate

	type(vec_f64_t) :: wave

	!********

	sample_rate = 44100

	notes = [freq]
	duras = [len_]

	wave = new_vec_f64()
	do ii = 1, size(notes)

		! Play frequency `notes(ii)` for duration `duras(ii)`
		f = notes(ii)
		do it = 1, int(duras(ii) * sample_rate)
			t = 1.d0 * it / sample_rate
			call wave%push( sin(2.d0 * PI * f * t) )
		end do

	end do
	call wave%trim()

	call write_wav(filename, audio_t(reshape(wave%v, [1, wave%len_]), sample_rate))

end subroutine write_wav_sine

!===============================================================================

subroutine write_wav_square(filename, freq, len_, env)

	! TODO: add more args:
	!   - amp :  max amplitude/volume
	!   - hold:  fraction of how long note is held out of `len_`.  name?
	!
	! Rename fn?  Maybe generalize to play onto an audio track instead of
	! directly writing to file.  That could be done separately

	character(len = *), intent(in) :: filename
	double precision, intent(in) :: freq, len_

	type(env_t), intent(in), optional :: env

	!********

	double precision, parameter :: amp = 1.d0  ! could be an arg later
	double precision :: f, t, tl, ampi, ampl, a, ad

	integer :: it, nads, nr
	integer(kind = 4) :: sample_rate

	type(vec_f64_t) :: wave

	!********

	sample_rate = 44100

	if (present(env)) then
		! cumsum of envelope segments
		a = env%a
		ad = a + env%d
	else
		a  = 0
		ad = 0
	end if

	wave = new_vec_f64()

	f = freq

	! ADS
	nads = int(len_ * sample_rate)
	do it = 1, nads
		t = 1.d0 * it / sample_rate

		if (present(env)) then
			! TODO: unroll loop or optimize branching?
			if (t < a) then
				! TODO: might want to make amp ramp exponentially.  Perception of
				! volume is not linear!
				ampi = lerp(0.d0, amp, (t / a))
			else if (t < ad) then
				ampi = lerp(amp, env%s * amp, (t - a) / (ad - a))
			else
				ampi = env%s * amp
			end if
		else
			ampl = 1.d0
		end if
		ampl = ampi ** AMP_EXP
		!ampl = exp(ampi)

		! TODO: probably don't want to use `push()` here due to release.
		! Release of one note can overlap with start of next note.  Instead of
		! pushing, resize once per note.  Then add sample to previous value
		! instead of (re) setting.  Fix release segment below too

		! TODO: make this a square() fn (unit square? take amp/f as args?)

		call wave%push( ampl * (2.d0 * (2 * floor(f * t) - floor(2 * f * t)) + 1) )

	end do

	if (present(env)) then
		! Release
		nr = int(env%r * sample_rate)
		do it = 1, nr
			! Amlitude is based on local time `tl` while waveform is based on `t`
			t = 1.d0 * (it + nads) / sample_rate
			tl = 1.d0 * it / sample_rate
			ampi = lerp(env%s, 0.d0, tl / env%r)
			ampl = ampi ** AMP_EXP
			!ampl = exp(ampi)
			call wave%push( ampl * (2.d0 * (2 * floor(f * t) - floor(2 * f * t)) + 1) )
		end do
	end if

	call wave%trim()

	call write_wav(filename, audio_t(reshape(wave%v, [1, wave%len_]), sample_rate))

end subroutine write_wav_square

!===============================================================================

subroutine write_wav_noise(filename, len_)

	character(len = *), intent(in) :: filename
	double precision, intent(in) :: len_

	!********

	double precision :: t, r

	integer :: i, it, nrng
	integer(kind = 4) :: sample_rate

	type(vec_f64_t) :: wave

	!********

	sample_rate = 44100

	!allocate(r(l))
	call random_seed(size = nrng)
	call random_seed(put = [(0, i = 1, nrng)])
	!call random_number(r)

	wave = new_vec_f64()

	do it = 1, int(len_ * sample_rate)
		t = 1.d0 * it / sample_rate
		call random_number(r)  ! in [0, 1)
		!call wave%push(r)
		call wave%push(2.d0 * r - 1.d0)
	end do

	call wave%trim()

	call write_wav(filename, audio_t(reshape(wave%v, [1, wave%len_]), sample_rate))

end subroutine write_wav_noise

!===============================================================================

subroutine write_wav_licc(filename)

	! This is a more flexible example that plays some notes from an array by
	! pushing their waveforms to a dynamic double `wave` vector

	character(len = *), intent(in) :: filename

	!********

	!double complex, allocatable :: xx(:)

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
			call wave%push( sin(2.d0 * PI * f * t) )
			!call wave%push( 2.d0 * (2 * floor(f * t) - floor(2 * f * t)) + 1 )  ! square wave
		end do

	end do
	call wave%trim()

	call write_wav(filename, audio_t(reshape(wave%v, [1, wave%len_]), sample_rate))

	!xx = fft(cmplx(wave%v, kind = 8))
	!print *, "xx = "
	!print "(2es16.6)", xx(1: 10)
	!!call write_wav("fft.wav", audio_t(dble(xx), sample_rate))

end subroutine write_wav_licc

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
	open(file = filename, newunit = fid, form = "unformatted", access = "stream")

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


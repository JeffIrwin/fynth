
module fynth

	use fynth__audio
	use fynth__io
	use fynth__notes
	use fynth__utils

	use numa, only: fft

	implicit none

	! TODO:
	!
	! - parse more args
	!   * -y to quietly overwrite a la ffmpeg.  otherwise just panic or prompt?
	!   * fft option
	!   * filtering
	!   * envelopes
	!   * random/white noise
	!   * more waveforms
	!   * concat wavs, crop start/end time, amplify, mix wavs
	! - convert input wav to output csv for plotting in gnuplot
	! - extend for stereo, 8-bit, float formats
	!   * i have a few wav samples in my music folder if test read data is
	!     needed.  could also save from audacity or just try writing and see if
	!     it will play (although, windows media player is fault tolerant -- it
	!     doesn't care e.g. if actual wav data is shorter than dlength from
	!     header)
	! - play notes with different wave forms envelopes (ADSR), filtering (e.g.
	!   low pass), etc.
	!   * sine and square wave done
	!   * tbd:  triangle, sawtooth
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

	character(len = *), parameter :: program_name = "fynth"

contains

!===============================================================================

subroutine write_wav_sine(filename, freq, len)

	! This is a more flexible example that plays some notes from an array by
	! pushing their waveforms to a dynamic double `wave` vector

	character(len = *), intent(in) :: filename
	double precision, intent(in) :: freq, len

	!********

	double precision :: f, t
	double precision, allocatable :: notes(:), duras(:)

	integer :: ii, it
	integer(kind = 4) :: sample_rate

	type(vec_f64_t) :: wave

	!********

	sample_rate = 44100

	notes = [freq]
	duras = [len]

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

	call write_wav(filename, audio_t(wave%v, sample_rate))

end subroutine write_wav_sine

!===============================================================================

subroutine write_wav_square(filename, freq, len)

	! This is a more flexible example that plays some notes from an array by
	! pushing their waveforms to a dynamic double `wave` vector

	character(len = *), intent(in) :: filename
	double precision, intent(in) :: freq, len

	!********

	double precision :: f, t
	double precision, allocatable :: notes(:), duras(:)

	integer :: ii, it
	integer(kind = 4) :: sample_rate

	type(vec_f64_t) :: wave

	!********

	sample_rate = 44100

	notes = [freq]
	duras = [len]

	wave = new_vec_f64()
	do ii = 1, size(notes)

		! Play frequency `notes(ii)` for duration `duras(ii)`
		f = notes(ii)
		do it = 1, int(duras(ii) * sample_rate)
			t = 1.d0 * it / sample_rate
			call wave%push( 2.d0 * (2 * floor(f * t) - floor(2 * f * t)) + 1 )
		end do

	end do
	call wave%trim()

	call write_wav(filename, audio_t(wave%v, sample_rate))

end subroutine write_wav_square

!===============================================================================

subroutine write_wav_noise(filename, len)

	! This is a more flexible example that plays some notes from an array by
	! pushing their waveforms to a dynamic double `wave` vector

	character(len = *), intent(in) :: filename
	double precision, intent(in) :: len

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

	do it = 1, int(len * sample_rate)
		t = 1.d0 * it / sample_rate
		call random_number(r)  ! in [0, 1)
		!call wave%push(r)
		call wave%push(2.d0 * r - 1.d0)
	end do

	call wave%trim()

	call write_wav(filename, audio_t(wave%v, sample_rate))

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

	call write_wav(filename, audio_t(wave%v, sample_rate))

	!! TODO: add options for fft and csv output
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

end module fynth


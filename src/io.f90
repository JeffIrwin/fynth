
module fynth__io

	use fynth__utils
	use numa, only: fft

	implicit none

	! This wav writer is based on the following:
	!
	!     https://gist.github.com/BjoernSchilberg/c5deafaa5b3d477f60543ef59fad0a00
	!
	! See also:
	!
	!     https://www.youtube.com/watch?v=8nOi-0kBv2Y
	!     https://docs.fileformat.com/audio/wav/

	double precision, parameter :: &
		A_440 = 440.d0, &
		C4  = A_440 * 2.d0 ** (3.d0 / 12.d0 - 1.d0), &  ! Middle C
		C3  = A_440 * 2.d0 ** (3.d0 / 12.d0 - 2.d0), &  ! one octave down from middle C
		C2  = A_440 * 2.d0 ** (3.d0 / 12.d0 - 3.d0), &
		C1  = A_440 * 2.d0 ** (3.d0 / 12.d0 - 4.d0), &
		C0  = A_440 * 2.d0 ** (3.d0 / 12.d0 - 5.d0), &  ! ~ 16.3 Hz, not audible to humans
		C5  = A_440 * 2.d0 ** (3.d0 / 12.d0 + 0.d0), &  ! one octave up from middle C
		C6  = A_440 * 2.d0 ** (3.d0 / 12.d0 + 1.d0), &
		C7  = A_440 * 2.d0 ** (3.d0 / 12.d0 + 2.d0), &
		C8  = A_440 * 2.d0 ** (3.d0 / 12.d0 + 3.d0), &
		C9  = A_440 * 2.d0 ** (3.d0 / 12.d0 + 4.d0)     ! C8 is already very high

	! TODO: move note frequencies to fynth.f90
	double precision, parameter :: &
		CS4 = C4 * 2.d0 ** ( 1.d0 / 12.d0), &  ! C sharp (just above middle C)
		DF4 = C4 * 2.d0 ** ( 1.d0 / 12.d0), &  ! D flat (enharmonic)
		D4  = C4 * 2.d0 ** ( 2.d0 / 12.d0), &
		DS4 = C4 * 2.d0 ** ( 3.d0 / 12.d0), &
		EF4 = C4 * 2.d0 ** ( 3.d0 / 12.d0), &  ! white key enharmonics are included
		E4  = C4 * 2.d0 ** ( 4.d0 / 12.d0), &
		FF4 = C4 * 2.d0 ** ( 4.d0 / 12.d0), &
		ES4 = C4 * 2.d0 ** ( 5.d0 / 12.d0), &
		F4  = C4 * 2.d0 ** ( 5.d0 / 12.d0), &
		FS4 = C4 * 2.d0 ** ( 6.d0 / 12.d0), &
		GF4 = C4 * 2.d0 ** ( 6.d0 / 12.d0), &
		G4  = C4 * 2.d0 ** ( 7.d0 / 12.d0), &
		GS4 = C4 * 2.d0 ** ( 8.d0 / 12.d0), &
		AF4 = C4 * 2.d0 ** ( 8.d0 / 12.d0), &
		A4  = C4 * 2.d0 ** ( 9.d0 / 12.d0), &
		AS4 = C4 * 2.d0 ** (10.d0 / 12.d0), &
		BF4 = C4 * 2.d0 ** (10.d0 / 12.d0), &
		B4  = C4 * 2.d0 ** (11.d0 / 12.d0), &
		BS4 = C4 * 2.d0 ** (12.d0 / 12.d0)

	double precision, parameter :: &
		CS3 = C3 * 2.d0 ** ( 1.d0 / 12.d0), &
		DF3 = C3 * 2.d0 ** ( 1.d0 / 12.d0), &
		D3  = C3 * 2.d0 ** ( 2.d0 / 12.d0), &
		DS3 = C3 * 2.d0 ** ( 3.d0 / 12.d0), &
		EF3 = C3 * 2.d0 ** ( 3.d0 / 12.d0), &
		E3  = C3 * 2.d0 ** ( 4.d0 / 12.d0), &
		FF3 = C3 * 2.d0 ** ( 4.d0 / 12.d0), &
		ES3 = C3 * 2.d0 ** ( 5.d0 / 12.d0), &
		F3  = C3 * 2.d0 ** ( 5.d0 / 12.d0), &
		FS3 = C3 * 2.d0 ** ( 6.d0 / 12.d0), &
		GF3 = C3 * 2.d0 ** ( 6.d0 / 12.d0), &
		G3  = C3 * 2.d0 ** ( 7.d0 / 12.d0), &
		GS3 = C3 * 2.d0 ** ( 8.d0 / 12.d0), &
		AF3 = C3 * 2.d0 ** ( 8.d0 / 12.d0), &
		A3  = C3 * 2.d0 ** ( 9.d0 / 12.d0), &
		AS3 = C3 * 2.d0 ** (10.d0 / 12.d0), &
		BF3 = C3 * 2.d0 ** (10.d0 / 12.d0), &
		B3  = C3 * 2.d0 ** (11.d0 / 12.d0), &
		BS3 = C3 * 2.d0 ** (12.d0 / 12.d0)

	! TODO: other octaves.  Obviously this is WET, but I like having direct note
	! names instead of having to index into an array of frequencies

	character(len = *), parameter :: &
		RIFF_ = "RIFF", &
		WAVE_ = "WAVE", &
		FMT__ = "fmt ", &  ! case and whitespace sensitive
		DATA_ = "data"

	type wav_header_t
	
		character(len = 4) :: riff = RIFF_
		integer(kind = 4)  :: flength         ! file length in bytes
		character(len = 4) :: wave = WAVE_
		character(len = 4) :: fmt_ = FMT__
		integer(kind = 4)  :: chunk_size      ! size of FMT chunk in bytes (usually 16)
		integer(kind = 2)  :: format_tag      ! 1 = PCM, 257 = Mu-Law, 258 = A-Law, 259 = ADPCM
		integer(kind = 2)  :: num_chans       ! 1 = mono, 2 = stereo
		integer(kind = 4)  :: sample_rate     ! sampling rate in samples per second
		integer(kind = 4)  :: bytes_per_sec   ! bytes per second = sample_rate * bytes_per_samp
		integer(kind = 2)  :: bytes_per_samp  ! 2 = 16-bit mono, 4 = 16-bit stereo
		integer(kind = 2)  :: bits_per_samp   ! number of bits per sample
		character(len = 4) :: data = DATA_
		integer(kind = 4)  :: dlength         ! data length in bytes (flength - 44 (header length))

	end type wav_header_t

contains

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

subroutine write_wav(filename, wave_f64, sample_rate)

	! TODO: this is the only method that belongs in io.f90.  Move others to test
	! or run (fynth --licc)

	! TODO: maybe wave_f64 and sample_rate should be encapsulated in a struct.
	! Could later add multiple tracks, stereo, etc.

	character(len = *), intent(in) :: filename
	double precision, intent(in) :: wave_f64(:)
	integer(kind = 4), intent(in) :: sample_rate

	!********

	double precision :: max_wave
	integer :: fid, io, header_length
	integer(kind = 2), allocatable :: buffer(:)

	type(wav_header_t) :: wavh

	wavh%sample_rate = sample_rate

	header_length = storage_size(wavh) / BITS_PER_BYTE  ! fortran's sizeof()
	print *, "header_length = ", header_length

	wavh%chunk_size = 16
	wavh%format_tag = 1
	wavh%num_chans = 1
	wavh%bits_per_samp = 16  ! TODO: tie this magic number to the type of `buffer`
	wavh%bytes_per_sec = wavh%sample_rate * wavh%bits_per_samp / BITS_PER_BYTE * wavh%num_chans
	wavh%bytes_per_samp = int(wavh%bits_per_samp / BITS_PER_BYTE * wavh%num_chans, 2)

	max_wave = maxval(abs(wave_f64))
	print *, "wave infty-norm = ", max_wave

	! TODO: warn if RMS norm is much less than max norm, i.e. if some kind of
	! wierd spike resulted in massively reducing the volume of the rest of the
	! wave track

	buffer = int(wave_f64 * (2 ** (wavh%bits_per_samp - 1) - 1) / max_wave, 2)
	!print *, "buff infty-norm = ", maxval(abs(buffer))

	wavh%dlength = size(buffer) * wavh%bytes_per_samp
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

end subroutine write_wav

!===============================================================================

subroutine write_wav_licc(filename)

	! This is a more flexible example that plays some notes from an array by
	! pushing their waveforms to a dynamic double `wave` vector

	character(len = *), intent(in) :: filename

	!********

	double complex, allocatable :: xx(:)

	double precision :: bpm, quarter_note, eigth_note, en, qn
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

	print *, "A_440 = ", A_440
	print *, "C4    = ", C4
	print *, "C0    = ", C0

	! The licc

	!notes = [D4, E4, F4, G4, E4, C4, D4]
	notes = [D3, E3, F3, G3, E3, C3, D3]
	!notes = [D4, E4, F4, G4, E4, C4, D4] * 0.5d0  ! another way to lower by an octave

	duras = [en, en, en, en, qn, en, qn]

	wave = new_vec_f64()
	do ii = 1, size(notes)

		! Play frequency `notes(ii)` for duration `duras(ii)`
		do it = 1, int(duras(ii) * sample_rate)
			call wave%push( sin(2 * PI * notes(ii) * it / sample_rate) )
		end do

	end do
	call wave%trim()

	!print *, "wave infty-norm = ", maxval(abs(wave%v))

	call write_wav(filename, wave%v, sample_rate)

	!xx = fft(wave%v)
	!xx = fft(complex(wave%v, zeroes(size(wave%v))))
	xx = fft(cmplx(wave%v, kind = 8))
	print *, "xx = "
	print "(2es16.6)", xx(1: 10)

	!call write_wav("fft.wav", dble(xx), sample_rate)

end subroutine write_wav_licc

!===============================================================================

end module fynth__io



module fynth__io

	use fynth__notes
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

subroutine write_wav(filename, wave_f64, sample_rate)

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

end module fynth__io



module fynth__io

	use fynth__utils
	implicit none

	! This is based on the following:
	!
	!     https://gist.github.com/BjoernSchilberg/c5deafaa5b3d477f60543ef59fad0a00
	!
	! See also:
	!
	!     https://www.youtube.com/watch?v=8nOi-0kBv2Y
	!     https://docs.fileformat.com/audio/wav/

	! TODO: 32-bit floating point waves?

	character(len = *), parameter :: &
		RIFF_ = "RIFF", &
		WAVE_ = "WAVE", &
		FMT__ = "fmt ", &  ! case-sensitive
		DATA_ = "data"

	double precision, parameter :: &
		C4  = 256.d0, &                        ! Middle C. TODO: this frequency is wrong
		CS4 = C4 * 2.d0 ** ( 1.d0 / 12.d0), &  ! C sharp
		DF4 = C4 * 2.d0 ** ( 1.d0 / 12.d0), &  ! D flat (enharmonic)
		D4  = C4 * 2.d0 ** ( 2.d0 / 12.d0), &
		DS4 = C4 * 2.d0 ** ( 3.d0 / 12.d0), &
		EF4 = C4 * 2.d0 ** ( 3.d0 / 12.d0), &
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

	type wav_header
	
		character(len = 4) :: riff = RIFF_
		integer(kind = 4)  :: flength         ! file length in bytes
		character(len = 4) :: wave = WAVE_
		character(len = 4) :: fmt_ = FMT__
		integer(kind = 4)  :: chunk_size      ! size of FMT chunk in bytes (usually 16)
		integer(kind = 2)  :: format_tag      ! 1 = PCM, 257 = Mu-Law, 258 = A-Law, 259 = ADPCM
		integer(kind = 2)  :: num_chans       ! 1 = mono, 2 = stereo
		integer(kind = 4)  :: srate           ! sampling rate in samples per second
		integer(kind = 4)  :: bytes_per_sec   ! bytes per second = srate * bytes_per_samp
		integer(kind = 2)  :: bytes_per_samp  ! 2 = 16-bit mono, 4 = 16-bit stereo
		integer(kind = 2)  :: bits_per_samp   ! number of bits per sample
		character(len = 4) :: data = DATA_
		integer(kind = 4)  :: dlength         ! data length in bytes (flength - 44 (header length))

	end type wav_header

contains

!===============================================================================

subroutine write_wav_test(filename)

	implicit none

	character(len = *), intent(in) :: filename

	integer :: i, fid, io, buffer_size, header_length
	integer(kind = 2), allocatable :: buffer(:)
	double precision :: duration_seconds

	type(wav_header) :: wavh

	wavh%srate = 8000
	duration_seconds = 10.d0
	buffer_size = int(wavh%srate * duration_seconds)

	allocate(buffer(buffer_size))

	header_length = storage_size(wavh) / BITS_PER_BYTE  ! fortran's sizeof()
	print *, "header_length = ", header_length

	wavh%chunk_size = 16
	wavh%format_tag = 1
	wavh%num_chans = 1
	wavh%bits_per_samp = 16
	wavh%bytes_per_sec = wavh%srate * wavh%bits_per_samp / BITS_PER_BYTE * wavh%num_chans
	wavh%bytes_per_samp = int(wavh%bits_per_samp / BITS_PER_BYTE * wavh%num_chans, 2)

	do i = 1, buffer_size
		if (i < buffer_size / 4) then
			buffer(i) = int(sin(2 * PI * C4  * i / wavh%srate) * 32000, 2)
		else if (i < buffer_size / 2) then
			buffer(i) = int(sin(2 * PI * D4  * i / wavh%srate) * 32000, 2)
		else if (i < buffer_size * 3 / 4) then
			buffer(i) = int(sin(2 * PI * E4 * i / wavh%srate) * 32000, 2)
		else
			buffer(i) = int(sin(2 * PI * G4  * i / wavh%srate) * 32000, 2)
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

end module fynth__io


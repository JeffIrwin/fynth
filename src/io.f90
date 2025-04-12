
module fynth__io

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

	double precision, parameter :: PI = 4.d0 * atan(1.d0)

	double precision, parameter :: &
		MIDDLE_C  = 256.d0, &  ! TODO: rename C4 (for extension to other octaves)
		MIDDLE_CS = MIDDLE_C * 2.d0 ** (1.d0 / 12.d0), &
		MIDDLE_D  = MIDDLE_C * 2.d0 ** (2.d0 / 12.d0)

	integer, parameter :: &
		BITS_PER_BYTE = 8

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

	!********

	! TODO: make utils.f90

	character, parameter :: NULL_CHAR = char(0)

	!! External C fns
	!integer, external :: &
	!	del_file, &
	!	make_dir

	!********

contains

!===============================================================================

subroutine write_wav_test(filename)

	implicit none

	!procedure(integer) :: del_file
	!integer, external :: del_file

	interface
	  integer function del_file(filename)
	    character(len = *), intent(in) :: filename
	  end function del_file
	end interface

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
			buffer(i) = int(sin(2 * PI * MIDDLE_C  * i / wavh%srate) * 32000, 2)
		else if (i < buffer_size / 2) then
			buffer(i) = int(sin(2 * PI * MIDDLE_D  * i / wavh%srate) * 32000, 2)
		else if (i < buffer_size * 3 / 4) then
			buffer(i) = int(sin(2 * PI * MIDDLE_CS * i / wavh%srate) * 32000, 2)
		else
			buffer(i) = int(sin(2 * PI * MIDDLE_D  * i / wavh%srate) * 32000, 2)
		end if
	end do

	wavh%dlength = buffer_size * wavh%bytes_per_samp
	wavh%flength = wavh%dlength + header_length

	print *, "dlength        = ", wavh%dlength
	print *, "flength        = ", wavh%flength
	print *, "bytes_per_sec  = ", wavh%bytes_per_sec
	print *, "bytes_per_samp = ", wavh%bytes_per_samp

	! Remove old file first, or junk will be left over at end
	io = del_file(filename//NULL_CHAR)
	open(file = filename, newunit = fid, form = "unformatted", access = "stream")

	! Holy fucking bingle.  Today I learned you can just write a whole struct to
	! a binary file all at once
	write(fid) wavh

	write(fid) buffer

	close(fid)

end subroutine write_wav_test

!===============================================================================

end module fynth__io



module fynth__io

	use fynth__audio
	use fynth__notes
	use fynth__utils

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

	integer, parameter :: &
		WAV_FMT_PCM = 1, &
		WAV_FMT_MU_LAW = 257, &
		WAV_FMT_A_LAW  = 258, &
		WAV_FMT_ADPCM  = 259

	type wav_header_t
	
		! TODO: some of these (flength, dlength) should be u32.  Make them kind
		! 8 in Fortran, read and convert appropriately.  See read_u32() in
		! cali/src/cali.f90

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

function read_wav(filename) result(audio)

	character(len = *), intent(in) :: filename
	type(audio_t) :: audio

	!********

	integer, parameter :: FSEEK_RELATIVE = 1

	integer :: io, fid
	integer :: buffer_size
	integer(kind = 2), allocatable :: buffer16(:,:)

	type(wav_header_t) :: wavh

	!********

	write(*,*) "Reading wav file """, filename, """"
	open(file = filename, newunit = fid, form = "unformatted", &
		access = "stream", status = "old", iostat = io)
	if (io /= 0) call panic("cannot open wav file for reading: """//filename//"""")

	read(fid, iostat = io) wavh
	if (io /= 0) call panic("cannot read wav header from file """//filename//"""")

	print *, ""
	print *, "wavh = ", wavh
	print *, "dlength = ", wavh%dlength
	print *, "bytes_per_samp = ", wavh%bytes_per_samp
	print *, "format_tag = ", wavh%format_tag

	if (wavh%riff /= RIFF_) then
		call panic("bad wav file.  "//RIFF_//" not found")
	end if
	if (wavh%wave /= WAVE_) then
		call panic("bad wav file.  "//WAVE_//" not found")
	end if
	if (wavh%chunk_size /= 16) then
		! We could probably extend support here, but I'm not sure if it offsets
		! data to be read after the format_tag
		call panic("bad wav file.  Only chunk size 16 is supported")
	end if
	if (wavh%format_tag /= WAV_FMT_PCM) then
		call panic("bad wav file.  Only PCM format tag ("//to_str(WAV_FMT_PCM) &
			//") is supported")
	end if

	if (wavh%bits_per_samp /= 16) then
		call panic("only 16-bit wav is supported.  bits_per_samp = """ &
			//to_str(wavh%bits_per_samp)//"""")
	end if

	audio%sample_rate = wavh%sample_rate

	print *, ""
	print *, "data = ", wavh%data
	do while (.true.)
		if (wavh%data == DATA_) exit

		! TODO: handle LIST data.  Maybe do a select/case here which covers both
		! "LIST" and (audio) "data"

		write(*,*) WARN_STR//"ignoring "//to_str(wavh%dlength) &
			//" bytes of """//wavh%data//""" data"

		!print *, "dlength = ", wavh%dlength

		! TODO: fseek is a gnu extension.  Intel needs to use ifport for this
		call fseek(fid, wavh%dlength, FSEEK_RELATIVE)

		read(fid, iostat = io) wavh%data
		if (io /= 0) call panic("cannot read wav chunk from file """//filename//"""")

		read(fid, iostat = io) wavh%dlength
		if (io /= 0) call panic("cannot read wav chunk from file """//filename//"""")

		print *, "data = ", wavh%data
		print *, "dlength = ", wavh%dlength

		!exit
	end do
	print *, "done loop"
	print *, ""

	buffer_size = wavh%num_chans * wavh%dlength / wavh%bytes_per_samp

	print *, "buffer_size = ", buffer_size

	allocate(buffer16(wavh%num_chans, buffer_size / wavh%num_chans))

	!print *, "size(buffer16) = ", size(buffer16, 1), size(buffer16, 2)

	read(fid, iostat = io) buffer16
	if (io /= 0) call panic("cannot read wav data from file """//filename//"""")
	!print *, "buffer16 = ", buffer16(1: 10)

	audio%channel = buffer16 / (2.d0 ** (wavh%bits_per_samp - 1) - 1.d0)
	!print *, "audio%channel = ", audio%channel(1: 10)

	close(fid)

end function read_wav

!===============================================================================

subroutine write_wav(filename, audio)

	character(len = *), intent(in) :: filename
	type(audio_t), intent(in) :: audio

	!********

	double precision :: max_wave
	integer :: fid, io, header_length
	!integer(kind = 2), allocatable :: buffer16(:)
	integer(kind = 2), allocatable :: buffer16(:,:)

	type(wav_header_t) :: wavh

	wavh%sample_rate = audio%sample_rate

	header_length = storage_size(wavh) / BITS_PER_BYTE  ! fortran's sizeof()
	!print *, "header_length = ", header_length

	wavh%chunk_size = 16
	wavh%format_tag = 1
	wavh%num_chans = size(audio%channel, 1, kind = 2)
	wavh%bits_per_samp = 16  ! TODO: tie this magic number to the type of `buffer16`
	wavh%bytes_per_sec = wavh%sample_rate * wavh%bits_per_samp / BITS_PER_BYTE * wavh%num_chans
	wavh%bytes_per_samp = int(wavh%bits_per_samp / BITS_PER_BYTE * wavh%num_chans, 2)

	max_wave = maxval(abs(audio%channel))
	write(*,*) "Wave infty-norm = ", max_wave

	! TODO: warn if RMS norm is much less than max norm, i.e. if some kind of
	! wierd spike resulted in massively reducing the volume of the rest of the
	! wave track

	! TODO: parameterize this magic number
	buffer16 = int(audio%channel * (2 ** (wavh%bits_per_samp - 1) - 1) / max_wave, 2)

	!print *, "size(buffer16) = ", size(buffer16, 1), size(buffer16, 2)

	!print *, "buff infty-norm = ", maxval(abs(buffer16))

	!wavh%dlength = size(buffer16) * wavh%bytes_per_samp
	wavh%dlength = size(buffer16) * wavh%bytes_per_samp / wavh%num_chans

	wavh%flength = wavh%dlength + header_length

	!print *, "dlength        = ", wavh%dlength
	!print *, "flength        = ", wavh%flength
	!print *, "bytes_per_sec  = ", wavh%bytes_per_sec
	!print *, "bytes_per_samp = ", wavh%bytes_per_samp

	! Remove old file first, or junk will be left over at end
	io = rm_file(filename)
	open(file = filename, newunit = fid, form = "unformatted", &
		access = "stream", iostat = io)
	if (io /= 0) call panic("cannot open file for writing: """//filename//"""")

	! Holy fucking bingle.  Today I learned you can just write a whole struct to
	! a binary file all at once
	write(fid) wavh

	write(fid) buffer16

	close(fid)
	write(*,*) "Finished writing file """, filename, """"

end subroutine write_wav

!===============================================================================

subroutine write_csv_audio(filename, audio)

	character(len = *), intent(in) :: filename
	type(audio_t), intent(in) :: audio

	!********

	double precision :: dt

	integer :: i, io, fid, nt

	dt = 1.d0 / audio%sample_rate
	nt = size(audio%channel)

	! TODO: write all channels in separate cols
	open(file = filename, newunit = fid, iostat = io)
	if (io /= 0) call panic("cannot open file for writing: """//filename//"""")

	write(fid, "(a)") "# time (s), channel amplitude"
	write(fid, "(2es16.6)") [(dt * i, audio%channel(1, i+1), i = 0, nt-1)]
	close(fid)

	write(*,*) "Finished writing file """, filename, """"

end subroutine write_csv_audio

!===============================================================================

subroutine write_csv_fft(filename, audio)

	! Do an FFT and then write it to a csv file

	use numa, only: fft

	character(len = *), intent(in) :: filename
	type(audio_t), intent(in) :: audio

	!********

	double complex, allocatable :: xx(:)

	double precision :: df

	integer :: i, io, fid, nf

	! TODO: iterate fft on all channels.  Increase rank of xx, write extra cols to csv
	xx = fft(cmplx(audio%channel(1,:), kind = 8))

	!print *, "xx = "
	!print "(2es16.6)", xx(1: 10)

	! Check open, even for writing, e.g. in case of files in dirs that don't
	! exist
	open(file = filename, newunit = fid, iostat = io)
	if (io /= 0) call panic("cannot open file for writing: """//filename//"""")

	! TODO: also write a magnitude column (using abs).  Update plotting script
	write(fid, "(a)") "# frequency (Hz), FFT real, FFT imag"

	! Get the frequency resolution `df`.  The fft fn only returns raw
	! amplitudes, so we have to do a little work to get the frequencies that go
	! with those amplitudes. See also numerical-analysis/src/exercises.f90 which
	! also does this
	df = 1.d0 * audio%sample_rate / size(xx)
	nf = size(xx)

	!print *, "sample_rate = ", audio%sample_rate
	!print *, "df = ", df

	write(fid, "(3es16.6)") [(df * (i-1), xx(i)%re, xx(i)%im, i = 1, nf)]

	close(fid)
	write(*,*) "Finished writing file """, filename, """"

end subroutine write_csv_fft

!===============================================================================

end module fynth__io


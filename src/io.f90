
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

!const float MIDDLE_C  = 256.00;
!const float MIDDLE_CS = MIDDLE_C * pow(2.0, 1.0/12);
!const float MIDDLE_D  = MIDDLE_C * pow(2.0, 2.0/12);

	type wav_header
		!  char riff[4];           /* "RIFF"                                  */
		!  int32_t flength;        /* file length in bytes                    */
		!  char wave[4];           /* "WAVE"                                  */
		!  char fmt[4];            /* "fmt "                                  */
		!  int32_t chunk_size;     /* size of FMT chunk in bytes (usually 16) */
		!  int16_t format_tag;     /* 1=PCM, 257=Mu-Law, 258=A-Law, 259=ADPCM */
		!  int16_t num_chans;      /* 1=mono, 2=stereo                        */
		!  int32_t srate;          /* Sampling rate in samples per second     */
		!  int32_t bytes_per_sec;  /* bytes per second = srate*bytes_per_samp */
		!  int16_t bytes_per_samp; /* 2=16-bit mono, 4=16-bit stereo          */
		!  int16_t bits_per_samp;  /* Number of bits per sample               */
		!  char data[4];           /* "data"                                  */
		!  int32_t dlength;        /* data length in bytes (filelength - 44)  */
	
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

!program main
subroutine write_wav_test()

	implicit none

	integer :: i, fid, buffer_size, header_length
	integer(kind = 2), allocatable :: buffer(:)
	double precision :: duration_seconds

	type(wav_header) :: wavh

	print *, "hello world"

	!wavh%flength = int(z"DEADBEEF")
	!wavh%flength = int(z"EFBEADDE")  ! swap endian deadbeef

	wavh%srate = 8000
	duration_seconds = 10.d0
	buffer_size = wavh%srate * duration_seconds

	allocate(buffer(buffer_size))

	header_length = storage_size(wavh) / BITS_PER_BYTE  ! fortran's sizeof()
	print *, "header_length = ", header_length

	wavh%chunk_size = 16
	wavh%format_tag = 1
	wavh%num_chans = 1
	wavh%bits_per_samp = 16
	wavh%bytes_per_sec = wavh%srate * wavh%bits_per_samp / BITS_PER_BYTE * wavh%num_chans
	wavh%bytes_per_samp = wavh%bits_per_samp / BITS_PER_BYTE * wavh%num_chans

	do i = 1, buffer_size
		if (i < buffer_size / 4) then
			buffer(i) = sin(2 * PI * MIDDLE_C  * i / wavh%srate) * 32000
		else if (i < buffer_size / 2) then
			buffer(i) = sin(2 * PI * MIDDLE_D  * i / wavh%srate) * 32000
		else if (i < buffer_size * 3 / 4) then
			buffer(i) = sin(2 * PI * MIDDLE_CS * i / wavh%srate) * 32000
		else
			buffer(i) = sin(2 * PI * MIDDLE_D  * i / wavh%srate) * 32000
		end if
	end do

!  // Playing a C Note
!  for (int i = 0; i < BUFFER_SIZE; i++) {
!
!    // There's a click at the C to D transition.  You could add a pause in
!	// between or change the phase of D to get a smooth transition
!    if (i < BUFFER_SIZE/2)
!      buffer[i] = (short int)((cos((2 * M_PI * MIDDLE_C * i) / sample_rate) * 32000));
!	else
!      buffer[i] = (short int)((cos((2 * M_PI * MIDDLE_D * i) / sample_rate) * 32000));
!
!  }

	wavh%dlength = buffer_size * wavh%bytes_per_samp
	wavh%flength = wavh%dlength + header_length

	print *, "dlength        = ", wavh%dlength
	print *, "flength        = ", wavh%flength
	print *, "bytes_per_sec  = ", wavh%bytes_per_sec
	print *, "bytes_per_samp = ", wavh%bytes_per_samp

	! TODO: remove old file first, or junk will be left over at end

	!open(file = "fort.wav", newunit = fid, form = "unformatted")
	open(file = "fort.wav", newunit = fid, form = "unformatted", access = "stream")

	! Holy fucking bingle.  Today I learned you can just write a whole struct to
	! a binary file all at once
	write(fid) wavh

	write(fid) buffer

!  wavh.dlength = BUFFER_SIZE * wavh.bytes_per_samp;
!  wavh.flength = wavh.dlength + header_length;
!
!  // Writing Wav File to Disk
!  FILE *fp = fopen("test.wav", "w");
!  fwrite(&wavh, 1, header_length, fp);
!  fwrite(buffer, 2, BUFFER_SIZE, fp);

	close(fid)

end subroutine write_wav_test


end module fynth__io

!// Populate Wav Struct
!
!struct wav_header wavh;
!
!const float MIDDLE_C  = 256.00;
!const float MIDDLE_CS = MIDDLE_C * pow(2.0, 1.0/12);
!const float MIDDLE_D  = MIDDLE_C * pow(2.0, 2.0/12);
!
!#define sample_rate 8000
!#define duration_seconds 10
!//const int buffer_size = sample_rate * duration_seconds;
!//#define BUFFER_SIZE (8000 * 10)  // sample_rate * duration_seconds
!#define BUFFER_SIZE (sample_rate * duration_seconds)  // sample_rate * duration_seconds
!short int buffer[BUFFER_SIZE] = {};
!
!const int header_length = sizeof(struct wav_header);
!
!int main(void)
!
!{
!  strncpy(wavh.riff, "RIFF", 4);
!  strncpy(wavh.wave, "WAVE", 4);
!  strncpy(wavh.fmt, "fmt ", 4);
!  strncpy(wavh.data, "data", 4);
!
!  wavh.chunk_size = 16;
!  wavh.format_tag = 1;
!  wavh.num_chans = 1;
!  wavh.srate = sample_rate;
!  wavh.bits_per_samp = 16;
!  wavh.bytes_per_sec = wavh.srate * wavh.bits_per_samp / 8 * wavh.num_chans;
!  wavh.bytes_per_samp = wavh.bits_per_samp / 8 * wavh.num_chans;
!
!  // Playing a C Note
!  for (int i = 0; i < BUFFER_SIZE; i++) {
!
!    // There's a click at the C to D transition.  You could add a pause in
!	// between or change the phase of D to get a smooth transition
!    if (i < BUFFER_SIZE/2)
!      buffer[i] = (short int)((cos((2 * M_PI * MIDDLE_C * i) / sample_rate) * 32000));
!	else
!      buffer[i] = (short int)((cos((2 * M_PI * MIDDLE_D * i) / sample_rate) * 32000));
!
!  }
!
!  wavh.dlength = BUFFER_SIZE * wavh.bytes_per_samp;
!  wavh.flength = wavh.dlength + header_length;
!
!  // Writing Wav File to Disk
!  FILE *fp = fopen("test.wav", "w");
!  fwrite(&wavh, 1, header_length, fp);
!  fwrite(buffer, 2, BUFFER_SIZE, fp);
!}
!

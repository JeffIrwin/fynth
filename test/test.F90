module fynth__test

	use fynth
	use fynth__audio
	use fynth__md5
	use fynth__utils

	implicit none

	interface test_eq_
		procedure :: test_eq_str
	end interface

	type args_t

		logical :: &
			rebase = .false.

	end type args_t

contains

!===============================================================================

subroutine get_next_arg(i, argv)
	! TODO: why is this not a fn that returns argv?

	integer, intent(inout) :: i
	character(len = :), allocatable, intent(out) :: argv
	!********
	character(len = :), allocatable, save :: argv0
	character(len = 1024) :: buffer
	integer, parameter :: STAT_TRUNC = -1
	integer :: io, argc
	logical, save :: first = .true.

	if (first) then
		first = .false.
		call get_command_argument(0, buffer)
		argv0 = trim(buffer)
	end if

	i = i + 1
	argc = command_argument_count()
	if (i > argc) then
		call panic("missing required argument after """//argv0//"""")
	end if

	call get_command_argument(i, buffer, status = io)
	if (io == STAT_TRUNC) then
		! Could make buffer allocatable and automatically try resizing
		call panic("command argument too long after """//argv0//"""")

	else if (io /= EXIT_SUCCESS) then
		call panic("cannot get command argument after """//argv0//"""")

	end if
	argv = trim(buffer)
	!print *, "argv = ", argv

	argv0 = argv

end subroutine get_next_arg

!===============================================================================

function read_args() result(args)

	! This argument parser is based on http://docopt.org/
	!
	! c.f. github.com/jeffirwin/cali, syntran, ribbit, etc.

	type(args_t) :: args

	!********

	character(len = :), allocatable :: argv

	integer :: i, argc, ipos

	logical :: error = .false.

	!! Defaults
	!args%maxerr = maxerr_def

	argc = command_argument_count()
	!print *, "argc = ", argc

	i = 0
	ipos = 0
	do while (i < argc)
		call get_next_arg(i, argv)

		select case (argv)

		case ("--rebase")
			args%rebase = .true.

		case default

			! Positional arg
			ipos = ipos + 1

			if (.false.) then
			!if (ipos == 1) then
			!	args%has_file1 = .true.
			!	args%file1 = argv

			!else if (ipos == 2) then
			!	args%has_file2 = .true.
			!	args%file2 = argv

			else
				write(*,*) ERROR_STR//"bad argument `"//argv//"`"
				error = .true.

			end if

		end select

	end do

	!if (ipos < 1 .and. .not. (args%help .or. args%version)) then
	!	write(*,*) ERROR_STR//"input file not defined"
	!	error = .true.
	!end if

	!if (args%has_file1) print *, "file1 = """, args%file1, """"

	if (error) then
		call fynth_exit(EXIT_FAILURE)
	end if

end function read_args

!===============================================================================

#define TEST_EQ(val, expect, ntot) test_eq_(val, expect, ntot, __FILE__, __LINE__)
integer function test_eq_str(val, expect, ntot, file_, line)

	character(len = *), intent(in) :: val, expect
	integer, intent(inout) :: ntot
	character(len = *), intent(in) :: file_
	integer, intent(in) :: line

	!print *, "len(val) = ", len(val)
	!print *, "len(expect) = ", len(expect)

	ntot = ntot + 1
	if (val == expect .and. len(val) == len(expect)) then
		test_eq_str = 0
	else
		test_eq_str = 1

		write(*,*) ERROR_STR//"str equality test failed at " &
			//file_//":"//to_str(line)
		write(*,*) "    Received value: """, val, """"
		write(*,*) "    Expected value: """, expect, """"

	end if
	!print *, "test_eq_str = ", test_eq_str

end function test_eq_str

!===============================================================================

subroutine test_md5(ntot, nfail)

	character(len = :), allocatable :: hash, expect
	integer, intent(inout) :: ntot, nfail

	write(*,*) "Testing md5 hashes ..."

	! String md5
	hash = md5_str("")
	expect = "d41d8cd98f00b204e9800998ecf8427e"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str("The quick brown fox jumps over the lazy dog")
	expect = "9e107d9d372bb6826bd81d3542a419d6"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	! File md5
	hash = md5_file("test/resources/quick.txt")
	expect = "9e107d9d372bb6826bd81d3542a419d6"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_file("test/resources/two-lines.txt")
	expect = "5982739113a22d42c8ecaa87ab68d11a"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str("jeff was here")
	expect = "8827ae539d0921970eb4837485220d18"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	! repeat to trigger multiple chunks
	hash = md5_str(repeat("jeff was here", 60))
	expect = "b33447f16a881ee569550ecff6182345"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str("abcdef609043")
	expect = "000001dbbfa3a5c83a2d506429c7b00e"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	!********************************************
	! Auto-generated tests from test/gen.sh
	!
	! These cover every possible string length mod 64

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 0))
	expect = "b33447f16a881ee569550ecff6182345"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 1))
	expect = "47e7dfb23631ee3092752519ab06f6fe"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 2))
	expect = "4cf9c920e78604f0aedc962c8a712589"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 3))
	expect = "3688de90e7d0e8569d82c3f637509952"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 4))
	expect = "29d02cd53d1200a2b5355ffdc85b737e"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 5))
	expect = "b610c5d3801e24aca3c3a25f1a8f301a"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 6))
	expect = "4d318ae0c7ba9dc42e7ebc6f312ed497"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 7))
	expect = "8a1c41b62ce2b7dd7a24fb499484143b"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 8))
	expect = "bdac85628dc4945a3e7896e78732d990"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 9))
	expect = "24102080879ae783fa9eb39f2fcb8f96"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 10))
	expect = "95af543af0fbbacd6d674b5aaedfa155"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 11))
	expect = "afbfc66571cbde4dee181684ed3e9b98"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 12))
	expect = "594ea89eb86f49bf2666864212d97cbe"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 13))
	expect = "523b002bc2b632f1d9820b16db1f13ab"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 14))
	expect = "e388fd784aabe7f11830140e413904f8"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 15))
	expect = "c3b463a244c76634fe00f109238a8d5c"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 16))
	expect = "f6c6838c57daa5a018cd2e54e8aefd3e"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 17))
	expect = "cbf10dce1299250f8f8dc6fa388b2563"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 18))
	expect = "a6ceafdd3a674dc3ddea3a92055dd3a2"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 19))
	expect = "aa2d3847dc1a768d135b90aff53e3375"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 20))
	expect = "03cc26208110ac2503489b03e3cdf5ef"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 21))
	expect = "1da778841193f0e16ede798ec0c1db2f"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 22))
	expect = "50852372dc9d800f7acf3c0e218845ad"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 23))
	expect = "cc6ca2a634230a4f887ff16fdb8d49ed"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 24))
	expect = "8fb5759720141d40e202a2d33e3a6a87"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 25))
	expect = "f365e57a009c891936ef7d7e3f6ae2c3"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 26))
	expect = "0b1341fcf9e0eb52187fb510eae86b9a"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 27))
	expect = "2c22d76dbb51de71383db0346cc44702"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 28))
	expect = "15c1ffe3df53ba73aae938736e8902ca"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 29))
	expect = "38e5d8d5ab7e5fc2090055b23ab4f551"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 30))
	expect = "56bf71a1877fb642b5a9bab1b4f06cb2"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 31))
	expect = "eaaf56aad35101c9b2b9db559a708c27"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 32))
	expect = "c5c1f47611ff463a45d0673b60cbbedd"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 33))
	expect = "d0c22f4dab2984876754c9260cde37d1"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 34))
	expect = "1b03023170198b3567a3e1e978d99d2f"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 35))
	expect = "f3d87998cda7db5043326aced77290c7"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 36))
	expect = "5b6794afc975a89ec125b05ff61d458b"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 37))
	expect = "8d172441bdc58960a972922057f9330a"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 38))
	expect = "94b34573b3cc01f98c521ff84c0f30ac"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 39))
	expect = "ab645443075410f200c8f7bbbb8c2f5c"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 40))
	expect = "cae582bc57090dba343ef6fdd87f2004"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 41))
	expect = "ea7a9d24624c2d76896d7667716d3de0"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 42))
	expect = "92a13d854725f8990eb87fd99c5e031f"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 43))
	expect = "55e3db2933e226b3c0d2077ac724f454"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 44))
	expect = "c2394c9a15094c80b89605e621faa9df"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 45))
	expect = "a0f1548ba8904b065180dd26224ec22e"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 46))
	expect = "c39483a3de6f4516a1aca9de1e249af1"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 47))
	expect = "1ea1221144a600ba406c2e6bf0db3896"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 48))
	expect = "b13a2526cc69df2c6abf24ac17b89770"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 49))
	expect = "97cc58742e7ee27816f24f9b8a4a0cc5"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 50))
	expect = "3b554e9c738e6b1eaf3ff076357fd2f2"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 51))
	expect = "e578ce6e97dbe4d094a689343df48e36"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 52))
	expect = "f565cae4970e710c9f9ceeecda71486e"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 53))
	expect = "8957dd759eaaecaa02b4fab7269ad616"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 54))
	expect = "38e8dcaae882b98d656886ea6058aac3"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 55))
	expect = "5110f07ffce5f3573eb1b96ecdbbc504"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 56))
	expect = "58b7037f8f16cef35553f2d1688dc19a"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 57))
	expect = "52e9a80cb73f9458123b6612f53fea12"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 58))
	expect = "94c127f15cd7e3e0b55a4d9990e0373e"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 59))
	expect = "ff389e4941824c389f1a127e48154a2d"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 60))
	expect = "909d25cdc4c253c93f35305ea1d2112e"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 61))
	expect = "b8ec9792326c00eea7019ec1cf44a232"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 62))
	expect = "3b6178b75248f13e984bcd8bc6d0d299"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 63))
	expect = "91615c939c8c566791ff587d0dffb4fd"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 64))
	expect = "53e90b499de0bd8dc7ac780069750fcb"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 65))
	expect = "da59149ecc0c1c925a141ac969b532a4"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 66))
	expect = "b288dcdc80495c50cbdb4c70cc4b6d95"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 67))
	expect = "7cd039520fcd864fd5cca4d4625b52c5"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 68))
	expect = "ad9097a4a5dd8f91a67f5edcb56b8580"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 69))
	expect = "af19dd6073c1f5289cb3fb8332cee0c6"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

	hash = md5_str(repeat("jeff was here", 60) // repeat(".", 70))
	expect = "1b4f8c94252180bf1411eb94691f9597"
	nfail = nfail + TEST_EQ(hash, expect, ntot)

end subroutine test_md5

!===============================================================================

subroutine test_basic_sounds(ntot, nfail, rebase)

	integer, intent(inout) :: ntot, nfail
	logical, intent(in) :: rebase

	!********

	character(len = :), allocatable :: fwav, fmd5, md5, md5_expect

	double precision :: freq, len_, cutoff

	type(env_t) :: env, fenv
	type(synth_t) :: synth

	write(*,*) "Testing basic sounds ..."

	! I'm not sure if this strategy of testing md5 checksums on wav files will
	! be robust.  Small numerical differences will break tests.  I could compare
	! waveform data between wav files, but that would require storing whole
	! baseline wav files in git, instead of just their checksums.  Maybe
	! compress them?
	!
	! Alternatively, we could do testing with a much smaller sample rate (or
	! length)

	! Set default null ADSR envelope and high cutoff
	env  = env_t(a = 0, d = 0, s = 1, r = 0)
	fenv = env_t(a = 0, d = 0, s = 0, r = 0)
	cutoff = 0.1d0 * huge(cutoff)

	synth%cutoff = cutoff
	synth%env = env
	synth%fenv = fenv

	!********
	fwav = "test/resources/sin.wav"
	fmd5 = fwav // ".md5"
	synth%wave => sine_wave
	freq = 310.d0
	len_ = 0.6d0
	call write_waveform(fwav, synth, freq, len_)
	md5 = md5_file(fwav)
	if (rebase) call write_file(fmd5, md5)
	md5_expect = read_file(fmd5)
	!print *, "md5 = ", md5
	nfail = nfail + TEST_EQ(md5, md5_expect, ntot)

	!********
	fwav = "test/resources/squ.wav"
	fmd5 = fwav // ".md5"
	synth%wave => square_wave
	freq = 320.d0
	len_ = 0.7d0
	call write_waveform(fwav, synth, freq, len_)
	md5 = md5_file(fwav)
	if (rebase) call write_file(fmd5, md5)
	md5_expect = read_file(fmd5)
	!print *, "md5 = ", md5
	nfail = nfail + TEST_EQ(md5, md5_expect, ntot)

	!********
	fwav = "test/resources/tri.wav"
	fmd5 = fwav // ".md5"
	synth%wave => triangle_wave
	freq = 330.d0
	len_ = 0.8d0
	call write_waveform(fwav, synth, freq, len_)
	md5 = md5_file(fwav)
	if (rebase) call write_file(fmd5, md5)
	md5_expect = read_file(fmd5)
	!print *, "md5 = ", md5
	nfail = nfail + TEST_EQ(md5, md5_expect, ntot)

	!********
	fwav = "test/resources/saw.wav"
	fmd5 = fwav // ".md5"
	synth%wave => sawtooth_wave
	freq = 340.d0
	len_ = 0.9d0
	call write_waveform(fwav, synth, freq, len_)
	md5 = md5_file(fwav)
	if (rebase) call write_file(fmd5, md5)
	md5_expect = read_file(fmd5)
	!print *, "md5 = ", md5
	nfail = nfail + TEST_EQ(md5, md5_expect, ntot)

	! TODO: consider adding a quiet arg for write_waveform() for clean test
	! logging
	write(*,*)

end subroutine test_basic_sounds

!===============================================================================

subroutine test_envelopes(ntot, nfail, rebase)

	integer, intent(inout) :: ntot, nfail
	logical, intent(in) :: rebase

	!********

	character(len = :), allocatable :: fwav, fmd5, md5, md5_expect

	double precision :: freq, len_, cutoff

	type(env_t) :: env, fenv
	type(synth_t) :: synth

	write(*,*) "Testing ADSR amplitude envelopes ..."

	! Set default null high cutoff
	cutoff = 0.1d0 * huge(cutoff)
	fenv = env_t(a = 0, d = 0, s = 0, r = 0)

	freq = 300.d0
	len_ = 1.0d0

	synth%cutoff = cutoff
	synth%fenv = fenv

	!********
	fwav = "test/resources/sin-env.wav"
	fmd5 = fwav // ".md5"
	env = env_t(a = 0.3d0, d = 0.2d0, s = 0.5d0, r = 0.4d0)
	synth%env = env
	synth%wave => sine_wave
	call write_waveform(fwav, synth, freq, len_)
	md5 = md5_file(fwav)
	if (rebase) call write_file(fmd5, md5)
	md5_expect = read_file(fmd5)
	nfail = nfail + TEST_EQ(md5, md5_expect, ntot)

	!********
	fwav = "test/resources/squ-env.wav"
	fmd5 = fwav // ".md5"
	env = env_t(a = 0.4d0, d = 0.3d0, s = 0.6d0, r = 0.5d0)
	synth%env = env
	synth%wave => square_wave
	call write_waveform(fwav, synth, freq, len_)
	md5 = md5_file(fwav)
	if (rebase) call write_file(fmd5, md5)
	md5_expect = read_file(fmd5)
	nfail = nfail + TEST_EQ(md5, md5_expect, ntot)

	!********
	fwav = "test/resources/tri-env.wav"
	fmd5 = fwav // ".md5"
	env = env_t(a = 0.2d0, d = 0.3d0, s = 0.7d0, r = 0.6d0)
	synth%env = env
	synth%wave => triangle_wave
	call write_waveform(fwav, synth, freq, len_)
	md5 = md5_file(fwav)
	if (rebase) call write_file(fmd5, md5)
	md5_expect = read_file(fmd5)
	nfail = nfail + TEST_EQ(md5, md5_expect, ntot)

	!********
	fwav = "test/resources/saw-env.wav"
	fmd5 = fwav // ".md5"
	env = env_t(a = 0.1d0, d = 0.2d0, s = 0.6d0, r = 0.3d0)
	synth%env = env
	synth%wave => sawtooth_wave
	call write_waveform(fwav, synth, freq, len_)
	md5 = md5_file(fwav)
	if (rebase) call write_file(fmd5, md5)
	md5_expect = read_file(fmd5)
	nfail = nfail + TEST_EQ(md5, md5_expect, ntot)

	!********
	! Edge case: len_ < attack + decay
	fwav = "test/resources/squ-env-ad.wav"
	fmd5 = fwav // ".md5"
	len_ = 0.25d0
	env = env_t(a = 0.1d0, d = 0.2d0, s = 0.7d0, r = 0.5d0)
	synth%env = env
	synth%wave => square_wave
	call write_waveform(fwav, synth, freq, len_)
	md5 = md5_file(fwav)
	if (rebase) call write_file(fmd5, md5)
	md5_expect = read_file(fmd5)
	nfail = nfail + TEST_EQ(md5, md5_expect, ntot)

	!********
	! Edge case: len_ < attack
	fwav = "test/resources/squ-env-a.wav"
	fmd5 = fwav // ".md5"
	len_ = 0.075
	env = env_t(a = 0.1d0, d = 0.2d0, s = 0.7d0, r = 0.5d0)
	synth%env = env
	synth%wave => square_wave
	call write_waveform(fwav, synth, freq, len_)
	md5 = md5_file(fwav)
	if (rebase) call write_file(fmd5, md5)
	md5_expect = read_file(fmd5)
	nfail = nfail + TEST_EQ(md5, md5_expect, ntot)

	!********
	!********
	! TODO: move this test to a filter subroutine
	fwav = "test/resources/squ-env-2.wav"
	fmd5 = fwav // ".md5"
	len_ = 3.4285714285714284d0
	env  = env_t(a = 1.2, d = 2.4, s = 0.8, r = 0.7)
	synth%fenv = env_t(a = 2.3, d = 1.3, s = 0, r = env%r)
	synth%cutoff = 300.d0
	synth%cutoff_max = 2250.d0
	synth%env = env
	synth%wave => square_wave
	call write_waveform(fwav, synth, freq, len_)
	md5 = md5_file(fwav)
	if (rebase) call write_file(fmd5, md5)
	md5_expect = read_file(fmd5)
	nfail = nfail + TEST_EQ(md5, md5_expect, ntot)

	write(*,*)

end subroutine test_envelopes

!===============================================================================

end module fynth__test

!===============================================================================

program main
	use fynth__test
	implicit none

	integer :: ntot, nfail

	type(args_t) :: args

	write(*,*) fg_bright_magenta//"Starting fynth tests"//color_reset
	write(*,*)

	args = read_args()

	ntot = 0
	nfail = 0

	call test_md5(ntot, nfail)
	call test_basic_sounds(ntot, nfail, args%rebase)
	call test_envelopes   (ntot, nfail, args%rebase)

	! TODO: more tests:
	!   - envelopes
	!   - filters
	!     * not sure if i have a stable api yet, e.g. filter envelopes, resonance
	!   - noise? fix seeding
	!   - csv conversion?
	!   - fft?
	!   - licc?

	write(*,*)
	write(*,*) fg_bright_magenta//"Finished fynth tests"//color_reset
	write(*,*) "Number of tests    = "//to_str(ntot)
	write(*,*) "Number of failures = "//to_str(nfail)
	if (nfail == 0) then
		write(*,*) fg_bright_green//"Success!"//color_reset
	else
		write(*,*) ERROR_STR//"some tests failed"
	end if

	call exit(nfail)

end program main

!===============================================================================


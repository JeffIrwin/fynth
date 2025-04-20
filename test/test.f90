module fynth__test

	use fynth
	use fynth__audio
	use fynth__md5
	use fynth__utils

	implicit none

	interface test_eq
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

integer function test_eq_str(val, expect, ntot)

	character(len = *), intent(in) :: val, expect
	integer, intent(inout) :: ntot

	!print *, "len(val) = ", len(val)
	!print *, "len(expect) = ", len(expect)

	ntot = ntot + 1
	if (val == expect .and. len(val) == len(expect)) then
		test_eq_str = 0
	else
		test_eq_str = 1

		write(*,*) ERROR_STR//"str equality test failed"
		write(*,*) "    Received value: """, val, """"
		write(*,*) "    Expected value: """, expect, """"

	end if
	!print *, "test_eq_str = ", test_eq_str

end function test_eq_str

!===============================================================================

subroutine test_md5(ntot, nfail)

	integer, intent(inout) :: ntot, nfail

	write(*,*) "Testing md5 hashes ..."

	! String md5
	nfail = nfail + test_eq(md5_str(""), &
		"d41d8cd98f00b204e9800998ecf8427e", ntot)

	nfail = nfail + test_eq(md5_str( &
		"The quick brown fox jumps over the lazy dog"), &
		"9e107d9d372bb6826bd81d3542a419d6", ntot)

	! File md5
	nfail = nfail + test_eq(md5_file( &
		"test/resources/quick.txt"), &
		"9e107d9d372bb6826bd81d3542a419d6", ntot)
	nfail = nfail + test_eq(md5_file( &
		"test/resources/two-lines.txt"), &
		"5982739113a22d42c8ecaa87ab68d11a", ntot)

	nfail = nfail + test_eq(md5_str("jeff was here"), &
		"8827ae539d0921970eb4837485220d18", ntot)

	! repeat to trigger multiple chunks
	nfail = nfail + test_eq(md5_str(repeat("jeff was here", 60)), &
		"b33447f16a881ee569550ecff6182345", ntot)

	nfail = nfail + test_eq(md5_str("abcdef609043"), &
		"000001dbbfa3a5c83a2d506429c7b00e", ntot)

	!********************************************
	! Auto-generated tests from test/gen.sh
	!
	! These cover every possible string length mod 64

	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 0)), &
		"b33447f16a881ee569550ecff6182345", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 1)), &
		"47e7dfb23631ee3092752519ab06f6fe", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 2)), &
		"4cf9c920e78604f0aedc962c8a712589", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 3)), &
		"3688de90e7d0e8569d82c3f637509952", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 4)), &
		"29d02cd53d1200a2b5355ffdc85b737e", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 5)), &
		"b610c5d3801e24aca3c3a25f1a8f301a", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 6)), &
		"4d318ae0c7ba9dc42e7ebc6f312ed497", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 7)), &
		"8a1c41b62ce2b7dd7a24fb499484143b", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 8)), &
		"bdac85628dc4945a3e7896e78732d990", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 9)), &
		"24102080879ae783fa9eb39f2fcb8f96", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 10)), &
		"95af543af0fbbacd6d674b5aaedfa155", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 11)), &
		"afbfc66571cbde4dee181684ed3e9b98", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 12)), &
		"594ea89eb86f49bf2666864212d97cbe", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 13)), &
		"523b002bc2b632f1d9820b16db1f13ab", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 14)), &
		"e388fd784aabe7f11830140e413904f8", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 15)), &
		"c3b463a244c76634fe00f109238a8d5c", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 16)), &
		"f6c6838c57daa5a018cd2e54e8aefd3e", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 17)), &
		"cbf10dce1299250f8f8dc6fa388b2563", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 18)), &
		"a6ceafdd3a674dc3ddea3a92055dd3a2", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 19)), &
		"aa2d3847dc1a768d135b90aff53e3375", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 20)), &
		"03cc26208110ac2503489b03e3cdf5ef", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 21)), &
		"1da778841193f0e16ede798ec0c1db2f", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 22)), &
		"50852372dc9d800f7acf3c0e218845ad", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 23)), &
		"cc6ca2a634230a4f887ff16fdb8d49ed", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 24)), &
		"8fb5759720141d40e202a2d33e3a6a87", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 25)), &
		"f365e57a009c891936ef7d7e3f6ae2c3", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 26)), &
		"0b1341fcf9e0eb52187fb510eae86b9a", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 27)), &
		"2c22d76dbb51de71383db0346cc44702", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 28)), &
		"15c1ffe3df53ba73aae938736e8902ca", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 29)), &
		"38e5d8d5ab7e5fc2090055b23ab4f551", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 30)), &
		"56bf71a1877fb642b5a9bab1b4f06cb2", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 31)), &
		"eaaf56aad35101c9b2b9db559a708c27", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 32)), &
		"c5c1f47611ff463a45d0673b60cbbedd", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 33)), &
		"d0c22f4dab2984876754c9260cde37d1", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 34)), &
		"1b03023170198b3567a3e1e978d99d2f", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 35)), &
		"f3d87998cda7db5043326aced77290c7", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 36)), &
		"5b6794afc975a89ec125b05ff61d458b", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 37)), &
		"8d172441bdc58960a972922057f9330a", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 38)), &
		"94b34573b3cc01f98c521ff84c0f30ac", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 39)), &
		"ab645443075410f200c8f7bbbb8c2f5c", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 40)), &
		"cae582bc57090dba343ef6fdd87f2004", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 41)), &
		"ea7a9d24624c2d76896d7667716d3de0", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 42)), &
		"92a13d854725f8990eb87fd99c5e031f", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 43)), &
		"55e3db2933e226b3c0d2077ac724f454", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 44)), &
		"c2394c9a15094c80b89605e621faa9df", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 45)), &
		"a0f1548ba8904b065180dd26224ec22e", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 46)), &
		"c39483a3de6f4516a1aca9de1e249af1", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 47)), &
		"1ea1221144a600ba406c2e6bf0db3896", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 48)), &
		"b13a2526cc69df2c6abf24ac17b89770", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 49)), &
		"97cc58742e7ee27816f24f9b8a4a0cc5", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 50)), &
		"3b554e9c738e6b1eaf3ff076357fd2f2", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 51)), &
		"e578ce6e97dbe4d094a689343df48e36", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 52)), &
		"f565cae4970e710c9f9ceeecda71486e", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 53)), &
		"8957dd759eaaecaa02b4fab7269ad616", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 54)), &
		"38e8dcaae882b98d656886ea6058aac3", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 55)), &
		"5110f07ffce5f3573eb1b96ecdbbc504", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 56)), &
		"58b7037f8f16cef35553f2d1688dc19a", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 57)), &
		"52e9a80cb73f9458123b6612f53fea12", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 58)), &
		"94c127f15cd7e3e0b55a4d9990e0373e", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 59)), &
		"ff389e4941824c389f1a127e48154a2d", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 60)), &
		"909d25cdc4c253c93f35305ea1d2112e", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 61)), &
		"b8ec9792326c00eea7019ec1cf44a232", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 62)), &
		"3b6178b75248f13e984bcd8bc6d0d299", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 63)), &
		"91615c939c8c566791ff587d0dffb4fd", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 64)), &
		"53e90b499de0bd8dc7ac780069750fcb", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 65)), &
		"da59149ecc0c1c925a141ac969b532a4", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 66)), &
		"b288dcdc80495c50cbdb4c70cc4b6d95", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 67)), &
		"7cd039520fcd864fd5cca4d4625b52c5", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 68)), &
		"ad9097a4a5dd8f91a67f5edcb56b8580", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 69)), &
		"af19dd6073c1f5289cb3fb8332cee0c6", ntot)
	nfail = nfail + test_eq(md5_str( &
		repeat("jeff was here", 60) &
		// repeat(".", 70)), &
		"1b4f8c94252180bf1411eb94691f9597", ntot)

end subroutine test_md5

!===============================================================================

subroutine test_basic_sounds(ntot, nfail, rebase)

	integer, intent(inout) :: ntot, nfail
	logical, intent(in) :: rebase

	!********

	character(len = :), allocatable :: fwav, fmd5, md5, md5_expect

	double precision :: freq, len_, cutoff

	procedure(fn_f64_to_f64), pointer :: waveform_fn

	type(env_t) :: env

	write(*,*) "Testing basic sounds ..."

	! I'm not sure if this strategy of testing md5 checksums on wav files will
	! be robust.  Small numerical differences could break tests.  I could
	! compare waveform data between wav files, but that would require storing
	! whole baseline wav files in git, instead of just their checksums

	! Set default null ADSR envelope and high cutoff
	env = env_t(a = 0, d = 0, s = 1, r = 0)
	cutoff = huge(cutoff)

	!********
	fwav = "test/resources/sin.wav"
	fmd5 = fwav // ".md5"
	waveform_fn => sine_wave
	freq = 300.d0
	len_ = 1.d0
	call write_waveform(fwav, waveform_fn, freq, len_, env, cutoff)
	md5 = md5_file(fwav)
	if (rebase) call write_file(fmd5, md5)
	md5_expect = read_file(fmd5)
	!print *, "md5 = ", md5
	nfail = nfail + test_eq(md5, md5_expect, ntot)

	!********
	fwav = "test/resources/squ.wav"
	fmd5 = fwav // ".md5"
	waveform_fn => square_wave
	freq = 300.d0
	len_ = 1.d0
	call write_waveform(fwav, waveform_fn, freq, len_, env, cutoff)
	md5 = md5_file(fwav)
	if (rebase) call write_file(fmd5, md5)
	md5_expect = read_file(fmd5)
	!print *, "md5 = ", md5
	nfail = nfail + test_eq(md5, md5_expect, ntot)

end subroutine test_basic_sounds

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


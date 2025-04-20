module fynth__test

	use fynth__md5

	implicit none

	interface test_eq
		procedure :: test_eq_str
	end interface

contains

integer function test_eq_str(a, b, ntot)

	character(len = *), intent(in) :: a, b
	integer, intent(inout) :: ntot

	!print *, "a = '", a, "'"
	!print *, "b = '", b, "'"
	!print *, "len(a) = ", len(a)
	!print *, "len(b) = ", len(b)

	ntot = ntot + 1
	if (a == b .and. len(a) == len(b)) then
		test_eq_str = 0
	else
		test_eq_str = 1
	end if
	!print *, "test_eq_str = ", test_eq_str

end function test_eq_str

subroutine test_md5(ntot, nfail)

	integer, intent(inout) :: ntot, nfail

	nfail = nfail + test_eq(md5_str(""), &
		"d41d8cd98f00b204e9800998ecf8427e", ntot)

	nfail = nfail + test_eq(md5_str( &
		"The quick brown fox jumps over the lazy dog"), &
		"9e107d9d372bb6826bd81d3542a419d6", ntot)

	nfail = nfail + test_eq(md5_str("jeff was here"), &
		"8827ae539d0921970eb4837485220d18", ntot)

	! repeat to trigger multiple chunks
	nfail = nfail + test_eq(md5_str(repeat("jeff was here", 60)), &
		"b33447f16a881ee569550ecff6182345", ntot)

	! TODO: test every possible len mod 64 (repeat a's or something).  need a
	! bash script to verify baseline before commiting

	nfail = nfail + test_eq(md5_str("abcdef609043"), &
		"000001dbbfa3a5c83a2d506429c7b00e", ntot)

end subroutine

end module fynth__test

program main
	use fynth__test
	implicit none

	integer :: ntot, nfail

	ntot = 0
	nfail = 0

	call test_md5(ntot, nfail)

	write(*, "(a,i0)") "Number of tests    = ", ntot
	write(*, "(a,i0)") "Number of failures = ", nfail

	call exit(nfail)

end program main

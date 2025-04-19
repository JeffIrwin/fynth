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

	nfail = nfail + test_eq(md5_str(""), "d41d8cd98f00b204e9800998ecf8427e", ntot)

!//	status += assert_eq(md5(
!//			""),
!//			"d41d8cd98f00b204e9800998ecf8427e"
!//	);
!//	status += assert_eq(md5(
!//			"The quick brown fox jumps over the lazy dog"),
!//			"9e107d9d372bb6826bd81d3542a419d6"
!//	);
!//	status += assert_eq(md5(
!//			"jeff was here"),
!//			"8827ae539d0921970eb4837485220d18"
!//	);
!//	status += assert_eq(md5(
!//			repeat("jeff was here", 60)), // repeat to trigger multiple chunks
!//			"b33447f16a881ee569550ecff6182345"
!//	);
!//	status += assert_eq(md5(
!//			"abcdef609043"),
!//			"000001dbbfa3a5c83a2d506429c7b00e"
!//	);

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


module fynth__utils

	implicit none

	!********

	! Constants

	character, parameter :: NULL_CHAR = char(0)

	double precision, parameter :: PI = 4.d0 * atan(1.d0)

	integer, parameter :: &
		BITS_PER_BYTE = 8

	!********

	type vec_f64_t
		double precision, allocatable :: v(:)
		integer :: len_, cap
		contains
			procedure :: push     => push_vec_f64
	end type vec_f64_t

	!********

	! External C fns
	interface
		integer function rm_file_c(filename)
			character(len = *), intent(in) :: filename
		end function rm_file_c
	end interface

contains

!===============================================================================

integer function rm_file(filename)
	! This fortran wrapper appends NULL_CHAR and removes the file using c
	character(len = *), intent(in) :: filename
	rm_file = rm_file_c(filename//NULL_CHAR)
end function rm_file

!===============================================================================

function new_vec_f64() result(vector)

	type(vec_f64_t) :: vector

	vector%len_ = 0
	vector%cap = 2

	allocate(vector%v( vector%cap ))

end function new_vec_f64

!===============================================================================

subroutine push_vec_f64(vector, val)

	class(vec_f64_t) :: vector

	double precision, intent(in) :: val

	!********

	double precision, allocatable :: tmp(:)

	integer :: tmp_cap

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len_
		allocate(tmp( tmp_cap ))
		tmp(1: vector%cap) = vector%v

		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap

	end if

	vector%v( vector%len_ ) = val

end subroutine push_vec_f64

!===============================================================================

end module fynth__utils


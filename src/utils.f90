
module fynth__utils

	implicit none

	character, parameter :: NULL_CHAR = char(0)

	double precision, parameter :: PI = 4.d0 * atan(1.d0)

	integer, parameter :: &
		BITS_PER_BYTE = 8

	! External C fns
	interface
		integer function rm_file_c(filename)
			character(len = *), intent(in) :: filename
		end function rm_file_c
	end interface

contains

	integer function rm_file(filename)
		! This fortran wrapper appends NULL_CHAR and removes the file using c
		character(len = *), intent(in) :: filename
		rm_file = rm_file_c(filename//NULL_CHAR)
	end function rm_file

end module fynth__utils



module fynth__utils

	implicit none

	!********

	! Constants

	double precision, parameter :: PI = 4.d0 * atan(1.d0)

	integer, parameter :: &
		BITS_PER_BYTE = 8

	integer, parameter :: &
		EXIT_FAILURE = -1, &
		EXIT_SUCCESS = 0

	character, parameter :: &
			NULL_CHAR       = char( 0), &
			TAB             = char( 9), &
			LINE_FEED       = char(10), &
			VERT_TAB        = char(11), &
			CARRIAGE_RETURN = char(13), &
			ESC             = char(27)

	! TODO: make these variables, with colors disabled if output_unit is not tty
	! and an option to --force-color
	character(len = *), parameter :: &
			fg_bold               = esc//"[;1m", &
			fg_yellow             = esc//"[33m", &
			fg_bright_red         = esc//"[91m", &
			fg_bold_bright_red    = esc//"[91;1m", &
			fg_bold_bright_yellow = esc//"[93;1m", &
			fg_bright_green       = esc//"[92m", &
			fg_bright_yellow      = esc//"[93m", &
			fg_bright_blue        = esc//"[94m", &
			fg_bright_magenta     = esc//"[95m", &
			fg_bright_cyan        = esc//"[96m", &
			fg_bright_white       = esc//"[97m", &
			color_reset           = esc//"[0m"

	character(len = *), parameter :: &
		ERROR_STR = fg_bold_bright_red//"Error"  //fg_bold//": "//color_reset, &
		WARN_STR  = fg_yellow//"Warning"//fg_bold//": "//color_reset

	!********

	type vec_f64_t
		double precision, allocatable :: v(:)
		integer :: len_, cap
		contains
			procedure :: &
				push => push_vec_f64, &
				trim => trim_vec_f64
	end type vec_f64_t

	!********

	interface to_str
		procedure :: to_str_i16
		procedure :: to_str_i32
		procedure :: to_str_i64
		procedure :: to_str_f64
	end interface to_str

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

subroutine trim_vec_f64(vector)
	class(vec_f64_t) :: vector
	vector%v = vector%v(1: vector%len_)
end subroutine trim_vec_f64

!===============================================================================

function to_str_i16(int_) result(str)
	integer(kind = 2), intent(in) :: int_
	character(len = :), allocatable :: str
	character :: buffer*8
	write(buffer, "(i0)") int_
	str = trim(buffer)
end function to_str_i16

!********

function to_str_i32(int_) result(str)
	integer(kind = 4), intent(in) :: int_
	character(len = :), allocatable :: str
	character :: buffer*16
	write(buffer, "(i0)") int_
	str = trim(buffer)
end function to_str_i32

!********

function to_str_i64(int_) result(str)
	integer(kind = 8), intent(in) :: int_
	character(len = :), allocatable :: str
	character :: buffer*24
	write(buffer, "(i0)") int_
	str = trim(buffer)
end function to_str_i64

!********

function to_str_f64(x) result(str)
	double precision, intent(in) :: x
	character(len = :), allocatable :: str
	character :: buffer*24
	write(buffer, "(es24.13)") x
	str = trim(adjustl(buffer))
end function to_str_f64

!===============================================================================

subroutine panic(msg)
	character(len = *), intent(in) :: msg
	write(*,*) ERROR_STR//msg
	call fynth_exit(EXIT_FAILURE)
end subroutine panic

!===============================================================================

subroutine fynth_exit(exit_code)
	integer, intent(in) :: exit_code
	if (exit_code == EXIT_SUCCESS) write(*,*) fg_bright_green//"Finished fynth"//color_reset
	call exit(exit_code)
end subroutine fynth_exit

!===============================================================================

function get_file_extension(filename) result(extension)

	! Include "." and double extensions if present (including whole basename for
	! dotfiles).  May be empty string

	character(len = *), intent(in)  :: filename
	character(len = :), allocatable :: extension
	!********
	integer :: beg_, end_, i

	beg_ = 1
	end_ = len(filename)

	!print *, 'len = ', end_

	i = scan(filename, '/\', .true.)
	if (i /= 0) beg_ = i + 1

	i = scan(filename(beg_:), '.')
	if (i /= 0) end_ = beg_ + i - 2

	extension = filename(end_ + 1:)
	!print *, 'beg_, end_ = ', beg_, end_
	!print *, 'extension = ', extension

end function get_file_extension

!===============================================================================

end module fynth__utils


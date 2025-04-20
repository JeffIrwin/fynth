module fynth__md5

	implicit none

	integer, parameter :: K_MD5(*) = [ &
		int(z"d76aa478"), int(z"e8c7b756"), int(z"242070db"), int(z"c1bdceee"), &
		int(z"f57c0faf"), int(z"4787c62a"), int(z"a8304613"), int(z"fd469501"), &
		int(z"698098d8"), int(z"8b44f7af"), int(z"ffff5bb1"), int(z"895cd7be"), &
		int(z"6b901122"), int(z"fd987193"), int(z"a679438e"), int(z"49b40821"), &
		int(z"f61e2562"), int(z"c040b340"), int(z"265e5a51"), int(z"e9b6c7aa"), &
		int(z"d62f105d"), int(z"02441453"), int(z"d8a1e681"), int(z"e7d3fbc8"), &
		int(z"21e1cde6"), int(z"c33707d6"), int(z"f4d50d87"), int(z"455a14ed"), &
		int(z"a9e3e905"), int(z"fcefa3f8"), int(z"676f02d9"), int(z"8d2a4c8a"), &
		int(z"fffa3942"), int(z"8771f681"), int(z"6d9d6122"), int(z"fde5380c"), &
		int(z"a4beea44"), int(z"4bdecfa9"), int(z"f6bb4b60"), int(z"bebfbc70"), &
		int(z"289b7ec6"), int(z"eaa127fa"), int(z"d4ef3085"), int(z"04881d05"), &
		int(z"d9d4d039"), int(z"e6db99e5"), int(z"1fa27cf8"), int(z"c4ac5665"), &
		int(z"f4292244"), int(z"432aff97"), int(z"ab9423a7"), int(z"fc93a039"), &
		int(z"655b59c3"), int(z"8f0ccc92"), int(z"ffeff47d"), int(z"85845dd1"), &
		int(z"6fa87e4f"), int(z"fe2ce6e0"), int(z"a3014314"), int(z"4e0811a1"), &
		int(z"f7537e82"), int(z"bd3af235"), int(z"2ad7d2bb"), int(z"eb86d391")  &
	]

	! S_MD5 specifies the per-round shift amounts
	integer, parameter :: S_MD5(*) = [ &
		7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22, &
		5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20, &
		4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23, &
		6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21  &
	]

	character(len = *), parameter :: HEX_CHARS_MD5 = "0123456789abcdef";

contains

!===============================================================================

integer function inot(a)
	integer :: a
	inot = ieor(a, int(z"ffffffff", 4))
end function inot

!===============================================================================

integer function leftrotate_md5(x, rot)
	integer, intent(in) :: x, rot
	! c.f. aes_rotl8() from syntran/samples/aes.syntran

	leftrotate_md5 = &
		ior(shiftl(x, rot), shiftr(x, 32-rot))

end function leftrotate_md5

!fn leftrotate_md5(x: i32, rot: i32): i32
!{
!	return ((x << rot) | (x >> (32 - rot)));
!}

!===============================================================================

!// S_MD5 specifies the per-round shift amounts
!let S_MD5 = [
!	7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
!	5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
!	4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
!	6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
!];
!

!let K_MD5 = [
!	0xd76a_a478, 0xe8c7_b756, 0x2420_70db, 0xc1bd_ceee,
!	0xf57c_0faf, 0x4787_c62a, 0xa830_4613, 0xfd46_9501,
!	0x6980_98d8, 0x8b44_f7af, 0xffff_5bb1, 0x895c_d7be,
!	0x6b90_1122, 0xfd98_7193, 0xa679_438e, 0x49b4_0821,
!	0xf61e_2562, 0xc040_b340, 0x265e_5a51, 0xe9b6_c7aa,
!	0xd62f_105d, 0x0244_1453, 0xd8a1_e681, 0xe7d3_fbc8,
!	0x21e1_cde6, 0xc337_07d6, 0xf4d5_0d87, 0x455a_14ed,
!	0xa9e3_e905, 0xfcef_a3f8, 0x676f_02d9, 0x8d2a_4c8a,
!	0xfffa_3942, 0x8771_f681, 0x6d9d_6122, 0xfde5_380c,
!	0xa4be_ea44, 0x4bde_cfa9, 0xf6bb_4b60, 0xbebf_bc70,
!	0x289b_7ec6, 0xeaa1_27fa, 0xd4ef_3085, 0x0488_1d05,
!	0xd9d4_d039, 0xe6db_99e5, 0x1fa2_7cf8, 0xc4ac_5665,
!	0xf429_2244, 0x432a_ff97, 0xab94_23a7, 0xfc93_a039,
!	0x655b_59c3, 0x8f0c_cc92, 0xffef_f47d, 0x8584_5dd1,
!	0x6fa8_7e4f, 0xfe2c_e6e0, 0xa301_4314, 0x4e08_11a1,
!	0xf753_7e82, 0xbd3a_f235, 0x2ad7_d2bb, 0xeb86_d391
!];

!//println("S_MD5 = ", S_MD5);
!//println("K_MD5 = ", K_MD5);
!
!let HEX_CHARS_MD5 = "0123456789abcdef";

function md5_str(msg_in) result(hex_digest)
	! Calculate the MD5 hash of a str message and return the digest as a
	! 32-char hex str
	!
	! The digest is also available as an array of 4 ints near the end of this
	! fn
	!
	! c.f. aoc-syntran/md5.syntran

	character(len = *), intent(in)  :: msg_in  ! TODO: rename in/loc vars
	character(len = :), allocatable :: hex_digest

	!********

	character(len = :), allocatable :: msg

	integer :: i, j, ij, k, ic, id, a0, b0, c0, d0, a, b, c, d, f, g, len0, num_zeros
	integer :: m(0: 15), digest(0: 3)

	print *, "starting md5_str()"
	print *, "msg_in = """, msg_in, """"

	! Initialize variables
	a0 = int(z"67452301")  ! note endianness
	b0 = int(z"efcdab89")
	c0 = int(z"98badcfe")
	d0 = int(z"10325476")

	len0 = len(msg_in) * 8; ! length *in bits* :(

	! Assume msg_in is complete bytes (no partial bytes)

	! Perform pre-process padding

	! This line could probably be simplified
	num_zeros = 63 - mod(mod((len(msg_in) - 56), 64) + 64, 64)
	!let num_zeros = 63 - ((i32(len(msg_in)) - 56) % 64 + 64) % 64;
	!//msg_in += repeat(char(0x00), num_zeros);
	print *, "num_zeros = ", num_zeros
	
	print *, "msg_in = `", msg_in, "`"
	print *, "len(msg_in) = ", len(msg_in)

	! append original length in bits mod 264 to message

	! Do all of the padding steps at once to avoid too much string growth
	! amortization
	msg = msg_in &
		// char(128) &
		// repeat(char(0), num_zeros) &
		// char( iand(255, shiftr(len0,  0) )) &
		// char( iand(255, shiftr(len0,  8) )) &
		// char( iand(255, shiftr(len0, 16) )) &
		// char( iand(255, shiftr(len0, 24) )) &
		// char(0) &
		// char(0) &
		// char(0) &
		// char(0)

	print *, "msg = `", msg, "`"
	print *, "len(msg) = ", len(msg)

	do i = 0, len(msg) - 1, 64
	!for i in [0: 64: len(msg)]

		print *, "i = ", i
		!//let chunk = msg[i: i + 64];
	
		! break chunk into sixteen 32-bit words M[j], 0 ≤ j ≤ 15
		m = 0
		do j = 0, 15
			ij = i + 4*j + 1 ; m(j) = ior(m(j), shiftl(ichar(msg(ij:ij)),  0) )
			ij = i + 4*j + 2 ; m(j) = ior(m(j), shiftl(ichar(msg(ij:ij)),  8) )
			ij = i + 4*j + 3 ; m(j) = ior(m(j), shiftl(ichar(msg(ij:ij)), 16) )
			ij = i + 4*j + 4 ; m(j) = ior(m(j), shiftl(ichar(msg(ij:ij)), 24) )
		end do
		print *, "m = ", m

		!for j in [0: 16]
		!{
		!	m[j] |= i32(msg[i + 4 * j + 0]) <<  0;
		!	m[j] |= i32(msg[i + 4 * j + 1]) <<  8;
		!	m[j] |= i32(msg[i + 4 * j + 2]) << 16;
		!	m[j] |= i32(msg[i + 4 * j + 3]) << 24;
		!}
		!//println("m = ", m);
	
		! Initialize hash value for this chunk:
		a = a0
		b = b0
		c = c0
		d = d0
	
		! Main loop
		do k = 0, 63
		!for k in [0: 64]

			f = 0
			g = 0
			if (0 <= k .and. k <= 15) then
			!if 0 <= k and k <= 15 {
				f = ior(iand(b, c), iand(inot(b), d))
				g = k
				!f = (b & c) | (!b & d);
				!g = k;
			else if (16 <= k .and. k <= 31) then
			!} else if 16 <= k and k <= 31 {
				f = ior(iand(d, b), iand(inot(d), c))
				g = mod(5*k + 1, 16)
				!f = (d & b) | (!d & c);
				!g = (5*k + 1) % 16;
			else if (32 <= k .and. k <= 47) then
			!} else if 32 <= k and k <= 47 {
				f = ieor(ieor(b, c), d)
				g = mod(3*k + 5, 16)
				!f = b ^ c ^ d;
				!g = (3*k + 5) % 16;
			else if (48 <= k .and. k <= 63) then
				f = ieor(c, ior(b, inot(d)))
				g = mod(7*k, 16)
			!} else if 48 <= k and k <= 63 {
				!f = c ^ (b | !d);
				!g = (7*k) % 16;
			!}
			end if

			! "Be wary of the below definitions of a,b,c,d" (what did they mean
			! by this?)
			f = f + a + K_MD5(k+1) + m(g)  ! m[g] must be a 32-bit block
			a = d
			d = c
			c = b
			b = b + leftrotate_md5(f, S_MD5(k+1))

		end do
	
		! Add this chunk's hash to result so far
		a0 = a0 + a
		b0 = b0 + b
		c0 = c0 + c
		d0 = d0 + d

	end do

	!****************

	digest = [a0, b0, c0, d0]
	print *, "digest = ", digest

	hex_digest = repeat(" ", 32)

	j = 0
	do id = 0, 3
		d = digest(id)
		do i = 0, 3
			ic = iand(shiftr(d, 4 * (2*i + 1)), int(z"f")) + 1
			hex_digest(j+1: j+1) = HEX_CHARS_MD5(ic: ic)

			ic = iand(shiftr(d, 4 * (2*i + 0)), int(z"f")) + 1
			hex_digest(j+2: j+2) = HEX_CHARS_MD5(ic: ic)
			j = j + 2
		end do
	end do
!	let j = 0;
!	for d in digest
!		for i in [0: 4]
!		{
!			hex_digest[j+0] = HEX_CHARS_MD5[(d >> (4 * (2*i + 1))) & 0xf];
!			hex_digest[j+1] = HEX_CHARS_MD5[(d >> (4 * (2*i + 0))) & 0xf];
!			j += 2;
!		}

	print *, "hex_digest = ", hex_digest

end function md5_str

!===============================================================================

end module fynth__md5


module fynth__notes

	implicit none

	double precision, parameter :: &
		A_440 = 440.d0, &
		C4  = A_440 * 2.d0 ** (3.d0 / 12.d0 - 1.d0), &  ! Middle C
		C3  = A_440 * 2.d0 ** (3.d0 / 12.d0 - 2.d0), &  ! one octave down from middle C
		C2  = A_440 * 2.d0 ** (3.d0 / 12.d0 - 3.d0), &
		C1  = A_440 * 2.d0 ** (3.d0 / 12.d0 - 4.d0), &
		C0  = A_440 * 2.d0 ** (3.d0 / 12.d0 - 5.d0), &  ! ~ 16.3 Hz, not audible to humans
		C5  = A_440 * 2.d0 ** (3.d0 / 12.d0 + 0.d0), &  ! one octave up from middle C
		C6  = A_440 * 2.d0 ** (3.d0 / 12.d0 + 1.d0), &
		C7  = A_440 * 2.d0 ** (3.d0 / 12.d0 + 2.d0), &
		C8  = A_440 * 2.d0 ** (3.d0 / 12.d0 + 3.d0), &
		C9  = A_440 * 2.d0 ** (3.d0 / 12.d0 + 4.d0)     ! C8 is already very high

	double precision, parameter :: &
		CS4 = C4 * 2.d0 ** ( 1.d0 / 12.d0), &  ! C sharp (just above middle C)
		DF4 = C4 * 2.d0 ** ( 1.d0 / 12.d0), &  ! D flat (enharmonic)
		D4  = C4 * 2.d0 ** ( 2.d0 / 12.d0), &
		DS4 = C4 * 2.d0 ** ( 3.d0 / 12.d0), &
		EF4 = C4 * 2.d0 ** ( 3.d0 / 12.d0), &  ! white key enharmonics are included
		E4  = C4 * 2.d0 ** ( 4.d0 / 12.d0), &
		FF4 = C4 * 2.d0 ** ( 4.d0 / 12.d0), &
		ES4 = C4 * 2.d0 ** ( 5.d0 / 12.d0), &
		F4  = C4 * 2.d0 ** ( 5.d0 / 12.d0), &
		FS4 = C4 * 2.d0 ** ( 6.d0 / 12.d0), &
		GF4 = C4 * 2.d0 ** ( 6.d0 / 12.d0), &
		G4  = C4 * 2.d0 ** ( 7.d0 / 12.d0), &
		GS4 = C4 * 2.d0 ** ( 8.d0 / 12.d0), &
		AF4 = C4 * 2.d0 ** ( 8.d0 / 12.d0), &
		A4  = C4 * 2.d0 ** ( 9.d0 / 12.d0), &
		AS4 = C4 * 2.d0 ** (10.d0 / 12.d0), &
		BF4 = C4 * 2.d0 ** (10.d0 / 12.d0), &
		B4  = C4 * 2.d0 ** (11.d0 / 12.d0), &
		BS4 = C4 * 2.d0 ** (12.d0 / 12.d0)

	double precision, parameter :: &
		CS3 = C3 * 2.d0 ** ( 1.d0 / 12.d0), &
		DF3 = C3 * 2.d0 ** ( 1.d0 / 12.d0), &
		D3  = C3 * 2.d0 ** ( 2.d0 / 12.d0), &
		DS3 = C3 * 2.d0 ** ( 3.d0 / 12.d0), &
		EF3 = C3 * 2.d0 ** ( 3.d0 / 12.d0), &
		E3  = C3 * 2.d0 ** ( 4.d0 / 12.d0), &
		FF3 = C3 * 2.d0 ** ( 4.d0 / 12.d0), &
		ES3 = C3 * 2.d0 ** ( 5.d0 / 12.d0), &
		F3  = C3 * 2.d0 ** ( 5.d0 / 12.d0), &
		FS3 = C3 * 2.d0 ** ( 6.d0 / 12.d0), &
		GF3 = C3 * 2.d0 ** ( 6.d0 / 12.d0), &
		G3  = C3 * 2.d0 ** ( 7.d0 / 12.d0), &
		GS3 = C3 * 2.d0 ** ( 8.d0 / 12.d0), &
		AF3 = C3 * 2.d0 ** ( 8.d0 / 12.d0), &
		A3  = C3 * 2.d0 ** ( 9.d0 / 12.d0), &
		AS3 = C3 * 2.d0 ** (10.d0 / 12.d0), &
		BF3 = C3 * 2.d0 ** (10.d0 / 12.d0), &
		B3  = C3 * 2.d0 ** (11.d0 / 12.d0), &
		BS3 = C3 * 2.d0 ** (12.d0 / 12.d0)

	! TODO: other octaves.  Obviously this is WET, but I like having direct note
	! names instead of having to index into an array of frequencies

end module fynth__notes


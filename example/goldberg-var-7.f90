
program example

	use fynth
	use fynth__notes

	implicit none

	character(len = :), allocatable :: filename

	!********

	double precision :: bpm, cutoff, t
	double precision :: n2d, n4, n4d, n8, n8d, n16, n16d, n32, n32d, n64

	integer :: irep

	type(audio_t) :: audio
	type(env_t) :: env, fenv
	type(synth_t) :: synth

	!********

	! JS Bach, BWV 988
	print *, "Running example goldberg-var-7"
	filename = "build/goldberg-var-7.wav"

	! Beats per minute
	bpm = 55.d0

	! Durations in seconds
	n4d = 60 / bpm  ! dotted quarter note
	
	n8 = n4d / 3  ! eight
	n4 = n8 * 2   ! quarter note
	n2d = 3 * n4
	n16 = n8 / 2
	n8d = n16 * 3
	n32 = n16 / 2
	n16d = n32 * 3
	n64 = n32 / 2
	n32d = 3 * n64

	audio = new_audio(num_chans = 1, sample_rate = 44100)

	cutoff = 300.d0
	cutoff = 1200.d0

	!env  = env_t(a = 1.2, d = 2.4, s = 0.8, r = 0.7)
	env  = env_t(a = 0.0, d = 0.4, s = 0.2, r = 0.3)
	fenv = env_t(a = 0.0, d = 0.3, s = 0, r = 1.0)

	!!synth = synth_t(cutoff, env, fenv, square_wave)
	!!synth = synth_t(cutoff, env, fenv, sawtooth_wave)
	!synth = synth_t(cutoff, env, fenv, triangle_wave)
	synth%cutoff = cutoff
	synth%env = env
	synth%fenv = fenv
	synth%wave => triangle_wave

	!print "(f10.4)", [D1, A1, E1, FS1, CS3, D3, E3, FS3, GS3, A3, B3, CS4, D4]

	t = 0

	do irep = 1, 2  ! first half repeat
	!do irep = 1, -1 ! for developing second half
	!********
	! Bar 1

	! LH (left hand)
	!call play_note(audio, synth, G3 , t+0  , n4 )
	call play_note(audio, synth, G3 , t+0  , n32)
	call play_note(audio, synth, FS3, t+n32, n32)
	call play_note(audio, synth, G3 , t+n16, n8d)
	call play_note(audio, synth, G2 , t+n4 , n8 )
	!call play_note(audio, synth, G3 , t+n4d, n2d)
	call play_note(audio, synth, G3 , t+n4d+0  , n32)
	call play_note(audio, synth, FS3, t+n4d+n32, n32)
	call play_note(audio, synth, G3 , t+n4d+n16, n16+n4+n4d)

	! RH (right hand)
	call play_note(audio, synth, B4 , t+0  , n8d )
	call play_note(audio, synth, A4 , t+n8d, n16 )
	call play_note(audio, synth, B4 , t+n4 , n8  )

	call play_note(audio, synth, D4 , t+n4d+0  , n8d )
	call play_note(audio, synth, G4 , t+n4d+n8d, n16 )
	call play_note(audio, synth, B4 , t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 2
	
	! LH
	! (after tie from last bar)
	call play_note(audio, synth, G3 , t+n4d+0   , n64 )
	call play_note(audio, synth, FS3, t+n4d+n64 , n64 )
	call play_note(audio, synth, G3 , t+n4d+n32 , n64 )
	call play_note(audio, synth, FS3, t+n4d+n32d, n64+n8)
	call play_note(audio, synth, E3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, D3 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, B4 , t+0   , n64 )
	call play_note(audio, synth, A4 , t+n64 , n64 )
	call play_note(audio, synth, B4 , t+n32 , n64 )
	call play_note(audio, synth, A4 , t+n32d, n64+n8)
	call play_note(audio, synth, G4 , t+n8d, n16 )
	call play_note(audio, synth, A4 , t+n4 , n8  )

	call play_note(audio, synth, D5 , t+n4d+0  , n4d )

	t = t + n2d
	!********
	! Bar 3

	! LH
	call play_note(audio, synth, E3 , t+0 , n4)
	call play_note(audio, synth, D4 , t+n4, n8)
	call play_note(audio, synth, CS4, t+n4d+0 , n4)
	call play_note(audio, synth, A3 , t+n4d+n4, n8)

	! RH
	call play_note(audio, synth, G5 , t+0  , n32 )
	call play_note(audio, synth, FS5, t+n32, n32 )
	call play_note(audio, synth, G5 , t+n16, n8  )
	call play_note(audio, synth, FS5, t+n8d, n16 )
	call play_note(audio, synth, G5 , t+n4 , n8  )

	call play_note(audio, synth, A4 , t+n4d+0  , n8d )
	call play_note(audio, synth, E5 , t+n4d+n8d, n16 )
	call play_note(audio, synth, G5 , t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 4

	! LH
	call play_note(audio, synth, D4 , t+0 , n4d+n8d)

	call play_note(audio, synth, E4 , t+n4d+n8d, n16 )
	call play_note(audio, synth, C4 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, G5 , t+0   , n64 )
	call play_note(audio, synth, FS5, t+n64 , n64 )
	call play_note(audio, synth, G5 , t+n32 , n64 )
	call play_note(audio, synth, FS5, t+n32d, n64+n8)
	call play_note(audio, synth, E5 , t+n8d, n16 )
	call play_note(audio, synth, FS5, t+n4 , n8  )

	call play_note(audio, synth, D5 , t+n4d+0  , n4d+n4)

	t = t + n2d
	!********
	! Bar 5

	! LH
	call play_note(audio, synth, B3 , t+0  , n8d )
	call play_note(audio, synth, C4 , t+n8d, n16 )
	call play_note(audio, synth, A3 , t+n4 , n8  )

	call play_note(audio, synth, G3 , t+n4d+0  , n8d )
	call play_note(audio, synth, A3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, B3 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, E5 , t+n4+0   , n32 )
	call play_note(audio, synth, FS5, t+n4+n32 , n32 )
	call play_note(audio, synth, G5 , t+n4+n16 , n32 )
	call play_note(audio, synth, A5 , t+n4+n16d, n32 )

	call play_note(audio, synth, B5 , t+n4d+0  , n8d )
	call play_note(audio, synth, G5 , t+n4d+n8d, n16 )
	call play_note(audio, synth, D5 , t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 6

	! LH
	call play_note(audio, synth, C4 , t+0  , n8d )
	call play_note(audio, synth, D4 , t+n8d, n16 )
	call play_note(audio, synth, B3 , t+n4 , n8  )

	call play_note(audio, synth, A3 , t+n4d+0  , n8d )
	call play_note(audio, synth, B3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, C4 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, E5 , t+0      , n4  )
	call play_note(audio, synth, FS5, t+n4+0   , n32 )
	call play_note(audio, synth, G5 , t+n4+n32 , n32 )
	call play_note(audio, synth, A5 , t+n4+n16 , n32 )
	call play_note(audio, synth, B5 , t+n4+n16d, n32 )

	call play_note(audio, synth, C6 , t+n4d+0  , n8d )
	call play_note(audio, synth, A5 , t+n4d+n8d, n16 )
	call play_note(audio, synth, E5 , t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 7

	! LH
	call play_note(audio, synth, D4 , t+0  , n8d )
	call play_note(audio, synth, C4 , t+n8d, n16 )
	call play_note(audio, synth, B3 , t+n4 , n8  )

	call play_note(audio, synth, C4 , t+n4d+0  , n4  )
	call play_note(audio, synth, D4 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, FS5, t+0  , n8d )
	call play_note(audio, synth, D5 , t+n8d, n16 )
	call play_note(audio, synth, G5 , t+n4 , n8  )

	call play_note(audio, synth, A4 , t+n4d+0  , n8d )
	call play_note(audio, synth, B4 , t+n4d+n8d, n16 )
	call play_note(audio, synth, C5 , t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 8

	! LH
	call play_note(audio, synth, G3 , t+0  , n8d )
	call play_note(audio, synth, FS3, t+n8d, n16 )
	call play_note(audio, synth, G3 , t+n4 , n8  )

	call play_note(audio, synth, D3 , t+n4d+0  , n8d )
	call play_note(audio, synth, A3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, C4 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, C5 , t+0   , n4    )
	call play_note(audio, synth, B4 , t+n4  , n8+n8d)

	call play_note(audio, synth, A4 , t+n4d+n8d, n16)
	call play_note(audio, synth, G4 , t+n4d+n8d+n16, n16)
	call play_note(audio, synth, FS4, t+n4d+n8d+n8 , n16)

	t = t + n2d
	!********
	! Bar 9

	! LH
	call play_note(audio, synth, C4 , t+0   , n64 )
	call play_note(audio, synth, B3 , t+n64 , n64 )
	call play_note(audio, synth, C4 , t+n32 , n64 )
	call play_note(audio, synth, B3 , t+n32d, n64+n8)
	call play_note(audio, synth, A3 , t+n8d, n16 )
	call play_note(audio, synth, B3 , t+n4 , n8  )

	call play_note(audio, synth, E3 , t+n4d+0  , n8d )
	call play_note(audio, synth, B3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, D4 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, G4 , t+0  , n4 )
	call play_note(audio, synth, D5 , t+n4 , n8 )
	call play_note(audio, synth, G5 , t+n4d+0  , n32)
	call play_note(audio, synth, FS5, t+n4d+n32, n32)
	call play_note(audio, synth, G5 , t+n4d+n16, n16+n4+n8)

	t = t + n2d
	!********
	! Bar 10

	! LH
	call play_note(audio, synth, D4 , t+0   , n64 )
	call play_note(audio, synth, CS4, t+n64 , n64 )
	call play_note(audio, synth, D4 , t+n32 , n64 )
	call play_note(audio, synth, CS4, t+n32d, n64+n8)
	call play_note(audio, synth, B3 , t+n8d, n16 )
	call play_note(audio, synth, A3 , t+n4 , n8  )

	call play_note(audio, synth, D4 , t+n4d+0  , n8d )
	call play_note(audio, synth, CS4, t+n4d+n8d, n16 )
	call play_note(audio, synth, D4 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, A5 , t+n8    , n16 )
	call play_note(audio, synth, G5 , t+n8d   , n16 )
	call play_note(audio, synth, FS5, t+n4    , n16 )
	call play_note(audio, synth, E5 , t+n4+n16, n16 )

	call play_note(audio, synth, FS5, t+n4d+0  , n32)
	call play_note(audio, synth, E5 , t+n4d+n32, n32)
	call play_note(audio, synth, FS5, t+n4d+n16, n16+n4+n8d)

	t = t + n2d
	!********
	! Bar 11

	! LH
	call play_note(audio, synth, G3 , t+0  , n8d )
	call play_note(audio, synth, B3 , t+n8d, n16 )
	call play_note(audio, synth, D4 , t+n4 , n8  )

	call play_note(audio, synth, G4 , t+n4d+0  , n4d+n8)

	! RH
	call play_note(audio, synth, G5 , t+n8d, n16 )
	call play_note(audio, synth, FS5, t+n4 , n8  )

	call play_note(audio, synth, E5 , t+n4d+0  , n8d )
	call play_note(audio, synth, CS5, t+n4d+n8d, n16 )
	call play_note(audio, synth, D5 , t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 12

	! LH
	call play_note(audio, synth, A4 , t+n8    , n16 )
	call play_note(audio, synth, G4 , t+n8d   , n16 )
	call play_note(audio, synth, FS4, t+n4    , n16 )
	call play_note(audio, synth, E4 , t+n4+n16, n16 )

	call play_note(audio, synth, FS4, t+n4d+0  , n8d )
	call play_note(audio, synth, G4 , t+n4d+n8d, n16 )
	call play_note(audio, synth, A4 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, D5 , t+0   , n64 )
	call play_note(audio, synth, CS5, t+n64 , n64 )
	call play_note(audio, synth, D5 , t+n32 , n64 )
	call play_note(audio, synth, CS5, t+n32d, n64+n8)
	call play_note(audio, synth, D5 , t+n8d, n16 )
	call play_note(audio, synth, E5 , t+n4 , n8  )

	call play_note(audio, synth, A4 , t+n4d+0  , n4d+n4 )

	t = t + n2d
	!********
	! Bar 13

	! LH
	call play_note(audio, synth, D4 , t+0  , n8d )
	call play_note(audio, synth, A3 , t+n8d, n16 )
	call play_note(audio, synth, FS3, t+n4 , n8  )

	call play_note(audio, synth, D3 , t+n4d+0  , n8d )
	call play_note(audio, synth, E3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, FS3, t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, B4 , t+n4+0   , n32 )
	call play_note(audio, synth, CS5, t+n4+n32 , n32 )
	call play_note(audio, synth, D5 , t+n4+n16 , n32 )
	call play_note(audio, synth, E5 , t+n4+n16d, n32 )

	call play_note(audio, synth, FS5, t+n4d+0  , n8d )
	call play_note(audio, synth, D5 , t+n4d+n8d, n16 )
	call play_note(audio, synth, A4 , t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 14

	! LH
	call play_note(audio, synth, G3 , t+0  , n8d )
	call play_note(audio, synth, A3 , t+n8d, n16 )
	call play_note(audio, synth, FS3, t+n4 , n8  )

	call play_note(audio, synth, E3 , t+n4d+0  , n8d )
	call play_note(audio, synth, FS3, t+n4d+n8d, n16 )
	call play_note(audio, synth, G3 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, B4 , t+0      , n4  )
	call play_note(audio, synth, CS5, t+n4+0   , n32 )
	call play_note(audio, synth, D5 , t+n4+n32 , n32 )
	call play_note(audio, synth, E5 , t+n4+n16 , n32 )
	call play_note(audio, synth, FS5, t+n4+n16d, n32 )

	call play_note(audio, synth, G5 , t+n4d+0  , n8d )
	call play_note(audio, synth, E5 , t+n4d+n8d, n16 )
	call play_note(audio, synth, B4 , t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 15

	! LH
	call play_note(audio, synth, A3 , t+0  , n8d )
	call play_note(audio, synth, G3 , t+n8d, n16 )
	call play_note(audio, synth, FS3, t+n4 , n8  )

	call play_note(audio, synth, B3 , t+n4d+0  , n8d )
	call play_note(audio, synth, G3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, A3 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, B4 , t+0    , n32   )
	call play_note(audio, synth, CS5, t+n32  , n32+n8)
	call play_note(audio, synth, A4 , t+n8d  , n16   )
	call play_note(audio, synth, A5 , t+n4   , n8    )

	call play_note(audio, synth, D5 , t+n4d+0  , n8d )
	call play_note(audio, synth, E5 , t+n4d+n8d, n16 )
	call play_note(audio, synth, CS5, t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 16

	! LH
	call play_note(audio, synth, D3 , t+0  , n8d )
	call play_note(audio, synth, FS3, t+n8d, n16 )
	call play_note(audio, synth, A3 , t+n4 , n8  )

	call play_note(audio, synth, D4 , t+n4d+0  , n4d )

	! RH
	call play_note(audio, synth, CS5, t+0    , n4d)
	call play_note(audio, synth, D5 , t+n4d  , n4d)

	t = t + n2d
	!********
	end do  ! end first half repeat

	!--------------------------------

	do irep = 1, 2  ! second half repeat
	!********
	! Bar 17

	! LH
	call play_note(audio, synth, D4 , t+0 , n4)
	call play_note(audio, synth, D3 , t+n4, n8)
	call play_note(audio, synth, D4 , t+n4d+0 , n4)
	call play_note(audio, synth, C4 , t+n4d+n4, n8)

	! RH
	call play_note(audio, synth, FS5, t+0  , n8d )
	call play_note(audio, synth, E5 , t+n8d, n16 )
	call play_note(audio, synth, FS5, t+n4 , n8  )

	call play_note(audio, synth, A4 , t+n4d+0  , n8d )
	call play_note(audio, synth, D5 , t+n4d+n8d, n16 )
	call play_note(audio, synth, FS5, t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 18

	! LH
	call play_note(audio, synth, C4 , t+0   , n64 )
	call play_note(audio, synth, B3 , t+n64 , n64 )
	call play_note(audio, synth, C4 , t+n32 , n64 )
	call play_note(audio, synth, B3 , t+n32d, n64+n8)
	call play_note(audio, synth, A3 , t+n8d, n16 )
	call play_note(audio, synth, B3 , t+n4 , n8  )

	call play_note(audio, synth, G3 , t+n4d+0  , n8d )
	call play_note(audio, synth, A3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, B3 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, G5 , t+0  , n8d )
	call play_note(audio, synth, FS5, t+n8d, n16 )
	call play_note(audio, synth, G5 , t+n4 , n8  )

	call play_note(audio, synth, B5 , t+n4d+0  , n4d )

	t = t + n2d
	!********
	! Bar 19

	! LH
	call play_note(audio, synth, C4 , t+0  , n8d )
	call play_note(audio, synth, D4 , t+n8d, n16 )
	call play_note(audio, synth, B3 , t+n4 , n8  )

	call play_note(audio, synth, A3 , t+n4d+0  , n8d )
	call play_note(audio, synth, B3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, C4 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, E5 , t+0      , n4  )
	call play_note(audio, synth, FS5, t+n4+0   , n32 )
	call play_note(audio, synth, G5 , t+n4+n32 , n32 )
	call play_note(audio, synth, A5 , t+n4+n16 , n32 )
	call play_note(audio, synth, B5 , t+n4+n16d, n32 )

	call play_note(audio, synth, C6 , t+n4d+0     , n16 )
	call play_note(audio, synth, B5 , t+n4d+n16   , n16 )
	call play_note(audio, synth, A5 , t+n4d+n8    , n16 )
	call play_note(audio, synth, G5 , t+n4d+n8d   , n16 )
	call play_note(audio, synth, FS5, t+n4d+n4    , n16 )
	call play_note(audio, synth, E5 , t+n4d+n4+n16, n16 )

	t = t + n2d
	!********
	! Bar 20

	! LH
	call play_note(audio, synth, B3 , t+0  , n4)
	call play_note(audio, synth, B2 , t+n4 , n8)

	call play_note(audio, synth, B3 , t+n4d+0  , n8d )
	call play_note(audio, synth, A3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, B3 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, E5 , t+0   , n64   )
	call play_note(audio, synth, DS5, t+n64 , n64   )
	call play_note(audio, synth, E5 , t+n32 , n64   )
	call play_note(audio, synth, DS5, t+n32d, n64+n8)
	call play_note(audio, synth, CS5, t+n8d , n16   )
	call play_note(audio, synth, DS5, t+n4  , n8    )

	call play_note(audio, synth, B4 , t+n4d+0  , n4d+n8d)

	t = t + n2d
	!********
	! Bar 21

	! LH
	call play_note(audio, synth, G3 , t+0      , n4  )
	call play_note(audio, synth, A3 , t+n4+0   , n32 )
	call play_note(audio, synth, B3 , t+n4+n32 , n32 )
	call play_note(audio, synth, CS4, t+n4+n16 , n32 )
	call play_note(audio, synth, DS4, t+n4+n16d, n32 )

	call play_note(audio, synth, E4 , t+n4d+0  , n8d )
	call play_note(audio, synth, B3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, G3 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, B5 , t+n8d, n16 )
	call play_note(audio, synth, FS5, t+n4 , n8  )

	call play_note(audio, synth, G5 , t+n4d+0  , n8d )
	call play_note(audio, synth, DS5, t+n4d+n8d, n16 )
	call play_note(audio, synth, E5 , t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 22

	! LH
	call play_note(audio, synth, A3 , t+0      , n4  )
	call play_note(audio, synth, B3 , t+n4+0   , n32 )
	call play_note(audio, synth, C4 , t+n4+n32 , n32 )
	call play_note(audio, synth, D4 , t+n4+n16 , n32 )
	call play_note(audio, synth, E4 , t+n4+n16d, n32 )

	call play_note(audio, synth, F4 , t+n4d+0  , n8d )
	call play_note(audio, synth, E4 , t+n4d+n8d, n16 )
	call play_note(audio, synth, DS4, t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, C5 , t+0  , n8d )
	call play_note(audio, synth, E5 , t+n8d, n16 )
	call play_note(audio, synth, GS5, t+n4 , n8  )

	call play_note(audio, synth, A5 , t+n4d, n4  )
	call play_note(audio, synth, B5 , t+n4d+n4     , n32)
	call play_note(audio, synth, A5 , t+n4d+n4+n32 , n32)
	call play_note(audio, synth, G5 , t+n4d+n4+n16 , n32)
	call play_note(audio, synth, FS5, t+n4d+n4+n16d, n32)

	t = t + n2d
	!********
	! Bar 23

	! LH
	call play_note(audio, synth, E4 , t+0  , n4)
	call play_note(audio, synth, G3 , t+n4 , n8)

	call play_note(audio, synth, C4 , t+n4d+0  , n8d )
	call play_note(audio, synth, A3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, B3 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, G5 , t+0  , n8d )
	call play_note(audio, synth, A5 , t+n8d, n16 )
	call play_note(audio, synth, B5 , t+n4 , n8  )

	call play_note(audio, synth, E5 , t+n4d+0  , n8d )
	call play_note(audio, synth, FS5, t+n4d+n8d, n16 )
	call play_note(audio, synth, DS5, t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 24

	! LH
	call play_note(audio, synth, E3 , t+0  , n4d+n8d)

	call play_note(audio, synth, E4 , t+n4d+n8d, n16 )
	call play_note(audio, synth, D4 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, E5 , t+0  , n8d )
	call play_note(audio, synth, B4 , t+n8d, n16 )
	call play_note(audio, synth, G4 , t+n4 , n8  )

	call play_note(audio, synth, E4 , t+n4d+0  , n4 )
	call play_note(audio, synth, B4 , t+n4d+n4 , n8 )

	t = t + n2d
	!********
	! Bar 25

	! LH
	call play_note(audio, synth, D4 , t+0   , n64 )
	call play_note(audio, synth, C4 , t+n64 , n64 )
	call play_note(audio, synth, D4 , t+n32 , n64 )
	call play_note(audio, synth, C4 , t+n32d, n64+n8)
	call play_note(audio, synth, B3 , t+n8d, n16 )
	call play_note(audio, synth, C4 , t+n4 , n8  )

	call play_note(audio, synth, GS3, t+n4d+0  , n8d )
	call play_note(audio, synth, B3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, E3 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, B4 , t+0  , n8d )
	call play_note(audio, synth, GS4, t+n8d, n16 )
	call play_note(audio, synth, A4 , t+n4 , n8  )

	call play_note(audio, synth, D5 , t+n4d+0     , n8 )
	call play_note(audio, synth, E5 , t+n4d+n8    , n16)
	call play_note(audio, synth, D5 , t+n4d+n8+n16, n16)
	call play_note(audio, synth, C5 , t+n4d+n4    , n16)
	call play_note(audio, synth, B4 , t+n4d+n4+n16, n16)

	t = t + n2d
	!********
	! Bar 26

	! LH
	call play_note(audio, synth, A3 , t+0  , n8d )
	call play_note(audio, synth, C4 , t+n8d, n16 )
	call play_note(audio, synth, B3 , t+n4 , n8  )

	call play_note(audio, synth, C4 , t+n4d+0  , n8d )
	call play_note(audio, synth, B3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, A3 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, C5 , t+0  , n8d )
	call play_note(audio, synth, E5 , t+n8d, n16 )
	call play_note(audio, synth, D5 , t+n4 , n8  )

	call play_note(audio, synth, E5 , t+n4d+0  , n8d )
	call play_note(audio, synth, G5 , t+n4d+n8d, n16 )
	call play_note(audio, synth, FS5, t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 27

	! LH
	call play_note(audio, synth, B3 , t+0  , n8d )
	call play_note(audio, synth, E4 , t+n8d, n16 )
	call play_note(audio, synth, D4 , t+n4 , n8  )

	call play_note(audio, synth, D4 , t+n4d+0   , n64 )
	call play_note(audio, synth, CS4, t+n4d+n64 , n64 )
	call play_note(audio, synth, D4 , t+n4d+n32 , n64 )
	call play_note(audio, synth, CS4, t+n4d+n32d, n64+n8)
	call play_note(audio, synth, B3 , t+n4d+n8d , n16 )
	call play_note(audio, synth, CS4, t+n4d+n4  , n8  )

	! RH
	call play_note(audio, synth, G5 , t+0     , n8 )
	call play_note(audio, synth, A5 , t+n8    , n16)
	call play_note(audio, synth, G5 , t+n8+n16, n16)
	call play_note(audio, synth, FS5, t+n4    , n16)
	call play_note(audio, synth, E5 , t+n4+n16, n16)

	call play_note(audio, synth, A5 , t+n4d+0  , n8d )
	call play_note(audio, synth, B5 , t+n4d+n8d, n16 )
	call play_note(audio, synth, G5 , t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 28

	! LH
	call play_note(audio, synth, D4 , t+0      , n4d+n8d)

	call play_note(audio, synth, E4 , t+n4d+n8d, n16 )
	call play_note(audio, synth, C4 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, G5 , t+0   , n64 )
	call play_note(audio, synth, FS5, t+n64 , n64 )
	call play_note(audio, synth, G5 , t+n32 , n64 )
	call play_note(audio, synth, FS5, t+n32d, n64+n8)
	call play_note(audio, synth, E5 , t+n8d, n16 )
	call play_note(audio, synth, FS5, t+n4 , n8  )

	call play_note(audio, synth, D5 , t+n4d+0, n4d+n4)

	t = t + n2d
	!********
	! Bar 29

	! LH
	call play_note(audio, synth, B3 , t+0  , n8d )
	call play_note(audio, synth, C4 , t+n8d, n16 )
	call play_note(audio, synth, A3 , t+n4 , n8  )

	call play_note(audio, synth, G3 , t+n4d+0  , n8d )
	call play_note(audio, synth, A3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, B3 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, E5 , t+n4+0   , n32 )
	call play_note(audio, synth, FS5, t+n4+n32 , n32 )
	call play_note(audio, synth, G5 , t+n4+n16 , n32 )
	call play_note(audio, synth, A5 , t+n4+n16d, n32 )

	call play_note(audio, synth, B5 , t+n4d+0  , n8d )
	call play_note(audio, synth, G5 , t+n4d+n8d, n16 )
	call play_note(audio, synth, D5 , t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 30

	! LH
	call play_note(audio, synth, C4 , t+0  , n8d )
	call play_note(audio, synth, D4 , t+n8d, n16 )
	call play_note(audio, synth, B3 , t+n4 , n8  )

	call play_note(audio, synth, A3 , t+n4d+0  , n8d )
	call play_note(audio, synth, B3 , t+n4d+n8d, n16 )
	call play_note(audio, synth, C4 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, E5 , t+0      , n4  )
	call play_note(audio, synth, FS5, t+n4+0   , n32 )
	call play_note(audio, synth, G5 , t+n4+n32 , n32 )
	call play_note(audio, synth, A5 , t+n4+n16 , n32 )
	call play_note(audio, synth, B5 , t+n4+n16d, n32 )

	call play_note(audio, synth, C6 , t+n4d+0  , n8d )
	call play_note(audio, synth, A5 , t+n4d+n8d, n16 )
	call play_note(audio, synth, E5 , t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 31

	! LH
	call play_note(audio, synth, D4 , t+0  , n8d )
	call play_note(audio, synth, C4 , t+n8d, n16 )
	call play_note(audio, synth, B3 , t+n4 , n8  )

	call play_note(audio, synth, E4 , t+n4d+0  , n8d )
	call play_note(audio, synth, C4 , t+n4d+n8d, n16 )
	call play_note(audio, synth, D4 , t+n4d+n4 , n8  )

	! RH
	call play_note(audio, synth, FS5, t+0  , n8d )
	call play_note(audio, synth, D5 , t+n8d, n16 )
	call play_note(audio, synth, D6 , t+n4 , n8  )

	call play_note(audio, synth, G5 , t+n4d+0  , n8d )
	call play_note(audio, synth, A5 , t+n4d+n8d, n16 )
	call play_note(audio, synth, FS5, t+n4d+n4 , n8  )

	t = t + n2d
	!********
	! Bar 32

	! LH
	call play_note(audio, synth, G3 , t+0  , n8d )
	call play_note(audio, synth, D3 , t+n8d, n16 )
	call play_note(audio, synth, B2 , t+n4 , n8  )

	call play_note(audio, synth, G2 , t+n4d+0  , n4d)

	! RH
	call play_note(audio, synth, G5 , t+0  , n4d+n4d)

	t = t + n2d
	!********
	end do  ! end second half repeat

	!--------------------------------

	print *, "writing wave file ..."
	call write_wav(filename, audio)

end program example


program example

	use fynth
	use fynth__notes

	implicit none

	character(len = :), allocatable :: filename

	!********

	double precision :: bpm, cutoff, t, tr
	double precision :: n2d, n4, n4d, n8, n8d, n16, n16d, n32, n32d, n64

	type(audio_t) :: audio
	type(env_t) :: env, fenv
	type(synth_t) :: synth

	!********

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
	synth = synth_t(cutoff, env, fenv, triangle_wave)
	!synth = synth_t(cutoff, env, fenv, square_wave)
	!synth = synth_t(cutoff, env, fenv, sawtooth_wave)

	!print "(f10.4)", [D1, A1, E1, FS1, CS3, D3, E3, FS3, GS3, A3, B3, CS4, D4]

	!********
	t = 0 * n2d

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

	!********
	t = 1 * n2d
	! TODO: increment time instead of hard-coding each measure start time
	
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

	!********
	t = 2 * n2d

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

	!********
	t = 3 * n2d

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

	!********
	t = 4 * n2d

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

	!********
	t = 5 * n2d

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

	!********
	t = 6 * n2d

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

	!********
	t = 7 * n2d

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

	!********
	t = 8 * n2d

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
	call play_note(audio, synth, G4 , t+0   , n4    )

	!********
	call write_wav(filename, audio)

end program example

program example

	use fynth
	use fynth__notes

	implicit none

	character(len = :), allocatable :: filename

	!********

	double precision :: bpm, cutoff
	double precision :: n2d, n4, n4d, n8, n8d, n16, n16d, n32, n32d, n64

	type(audio_t), target :: audio
	type(env_t) :: env, fenv
	type(synth_t) :: synth
	type(voice_t) :: lh, rh

	!********

	! JS Bach, BWV 988
	print *, "Running example goldberg-var-1"
	filename = "build/goldberg-var-1.wav"

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

	!********

	audio = new_audio(num_chans = 1, sample_rate = 44100)

	cutoff = 300.d0
	cutoff = 1200.d0

	!env  = env_t(a = 1.2, d = 2.4, s = 0.8, r = 0.7)
	env  = env_t(a = 0.0, d = 0.4, s = 0.2, r = 0.3)
	fenv = env_t(a = 0.0, d = 0.3, s = 0, r = 1.0)

	!synth = synth_t(cutoff, env, fenv, triangle_wave)
	synth%cutoff = cutoff
	synth%cutoff_max = 6000.d0
	synth%env = env
	synth%fenv = fenv
	synth%wave => triangle_wave

	lh = new_voice(audio, synth)  ! left hand voice
	rh = new_voice(audio, synth)  ! right hand voice

	!********
	! Bar 1

	lh%legato = 0.25d0
	!rh%legato = 0.25d0

	call lh%play(G2 , n8  )
	call lh%play(B3 , n16 )
	call lh%play(A3 , n16 )
	call lh%play(B3 , n8  )

	call lh%play(G3 , n8  )
	call lh%play(G2 , n8  )
	call lh%play(G3 , n8  )

	call rh%play(G4 , n16 )
	call rh%play(FS4, n16 )
	call rh%play(G4 , n8d )

	call rh%play(D4 , n16 )
	call rh%play(E4 , n16 )
	call rh%play(FS4, n16 )

	call rh%play(G4 , n16 )
	call rh%play(A4 , n16 )
	call rh%play(B4 , n16 )
	call rh%play(CS5, n16 )

	!********
	! Bar 2

	call lh%play(FS2, n8  )
	call lh%play(FS3, n16 )
	call lh%play(E3 , n16 )
	call lh%play(FS3, n8  )
	call lh%play(D3 , n8  )
	call lh%play(FS2, n8  )
	call lh%play(D3 , n8  )

	call rh%play(D5 , n16 )
	call rh%play(CS5, n16 )
	call rh%play(D5 , n8d )

	call rh%play(A4 , n16 )
	call rh%play(B4 , n16 )
	call rh%play(CS5, n16 )

	call rh%play(D5 , n16 )
	call rh%play(E5 , n16 )
	call rh%play(FS5, n16 )
	call rh%play(D5 , n16 )

	!********
	! Bar 3

	call lh%play(E2 , n8  )
	call lh%play(E3 , n16 )
	call lh%play(D3 , n16 )
	call lh%play(E3 , n8  )
	call lh%play(G3 , n8  )
	call lh%play(A2 , n8  )
	call lh%play(CS4, n8  )

	call rh%play(G5 , n16 )
	call rh%play(FS5, n16 )
	call rh%play(G5 , n8d )

	call rh%play(FS5, n16 )
	call rh%play(E5 , n16 )
	call rh%play(D5 , n16 )

	call rh%play(CS5, n16 )
	call rh%play(E5 , n16 )
	call rh%play(A4 , n16 )
	call rh%play(G4 , n16 )

	!********
	! Bar 4

	call lh%play(D3 , n8  )
	call lh%play(FS3, n16 )
	call lh%play(E3 , n16 )
	call lh%play(FS3, n8  )
	call lh%play(D3 , n8  )
	call lh%play(D2 , n8  )
	call lh%rest(n16)

	call rh%play(FS4, n16 )
	call rh%play(E4 , n16 )
	call rh%play(D4 , n16 )
	call rh%play(CS4, n16 )

	call rh%play(D4 , n16 )
	call rh%play(FS4, n16 )
	call rh%play(A3 , n16 )
	call rh%play(G3 , n16 )

	call rh%play(FS3, n16 )
	call rh%play(A3 , n16 )
	call rh%play(D3 , n8  )

	!********
	! Bar 5

	call lh%play(C3 , n16 )  ! pickup
	call lh%play(B2 , n16 )
	call lh%play(A2 , n16 )
	call lh%play(B2 , n8d )

	call lh%play(D3 , n16 )
	call lh%play(E3 , n16 )
	call lh%play(FS3, n16 )

	call lh%play(G3 , n16 )
	call lh%play(A3 , n16 )
	call lh%play(B3 , n16 )
	call lh%play(G3 , n16 )

	call rh%rest(n8)
	call rh%play(D5 , n16 )
	call rh%play(C5 , n16 )

	call rh%play(D5 , n8  )
	call rh%play(G4 , n8  )
	call rh%play(B3 , n8  )
	call rh%play(D5 , n8  )

	!********
	! Bar 6

	call lh%play(C3 , n16 )
	call lh%play(B2 , n16 )
	call lh%play(C3 , n8d )

	call lh%play(E3 , n16 )
	call lh%play(FS3, n16 )
	call lh%play(G3 , n16 )

	call lh%play(A3 , n16 )
	call lh%play(B3 , n16 )
	call lh%play(C4 , n16 )
	call lh%play(A3 , n16 )

	call rh%rest(n8)
	call rh%play(E5 , n16 )
	call rh%play(D5 , n16 )

	call rh%play(E5 , n8  )
	call rh%play(A4 , n8  )
	call rh%play(C4 , n8  )
	call rh%play(E5 , n8  )

	!********
	! Bar 7

	call lh%play(D3 , n16 )
	call lh%play(CS3, n16 )
	call lh%play(D3 , n8d )

	call lh%play(A3 , n16 )
	call lh%play(B3 , n16 )
	call lh%play(C4 , n16 )

	call lh%play(D4 , n16 )
	call lh%play(E4 , n16 )
	call lh%play(FS4, n16 )
	call lh%play(D4 , n16 )

	call rh%rest(n8)
	call rh%play(FS5, n16 )
	call rh%play(E5 , n16 )

	call rh%play(FS5, n8  )
	call rh%play(D5 , n8  )
	call rh%play(A5 , n8  )
	call rh%play(C5 , n4  )  ! (tie into next bar)

	!********
	! Bar 8

	call lh%play(G4 , n16 )
	call lh%play(FS4, n16 )
	call lh%play(G4 , n16 )
	call lh%play(D4 , n16 )

	call lh%play(B3 , n16 )
	call lh%play(D4 , n16 )
	call lh%play(G3 , n16 )
	call lh%play(B3 , n16 )

	call lh%play(D3 , n16 )
	call lh%play(G3 , n16 )
	call lh%play(B2 , n16 )
	call lh%play(D3 , n16 )

	call rh%play(B4 , n8  )
	call rh%rest(n16)

	call rh%play(G4 , n16 )
	call rh%play(B4 , n16 )
	call rh%play(D5 , n16 )

	call rh%play(G5 , n16 )
	call rh%play(D5 , n16 )
	call rh%play(G5 , n16 )
	call rh%play(A5 , n16 )

	!********
	! Bar 9

	call lh%play(G2 , n8  )
	call lh%play(G3 , n8  )
	call lh%play(B3 , n8  )
	call lh%play(G3 , n8  )
	call lh%play(G2 , n8  )
	call lh%play(G3 , n8  )

	call rh%play(B5 , n16 )
	call rh%play(G5 , n16 )
	call rh%play(D5 , n16 )
	call rh%play(B4 , n16 )

	call rh%play(G4 , n16 )
	call rh%play(B4 , n16 )
	call rh%play(D5 , n16 )
	call rh%play(G5 , n16 )

	call rh%play(B5 , n16 )
	call rh%play(G5 , n16 )
	call rh%play(FS5, n16 )
	call rh%play(E5 , n16 )

	!********
	! Bar 10

	call lh%play(FS2, n8  )
	call lh%play(FS3, n8  )
	call lh%play(A3 , n8  )
	call lh%play(FS3, n8  )
	call lh%play(FS2, n8  )
	call lh%play(FS3, n8  )

	call rh%play(A5 , n16 )
	call rh%play(E5 , n16 )
	call rh%play(CS5, n16 )
	call rh%play(A4 , n16 )

	call rh%play(FS4, n16 )
	call rh%play(A4 , n16 )
	call rh%play(CS5, n16 )
	call rh%play(E5 , n16 )

	call rh%play(A5 , n16 )
	call rh%play(FS5, n16 )
	call rh%play(E5 , n16 )
	call rh%play(D5 , n16 )

	!********
	! Bar 11

	call lh%play(E2 , n8  )
	call lh%play(E3 , n8  )
	call lh%play(G3 , n8  )
	call lh%play(E3 , n8  )
	call lh%play(E2 , n8  )
	call lh%play(G3 , n8  )

	call rh%play(G5 , n16 )
	call rh%play(D5 , n16 )
	call rh%play(B4 , n16 )
	call rh%play(G4 , n16 )

	call rh%play(E4 , n16 )
	call rh%play(G4 , n16 )
	call rh%play(B4 , n16 )
	call rh%play(D5 , n16 )

	call rh%play(G5 , n16 )
	call rh%play(FS5, n16 )
	call rh%play(E5 , n16 )
	call rh%play(D5 , n16 )

	!********
	! Bar 12

	call lh%play(A2 , n8  )
	call lh%play(E3 , n8  )
	call lh%play(G3 , n8  )
	call lh%play(E3 , n8  )
	call lh%play(A2 , n8  )
	call lh%play(G3 , n8  )

	call rh%play(CS5, n16 )
	call rh%play(G4 , n16 )
	call rh%play(E4 , n16 )
	call rh%play(CS4, n16 )

	call rh%play(A3 , n16 )
	call rh%play(CS4, n16 )
	call rh%play(E4 , n16 )
	call rh%play(G4 , n16 )

	call rh%play(CS5, n16 )
	call rh%play(E5 , n16 )
	call rh%play(D5 , n16 )
	call rh%play(CS5, n16 )

	!********
	! Bar 13

	call lh%play(FS3, n16 )
	call lh%play(A3 , n16 )
	call lh%play(D4 , n16 )
	call lh%play(FS4, n16 )

	call lh%play(A4 , n16 )
	call lh%play(FS4, n16 )
	call lh%play(D4 , n16 )
	call lh%play(A3 , n16 )

	call lh%play(FS3, n16 )
	call lh%play(A3 , n16 )
	call lh%play(D3 , n16 )
	call lh%play(FS3, n16 )

	call rh%play(D5 , n8  )

	call rh%play(FS3, n8  )
	call rh%play(FS3, n8  )

	call rh%play(A4 , n8  )
	call rh%play(D5 , n8  )
	call rh%play(FS5, n8  )

	!********
	! Bar 14

	call lh%play(G3 , n16 )
	call lh%play(B3 , n16 )
	call lh%play(D4 , n16 )
	call lh%play(G4 , n16 )

	call lh%play(B4 , n16 )
	call lh%play(G4 , n16 )
	call lh%play(D4 , n16 )
	call lh%play(B3 , n16 )

	call lh%play(G3 , n16 )
	call lh%play(B3 , n16 )
	call lh%play(E3 , n16 )
	call lh%play(G3 , n16 )

	call rh%play(B4 , n8  )

	call rh%play(G3 , n8  )
	call rh%play(G3 , n8  )

	call rh%play(B4 , n8  )
	call rh%play(E5 , n8  )
	call rh%play(G5 , n8  )

	!********
	! Bar 15

	call lh%play(A3 , n8  )
	call lh%play(CS4, n8  )

	call lh%play(D4 , n16 )
	call lh%play(A3 , n16 )
	call lh%play(FS3, n16 )
	call lh%play(D3 , n16 )

	call lh%play(A3 , n8  )
	call lh%play(A2 , n8  )

	call rh%play(CS5, n16 )
	call rh%play(E5 , n16 )
	call rh%play(A4 , n16 )
	call rh%play(G4 , n16 )

	call rh%play(FS4, n16 )
	call rh%play(A4 , n16 )
	call rh%play(D5 , n16 )
	call rh%play(FS5, n16 )

	call rh%play(G5 , n16 )
	call rh%play(E5 , n16 )
	call rh%play(D5 , n16 )
	call rh%play(CS5, n16 )

	!********
	! Bar 16

	call lh%play(D2 , n16 )
	call lh%play(D3 , n16 )
	call lh%play(E3 , n16 )
	call lh%play(FS3, n16 )

	call lh%play(G3 , n16 )
	call lh%play(A3 , n16 )
	call lh%play(B3 , n16 )
	call lh%play(CS4, n16 )

	call lh%play(D4 , n4  )

	call rh%play(FS5, n16 )
	call rh%play(D5 , n16 )
	call rh%play(CS5, n16 )
	call rh%play(B4 , n16 )

	call rh%play(A4 , n16 )
	call rh%play(G4 , n16 )
	call rh%play(FS4, n16 )
	call rh%play(E4 , n16 )

	call rh%play(D4 , n4  )

	!--------------------------------

	print *, "writing wave file ..."
	call write_wav(filename, audio)

end program example

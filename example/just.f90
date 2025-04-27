program example

	use fynth
	use fynth__notes

	implicit none

	character(len = :), allocatable :: filename

	!********

	double precision :: bpm, quarter_note, eigth_note, en, qn, wn, hn, dhn, &
		cutoff, t, tr
	double precision :: D1J, A1J, E1J, FS1J, B2J, CS3J, D3J, E3J, FS3J, GS3J, &
		A3J, B3J, CS4J, D4J

	type(audio_t) :: audio
	type(env_t) :: env, fenv
	type(synth_t) :: synth

	procedure(fn_f64_to_f64), pointer :: wave

	!********

	print *, "running example just"
	filename = "build/just.wav"

	! Beats per minute
	bpm = 70.d0

	! Durations in seconds
	quarter_note = 60.d0 / bpm
	eigth_note = quarter_note / 2.d0

	! Aliases
	qn = quarter_note
	en = eigth_note
	hn  = 2 * qn
	dhn = 3 * qn
	wn  = 4 * qn

	audio = new_audio(num_chans = 1, sample_rate = 44100)

	!wave => sawtooth_wave
	wave => square_wave

	cutoff = 300.d0

	!env  = env_t(a = 1.2, d = 2.4, s = 0.8, r = 0.7)
	env  = env_t(a = 0.3, d = 2.4, s = 0.8, r = 0.7)

	fenv = env_t(a = 2.3, d = 1.3, s = 0, r = 100)

	! Transpose up from A to D
	tr = D2 / A1

	A1J = A_440 / 8
	D1J = A1J * 2 / 3
	E1J = A1J * 3 / 4
	CS3J = A1J * 5 / 4 * 2

	!FS1J = CS3J * 2 / 3 / 2
	FS1J = D1J * 5 / 4  ! same thing

	B2J = E1J * 3 / 2 * 2
	D3J = D1J * 4
	E3J = E1J * 4
	FS3J = FS1J * 4
	GS3J = E1J * 5 / 4 * 4
	A3J = A1J * 4
	B3J = E1J * 3 / 2 * 4
	CS4J = CS3J * 2
	D4J = D3J * 2

	print "(*(f10.4))", [D1, E1, FS1, A1, CS3, D3, E3]
	print "(*(f10.4))", [D1J, E1J, FS1J, A1J, CS3J, D3J, E3J]
	print *, ""

	print "(*(f10.4))", [FS3, GS3, A3, B3, CS4, D4]
	print "(*(f10.4))", [FS3J, GS3J, A3J, B3J, CS4J, D4J]
	print "(*(f10.4))", [FS3J*6/5]
	print *, ""

	synth = synth_t(cutoff, env, fenv, square_wave)

	!********
	t = 0 * wn
	! A
	call play_note(audio, synth, tr*A1J , t, wn)
	call play_note(audio, synth, tr*E3J , t, wn)
	call play_note(audio, synth, tr*A3J , t, wn)

	call play_note(audio, synth, tr*B3J , t, qn )
	call play_note(audio, synth, tr*CS4J, t+qn, dhn)

	t = 1 * wn
	! D
	call play_note(audio, synth, tr*D1J , t, wn)
	call play_note(audio, synth, tr*FS3J, t, wn)
	call play_note(audio, synth, tr*A3J , t, wn)
	call play_note(audio, synth, tr*D4J , t, wn)

	t = 2 * wn
	! F#m
	call play_note(audio, synth, tr*FS1J, t, wn)
	call play_note(audio, synth, tr*FS3J, t, wn)

	call play_note(audio, synth, tr*A3J , t, wn)
	!call play_note(audio, synth, tr*FS3J*5/4, wn, t)   ! major, oops
	!call play_note(audio, synth, tr*FS3J*6/5, wn, t)   ! same thing
	!call play_note(audio, synth, tr*FS3J*19/16, wn, t) ! other just intonations
	!call play_note(audio, synth, tr*FS3J*32/27, wn, t)

	call play_note(audio, synth, tr*CS4J, t, wn)

	t = 3 * wn
	! D (different voicing now)
	call play_note(audio, synth, tr*D1J , t, wn)
	call play_note(audio, synth, tr*D3J , t, wn)
	call play_note(audio, synth, tr*FS3J, t, wn)
	call play_note(audio, synth, tr*A3J , t, wn)

	!********
	t = 4 * wn
	! A
	call play_note(audio, synth, tr*A1J , t, wn)
	call play_note(audio, synth, tr*E3J , t, wn)
	call play_note(audio, synth, tr*A3J , t, wn)

	call play_note(audio, synth, tr*B3J , t, qn )
	call play_note(audio, synth, tr*CS4J, t+qn, dhn)

	t = 5 * wn
	! E
	call play_note(audio, synth, tr*E1J , t, wn)
	call play_note(audio, synth, tr*B2J , t, wn)
	call play_note(audio, synth, tr*E3J , t, wn)
	call play_note(audio, synth, tr*GS3J, t, wn)

	t = 6 * wn
	! F#m
	call play_note(audio, synth, tr*FS1J, t, wn)
	call play_note(audio, synth, tr*FS3J, t, wn)
	call play_note(audio, synth, tr*A3J , t, wn)

	call play_note(audio, synth, tr*CS3J, t+hn, hn)

	t = 7 * wn
	! D
	call play_note(audio, synth, tr*D1J , t, wn)
	call play_note(audio, synth, tr*D3J , t, wn)
	call play_note(audio, synth, tr*FS3J, t, wn)
	call play_note(audio, synth, tr*A3J , t, wn)

	!********

	call write_wav(filename, audio)

end program example

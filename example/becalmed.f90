program example

	use fynth
	use fynth__notes

	implicit none

	character(len = :), allocatable :: filename

	!********

	double precision :: bpm, quarter_note, eigth_note, en, qn, wn, hn, dhn, &
		cutoff, t, tr

	type(audio_t) :: audio
	type(env_t) :: env, fenv
	type(synth_t) :: synth

	!********

	print *, "Running example becalmed"
	filename = "build/becalmed.wav"

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

	cutoff = 300.d0
	!env  = env_t(a = 1.2, d = 2.4, s = 0.8, r = 0.7)
	env  = env_t(a = 0.3, d = 2.4, s = 0.8, r = 0.7)
	fenv = env_t(a = 2.3, d = 1.3, s = 0, r = 100)
	synth = synth_t(cutoff, env, fenv, square_wave)

	! Transpose up from A to D
	tr = D2 / A1

	!print "(f10.4)", [D1, A1, E1, FS1, CS3, D3, E3, FS3, GS3, A3, B3, CS4, D4]

	!********
	t = 0 * wn
	! A
	call play_note(audio, synth, tr*A1 , t, wn)
	call play_note(audio, synth, tr*E3 , t, wn)
	call play_note(audio, synth, tr*A3 , t, wn)

	call play_note(audio, synth, tr*B3 , t, qn )
	call play_note(audio, synth, tr*CS4, t+qn, dhn)

	t = 1 * wn
	! D
	call play_note(audio, synth, tr*D1 , t, wn)
	call play_note(audio, synth, tr*FS3, t, wn)
	call play_note(audio, synth, tr*A3 , t, wn)
	call play_note(audio, synth, tr*D4 , t, wn)

	t = 2 * wn
	! F#m
	call play_note(audio, synth, tr*FS1, t, wn)
	call play_note(audio, synth, tr*FS3, t, wn)
	call play_note(audio, synth, tr*A3 , t, wn)
	call play_note(audio, synth, tr*CS4, t, wn)

	t = 3 * wn
	! D (different voicing now)
	call play_note(audio, synth, tr*D1 , t, wn)
	call play_note(audio, synth, tr*D3 , t, wn)
	call play_note(audio, synth, tr*FS3, t, wn)
	call play_note(audio, synth, tr*A3 , t, wn)

	!********
	t = 4 * wn
	! A
	call play_note(audio, synth, tr*A1 , t, wn)
	call play_note(audio, synth, tr*E3 , t, wn)
	call play_note(audio, synth, tr*A3 , t, wn)

	call play_note(audio, synth, tr*B3 , t, qn )
	call play_note(audio, synth, tr*CS4, t+qn, dhn)

	t = 5 * wn
	! E
	call play_note(audio, synth, tr*E1 , t, wn)
	call play_note(audio, synth, tr*B2 , t, wn)
	call play_note(audio, synth, tr*E3 , t, wn)
	call play_note(audio, synth, tr*GS3, t, wn)

	t = 6 * wn
	! F#m
	call play_note(audio, synth, tr*FS1, t, wn)
	call play_note(audio, synth, tr*FS3, t, wn)
	call play_note(audio, synth, tr*A3 , t, wn)

	call play_note(audio, synth, tr*CS3, t+hn, hn)

	t = 7 * wn
	! D
	call play_note(audio, synth, tr*D1 , t, wn)
	call play_note(audio, synth, tr*D3 , t, wn)
	call play_note(audio, synth, tr*FS3, t, wn)
	call play_note(audio, synth, tr*A3 , t, wn)

	!********

	call write_wav(filename, audio)

end program example

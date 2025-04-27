program example

	use fynth
	use fynth__notes

	implicit none

	character(len = :), allocatable :: filename

	!********

	double precision :: bpm, quarter_note, eigth_note, en, qn, wn, hn, dhn, &
		cutoff, t, tr

	!integer :: ii!, it

	type(audio_t) :: audio
	type(env_t) :: env, fenv

	!********

	print *, "running example becalmed"
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

	! Transpose up from A to D
	tr = D2 / A1

	print "(f10.4)", [D1, A1, E1, FS1, CS3, D3, E3, FS3, GS3, A3, B3, CS4, D4]

	!********
	t = 0 * wn
	! A
	call play_note(audio, square_wave, tr*A1 , wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*E3 , wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*A3 , wn, t, env, cutoff, fenv)

	call play_note(audio, square_wave, tr*B3 , qn , t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*CS4, dhn, t+qn, env, cutoff, fenv)

	t = 1 * wn
	! D
	call play_note(audio, square_wave, tr*D1 , wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*FS3, wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*A3 , wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*D4 , wn, t, env, cutoff, fenv)

	t = 2 * wn
	! F#m
	call play_note(audio, square_wave, tr*FS1, wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*FS3, wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*A3 , wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*CS4, wn, t, env, cutoff, fenv)

	t = 3 * wn
	! D (different voicing now)
	call play_note(audio, square_wave, tr*D1 , wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*D3 , wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*FS3, wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*A3 , wn, t, env, cutoff, fenv)

	!********
	t = 4 * wn
	! A
	call play_note(audio, square_wave, tr*A1 , wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*E3 , wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*A3 , wn, t, env, cutoff, fenv)

	call play_note(audio, square_wave, tr*B3 , qn , t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*CS4, dhn, t+qn, env, cutoff, fenv)

	t = 5 * wn
	! E
	call play_note(audio, square_wave, tr*E1 , wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*B2 , wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*E3 , wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*GS3, wn, t, env, cutoff, fenv)

	t = 6 * wn
	! F#m
	call play_note(audio, square_wave, tr*FS1, wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*FS3, wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*A3 , wn, t, env, cutoff, fenv)

	call play_note(audio, square_wave, tr*CS3, hn, t+hn, env, cutoff, fenv)

	t = 7 * wn
	! D
	call play_note(audio, square_wave, tr*D1 , wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*D3 , wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*FS3, wn, t, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*A3 , wn, t, env, cutoff, fenv)

	!********

	call write_wav(filename, audio)

end program example

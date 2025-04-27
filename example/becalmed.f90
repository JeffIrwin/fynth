program example
	use fynth
	implicit none

	character(len = :), allocatable :: filename

	!********

	double precision :: bpm, quarter_note, eigth_note, en, qn, &
		cutoff, tb1, tb2, tb3, tb4, wn, tb5, tb6, tb7, tb8, tr

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
	wn = 4 * qn

	! TODO: use time differently.  Set a baseline at the start of each bar, then
	! set note times as the bar start plus on offset.  This should be less error
	! prone and make it easier to cut/paste bars
	tb1 = 0.d0
	tb2 = 4 * quarter_note
	tb3 = 2 * tb2
	tb4 = 3 * tb2
	tb5 = 4 * tb2
	tb6 = 5 * tb2
	tb7 = 6 * tb2
	tb8 = 7 * tb2

	audio = new_audio(num_chans = 1, sample_rate = 44100)

	cutoff = 300.d0

	!env  = env_t(a = 0, d = 0, s = 1, r = 0)
	env  = env_t(a = 1.2, d = 2.4, s = 0.8, r = 0.7)
	!env  = env_t(a = 0, d = 0.1, s = 0.5, r = 0.5)

	!fenv = env_t(a = 0, d = 0, s = 0, r = 0)
	!fenv = env_t(a = 0, d = 0.2, s = 0, r = 0)
	fenv = env_t(a = 2.3, d = 1.3, s = 0, r = 100)

	! Transpose up from A to D
	tr = D2 / A1

	!********
	! A
	call play_note(audio, square_wave, tr*A1 , wn, tb1, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*E3 , wn, tb1, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*A3 , wn, tb1, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*CS4, wn, tb1, env, cutoff, fenv)

	! TODO: appogiaturas

	! D
	call play_note(audio, square_wave, tr*D1 , wn, tb2, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*FS3, wn, tb2, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*A3 , wn, tb2, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*D4 , wn, tb2, env, cutoff, fenv)

	! F#m
	call play_note(audio, square_wave, tr*FS1, wn, tb3, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*FS3, wn, tb3, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*A3 , wn, tb3, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*CS4, wn, tb3, env, cutoff, fenv)

	! D (different voicing now)
	call play_note(audio, square_wave, tr*D1 , wn, tb4, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*D3 , wn, tb4, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*FS3, wn, tb4, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*A3 , wn, tb4, env, cutoff, fenv)

	!********
	! A
	call play_note(audio, square_wave, tr*A1 , wn, tb5, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*E3 , wn, tb5, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*A3 , wn, tb5, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*CS4, wn, tb5, env, cutoff, fenv)

	! E
	call play_note(audio, square_wave, tr*E1 , wn, tb6, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*B2 , wn, tb6, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*E3 , wn, tb6, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*GS3, wn, tb6, env, cutoff, fenv)

	! F#m
	call play_note(audio, square_wave, tr*FS1, wn, tb7, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*CS3, wn, tb7, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*FS3, wn, tb7, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*A3 , wn, tb7, env, cutoff, fenv)

	! D
	call play_note(audio, square_wave, tr*D1 , wn, tb8, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*D3 , wn, tb8, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*FS3, wn, tb8, env, cutoff, fenv)
	call play_note(audio, square_wave, tr*A3 , wn, tb8, env, cutoff, fenv)

	!********

	call write_wav(filename, audio)

end program example

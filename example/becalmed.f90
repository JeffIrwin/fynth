program example

	use fynth
	use fynth__notes

	implicit none

	character(len = :), allocatable :: filename

	!********

	double precision :: bpm, quarter_note, eigth_note, en, qn, wn, hn, dhn, &
		cutoff_min, tr

	type(audio_t), target :: audio
	type(env_t) :: env, fenv
	type(synth_t) :: synth
	type(voice_t) :: sv, av, tv, bv

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

	cutoff_min = 300.d0
	!env  = env_t(a = 1.2, d = 2.4, s = 0.8, r = 0.7)
	env  = env_t(a = 0.3, d = 2.4, s = 0.8, r = 0.7)
	fenv = env_t(a = 2.3, d = 1.3, s = 0, r = 100)

	synth%cutoff_min = cutoff_min
	synth%cutoff_max = 2250.d0
	synth%env = env
	synth%fenv = fenv
	synth%wave => square_wave

	sv = new_voice(audio, synth)  ! soprano voice
	av = new_voice(audio, synth)  ! alto
	tv = new_voice(audio, synth)  ! tenor
	bv = new_voice(audio, synth)  ! bass

	! Transpose up from A to D
	tr = D2 / A1

	!print "(f10.4)", [D1, A1, E1, FS1, CS3, D3, E3, FS3, GS3, A3, B3, CS4, D4]

	! A
	call bv%play(tr*A1, wn)
	call tv%play(tr*E3, wn)
	call av%play(tr*A3, wn)

	call sv%play(tr*B3 , qn)
	call sv%play(tr*CS4, dhn)

	! D
	call bv%play(tr*D1 , wn)
	call tv%play(tr*FS3, wn)
	call av%play(tr*A3 , wn)
	call sv%play(tr*D4 , wn)

	! F#m
	call bv%play(tr*FS1, wn)
	call tv%play(tr*FS3, wn)
	call av%play(tr*A3 , wn)
	call sv%play(tr*CS4, wn)

	! D (different voicing now)
	call bv%play(tr*D1 , wn)
	call tv%play(tr*D3 , wn)
	call av%play(tr*FS3, wn)
	call sv%play(tr*A3 , wn)

	!********
	! A
	call bv%play(tr*A1, wn)
	call tv%play(tr*E3, wn)
	call av%play(tr*A3, wn)

	call sv%play(tr*B3 , qn)
	call sv%play(tr*CS4, dhn)

	! E
	call bv%play(tr*E1 , wn)
	call tv%play(tr*B2 , wn)
	call av%play(tr*E3 , wn)
	call sv%play(tr*GS3, wn)

	! F#m
	call bv%play(tr*FS1, wn)

	call tv%rest(hn)
	call tv%play(tr*CS3, hn)

	call av%play(tr*FS3, wn)
	call sv%play(tr*A3 , wn)

	! TODO:  Maybe port goldberg var 7 as well from play_note() to voice%play(),
	! although I'm not sure if I want to touch those 800 lines

	! D
	call bv%play(tr*D1 , wn)
	call tv%play(tr*D3 , wn)
	call av%play(tr*FS3, wn)
	call sv%play(tr*A3 , wn)

	!********

	call write_wav(filename, audio)

end program example

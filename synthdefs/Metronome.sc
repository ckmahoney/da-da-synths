Metronome {
	classvar <>added = false;
	classvar <name = \metronome;

	*addSynth {
	    var synth = SynthDef.new(name, {|out = 0, freq = 2400, amp = 0.1|
			var sig = amp * Pulse.ar(freq) * EnvGen.ar(Env.perc, timeScale: 1/3, doneAction: 2);
			sig = BPF.ar(sig, freq);
			Out.ar(out, sig!2);
		});
		synth.add;
		added = true;
	}

	*updateTempo {|cps = nil|
		if (cps.isNil.not, {
			TempoClock.tempo = cps;
		});
	}

	/** A metronome tuned to cps. */
    *clock {|cps = nil, freq = 1000|
		Metronome.updateTempo(cps);
		if (added.not, {Metronome.addSynth});

		Pbindef(\metronome, \instrument, name, \dur, Pn(1, inf), \freq, freq).play;
	}

	*play {|cps, freq|
		^this.clock(cps, freq);
	}

	/** Given a pattern of boolean values, sets 0s to Rest and plays all with uniform duration. */
	*select {|pat, freq = 2000, cps|
		var durs = pat.collect({|x| if (x == 0, { Rest(1) }, { 1 })});
		var key = "sampler_"+pat.asString;
		if (added.not, {Metronome.addSynth});

		Metronome.updateTempo(cps);
		Pbindef(key, \instrument, name, \dur, Pseq(durs, inf), \freq, freq).play;
		key;
	}
}
Inst_FM_Synth {
	classvar synths_;

	*synths {|synth|
		if (synths_.isNil, { synths_ = [] });
		if (synth.isNil.not, { synths_ = synths_.add(synth) });
		^synths_;
	}

	*make {|name, role, voice = 8|

		^switch(role,
			\bass, { Inst_FM_Synth.low(name) },
			\chords, { if (voice < 9, { Inst_FM_Synth.low(name) }, { Inst_FM_Synth.high(name) } ) },
			\lead, { Inst_FM_Synth.high(name)  },
			{ Inst_FM_Synth.high(name) }
		);
	}


	*low {|name|
		var timbreCar = [LFTri, LFSaw, SinOsc].choose;
		var modulationRatio = (1..16).choose / (1..8).choose;
		var carrier = [SinOsc, Pulse, LFSaw].choose;
		var lowAmpAdj = 0.85;
		var envGen = {|dur = 1|
			var breath = (1..3).choose.half;
			var ampMod = 2.pow((3..6).choose);
			// create an envelope measured in seconds
			var atk = 5 + 30.rand;
			var sus = (10..80).choose + (SinOsc.kr(breath).range(0, ampMod)/2);
			var rel = 100 - atk - sus + 15.rand;

			var a0, a1, a2, a3;
			a0 = 20 + 80.rand;
			a1 = 85 + 15.rand;
			a2 = (a1/2) + (a1/4).rand;
			a3 = 0;
			EnvGen.ar(Env.new([a0, a1, a2, a3]/100, [atk, sus, rel]*dur/100, \exp));
		};

		name = name ? ("fmSynth_low_" ++ Inst_FM_Synth.synths.size.asString); // set default value to enclosed
		^SynthDef(name, {|freq=300, amp=0.1, dur=4, cps=2.1, t=0|
			var sig = 0;
			// timbreMixMod is the amount of color change over time
			var timbreMixMod = timbreCar.ar(cps/128, t.mod(32)/32).range(0, 1);
			var ampEnv = envGen.value(dur);
			var formants = [];

			// use a stack of timbre shapers for thicc results
			12.do{|i| formants = formants.add(i+1) };
			formants.scramble.keep(4);

			formants.do{|hz, i|
				var modTimeScale = 2.pow(i.rand);
				var gain = (formants.size - i)/(i + 1);
				var timbre = timbreCar.ar(hz * cps/modTimeScale).range(0, 2.pow(i+5));
				var modIndex = timbreMixMod * timbre;
				var mod = modIndex * SinOsc.kr(modulationRatio * freq, mul: gain * formants.size.reciprocal);
				sig = sig + carrier.ar(freq + mod, mul: amp * formants.size.reciprocal);
			};

			sig = sig * ampEnv * lowAmpAdj/10;
			Out.ar(0, Pan2.ar(sig));
			DetectSilence.ar(sig, doneAction: 2);
		});
	}

	*high {|name|
		var modulationRatio = (1..16).choose / (1..8).choose;
		var carrier = [SinOsc, Pulse, LFSaw].choose;
		var modulator = [LFTri, LFSaw, SinOsc].choose;
		var duration = 2, focus = 14 + 8.rand;
		var pan = (if (0.5.coin, -1, 1)) * 0.6.rand;

		name = name ? "fmSynth_high_" ++ Inst_FM_Synth.synths.size.asString;


		^SynthDef(name, {|freq=300, amp=0.1, dur=4, cps=2.1, t=0, root = 1|
			var main = 0;
			var nVoices = 3;
			var formantsPerVoice = 4;

			nVoices.do {|j|
				var fmod = [root - 2.pow(1 + j.neg), root + 2.pow(1 + j.neg)]/8;
				var sig = 0, progress = t.mod(duration)/duration;

				var timbreMixMod = LFSaw.ar(cps/duration, progress).range(0, 1); // share the timbre mix across all formants
				var ampEnv = GenEnv.decayHigh(dur);
				var formants = [];
				var df = modulationRatio * freq / 2.pow(focus);

				timbreMixMod = 1;

				formantsPerVoice.do{|i| formants = formants.add(8.rand) };

				formants.do{|hz, i|
					var env = if (i.even, { GenEnv.decayHigh(dur) }, { GenEnv.decayLow(dur) });
					var modTimeScale = 2.pow(i-1);
					var mul = (formants.size - i)/(i + 1);
					var timbre = modulator.ar(hz * cps/modTimeScale).range(0, 2.pow(i+5));
					var modIndex = env * timbreMixMod * timbre;

					var mod = modIndex * mul * SinOsc.kr(freq).range(0, df);
					sig = sig + carrier.ar((freq *fmod) + mod, mul: amp * (nVoices.reciprocal * formantsPerVoice.reciprocal));
				};

				sig = sig * ampEnv/10;

				main = main + sig;
			};

			Out.ar(0, Pan2.ar(main, pan));
			DetectSilence.ar(main, doneAction: 2);
		});
	}
}
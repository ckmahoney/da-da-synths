Drum_FM_Synth {
	classvar synths_;

	*synths {|synth|
		if (synths_.isNil, { synths_ = [] });
		if (synth.isNil.not, { synths_ = synths_.add(synth) });
		^synths_;
	}

	*make {|name, role, voice = 8|

		^switch(role,
			\kick, { Drum_FM_Synth.low(name) },
			\perc, { Drum_FM_Synth.mid(name) },
			\hats, { Drum_FM_Synth.high(name)  },
			{ Drum_FM_Synth.high }
		);
	}

	*low {|name|
		name = name ? "fmSynth_drum_low_" ++ Drum_FM_Synth.synths.size.asString;

		^SynthDef(name, {
			arg freq,
			//Standard Values:
			out = 0, amp = 0.1, pan = 0, att = 0.005, rel = 0.29, curve = -4,
			//Other Controls:
			filterHarmonic = 1.5, preamp = 1.25, pulseAmp = 0.5, noiseAmp = 1, sineAmp = 1;

			var env, envp, snd;
			var lowAmpAdj = 0.85;

			env = Env.new([0.5, 1, 0.5, 0], [att, rel * 0.2, rel * 0.9], [curve, curve/2, curve]).kr;
			envp = Env.new([freq, freq/2, freq/4], [att, rel], [curve, curve * 1.25]).kr.midicps;

			snd = LFPulse.ar(envp).range(-1 * pulseAmp, pulseAmp);
			snd = snd + WhiteNoise.ar(mul: noiseAmp);
			snd = LPF.ar(snd, envp * filterHarmonic, mul: env);
			snd = snd + SinOsc.ar(envp, 0.5, mul: env * sineAmp);

			snd = Mix.ar(snd) * preamp;
			snd = snd.clip2(1) * amp * lowAmpAdj;

			Out.ar(out, Pan2.ar(snd));
			DetectSilence.ar(snd, doneAction: 2);
		});
	}




	*lowNex {|name, sweep = 3|
		var useLong =0.5.coin;
		name = name ? ("fmSynth_drum_low_" ++ Drum_FM_Synth.synths.size.asString);

		if (0.25.coin, { sweep = sweep + 1 });
		if (0.25.coin, { sweep = sweep - 1 });

		^SynthDef(name, {
			arg freq,
			//Standard Values:
			out = 0, dur = 1, amp = 0.1, att = 0.005, rel = 0.29, curve = -4,
			//Other Controls:
			filterHarmonic = 1.5, preamp = 1.25;

			var env, envp, snd;
			var lowAmpAdj = 0.85;



			// env = Env.new([0.5, 1, 0.5, 0], [att, rel * 0.2, rel * 0.9], [curve, curve/2, curve]).kr;
			// env = if (useLong, GenEnv.decayLow(dur), GenEnv.decayHigh(dur));
			env = GenEnv.decayHigh(dur);
			envp = 0.2 * Env.new([1, 1/2.pow(sweep-1), 1/2.pow(sweep)] * 2.pow(sweep), [att, rel], [curve, curve * 1.25]).kr.midicps;

			snd = LFPulse.ar(envp, mul: amp/2);
			snd = snd + WhiteNoise.ar(mul: amp);
			snd = LPF.ar(snd, envp * filterHarmonic, mul: env);
			snd = snd + SinOsc.ar(freq, 0.5, mul: env);

			snd = snd.clip2(1) * amp * lowAmpAdj;

			Out.ar(out, Pan2.ar(snd, 0));
			DetectSilence.ar(snd, doneAction: 2);
		});
	}


	*mid {|name, useLong = false|

		var carrier = [SinOsc, Pulse, LFSaw].choose;
		var modulator = [LFTri, LFSaw, SinOsc].choose;
		var duration = 2;
		var envGen = if (useLong || 0.5.coin, { {|dur|GenEnv.decayLow(dur);} }, { {|dur|GenEnv.shortHigh(dur);} });
		var nodes = 8.collect({ 1 + 1.0.rand + 11.rand });
		var resonances = [5, 5, 3, 3, 2, 2, 1, 1]; // add some harmonics to the first three nodes
		var pan = (if (0.5.coin, -1, 1)) * 1.0.rand;

		resonances.do({|n, index|
			n.do({|i|
				var resonator = nodes[index] * 2.pow(i+1);
				nodes = nodes.add(resonator);
			});
		});

		nodes.sort;
		name = name ? ("fmSynth_drum_mid_" ++ Drum_FM_Synth.synths.size.asString);

		^SynthDef(name, {|freq=300, amp=0.08, dur=4, cps=2.1, t=0, pan = 0|
			var main = 0;
			var nVoices = 3;

			nVoices.do{
				var sig = 0, progress = t.mod(duration)/duration;
				var ampEnv = envGen.value(dur);
				var formants = [];

				nodes.do{|f, i|
					var index = (500 + 500.rand) * GenEnv.decayHigh(dur);
					var mix = (nodes.size - i)/nodes.size;
					var env = if (i.even, {GenEnv.decayHigh(dur*mix)}, {GenEnv.shortHigh(dur*mix)});
					var mod = env * index * SinOsc.kr(f, 0);
					sig = sig + carrier.ar(freq + mod, mul: amp * nodes.size.reciprocal * nVoices.reciprocal);
				};

				sig =sig * ampEnv;
				main = sig + main;
			};
			Out.ar(0, Pan2.ar(main, pan));
			DetectSilence.ar(main, doneAction: 2);
		});
	}

	*high {|name, useLong = false|

		^SynthDef(\hats_1, {arg out = 0, amp = 0.08, dur = 1, att = 0.01, rel = 0.2, freq = 6000, pan = 0;
			var snd;
			var env = if (useLong, GenEnv.decayHigh(dur), GenEnv.shortHigh(dur));
			env = Env.perc(att, rel, amp).kr(doneAction: 2);
			snd = WhiteNoise.ar;
			snd = HPF.ar(in: snd, freq: freq, mul: env);
			snd = Limiter.ar(snd, 0.1);
			Out.ar(0, Pan2.ar(snd, pan));
		});
	}



	*highOld {|name, useLong = false|
		var carrier = [SinOsc, Pulse, LFSaw].choose;
		var modulator = [LFTri, LFSaw, SinOsc].choose;
		var duration = 2, focus = 9 + 2.rand;
		var envGen = if (useLong || 0.5.coin, { {|d|GenEnv.decayHigh(d)} }, { {|d|GenEnv.shortHigh(d)} });
		var pan = (if (0.5.coin, -1, 1)) * 0.8.rand;

		name = name ? ("fmSynth_drum_high_" ++ Drum_FM_Synth.synths.size.asString);

		^SynthDef(name, {|freq=300, amp=0.1, dur=4, cps=2.1, t=0|
			var sig = 0, progress = t.mod(duration)/duration;

			var timbreMixMod = LFSaw.ar(cps/duration, progress).range(1 , 1); // share the timbre mix across all formants
			var ampEnv = envGen.value(dur);
			var formants = [];

			timbreMixMod = 1;

			8.do{|i| formants = formants.add(8 + 8.rand) };

			formants.do{|hz, i|
				var env = GenEnv.decayHigh(dur);
				var modulationRatio = (hz*0.13) + ((111.rand + (1,3..19).choose) / (111.rand + (3,5..21).choose));
				var mod = modulator.ar(dur).range(100, 1000) * SinOsc.kr(freq.rand * modulationRatio);
				sig = sig + carrier.ar(freq + mod, mul: amp * formants.size.reciprocal);
			};

			sig = sig * ampEnv;
			Out.ar(0, Pan2.ar(sig, pan));
			DetectSilence.ar(sig, doneAction: 2);
		});
	}
}

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
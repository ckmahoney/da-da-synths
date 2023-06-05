Drum_FM_Synth2 {
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
}

Inst_FM_Synth2 {
	classvar synths_;

	*synths {|synth|
		if (synths_.isNil, { synths_ = [] });
		if (synth.isNil.not, { synths_ = synths_.add(synth) });
		^synths_;
	}

	*make {|name, role, voice = 8|

		^switch(role,
			\bass, { Inst_FM_Synth.low(name) },
			\chords, { Inst_FM_Synth2.high(name) },
			\lead, { Inst_FM_Synth2.high(name)  },
			{ Inst_FM_Synth.high(name) }
		);
	}

	*clean 	{|buf, dbuf = 1, minFreq, maxFreq, minHarmonic, maxHarmonic, name|
		var durationThresh = 0.05;
		^SynthDef((\fm_0_ ++ name).asSymbol, {|freq = 100, amp = 1, dur = 1, pan = 0, cps = 2.1, root = 1, t=0, p = 0.5, phrase=16|
			var sig, pos;
			var add;
			var car, mod;
			var hEnv, ampEnv;

			var modH = SinOsc.kr(cps/4, 2pi * t.mod(phrase)).range(0, 1);

			var timeScale = if ((p * cps.reciprocal) < durationThresh, durationThresh * 60 * cps.reciprocal, p);

			hEnv = 100 * GenEnv.decayHigh(dur);

			// mod = modH * SinOsc.kr(p * 12 *freq, mul: cps*4).range(0, 1);
			mod = SinOsc.kr(p * 12 *freq, mul: cps*4).range(0, 1);
			car = SinOsc.ar(freq + mod, mul: amp/2);

			// pos = modH * hEnv * LFTri.kr(phrase * cps.reciprocal, 2pi * p).range(buf, dbuf);
			pos = hEnv * LFTri.kr(phrase * cps.reciprocal, 2pi * p).range(buf, dbuf);

			ampEnv = if (0.5.coin, { GenEnv.shortHigh(dur, timeScale) }, { GenEnv.decayHigh(dur, timeScale) });

			sig = HPF.ar(car, minHarmonic);
			sig = amp * ampEnv * LPF.ar(sig, maxHarmonic);

			Out.ar(0, Pan2.ar(sig, pan));
			DetectSilence.ar(sig, time: dur/8, doneAction: 2);
		});
	}

	*bass {|conf, maxFreq, minHarmonic, maxHarmonic, name|
		var carrier = [SinOsc, LFSaw, LFPulse, LFTri].choose;

		var pan = (if (0.5.coin, -1, 1)) * 0.6.rand;
		var cps = conf.at(\cps), root = conf.at(\root), cpc = conf.at(\cpc);

		var modulators = [LFTri, LFSaw, SinOsc].scramble;

		var nCarriers = 5;
		var modsPerCarrier = 2;

		var highestOctave = [maxHarmonic, maxFreq].maxItem.log2;
		var maxOctave = 11; // General maximum value for frequencies to prevent aliasing

		var glide = [12,18,24, 48, 72, 96, 132, 164].choose.reciprocal;

		// When using more than 1 carrier, turn them down a decible per carrier so it doesn't get too loud
		var carScale = (1/nCarriers) * (1/3);

		var numerator = if (highestOctave < 8, [2,3].choose,  [3,4,5].choose);
		var denominator = numerator - 1;
		var modulationRatio = numerator / denominator;
		var ampMod = 3.pow(-2);

		// Extra safetey check to prevent aliasing above the Nyquist frequency
		if (highestOctave > maxOctave, {
			// lower maxFreq and maxHarmonic to be under 22,000 Hz
			maxFreq = maxFreq * 2.pow(maxOctave - highestOctave);
			maxHarmonic = maxHarmonic * 2.pow(maxOctave - highestOctave);
		});

		// Allow more sparkle on top when available
		if ((maxHarmonic *3)< 20000, { maxHarmonic = maxHarmonic * 2 });
		maxHarmonic = 5000;


		^SynthDef(name, { arg out = 0,
			freq=300, amp=0.1, dur=4, // basic music playback args
			pan = 0, sus = 1, pfreq = 100, // positional and color args
			t = 0,  cpc = 4, phrase = 16; // phrasing parameters for functions of time

			// The final outputzz signal, starts with nothing.
			var sig = 0;

			// An array to hold the carriers as we make them
			var cars = [];

			// How far we are into the current measure of music
			var cellProgress = t.mod(cpc)/cpc;

			// When looping bars of music indefinitely,
			// reset the param automation for each phrase
			var phraseProgress = t.mod(phrase)/phrase;

			var vibrato = SinOsc.kr(freq/100, mul: 10.rand) * LFSaw.ar(cps.reciprocal * dur, (1 + 12.rand).reciprocal*pi).range(0, 1);

			// Create a glide using the previous frequency (pfreq)
			freq = Line.ar(pfreq, freq, dur*glide);


			// call a callback function to generate carriers
			cars = nCarriers.collect({|j|
				// The audio signal for this carrier, starts with nothing.
				var snd = 0;

				// The modulation signal for this carrier.
				var mod = 0;

				var nEnvs = 3;
				var env;
				var ampEnv = 0;

				// A dynamic control for the modulator
				// The visible portion of modulation increases as we get to the end of the phrase
				var direction = [0, 1].scramble;
				var timbreMix = LFSaw.ar((cps * phrase).reciprocal, pi * phraseProgress).range(*direction); // share the timbre mix across all formants

				// detune the frequency using optimal values, with a higher tone in one channel and lower in the other

				// Give this carrier a unique amplitude envelope
				nEnvs.do{

					var env = [Env.perc, Env.linen, Env.sine, Env.triangle].choose;

					// Some random attack and release values
					var atk = dur * 2.pow((-8..1).choose);
					var rel = dur * 2.pow((-4..4).choose);

					// Construct the envelope
					ampEnv = ampEnv + EnvGen.ar(env.value(attackTime: atk * dur, releaseTime: rel * dur), timeScale: sus * (0.2 + 0.9.rand));
				};
				ampEnv = amp * ampEnv * nEnvs.reciprocal;

				modsPerCarrier.do{|rate, i|
					var modMix, modIndex, modulator, timbre, env, modEnv;
					var modHelper = 10 + 50.rand;

					env = [Env.perc, Env.linen, Env.sine, Env.triangle].choose;
					// Generate an env sustain value based on index. First one has the most influence, the last has the least
					modEnv = EnvGen.ar(env, timeScale: sus * 2 / (i + 1));

					// Generate a mix value based on index. First one has the most influence, the last has the least
					modMix = (modsPerCarrier - i)/modsPerCarrier;

					// Control the amplitude of sidebands with some dynamic params
					modIndex = timbreMix * modMix * modEnv;


					// pick a modulation source
					modulator = modulators.wrapAt(i);

					// create the modulator
					timbre = modHelper * modulator.ar(freq: modulationRatio * freq, mul: modulationRatio * modIndex);

					// add this modulation component to the modulation mixture
					mod = mod + timbre;
				};

				// return a modulated carrier wave to the cars array
				ampEnv * carrier.ar(freq + mod + vibrato, mul: carScale);
			});


			// Mix em up
			sig = 3 * Mix.ar(cars);


			// Apply high pass and low pass filters to keep the sound in bounds
			sig = HPF.ar(sig, minHarmonic);
			sig = LPF.ar(sig, maxHarmonic);

			// Put the signal somewhere in space
			sig = Limiter.ar(Pan2.ar(sig, pan), ampMod * amp);

			Out.ar(0, sig);

			// Use doneAction: 2 to free the node when the amplitude drops to silence
			DetectSilence.ar(sig, time: dur/3, doneAction: 2);
		});
	}

	*chords {|conf, maxFreq, minHarmonic, maxHarmonic, name|
		var carrier = [SinOsc, LFSaw, LFPulse, LFTri].choose;

		var pan = (if (0.5.coin, -1, 1)) * 0.6.rand;
		var cps = conf.at(\cps), root = conf.at(\root), cpc = conf.at(\cpc);

		var modulators = [LFTri, LFSaw, SinOsc].scramble;

		var nCarriers = 3;
		var modsPerCarrier = 8;

		var highestOctave = [maxHarmonic, maxFreq].maxItem.log2;
		var maxOctave = 11; // General maximum value for frequencies to prevent aliasing

		var glide = [8,12,18,24, 48, 72, 96].choose.reciprocal;

		// When using more than 1 carrier, turn them down a decible per carrier so it doesn't get too loud
		var carScale = (1/nCarriers) * (1/3);


		var numerator = if (highestOctave < 8, [2,3,4].choose,  [3,4,5,6].choose);
		var denominator = numerator - 1;
		var modulationRatio = numerator / denominator;
		var ampMod = 3.pow(-2);


		// Extra safetey check to prevent aliasing above the Nyquist frequency
		if (highestOctave > maxOctave, {
			// lower maxFreq and maxHarmonic to be under 22,000 Hz
			maxFreq = maxFreq * 2.pow(maxOctave - highestOctave);
			maxHarmonic = maxHarmonic * 2.pow(maxOctave - highestOctave);
		});


		// Allow more sparkle on top when available
		if ((maxHarmonic *3)< 20000, { maxHarmonic = maxHarmonic * 2 });
		maxHarmonic = 15000;


		^SynthDef(name, { arg out = 0,
			freq=300, amp=0.1, dur=4, // basic music playback args
			pan = 0, sus = 1, pfreq = 100, // positional and color args
			t = 0,  cpc = 4, phrase = 16; // phrasing parameters for functions of time

			// The final outputzz signal, starts with nothing.
			var sig = 0;

			// An array to hold the carriers as we make them
			var cars = [];

			// How far we are into the current measure of music
			var cellProgress = t.mod(cpc)/cpc;

			// When looping bars of music indefinitely,
			// reset the param automation for each phrase
			var phraseProgress = t.mod(phrase)/phrase;

			var vibrato = SinOsc.kr(freq/100, mul: (1.exprand(20)-1)) * LFSaw.ar(cps.reciprocal * dur, (1 + 12.rand).reciprocal*pi).range(0, 1);

			// Create a glide using the previous frequency (pfreq)
			freq = Line.ar(pfreq, freq, dur*glide);


			// call a callback function to generate carriers
			cars = nCarriers.collect({|j|
				// The audio signal for this carrier, starts with nothing.
				var snd = 0;

				// The modulation signal for this carrier.
				var mod = 0;

				var nEnvs = 3;
				var env;
				var ampEnv = 0;

				// A dynamic control for the modulator
				// The visible portion of modulation increases as we get to the end of the phrase
				var direction = [0, 1].scramble;
				var timbreMix = LFSaw.ar((cps * phrase).reciprocal, pi * phraseProgress).range(*direction); // share the timbre mix across all formants

				// detune the frequency using optimal values, with a higher tone in one channel and lower in the other

				// Give this carrier a unique amplitude envelope
				nEnvs.do{

					var env = [Env.perc, Env.linen, Env.sine, Env.triangle].choose;

					// Some random attack and release values
					var atk = dur * 2.pow((-8..1).choose);
					var rel = dur * 2.pow((-4..4).choose);
					var s = (0.2 + 0.9.rand);

					// Construct the envelope
					ampEnv = ampEnv + EnvGen.ar(env.value(attackTime: atk*dur, releaseTime: rel*dur), timeScale: sus * s);
				};

				ampEnv = amp * ampEnv * nEnvs.reciprocal;

				modsPerCarrier.do({|rate, i|
					var modMix, modIndex, modulator, timbre, env;

					var nmEnvs = 3;

					var modEnv = 0;
					var modHelper = 50 + 100.exprand(600);

					nmEnvs.do({

						var env = [Env.perc, Env.linen, Env.sine, Env.triangle].choose;

						// Some random attack and release values
						var atk = dur * 2.pow((-8..1).choose);
						var rel = dur * 2.pow((-4..4).choose);
						var s = (0.2 + 0.9.rand);

						// Construct the envelope
						modEnv = modEnv + EnvGen.ar(env.value(attackTime: atk*dur, releaseTime: rel*dur), timeScale: sus * s);
					});

					modEnv = modEnv * nmEnvs.reciprocal;

					// Generate a mix value based on index. First one has the most influence, the last has the least
					modMix = (modsPerCarrier - i)/modsPerCarrier;

					// Control the amplitude of sidebands with some dynamic params
					modIndex = timbreMix * modMix * modEnv;


					// pick a modulation source
					modulator = modulators.wrapAt(i);

					// create the modulator
					timbre = modHelper * modulator.ar(freq: modulationRatio * freq, mul: modulationRatio * modIndex);

					// add this modulation component to the modulation mixture
					mod = mod + timbre;
				});

				// return a modulated carrier wave to the cars array
				ampEnv * carrier.ar(freq + mod + vibrato, mul: carScale);
			});


			// Mix em up
			sig = 1.5 * Mix.ar(cars);


			// Apply high pass and low pass filters to keep the sound in bounds
			sig = HPF.ar(sig, minHarmonic);
			sig = LPF.ar(sig, maxHarmonic);

			// Put the signal somewhere in space
			sig = Limiter.ar(Pan2.ar(sig, pan), ampMod * amp);

			Out.ar(0, sig);

			// Use doneAction: 2 to free the node when the amplitude drops to silence
			DetectSilence.ar(sig, time: dur/3, doneAction: 2);
		});

	}

	*lead {|conf, maxFreq, minHarmonic, maxHarmonic, name|
		var carrier = [SinOsc, LFSaw, LFPulse, LFTri].choose;

		var pan = (if (0.5.coin, -1, 1)) * 0.6.rand;
		var cps = conf.at(\cps), root = conf.at(\root), cpc = conf.at(\cpc);

		var modulators = [LFTri, LFSaw, SinOsc].scramble;

		var nCarriers = 5;
		var modsPerCarrier = 4;

		var highestOctave = [maxHarmonic, maxFreq].maxItem.log2;
		var maxOctave = 11; // General maximum value for frequencies to prevent aliasing

		var glide = [6,8,12,18,24, 48, 72].choose.reciprocal;

		// When using more than 1 carrier, turn them down a decible per carrier so it doesn't get too loud
		var carScale = (1/nCarriers) * (1/3);


		var numerator = if (highestOctave < 8, [2,3,4].choose,  [3,4,5,6].choose);
		var denominator = numerator - 1;
		var modulationRatio = numerator / denominator;
		var ampMod = 3.pow(-2);


		// Extra safetey check to prevent aliasing above the Nyquist frequency
		if (highestOctave > maxOctave, {
			// lower maxFreq and maxHarmonic to be under 22,000 Hz
			maxFreq = maxFreq * 2.pow(maxOctave - highestOctave);
			maxHarmonic = maxHarmonic * 2.pow(maxOctave - highestOctave);
		});


		// Allow more sparkle on top when available
		if ((maxHarmonic *3)< 20000, { maxHarmonic = maxHarmonic * 2 });
		maxHarmonic = 15000;


		^SynthDef(name, { arg out = 0,
			freq=300, amp=0.1, dur=4, // basic music playback args
			pan = 0, sus = 1, pfreq = 100, // positional and color args
			t = 0,  cpc = 4, phrase = 16; // phrasing parameters for functions of time

			// The final outputzz signal, starts with nothing.
			var sig = 0;

			// An array to hold the carriers as we make them
			var cars = [];

			// How far we are into the current measure of music
			var cellProgress = t.mod(cpc)/cpc;

			// When looping bars of music indefinitely,
			// reset the param automation for each phrase
			var phraseProgress = t.mod(phrase)/phrase;

			var vibrato = SinOsc.kr(freq/100, mul: (3.exprand(20)-1)) * LFSaw.ar(cps.reciprocal * dur, (1 + 12.rand).reciprocal*pi).range(0, 1);

			// Create a glide using the previous frequency (pfreq)
			freq = Line.ar(pfreq, freq, dur*glide);


			// call a callback function to generate carriers
			cars = nCarriers.collect({|j|
				// The audio signal for this carrier, starts with nothing.
				var snd = 0;

				// The modulation signal for this carrier.
				var mod = 0;

				var nEnvs = 3;
				var env;
				var ampEnv = 0;

				// A dynamic control for the modulator
				// The visible portion of modulation increases as we get to the end of the phrase
				var direction = [0, 1].scramble;
				var timbreMix = LFSaw.ar((cps * phrase).reciprocal, pi * phraseProgress).range(*direction); // share the timbre mix across all formants

				// detune the frequency using optimal values, with a higher tone in one channel and lower in the other

				// Give this carrier a unique amplitude envelope
				nEnvs.do{

					var env = [Env.perc, Env.linen, Env.sine, Env.triangle].choose;

					// Some random attack and release values
					var atk = dur * 2.pow((-8..1).choose);
					var rel = dur * 2.pow((-4..4).choose);
					var s = (0.2 + 0.7.rand);

					// Construct the envelope
					ampEnv = ampEnv + EnvGen.ar(env.value(attackTime: atk*dur, releaseTime: rel*dur), timeScale: sus * s);
				};

				ampEnv = amp * ampEnv * nEnvs.reciprocal;

				modsPerCarrier.do({|rate, i|
					var modMix, modIndex, modulator, timbre, env;

					var nmEnvs = 3;

					var modEnv = 0;
					var modHelper = 100 + 100.exprand(900);

					nmEnvs.do({

						var env = [Env.perc, Env.linen, Env.sine, Env.triangle].choose;

						// Some random attack and release values
						var atk = dur * 2.pow((-8..1).choose);
						var rel = dur * 2.pow((-4..4).choose);
						var s = (0.2 + 0.9.rand);

						// Construct the envelope
						modEnv = modEnv + EnvGen.ar(env.value(attackTime: atk*dur, releaseTime: rel*dur), timeScale: sus * s);
					});

					modEnv = modEnv * nmEnvs.reciprocal;

					// Generate a mix value based on index. First one has the most influence, the last has the least
					modMix = (modsPerCarrier - i)/modsPerCarrier;

					// Control the amplitude of sidebands with some dynamic params
					modIndex = timbreMix * modMix * modEnv;


					// pick a modulation source
					modulator = modulators.wrapAt(i);

					// create the modulator
					timbre = modHelper * modulator.ar(freq: modulationRatio * freq, mul: modulationRatio * modIndex);

					// add this modulation component to the modulation mixture
					mod = mod + timbre;
				});

				// return a modulated carrier wave to the cars array
				ampEnv * carrier.ar(freq + mod + vibrato, mul: carScale);
			});


			// Mix em up
			sig = 3 * Mix.ar(cars);


			// Apply high pass and low pass filters to keep the sound in bounds
			sig = HPF.ar(sig, minHarmonic);
			sig = LPF.ar(sig, maxHarmonic);

			// Put the signal somewhere in space
			sig = Limiter.ar(Pan2.ar(sig, pan), ampMod * amp);

			Out.ar(0, sig);

			// Use doneAction: 2 to free the node when the amplitude drops to silence
			DetectSilence.ar(sig, time: dur/3, doneAction: 2);
		});

	}

	*low {|conf, minFreq, maxFreq, minHarmonic, maxHarmonic, name|
		var carrier = [SinOsc, LFSaw, LFPulse, LFTri].choose;
		var modulators = [LFTri, LFSaw, Pulse].scramble;
		var duration = 2, focus = 14 + 8.rand;
		var pan = (if (0.5.coin, -1, 1)) * 0.6.rand;
		var cps = conf.at(\cps), root = conf.at(\root);

		var nCarriers = 3;
		var maxFmod = [root*2, maxFreq].minItem;
		var maxVoice = [maxHarmonic, maxFreq].maxItem.log2;
		var glide = [3, 6,8,12].choose.reciprocal;


		if (maxVoice > 11, {
			maxFreq = maxFreq* 2.pow(11-maxVoice);
			maxHarmonic = maxHarmonic * 2.pow(11-maxVoice);
		});

		if ((maxHarmonic *3)< 20000, { maxHarmonic = maxHarmonic * 3 });


		name = name ? "fmSynth2_bass_" ++ Inst_FM_Synth.synths.size.asString;


		^SynthDef(name, {|freq=100, amp=0.1, dur=4, sus = 1, pfreq = 100, t=0, p = 0.5, phrase = 8, cpc = 4|
			var sig = 0;
			var modsPerCarrier = 3;
			freq = Line.ar(pfreq, freq, dur*glide);

			nCarriers.do {|j|
				var snd = 0, progress = p;
				var tMod = 10 * LFTri.kr(cps.reciprocal * phrase, (p*4).mod(0));
				var modulationRatio = [3,5,7,9].choose;

				var ampEnv = EnvGen.ar([Env.perc, Env.linen, Env.sine, Env.triangle].choose, timeScale: sus * 0.95);

				modsPerCarrier.do{|rate, i|
					var f;
					var feg = (3..10).choose * 10 * GenEnv.decayHigh(dur);
					var modMix = (modsPerCarrier - i)/modsPerCarrier;
					var env = if (i.even, { GenEnv.decayHigh(dur*2) }, { GenEnv.decayLow(dur) });
					var modIndex = modMix * env;

					var timbre = feg * tMod * modulators.wrapAt(i).ar(modulationRatio * freq).range(-1, 1);

					var vibrato = GenEnv.decayHigh(dur*(i+1)) * SinOsc.kr(cps.reciprocal/(3 * (i+1)), (2pi*(i/nCarriers)) + 2pi*p);

					var mod = modIndex * timbre;
					var ampEnv = Env.pairs([[0, 0.1], [0.005, 0.4], [0.01, 0.8], [dur/2, 1], [dur, 0.01], [dur*2, 0]], \exp).ar(doneAction: 2);

					freq = vibrato + freq;
					// f = freq * [1 - 0.005.rand, 1 + 0.005.rand];
					f = freq;
					snd = ampEnv * (snd + carrier.ar(f + mod, 2pi.rand, nCarriers.reciprocal.squared * modsPerCarrier.reciprocal.squared));
				};

				sig = sig + snd;
			};

			sig = HPF.ar(sig, minHarmonic);
			sig = LPF.ar(sig, maxHarmonic);
			sig = Pan2.ar(sig, pan);

			Out.ar(0, Pan2.ar(sig, 0));
			DetectSilence.ar(sig, time: dur/3);
		});
	}

	*high {|conf, minFreq, maxFreq, minHarmonic, maxHarmonic, name|
		var carrier = [SinOsc, LFSaw, LFPulse, LFTri].choose;
		var numerator = 2;
		var denominator = numerator - [1,2].choose;
		var modulationRatio = numerator / 1;
		var modulators = [LFTri, LFSaw, SinOsc].scramble;
		var duration = 2, focus = 14 + 8.rand;
		var pan = (if (0.5.coin, -1, 1)) * 0.6.rand;
		var cps = conf.at(\cps), root = conf.at(\root);

		var nCarriers = 3;
		var maxFmod = [root*2, maxFreq].minItem;
		var maxVoice = [maxHarmonic, maxFreq].maxItem.log2;
		var glide = [6,8,12,18,24, 48, 72, 96].choose.reciprocal;


		if (maxVoice > 11, {
			maxFreq = maxFreq* 2.pow(11-maxVoice);
			maxHarmonic = maxHarmonic * 2.pow(11-maxVoice);
		});
		if ((maxHarmonic *3)< 20000, { maxHarmonic = maxHarmonic * 3 });

		name = name ? "fmSynth2_high_" ++ Inst_FM_Synth.synths.size.asString;

		^SynthDef(name, {|freq=300, amp=0.1, dur=4, pan = 0, pfreq = 100, sus = 1, t=0, p = 0.5, phrase = 8, cpc = 4|
			var sig = 0;
			var modsPerCarrier = 3;
			var phraseProgress = t.mod(phrase)/phrase;
			freq = Line.ar(pfreq, freq, dur*glide);

			nCarriers.do {|j|
				var env = [Env.perc, Env.linen, Env.sine, Env.triangle].choose;
				var snd = 0;

				var timbreMix = 6  * LFSaw.ar(cps.reciprocal, p); // share the timbre mix across all formants
				var atk = dur * 2.pow((-8..-2).choose);
				var rel = dur * 2.pow((-2..2).choose);

				var ampEnv = EnvGen.ar(env.value(attackTime: dur/8, releaseTime: rel), timeScale: sus * 0.7);

				modsPerCarrier.do{|rate, i|
					var f;
					var feg = (1..6).choose * 5 * GenEnv.decayHigh(cps.reciprocal/3);
					var env = if (i.even, { GenEnv.decayHigh(dur*2) }, { GenEnv.decayLow(dur) });
					var modMix = (modsPerCarrier - i)/modsPerCarrier;

					var timbre = feg * timbreMix * modulators.wrapAt(i).ar(modulationRatio * freq);
					var modIndex = modMix * env * timbre;

					var vibrato = GenEnv.decayHigh(dur*(i+1)) * SinOsc.kr(cps.reciprocal/(3 * (i+1)), (2pi*(i/nCarriers)) + 2pi*p);

					var mod = modIndex * timbre;
					freq = vibrato + freq;
					f = freq * [2.pow(j-1)*root, 2.pow(j-1)*root.neg].scramble;
					snd = Env.perc.ar * (snd + carrier.ar(f + mod, 2pi.rand, nCarriers.reciprocal.squared * modsPerCarrier.reciprocal.squared));
				};

				sig = sig + snd;
			};

			sig = sig;

			sig = HPF.ar(sig, minHarmonic);
			sig = LPF.ar(sig, maxHarmonic);
			sig = Pan2.ar(sig/4.5, pan);

			Out.ar(0, Pan2.ar(sig, pan));
			DetectSilence.ar(sig, time: dur/3, doneAction: 2);
		});
	}
}
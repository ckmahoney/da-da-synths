Voscillator {
	classvar <>t = 0;

	/*
	Generate n frames of vosc harmonics bound by a maximum frequency.
	*/
	*stack3 {|frames|
		var freq, amp, phase;

		// since this is a pure harmonic synth, share the same list of
		// harmonics across all tables
		var freqs = (1..(2.pow(frames))).asInt;

		^frames.collect({|h|
			var maxFreq = 2.pow(h+1);
			var as = [], ps = [];
			var useSquare = 0.5.coin; // square wave is odd harmonics
			// All tables must have the same size, so use the amplitude to turn off the undesired components.
			freqs.do({|freq, i|
				// generate an amplitude based on index in the tables
				amp = if (freq < maxFreq, 1, 0);
				amp = amp * if (useSquare, {
					if (i.even, 1.0.rand/50, 1)
				}, {
					if (i.odd, 1.0.rand/50, 1)
				});

				// generate an arbitrary phase
				phase = 2pi.rand;

				as = as.add(amp);
				ps = ps.add(phase);
			});
			[freqs, as, ps];
		});
	}

	*genDef {|buf, dbuf = 1, minFreq, maxFreq, minHarmonic, maxHarmonic, name, nAdditives = 4, conf|
		var conf_ = conf ? (cpc: 4, cps: 1.2);
		var durationThresh = 0.05;

		var maxVoice = [maxHarmonic, maxFreq].maxItem.log2;
		var glide = [18,24, 48, 72,128,1024].choose.reciprocal;
		var ampMod = 3.pow(nAdditives.neg + 1);
		var oscRate = conf_.at(\cps).reciprocal / conf_.at(\cpc) / (1..12).choose;

		if (maxVoice > 11, {
			maxFreq = maxFreq* 2.pow(11-maxVoice);
			maxHarmonic = maxHarmonic * 2.pow(11-maxVoice);
		});

		if ((maxHarmonic *3)< 20000, { maxHarmonic = maxHarmonic * 3 });
		maxHarmonic = 18000;

		^SynthDef((\wavetable_ ++ name).asSymbol, {|out = 0, freq = 100, pfreq, amp = 0.1, dur = 1, pan = 0, cps = 2.1, sus = 1,  p = 0.5, t=0, phrase=16, cpc = 4|
			var sig, pos;
			var add = 0;
			var car, mod;
			var ampEnv1, ampEnv2, ampEnv;

			// increase sustain as phrase progresses
			var sweeper = [SinOsc, LFTri].choose;

			freq = XLine.ar(pfreq, freq, dur*glide);

			ampEnv1 = EnvGen.ar([Env.perc, Env.linen, Env.sine, Env.triangle].choose, timeScale: sus);
			ampEnv2 = EnvGen.ar(Env.pairs([[0, 0.005], [dur/IRand(10, 100), 2/3], [dur/IRand(3,6), 1], [dur*4/5, 2/3], [dur, 0]], \exp), timeScale: sus);
			ampEnv = (ampEnv1 + ampEnv2)/2;
			nAdditives.do {|i|
				var mx, mn, pos;
				var f = freq + [2.pow(i-5).rand, 2.pow(i-5).rand.neg].scramble;
				if (0.5.coin, { mx = buf; mn = dbuf + buf }, { mx = dbuf + buf; mn = buf});
				pos = sweeper.ar(oscRate, 2pi * p).range(mn, mx);
				pos = buf;
				add = add + (VOsc.ar(pos, f, mul: 1/nAdditives/3));
			};

			sig = HPF.ar(add, minHarmonic);
			sig = amp * ampEnv * LPF.ar(sig, maxHarmonic);

			sig = HPF.ar(sig, minHarmonic);
			sig = LPF.ar(sig, maxHarmonic);

			Out.ar(out, Pan2.ar(sig, pan));
		});
	}

	*genSynth {|voice, bufnum, minF=nil, maxF=nil, minH=nil, maxH=nil, name, maxVoice=13, nAdditives = 8, serv|
		var server = serv ? Server.default;
		var buffStuff, loadMsgs = [], afterLoadMsgs = [], freeMsgs = [], buffers = [];
		var tables, synth;

		name = name ? "voice_buf_" ++ bufnum;
		minF = minF ? 2.pow(voice - 1);
		maxF = maxF ? 2.pow(voice + 1);
		minH = minH ? (minF *2/3);
		maxH = maxH ? (maxF* 3/2);

		tables = Voscillator.stack3(1 + maxVoice - voice);
		tables.size.do({|i|
			var buffer = Buffer.new(Server.default, tables[0][0].size, 1, bufnum: bufnum + i);
			buffers = buffers.add(buffer);
			loadMsgs = loadMsgs.add(buffer.allocMsg);
			afterLoadMsgs = afterLoadMsgs.add(buffer.sine3Msg(*tables[i]));
			freeMsgs = freeMsgs.add(buffer.freeMsg);
		});

		synth = Voscillator.genDef(bufnum, buffers.size - 1, minF, maxF, minH, maxH, name, nAdditives);
		buffStuff = (
			buffers: buffers,
			loadMsgs: loadMsgs,
			afterLoadMsgs: afterLoadMsgs,
			freeMsgs:freeMsgs
		);
		^[buffStuff, synth];
	}


	*test {|nAdditives = 8, vs|
		// as polyphonic voices
		var maxVoice = 13;
		var midikey = 5;
		var cps = 2.4;
		var root = MidiMap.chromaToRoot(midikey);
		var offset = 0;
		var phrase = 8;

		vs = vs ? (6..11);

		TempoClock.tempo = cps;
		Window.closeAll;

		^vs.collect({|voice, i =0|
			var midinotes = midikey + [0, 2, 4, 5, 7, 9, 11].scramble + (12 * (voice - 3));
			var buffs, synth;
			var rhythm = midinotes.collect({[1/2, 1, 2].choose});
			var bind;
			var t = 0;
			#buffs, synth = Voscillator.genSynth(voice, offset, name: "frank_"++voice.asString, maxVoice: maxVoice, nAdditives: nAdditives);
			synth.add;
			bind = Pbindef("test_frank_" ++ voice.asString,
				\instrument, synth.name,
				\midinote, Pseq(midinotes, inf),
				\dbuf, buffs.size-1,
				\bufnum, offset,
				\root, root,
				\dur, Pseq(rhythm, inf),
				\cps, cps,
				\amp, 1,
				\phrase, phrase,
				\t, Pfunc({|event| var dur = event.at(\dur); t = t + dur; ~t = t; t }),
				\p, Pfunc({ ~t.mod(phrase)/phrase });
			);
			offset = offset + buffs.size;
			bind.play;
			bind

		});
	}
}
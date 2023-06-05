GrabBag {
	classvar <ampMod = 1;
	*kicks {
		^[
			SynthDef(\kick_chirp, {

				arg out = 0, amp = 0.1, pan = 0, curve = -20, att = 0.001, rel = 0.5, freq = 250;

				var env, snd, aMod;
				freq = freq * 4;

				aMod = if (freq.log2 > 10, 1/3, { if (freq.log2 > 7, 2/3, 1) });

				env = Env.perc(attackTime: att, releaseTime: rel, curve: curve).exprange(0, freq*2).kr(doneAction: 2);

				snd = SinOsc.ar(freq: env, mul: amp);
				snd = aMod * LeakDC.ar(snd);


				snd = HPF.ar(snd, 21);
				snd = LPF.ar(snd, 21950);

				Out.ar(out, Pan2.ar(snd, pan))
			},
			metadata: (
				notes: "Has low presence and some high sweep, pretty satisfying general kick",
				credit: "Original by dan stowell. public domain",
				category: \drums,
				tags: [\kick, \drum, \percussion, \chirp],
				q: 5/5
			)
			),
			SynthDef(\kick_electro, {
				arg out = 0, freq = 100, pan = 0, amp = 0.1, att = 0.005, rel = 0.3;

				var body, bodyFreq, bodyAmp, pop, popFreq, popAmp, click, clickAmp, snd;

				// body starts midrange, quickly drops down to low freqs, and trails off
				bodyFreq = Env.new(
					levels: [261, 120, 51] * freq / 51,
					times: [rel / 8.57, rel / 3.75],
					curve: \exp
				).kr;

				bodyAmp = Env.linen(
					attackTime: att,
					sustainTime: rel / 3,
					releaseTime: rel
				).kr;

				body = SinOsc.ar(freq: bodyFreq, mul: bodyAmp);

				// pop sweeps over the midrange
				popFreq = XLine.kr(start: 750 * freq/51, end: 261 * freq/51, dur: 0.02);

				popAmp = Env.linen(
					attackTime: att / 5,
					sustainTime: rel / 15,
					releaseTime: rel / 30,
					level: 0.15
				).kr;

				pop = SinOsc.ar(freq: popFreq, mul: popAmp);

				// click is spectrally rich, covering the high-freq range
				// you can use Formant, FM, noise, whatever
				clickAmp = Env.perc(
					attackTime: att / 5,
					releaseTime: rel / 300,
					level: 0.15
				).kr;

				click = LPF.ar(
					in: Formant.ar(fundfreq: 910 * freq/51, formfreq: 4760 * freq/51, bwfreq: 2110 * freq/51),
					freq: 3140 * freq/51,
					mul: clickAmp
				);

				//Putting it all together:
				snd = Mix.ar([body, pop, click]);
				snd = snd.tanh * amp;

				snd = HPF.ar(snd, 21);
				snd = LPF.ar(snd, 21950);


				DetectSilence.ar(in: snd, doneAction: 2);

				Out.ar(out, Pan2.ar(snd, pan, amp));
			},
			metadata: (
				notes: "highly resonant drum, not like a kick.",
				credit: "By Nathan Ho aka Snappizz",
				category: \drums,
				tags: [\percussive, \bass, \kick, \electronic],
				q: 1/5
			)
			),
			SynthDef("kick808", {arg out = 0, freq= 240, freq2 = 60, amp = 0.1, ringTime = 10, att = 0.001, rel = 1, dist = 0.5, pan = 0;
				var snd, env;
				var decay  = if (freq.log2 > 7, { freq.log2.reciprocal }, ringTime);
				snd = Ringz.ar(
					in: Impulse.ar(0), // single impulse
					freq: XLine.ar(freq*2, freq, 0.1),
					decaytime: decay);
				env = Env.perc(att, rel, amp*1.2).kr(doneAction: 2);
				snd = (1.0 - dist) * snd + (dist * (snd.distort));
				snd = snd * env;
				snd = HPF.ar(snd, 21);
				snd = LPF.ar(snd, 21950);

				Out.ar(0, Pan2.ar(snd, pan));
			},
			metadata: (
				notes: "deep and boomy with sustained sub frequencies",
				credit: "unknown",
				category: \drums,
				tags: [\percussion, \kick, \808],
				readiness: 5/5
			)
			),

			SynthDef("kik3", {
				arg
				//Standard Values
				amp = 0.1, out = 0, pan = 0, freq = 66,
				//Amplitude Controls
				att = 0.01, dec = 0.1, decaylevel = 0.8, rel = 0.3, envCurve = -4,
				//Timbre Controls
				sweeptime = 0.08, sweepCurve = \exp, harmonic = 6;

				var snd, env, fenv;

				decaylevel = decaylevel * if (freq.log2 > 7, freq.log2.reciprocal, 1);


				env = Env.new(levels: [0, amp, decaylevel * amp, 0], times: [att, dec, rel] * 0.8, curve: envCurve).kr(doneAction: 2);

				fenv = Env.new(levels: [freq * harmonic, freq], times: [sweeptime/3], curve: sweepCurve).kr;

				snd = SinOsc.ar(freq: fenv, mul: amp).distort;

				snd = Normalizer.ar(in: snd, level: env)/3;
				snd = HPF.ar(snd, 21);
				snd = LPF.ar(snd, 21950);


				Out.ar(out, Pan2.ar(snd, pan));
			},
			metadata: (
				notes: "bubbly, reminds me of NES chip sounds",
				credit: "Author Unknown",
				category: \drums,
				tags: [\unpitched, \bass]
			)
			)
		]
	}

	*percs {|nPercs=1|
		^{|i|

			SynthDef(("fm_perc_" ++ i.asString).asSymbol, {|out = 0, freq = 100, amp=0.1, dur = 1, p = 0.5, cps = 1, pan = 0, sus = 1, cpc = 4, phrase = 8|

				var df = freq.log2;

				var atk = ExpRand(dur/48, dur/4);
				var rel = ExpRand(dur/4, dur*3/4);
				var modAmp = ExpRand(2.pow(df-1), 2.pow(df+1));
				var modHz = ExpRand(2.pow(df-1), 2.pow(df+1));

				var env = EnvGen.kr(Env.perc(atk, rel), doneAction: 2);
				var mod = SinOsc.ar(modHz, mul: modAmp);
				var car = SinOsc.ar(freq + mod) * env * amp/6;

				car = Pan2.ar(car, pan);
				Out.ar(out, car);

			});
		}.dup(nPercs)
		++ [
			SynthDef("kickBlocks", {
				arg
				//Standard Values
				out = 0, pan = 2, amp = 0.1, curve = -4,
				//tone1 arguments
				freq = 400, t1harmonic = 2, t1glide = 0.01,
				t1att = 0.0005, t1rel = 0.01, t1curve = -4, t1del = 0,
				//tone2 arguments
				t2harmonic = 3.44, t2glide = 0.01,
				t2att = 0.0001, t2rel = 0.3, t2curve = \lin, t2del = 0.005,
				//hit1 arguments
				h1harmonic = 8, h1glide = 0.01, h1rq = 0.6,
				h1att = 0.001, h1rel = 0.02, h1curve = -4, h1del = 0.001,
				//hit2 arguments
				h2harmonic = 1, h2glide = 0,
				h2att = 0.003, h2rel = 0.03, h2curve = -4, h2del = 0,
				//click arguments
				crq = 1;

				var snd, noise, tone1, hit1, hit2, click;

				noise = Hasher.ar(Sweep.ar, amp/2); //deterministic white noise

				tone1 = SinOsc.ar(
					freq: XLine.ar(start: freq * 8 * t1harmonic, end: freq * 2, dur: t1glide),
					mul: Env.perc(attackTime: t1att, releaseTime: t1rel, level: amp/2, curve: t1curve).delay(t1del).ar);


				hit1 = BPF.ar(
					in: noise,
					freq: XLine.ar(start: freq * 4 * h1harmonic, end: freq * 4, dur: h1glide),
					rq: h1rq,
					mul: Env.perc(attackTime: h1att, releaseTime: h1rel, level: amp/2, curve: h1curve).delay(h1del).ar);

				hit2 = HPF.ar(
					in: noise,
					freq: XLine.ar(start: freq * 132/40 * h2harmonic, end: freq * 132/40, dur: h2glide),
					mul: Env.perc(attackTime: h2att, releaseTime: h2rel, level: amp/4, curve: h2curve).delay(h2del).ar);

				click = BPF.ar(
					in: Impulse.ar(0) * SampleRate.ir / 48000,
					freq:  freq * 61/4,
					rq: crq,
					mul: amp*2/3);

				snd = Mix.ar(tone1 +  hit1 + hit2 + click).tanh * 1.15 * 3;
				snd = HPF.ar(snd, 21);
				snd = LPF.ar(snd, 21950);


				DetectSilence.ar(in: snd, doneAction: 2);

				Out.ar(out, Pan2.ar(snd, pan));
			},
			metadata: (
				notes: "burst of noise from tops to mids, not much for lows though. Kick a la hardstyle",
				credit: "originals by Nathan Ho aka snapizz",
				category: \drums,
				tags: [\bass],
				q: 4/5,
			)
			),

			SynthDef("kickRingz", {
				arg out = 0, pan = 0, freq = 40, amp = 0.1, decay = 0.25, ffreq = 1000;
				var snd;

				snd = Ringz.ar(
					in: LPF.ar(in: Impulse.ar(0), freq: ffreq),
					freq: freq,
					decaytime: decay,
					mul: amp
				);

				snd = snd.tanh.sin ;
				snd = HPF.ar(snd, 21);
				snd = LPF.ar(snd, 21950);


				DetectSilence.ar(in: snd, doneAction: 2);

				Out.ar(out, Pan2.ar(snd, pan));
			},
			metadata: (
				notes: "sounds like a tom drum or no-noise snare",
				credit: "Author Unknown",
				category: \drums,
				tags: [\unpitched, \bass],
				q: 1/5,
			)
			),
			SynthDef("clapOto309", {
				arg out = 0, freq = 600, amp = 0.1, pan = 0;
				var env1, env2, snd, noise1, noise2;

				var df = freq.log2;

				df  = if (df < 7, 3, { if(df > 11, -1, 0) });
				freq = freq * 2.pow(df);

				env1 = Env.new(
					levels: [0, 1, 0, 1, 0, 1, 0, 1, 0],
					times: [0.001, 0.013, 0, 0.01, 0, 0.01, 0, 0.03],
					curve: [0, -3, 0, -3, 0, -3, 0, -4]
				).ar;
				env2 = Env.new(
					levels: [0, 1, 0],
					times: [0.02, 0.3],
					curve: [0, -4]
				).ar(doneAction: 2);

				noise1 = WhiteNoise.ar(env1);
				noise1 = HPF.ar(noise1, freq);
				noise1 = BPF.ar(noise1, 10*freq/3, 3);

				noise2 = WhiteNoise.ar(env2);
				noise2 = HPF.ar(noise2, 5*freq/3);
				noise2 = BPF.ar(noise2, 2*freq, 0.7, 0.7);

				snd = noise1 + noise2;
				snd = snd * 2;
				snd = snd.softclip * amp/2;
				snd = HPF.ar(snd, 21);
				snd = LPF.ar(snd, 21950);


				Out.ar(out, Pan2.ar(snd, pan));
				DetectSilence.ar(snd, doneAction: 2);
			},
			metadata: (
				notes:" bright and punchy great clap",
				credit: "08091500Acid309 by otophilia",
				category: \drums,
				tags: [\clap, \percussion],
				q: 5/5
			)
			),
			SynthDef("purpleNoise", {
				arg out = 0, freq = 100, dur = 1, pan = 0, amp = 0.1, gate = 1, att = 0.01, rel = 0.75, curve = -6;

				var noise, a, b, c, d, e, f, g, h, i, env, snd;

				noise = WhiteNoise.ar(1);

				a = HPF.ar(noise, 62,    1/9);
				b = HPF.ar(noise, 125,   2/9);
				c = HPF.ar(noise, 250,   3/9);
				d = HPF.ar(noise, 500,   4/9);
				e = HPF.ar(noise, 1000,  5/9);
				f = HPF.ar(noise, 2000,  6/9);
				g = HPF.ar(noise, 4000,  7/9);
				h = HPF.ar(noise, 8000,  8/9);
				i = HPF.ar(noise, 16000, 9/9);

				env = EnvGen.kr(Env.pairs([ [0, 1], [3*dur/12, SinOsc.ar(ExpRand(freq/5, freq*5).range(0.25, 0.75))], [dur, 0] ], -2));

				snd = Mix([a, b, c, d, e, f, g, h, i] * amp) * env/6;

				snd = HPF.ar(snd, 21);
				snd = LPF.ar(snd, 21950);


				Out.ar(out, Pan2.ar(snd, pan));
				DetectSilence.ar(snd, doneAction: 2);
			},
			metadata: (
				notes: "standard noise snare",
				credit: "Josh Mitchell",
				category: \bass,
				tags: [\noise, \unpitched, \cymbal]
			)
			),
			SynthDef("snareElectro", {
				arg
				//Standard Values
				out = 0, pan = 0, amp = 0.1, att = 0.001, rel = 0.15, curve = -4,
				//Other Controls, blend ranges from 0 to 1
				freq = 160, sweep = 0.01,  rq = 1.6, dur = 1;

				var pop, popEnv, popSweep, noise, noiseEnv, snd;
				var snares = Array.fill(8, { 2.pow(IRand(8) * -1) }) * [-1, 1];

				var df = if (freq.log2 < 7, 0, { if (freq.log2 > 9, -2, -2 ) });
				freq = freq * 2.pow(df);

				freq = freq + (freq * snares).scramble;

				// pop makes a click coming from very high frequencies
				// slowing down a little and stopping in mid-to-low
				popSweep = Env.new(levels: [20.4, 2.6, 1] * freq, times: [dur / 2, dur], curve: \exp).ar;

				popEnv = Env.perc(attackTime: att, releaseTime: 0.33 * rel, level: 1, curve: curve).kr;

				pop = SinOsc.ar(freq: popSweep, mul: popEnv);


				// bandpass-filtered white noise
				noiseEnv = Env.perc(attackTime: att, releaseTime: rel * 2/3, level: 1, curve: -3).kr(doneAction: 2);


				noise = BPF.ar(in: WhiteNoise.ar, freq: [9, 11]*freq, rq: rq, mul: noiseEnv);

				snd = Splay.ar((pop + noise) * amp/6, 2/3);

				snd = HPF.ar(snd, 21);
				snd = LPF.ar(snd, 21950);


				Out.ar(out, Pan2.ar(snd, pan));

			},
			metadata: (
				notes: "sounds like a poppy electro snare. Has great sound over wide midirange",
				credit: "Nathan Ho aka Snappizz",
				category: \drum,
				tags: [\pitched],
				q: 5/5
			)
			)

		]
	}

	*hats {|nSynths = 2|
		^{|i|
			var m4Index = 100;
			var m3Ratio = [5,7,11,13,15].choose / [3,5,7,11].choose;
			var m3Index = 5 + 1.0.rand;
			var m2Ratio = [5,7,11,13,15].choose / [3,5,7,11].choose;
			var m2Index = 22 + 6.0.rand;
			var m1Ratio = 1.301 + 1.0.rand;
			var m1Index = 10 + 1.0.rand;
			var cRatio = 1.014;
			var rate1 = 23 + 70;
			var rate2 = 43 + 50;
			var rate3 = 10 + 101;
			var decay = 2.pow((1..8).choose.neg) * [2,3].choose;

			var fEnv4, fEnv3, fEnv2, fEnv1;
			var env, mod3, mod2, mod, car, sig;

			SynthDef(("fm_hats_" ++ i.asString).asSymbol, {|out = 0, freq = 100, amp=0.1, dur = 1, p = 0.5, cps = 1, pan = 0, sus = 1, cpc = 4, phrase = 8|
				var d = freq.log2;
				var df = if(d < 8, 3, { if (d > 11, -3, 0) });
				sus = sus + SinOsc.kr(cps.reciprocal, 2pi * p).range(0.5, 1.3);
				freq = freq * 2.pow(df);

				fEnv4 = m4Index * Pulse.ar(cps.reciprocal * dur/4);
				fEnv3 = m3Index * EnvGen.ar(Env.perc, timeScale: dur/2);
				fEnv2 = m2Index * EnvGen.ar(Env.perc, timeScale: dur*2/3) * SinOsc.ar(rate2);
				fEnv1 = m1Index * EnvGen.ar(Env.perc, timeScale: sus * dur) * SinOsc.ar(rate1);
				env = decay * Env.perc.ar(doneAction: 2);
				mod3 = SinOsc.ar(freq*m3Ratio, 0, mul: m3Ratio * fEnv3);
				mod2 = SinOsc.ar(freq*m2Ratio + mod3, 0, mul: m2Ratio * fEnv2);
				mod = SinOsc.ar(freq * m1Ratio + mod2, 2pi*p, freq * m1Ratio * fEnv1);
				car = SinOsc.ar(freq * cRatio + mod ) * env * amp * 1.5;

				sig = HPF.ar(car, freq*7);
				sig = LPF.ar(sig, 22000);
				sig = Limiter.ar(sig, amp/3);

				Out.ar(out, Pan2.ar(sig, pan));
			});
		}.dup(nSynths) ++
		[
			SynthDef("hihat1", {arg out = 0, amp = 0.5, att = 0.01, dur = 1,  freq = 6000, pan = 0;
				var rel = dur/6;
				var snd = WhiteNoise.ar(amp);
				var env = Env.perc(att, rel).kr(doneAction: 2);
				var vOffset = 5;

				freq = freq / 2.pow(vOffset);
				snd = HPF.ar(snd * env, freq)/7;

				snd = HPF.ar(snd, 21);
				snd = LPF.ar(snd, 21950);

				Out.ar(out, Pan2.ar(snd, pan));
			},
			metadata: (
				credit: "Bruno Tucunduva Ruviaro",
				category: \drums,
				tags: [\percussion, \hihat]
			)
			),
			SynthDef("hihatElectro", {
				arg out = 0, pan = 0, amp = 0.3, att = 0.001, dur = 1, curve = -8, freq = 4010, rq = 0.56;

				var env, snd;
				var rel = dur / 5;
				var vOffset = 2;

				freq = freq / 2.pow(vOffset);

				// noise -> resonance -> exponential dec envelope
				env = Env.perc(attackTime: att, releaseTime: rel, curve: curve).kr(doneAction: 2);

				snd = ClipNoise.ar(amp);
				snd = BPF.ar(
					in: snd,
					freq: [1, 1.035] * freq,
					rq: [0.27, 1] * rq,
					mul: [1.0, 0.6]
				);
				snd = Mix(snd) * env/7.5;
				snd = HPF.ar(snd, 21);
				snd = LPF.ar(snd, 21950);


				Out.ar(out, Pan2.ar(snd, pan));
			},
			metadata: (
				credit: "By Nathan Ho aka Snappizz",
				category: \drums,
				tags: [\clap, \percussion, \hihat]
			)
			),

		]

	}



	*synthParams {
		^{arg freq=100, amp=0.1, pan = 0, dur=4, sus = 1, t=0, p = 0.5, phrase = 8, cpc = 4;
		}
	}

	*guitars {
		^[
			SynthDef(\modalElectricGuitar, {
				arg
				// Standard values
				out = 0, pan = 0, freq = 440, amp = 0.1, rel = 5,
				// String controls (pickPos goes from 0 to 1)
				decayCoef = 0.125, dampCoef = 0.0002, pickPos = 0.414, openFreq = 82.5, muteSus = 5.5,
				// Pickup Controls (pickupPos goes from 0 to 1)
				pickupPos = 0.17, pickupWidth = 0.75, resFreq = 4000, rq = 0.5, toneFreq = 3250,
				dur=4, sus = 1, t=0, p = 0.5, phrase = 8, cpc = 4;

				var exciter, freqArray, ampArray, decArray, constant, mute, snd;

				// Make a Constant from pickupWidth for ampArray
				constant = pickupWidth/25.5; // The scale length is set to 25.5 inches
				constant = constant * pi/2;
				constant = constant/openFreq;

				// Stiff String Model for Frequencies
				freqArray = Array.fill(50, {
					arg i;
					(i + 1) * sqrt(1 + ((i + 1).pow(2) * 0.00001))
				});
				freqArray = freqArray/freqArray[0];

				// Decay Times
				decArray = Array.fill(50, {
					arg i;
					exp(
						(-1 * i)/
						(
							(1/decayCoef) +
							((dampCoef/10) * freq.pow(2)) +
							(dampCoef * freqArray[i].pow(2))
						)
					)
				});
				decArray = decArray/decArray[0];

				// Rescale freqArray for ampArray and Klank
				freqArray = freqArray * freq;

				// Effects of Pick Position and Pickup Placement
				ampArray = Array.fill(50, {
					arg i;
					((1 - ((freqArray[i] - 21950)/1000).tanh)/2) *
					sin(((i + 1) * pi) * pickPos) *
					(
						sin(pi * pickupPos * freqArray[i]/openFreq) *
						(
							(
								sin(constant * freqArray[i])/
								(constant * freqArray[i])
							) - cos(constant * freqArray[i])
						)
					)/(freqArray[i].pow(2))
				});
				ampArray = ampArray * 2/(constant.pow(2));

				// The Pick
				exciter = Impulse.ar(0) * 0.1;

				// The String
				snd = Klank.ar(
					specificationsArrayRef:
					Ref.new([freqArray, ampArray, decArray]),
					input: exciter,
					decayscale: rel
				);

				snd = Mix.ar(snd);

				// The Pickup
				snd = RLPF.ar(
					in: snd,
					freq: resFreq,
					rq: rq);

				snd = LPF.ar(
					in: snd,
					freq: toneFreq);

				// An Envelope for Muting the String
				mute = Env.new(
					levels: [1, 1, 0, 0],
					times: [sus*muteSus, 0.05, 0.01]).ar(doneAction: 2);

				// Mute the String
				snd = LPF.ar(
					in: snd,
					freq: LinExp.ar(
						in: mute,
						srclo: 0, srchi: 1,
						dstlo: 20, dsthi: 20000));

				// Output Stuff
				snd = snd * amp;
				snd = Limiter.ar(snd);



				DetectSilence.ar(in: snd, doneAction: 2);

				Out.ar(out, Pan2.ar(snd, pan));
			},
			metadata: (
				credit: "by Josh Mitchell",
				category: \guitar,
				tags: [\pitched, \modal]
			)
			)
		]
	}

	*pianos {
		^[
			SynthDef(\otey_piano, { |out=0, freq=440,gate=1, amp=0.1,rho=1, pan = 0, dur=4, sus = 1, t=0, p = 0.5, phrase = 8, cpc = 4|
				var env =  EnvGen.ar(Env.asr(0,1,0.1),gate);

				var sig = OteyPiano.ar(freq, rho:rho);

				sig = amp * sig * env/3;
				Out.ar(out, Pan2.ar(sig, pan));
				DetectSilence.ar(sig, 0.01, doneAction:2);
			}),

			SynthDef(\mda_piano, { |out=0, freq=440,gate=1, amp=0.1,rho=1, pan = 0, dur=4, sus = 1, t=0, p = 0.5, phrase = 8, cpc = 4|
				var sig = amp * MdaPiano.ar(freq, gate, release: sus * 0.9, stereo: 1 - p, sustain: 0);
				DetectSilence.ar(sig, 0.01, doneAction:2);
				Out.ar(out, sig);

			}),

			SynthDef(\mix_piano, { |out=0, freq=440,gate=1, amp=0.1,rho=1, pan = 0, dur=4, sus = 1, t=0, p = 0.5, phrase = 8, cpc = 4|
				var piano1 = p * EnvGen.ar(Env.asr(0,1,0.1),gate) * OteyPiano.ar(freq, rho:rho) /3;
				var piano2 = (1-p) * MdaPiano.ar(freq, gate, release: sus * 0.9, stereo: 1 - p, sustain: 0);
				var sig = amp * (piano1 + piano2);

				Out.ar(out, Pan2.ar(sig, pan));
				DetectSilence.ar(sig, 0.01, doneAction:2);

			})
		];
	}

	*get {|role|
		var synth = switch(role,
			\kick, { GrabBag.kicks.choose },
			\perc, { GrabBag.percs.choose },
			\hats, { GrabBag.hats.choose },
			nil
		);

		if (synth.isNil, { Io.logErr(["Asked to GrabBag.get a synth for role ", role, " but it did not exist"]) })

		^synth;
	}
}
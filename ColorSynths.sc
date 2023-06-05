ColorSynths {
	*descrption {
		^"Static synthdefs.";
	}

	*source {
		^SynthDef(\source, {|out, bufnum, amp = 1|
			var args = [out, bufnum, amp];
			var sig = amp * PlayBuf.ar(2, bufnum, BufRateScale.kr(bufnum));
			Out.ar(out, sig);
		})
	}

	*peak_limiter {
		^SynthDef(\peak_limiter, {|in, out, mix|
			var result;
			var max = 0.92;
			var dry = In.ar(in, 2);
			var wet = Limiter.ar(dry, max, 0.005);

			result = ColorDo.mix(dry, wet, mix);
			Out.ar(out, result);
		})
	}

	*all {
		^[
			ColorSynths.peak_limiter,
			ColorSynths.delay_1,
			ColorSynths.delay_2,
			ColorSynths.reverb,
			ColorSynths.source,
			ColorSynths.mixer,
			// these seem a bit buggy
			// ColorSynths.limit,
			// ColorSynths.boost,
			ColorSynths.compressor,
			ColorSynths.booster,
			ColorSynths.crunch,
			ColorSynths.phasor,
			ColorSynths.hpf,
			ColorSynths.lpf,
		]
	}

	*delay_1 {
		^SynthDef(\delay_1, {|in, out, mix, dt, decay, fb, hpf, lpf|
			// var args = [in, out, mix, dt, decay, fb, hpf, lpf].poll(0.1);
			var pool = LocalIn.ar(numChannels: 1);
			var result;
			var dry = In.ar(in, 2);
			var wet = AllpassL.ar(dry, dt * decay, dt, decay);

			/*
			SynthDef.wrap(ColorWrap.pool(sig, fb, 1)
			*/

			wet = ColorDo.filters(wet, hpf, lpf);
			result = ColorDo.mix(dry, wet, mix);
			pool = pool + (fb * wet);
			LocalOut.ar(pool);
			Out.ar(out, result);
		})
	}

	*delay_2 {
		^SynthDef(\delay_2, {|in, out, mix, dt, decay, fb, hpf, lpf|
			var pool = LocalIn.ar(numChannels: 2);
			var result;
			var dry = In.ar(in, 2);

			var wet = AllpassL.ar(dry, dt * decay, dt, decay);

			wet = ColorDo.filters(wet, hpf, lpf);
			result = ColorDo.mix(dry, wet, mix);
			pool = pool + (fb * wet);
			LocalOut.ar(pool);
			Out.ar(out, result);
		})
	}

	*reverb {
		^SynthDef(\reverb, {|in, out, mix, decay, fb, hpf, lpf|
			// var args = [in,out,mix].poll(0.1);
			var pool = LocalIn.ar(numChannels: 2);
			var dry = In.ar(in, 2);
			var result;

			var wet = FreeVerb2.ar(dry, dry, 1, decay.sqrt);
			wet = Mix.ar([dry, dry]);
			wet = ColorDo.filters(wet, hpf, lpf);

			result = ((1-mix) * dry) + (mix * wet);

			pool = pool + (fb * wet);
			Out.ar(out, result);
		})
	}

	*compressor {
		^SynthDef(\compressor, {|in, out, mix, level = 7, atk = 0.03, rel = 0.12, ratio = 1.5|
			var wet, result;
			var dry = In.ar(in, 2);
			var maxAmp = ((11 - level) * 10.neg).dbamp;

			wet = CompanderD.ar(dry, maxAmp, slopeAbove: 1/ratio, clampTime: atk, relaxTime: rel);
			result = ColorDo.mix(dry, wet, mix);
			Out.ar(out, result);
		});
	}

	*booster {
		^SynthDef(\booster, {|in, out, mix, level = 7, atk = 0.03, rel = 0.12, ratio = 1.5|
			var wet, result;
			var dry = In.ar(in, 2);
			var minAmp = ((11 - level) * 10.neg).dbamp;

			wet = CompanderD.ar(dry, minAmp, slopeBelow: 1/ratio, clampTime: atk, relaxTime: rel);
			result = ColorDo.mix(dry, wet, mix);
			Out.ar(out, result);
		});
	}

	*compressorcustom {
		^SynthDef(\compressor, {|in, out, mix, level, atk, rel,ratio|
			var wet, result;
			var dry = In.ar(in, 2);
			var amplitudeDb, gainDb;
			var amp = Amplitude.ar(dry, atk, rel).ampdb;
			var a;
			/*
a=[1,3,10,30,100,300,1000,3000,10000,30000,100000]
a.size
a.collect(_.ampdb)
a.collect(_.reciprocal.ampdb)

			gainDb = ((amp - thresh) * (1 / ratio - 1)).min(0);
			wet = dry * gainDb.dbamp;
			result = ColorDo.mix(dry, wet, mix);
			Out.ar(out, result);
			*/
		});
	}

	*crunch {
		^SynthDef(\compressor_crunch, {|in, out, mix, thresh, gain|
			var max = 0.9;
			var dry = In.ar(in, 2);
			var wet = Limiter.ar(gain * Limiter.ar(dry, thresh), max);

			var result = ColorDo.mix(dry, wet, mix);
			Out.ar(out, result);
		});
	}
	/*

	*boost {
		^SynthDef(\boost, {|in, out, mix, thresh, ratio, atk, rel|
			var dry = In.ar(in, 2);
			var wet = CompanderD.ar(dry, thresh, slopeBelow: thresh, clampTime: atk, relaxTime: rel);
			var result = Color.mix(dry, wet, mix);
			Out.ar(out, result);
		})
	}

	*limit {
		^SynthDef(\limit, {|in, out, mix, thresh, ratio, atk, rel|
			var dry = In.ar(in, 2);
			var wet = CompanderD.ar(dry, thresh, slopeAbove: thresh, clampTime: atk, relaxTime: rel);
			var result = Color.mix(dry, wet, mix);
			Out.ar(out, result);
		})
	}
*/
	*phasor {
		^SynthDef(\phasor, {|in, out, mix, rate, fb|
			var dry = In.ar(in, 2);
			var lfo = [LFTri, LFSaw, SinOsc].choose.ar(rate);
			var wet = AnalogPhaser.ar(dry, lfo, feedback: fb);
			var result = ColorDo.mix(dry, wet, mix);
			Out.ar(out, result);
		});
	}

	*hpf {
		^SynthDef(\hpf, {|in, out, mix, freq, q|
			var dry = In.ar(in, 2);
			var wet = HPF.ar(dry, freq);
			var result = ColorDo.mix(dry, wet, mix);
			Out.ar(out, result);
		});
	}

	*lpf {
		^SynthDef(\lpf, {|in, out, mix, freq, q|
			var dry = In.ar(in, 2);
			var wet = LPF.ar(dry, freq);
			var result = ColorDo.mix(dry, wet, mix);
			Out.ar(out, result);
		});
	}

	/* the `mix` control describes the bias of dry/wet. So mix=0 is only a, and mix=1 is only b. */
	*mixer {
		^SynthDef(\mixer, {|out, a, b, mix|
			var args = [out, a, b, mix];
			var in1 = In.ar(a, 2);
			var in2 = In.ar(b, 2);
			var result = ((1-mix) * in1) + (mix * in2);
			Out.ar(out, result);
		})
	}
}
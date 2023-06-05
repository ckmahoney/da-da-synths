Acoustic {
	*guitar {|label|
		^SynthDef("g_"++label, {
			arg
			// Standard values
			out = 0, pan = 0, freq = 440, amp = 0.1, rel = 5,
			// String controls (pickPos goes from 0 to 1)
			decayCoef = 0.125, dampCoef = 0.0002, pickPos = 0.414, openFreq = 82.5, muteSus = 5.5,
			// Pickup Controls (pickupPos goes from 0 to 1)
			pickupPos = 0.17, pickupWidth = 0.75, resFreq = 4000, rq = 0.5, toneFreq = 3250,
			dur=4, sus = 1, t=0, p = 0.5, phrase = 8, cpc = 4;

			var exciter, freqArray, ampArray, decArray, constant, mute, snd;

			var nThings = 20;

			// Make a Constant from pickupWidth for ampArray
			constant = pickupWidth/25.5; // The scale length is set to 25.5 inches
			constant = constant * pi/2;
			constant = constant/openFreq;

			// Stiff String Model for Frequencies
			freqArray = Array.fill(nThings, {
				arg i;
				(i + 1) * sqrt(1 + ((i + 1).pow(2) * 0.00001))
			});
			freqArray = freqArray/freqArray[0];

			// Decay Times
			decArray = Array.fill(nThings, {
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
			ampArray = Array.fill(nThings, {
				arg i;
				((1 - ((freqArray[i] - 19000)/1000).tanh)/2) *
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
			snd = snd * amp/1.5;
			snd = Limiter.ar(snd);

			DetectSilence.ar(in: snd, doneAction: 2);

			Out.ar(out, Pan2.ar(snd, pan));
		})

	}

	*piano1 {|label|
		var da = 1/9;
		^SynthDef("p1_"++label, { |out=0, freq=440,gate=1, amp=0.1,rho=1, pan = 0, dur=4, sus = 1, t=0, p = 0.5, phrase = 8, cpc = 4|
			var env =  EnvGen.ar(Env.asr(0,1,0.1),gate);

			var sig = OteyPiano.ar(freq, rho:rho);

			sig = amp * da * sig * env;
			Out.ar(out, Pan2.ar(sig, pan));
			DetectSilence.ar(sig, 0.01, doneAction:2);
		})
	}

	/* This piano has ET-12 built into it. All frequency values are rounded to the nearest twelve-tone vlaue.
	This instrument sounds amazing but I can not use it like this.
	*/
/*
	*piano2 {|label|
					var tuneForHardcodedPiano = 0.515;

		^SynthDef("p2_"++label, { |out=0, freq=440,gate=1, amp=0.1,rho=1, pan = 0, dur=4, sus = 1, t=0, p = 0.5, phrase = 8, cpc = 4, tune = 0.5|
			var sig = amp * MdaPiano.ar(freq, gate, release: sus * 0.9, stereo: 1 - p, sustain: 0, tune: tune);
			DetectSilence.ar(sig, 0.01, doneAction:2);
			Out.ar(out, sig);

		})
	}

	*pianoMix {|label|
					var tuneForHardcodedPiano = 0.515;

		^SynthDef("p3_"++label, { |out=0, freq=440,gate=1, amp=0.1,rho=1, pan = 0, dur=4, sus = 1, t=0, p = 0.5, phrase = 8, cpc = 4|

			var piano1 = p * EnvGen.ar(Env.asr(0,1,0.1),gate) * OteyPiano.ar(freq, rho:rho) /3;
			var piano2 = (1-p) * MdaPiano.ar(freq, gate, release: sus * 0.9, stereo: 1 - p, sustain: 0, tune: tuneForHardcodedPiano);
			var sig = amp * (piano1 + piano2);

			Out.ar(out, Pan2.ar(sig, pan));
			DetectSilence.ar(sig, 0.01, doneAction:2);

		})
		}
*/
	}
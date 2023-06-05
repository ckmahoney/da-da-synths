SynthDefaults {

	//\\##//\\##  SynthonizedDefaults   //\\##//\\##//\\


	/*
	- A Collection of Tradicional SynthDefs-

	Organized by Zé Craum

	This is the first scratch, ASAP it will be avaible in a more organized form.

	//Some Guidelines from uploading contributiong or helping organizing existing stuff:
	1.Executing code should return a SynthDef;
	2.The SynthDEFaults should have ControlSpecs for every parameter to define an acceptable range;
	3.The SynthDEFaults should not depend on things outside of the core lib or sc3-plugins;
	4.The SynthDEFaults should have reasonable defaults that allow it to make a representative sound with no arguments;
	5. SynthDEFaults symbol should be always be like \Piano_BabyGrandPiano; \Guitar_FDBEletric...
	6. SynthDEFaults must be written using NamedControl Style (Still being updated);
	6.1. Additional arguments besides from amp/dur/pitch default events must be written as local variables and must have comments;
	7. All the SynthDef should be designed for working with the main scope of Patterns

	Examples at: http://sccode.org/1-5aE

	//

	*/

	classvar <>loaded = false;

	*kick_1 {
		^SynthDef(\kick_1, {|amp = 0.1, freq = 100|
			var snd;
			snd = DC.ar(0);
			snd = snd + (SinOsc.ar(XLine.ar(freq * 15, freq * 8, 0.01)) * Env.perc(0.0005, 0.01, curve: \lin).ar);
			snd = snd + (BPF.ar(Impulse.ar(0) * SampleRate.ir / 48000, freq, 1.0) * 3.dbamp);
			snd = snd + (BPF.ar(Hasher.ar(Sweep.ar), freq * [2/3, 3/2], 0.9) * Env.perc(0.001, 0.02).ar);
			snd = snd + (SinOsc.ar(XLine.ar(freq * 4, freq / 2, 0.045)) * Env.perc(0.0001, 0.3, curve: \lin).delay(0.005).ar(2));
			snd = amp * snd.tanh;
			Out.ar(\out.kr(0), Pan2.ar(snd, \pan.kr(0)));
		});
	}

	*snare_1 {
		^SynthDef(\snare_1,{|out=0, velocity = 1, amp = 0.1|
			var excitation, sig;

			excitation =  amp * LPF.ar(WhiteNoise.ar(1), 7040, 1) * (0.1 + velocity);
			sig = amp * (
				/* Two simple enveloped oscillators represent the loudest resonances of the drum membranes */
				(LFTri.ar(330,0,1) * EnvGen.ar(Env.perc(0.0005,0.055),doneAction:0) * 0.25)
				+(LFTri.ar(185,0,1) * EnvGen.ar(Env.perc(0.0005,0.075),doneAction:0) * 0.25)

				/* Filtered white noise represents the snare */
				+(excitation * EnvGen.ar(Env.perc(0.0005,0.4),doneAction:2) * 0.2)
				+(HPF.ar(excitation, 523, 1) * EnvGen.ar(Env.perc(0.0005,0.283),doneAction:0) * 0.2)

			);

			Out.ar(out, sig!2)
		})
	}

	*hats_1 {
		^SynthDef(\hats_1, {arg out = 0, amp = 0.5, att = 0.01, rel = 0.2, ffreq = 6000, pan = 0;
			var env, snd;
			env = Env.perc(att, rel, amp).kr(doneAction: 2);
			snd = WhiteNoise.ar;
			snd = HPF.ar(in: snd, freq: ffreq, mul: env);
			Out.ar(out, Pan2.ar(snd, pan));
		})
	}

	*organ_1 {
		^SynthDef(\organ_1,{|out= 0 freq = 440 amp = 0.1 gate=1 lforate = 4.85 lfowidth= 0.5 cutoff= 2000 rq=0.5 pan = 0.0|

			//Electric Piano
			var pulse, filter, env;

			pulse = Pulse.ar(freq*[1,33.5.midiratio],[0.2,0.1],[0.7,0.3]);

			env = EnvGen.ar(Env.adsr(0.0,1.0,0.8,3.0),gate,doneAction:2);

			//keyboard tracking filter cutoff
			filter = BLowPass4.ar(pulse,(cutoff*(env.squared))+200+freq,rq);

			Out.ar(out,Pan2.ar(Mix(filter)*env*amp,pan));

		})
	}

	*organ_2 {
		^SynthDef(\organ_2, {

			arg freq = 440, amp = 0.7, atk = 0.001, sus = 0.2, rel = 0.01, pan = 0,
			bass = 1, quint = 1, fundamental = 1, oct = 1, nazard = 1, blockFlute = 1, tierce = 1, larigot = 1, sifflute = 1, //organ voices (drawbars) amplitudes
			vrate = 3, vdepth = 0.008, vdelay = 0.1, vonset = 0, vrateVariation = 0.1, vdepthVariation = 0.1; //vibrato arguments
			var sig, env, vibrato;

			vibrato = Vibrato.kr(DC.kr(freq), DC.kr(vrate), DC.kr(vdepth), DC.kr(vdelay), DC.kr(vonset), DC.kr(vrateVariation), DC.kr(vdepthVariation));
			env = EnvGen.ar(Env.linen(atk, sus, rel), doneAction: Done.freeSelf);
			sig = DynKlang.ar(`[[1/4,  1/3, 1/2, 2, 3, 4, 5, 6, 10], ([DC.ar(bass) , DC.ar(quint), DC.ar(fundamental), DC.ar(oct), DC.ar(nazard), DC.ar(blockFlute), DC.ar(tierce), DC.ar(larigot), DC.ar(sifflute)].normalizeSum), nil], vibrato);

			sig = sig * env;
			Out.ar(0, Pan2.ar(sig, pan, amp));

		});
	}

	*organ_3 {
		^SynthDef(\organ_3, {
			//subtractive tonewheel organ with cheap CPU usage
			arg freq = 440, amp = 0.9, atk = 0.001, sus = 0.4, rel = 0.1, tune = 0.29, tuneRate = 3.0, rq = 1;
			var sig, env, vibrato;
			env = EnvGen.ar(Env.linen(atk, sus, rel, 0.5), doneAction: Done.freeSelf);
			vibrato = SinOsc.ar(tuneRate).range(freq, freq * (tune.midiratio));
			sig = LFPulse.ar(freq, 0, 0.5, 0.2) + LFPulse.ar(freq + vibrato, 0, 0.18);
			sig = RLPF.ar(sig + PinkNoise.ar(20/freq), 5 * freq, rq) ;

			sig = sig * env;
			sig = sig * amp;
			sig = LeakDC.ar(sig);
			Out.ar(0, sig!2);

		});
	}

	*organ_4 {
		^SynthDef(\organ_4, {
			//subtractive tonewheel organ with more CPU usage
			arg freq = 440, amp = 0.9, atk = 0.001, sus = 0.4, rel = 0.1, vrate = 6, vdepth = 0.02, vdelay = 0.1, vonset = 0, vrateVariation = 0.1, vdepthVariation = 0.1, rq =1;
			var sig, env, vibrato;
			env = EnvGen.ar(Env.linen(atk, sus, rel, 0.5), doneAction: Done.freeSelf);
			// vibrato = Vibrato.kr(DC.kr(freq), DC.kr(vrate), DC.kr(vdepth), DC.kr(vdelay), DC.kr(vonset), DC.kr(vrateVariation), DC.kr(vdepthVariation));
			sig = LFPulse.ar(freq, 0, 0.5, 0.2) + LFPulse.ar(freq, 0, 0.18);
			sig = BLowPass4.ar(sig, freq*5, rq);

			sig = sig * env;
			sig = sig * amp;
			sig = LeakDC.ar(sig);
			Out.ar(0, sig!2);

		});
	}

	*organ_5 {
		^SynthDef(\organ_5,{|out= 0 freq = 440 amp = 0.1 gate=1 lforate = 4.85 lfowidth= 0.1 cutoff= 5000 rq=0.25 pan = 0.0, bpf = 600, cps = 1, cpc = 4|
			//Subtractive tonewheel organ from Steal this Sound example
			var lfo, pulse, filter, env;
			var df = 300;
			var sweep = SinOsc.kr((cps/4)/cpc).range(df.half.neg, df.half);

			lfo = LFTri.kr(lforate*[1,1.01],Rand(0,2.0)!2);

			pulse = Pulse.ar( (((freq*[1,3]).cpsmidi) + (lfo*lfowidth)).midicps,[0.5,0.51],[0.4,0.6]);

			env = EnvGen.ar(Env.adsr(0.0,0.0,1.0,0.1),gate,doneAction:2);

			filter = BLowPass4.ar(pulse,cutoff,rq);

			filter= BPeakEQ.ar(filter, bpf + sweep, 1.0,3);

			Out.ar(out,Pan2.ar(Mix(filter)*env*amp,pan));
		});
	}

	*organ_6 {
		^SynthDef(\organ_6, {|out = 0, freq = 100, amp = 0.1, dur = 1, cps = 1, sus = 0.85|
			var sig = 0;
			var partials = [
				[0.5, 0.2],
				[1, 1],
				[1.5, 0.4],
				[2, 0.3],
				[3, 0.2],
				[5, 0.2],
				[15, 0.05],
				[16, 0.02],
				[24, 0.01],
			];

			var atk = EnvGen.ar(Env.new([0.7, 1, 0], [dur * 2.pow(-6), dur]/cps), timeScale: sus);
			var rel = EnvGen.ar(Env.new([1, 1, 0], [dur * sus, dur * (1-sus)]/cps), timeScale: sus);

			partials.do({|stop|
				var mul = stop[0], vol = stop[1];
				var f = freq * mul;
				sig = sig + SinOsc.ar(f, mul: amp * vol * partials.size.reciprocal)
			});
			// sig = atk * rel * sig;

			sig = Pan2.ar(sig, SinOsc.ar(cps/4));

			Out.ar(out, sig);
			DetectSilence.ar(sig, doneAction: 2);
		}
		);
	}



	*flute_1 {
		^SynthDef(\flute_1, { arg scl = 0.2, freq = 440, ipress = 0.9, ibreath = 0.09, ifeedbk1 = 0.4, ifeedbk2 = 0.4, dur = 1, gate = 1, amp = 0.4;

			var kenv1, kenv2, kenvibr, kvibr, sr, cr, block;
			var poly, signalOut, ifqc;
			var aflow1, asum1, asum2, afqc, atemp1, ax, apoly, asum3, avalue, atemp2, aflute1;
			var fdbckArray;

			sr = SampleRate.ir;
			cr = ControlRate.ir;
			block = cr.reciprocal;

			ifqc = freq;

			// noise envelope
			kenv1 = EnvGen.kr(Env.new(
				[ 0.0, 1.1 * ipress, ipress, ipress, 0.0 ], [ 0.06, 0.2, dur - 0.46, 0.2 ], 'linear' )
			);
			// overall envelope
			kenv2 = EnvGen.kr(Env.new(
				[ 0.0, amp, amp, 0.0 ], [ 0.1, dur - 0.02, 0.1 ], 'linear' ), doneAction: 2
			);
			// vibrato envelope
			kenvibr = EnvGen.kr(Env.new( [ 0.0, 0.0, 1, 1, 0.0 ], [ 0.5, 0.5, dur - 1.5, 0.5 ], 'linear') );

			// create air flow and vibrato
			aflow1 = LFClipNoise.ar( sr, kenv1 );
			kvibr = SinOsc.ar( 5, 0, 0.1 * kenvibr );

			asum1 = ( ibreath * aflow1 ) + kenv1 + kvibr;
			afqc = ifqc.reciprocal - ( asum1/20000 ) - ( 9/sr ) + ( ifqc/12000000 ) - block;

			fdbckArray = LocalIn.ar( 1 );

			aflute1 = fdbckArray;
			asum2 = asum1 + ( aflute1 * ifeedbk1 );

			//ax = DelayL.ar( asum2, ifqc.reciprocal * 0.5, afqc * 0.5 );
			ax = DelayC.ar( asum2, ifqc.reciprocal - block * 0.5, afqc * 0.5 - ( asum1/ifqc/cr ) + 0.001 );

			apoly = ax - ( ax.cubed );
			asum3 = apoly + ( aflute1 * ifeedbk2 );
			avalue = LPF.ar( asum3, 2000 );

			aflute1 = DelayC.ar( avalue, ifqc.reciprocal - block, afqc );

			fdbckArray = [ aflute1 ];

			LocalOut.ar( fdbckArray );

			signalOut = avalue;

			OffsetOut.ar( 0, 2 * [ signalOut * kenv2, signalOut * kenv2 ] );
		})
	}



	*getSome {
		^[
			this.organ_1,
			this.organ_2,
			this.organ_4,
			this.organ_5,
			this.organ_6
		];
	}

	*loadSynths {

		//  //  //  //  //  //  //  //  //
		////     Electric Pianos      ////
		//  //  //  //  //  //  //  //  //


		SynthDef(\piano_2, {
			//FM Rhodes Synthethizer
			|
			// standard meanings
			out = 0, freq = 440, gate = 1, pan = 0, amp = 0.1,
			// all of these range from 0 to 1
			vel = 0.8, modIndex = 0.2, mix = 0.2, lfoSpeed = 0.4, lfoDepth = 0.1
			|
			var env1, env2, env3, env4;
			var osc1, osc2, osc3, osc4, snd;

			lfoSpeed = lfoSpeed * 12;

			freq = freq * 2;

			env1 = EnvGen.ar(Env.adsr(0.001, 1.25, 0.0, 0.04, curve: \lin));
			env2 = EnvGen.ar(Env.adsr(0.001, 1.00, 0.0, 0.04, curve: \lin));
			env3 = EnvGen.ar(Env.adsr(0.001, 1.50, 0.0, 0.04, curve: \lin));
			env4 = EnvGen.ar(Env.adsr(0.001, 1.50, 0.0, 0.04, curve: \lin));

			osc4 = SinOsc.ar(freq * 0.5) * 2pi * 2 * 0.535887 * modIndex * env4 * vel;
			osc3 = SinOsc.ar(freq, osc4) * env3 * vel;
			osc2 = SinOsc.ar(freq * 15) * 2pi * 0.108819 * env2 * vel;
			osc1 = SinOsc.ar(freq, osc2) * env1 * vel;
			snd = Mix((osc3 * (1 - mix)) + (osc1 * mix));
			snd = snd * (SinOsc.ar(lfoSpeed) * lfoDepth + 1);

			// using the doneAction: 2 on the other envs can create clicks (bc of the linear curve maybe?)
			snd = snd * EnvGen.ar(Env.asr(0, 1, 0.1), gate, doneAction: 2);
			snd = Pan2.ar(snd, pan, amp);

			Out.ar(out, snd);
		}).add;


		SynthDef(\organ_1,{|out= 0 freq = 440 amp = 0.1 gate=1 lforate = 4.85 lfowidth= 0.5 cutoff= 2000 rq=0.5 pan = 0.0|

			//Electric Piano
			var pulse, filter, env;

			pulse = Pulse.ar(freq*[1,33.5.midiratio],[0.2,0.1],[0.7,0.3]);

			env = EnvGen.ar(Env.adsr(0.0,1.0,0.8,3.0),gate,doneAction:2);

			//keyboard tracking filter cutoff
			filter = BLowPass4.ar(pulse,(cutoff*(env.squared))+200+freq,rq);

			Out.ar(out,Pan2.ar(Mix(filter)*env*amp,pan));

		}).add;


		//  //  //  //  //  //  //  //
		////    Harpsichord     ////
		//  //  //  //  //  //  //  //

		SynthDef(\piano_5, { arg out = 0, freq = 440, amp = 0.1, pan = 0;
			var env, snd;
			env = Env.perc.kr(doneAction: 2);
			snd = Pulse.ar(freq, 0.25, 0.75);
			snd = amp * snd * env;
			Out.ar(out, Pan2.ar(snd, pan));
		}).add;

		SynthDef(\piano_4, {
			|amp=0.1, freq=440, pan=0, atk=0, rel=0, trig= 1, maxdelaytime= 0.2, decaytime= 7, coef= 0.1|
			var env, sig, delay;
			env = EnvGen.kr(Env.linen(atk, decaytime, rel), doneAction: Done.freeSelf);
			sig = PinkNoise.ar(amp); //Can use white noise here, but Pink is more realistic
			delay = freq.reciprocal;
			sig = Pluck.ar(sig, trig, maxdelaytime , delay , decaytime , coef ) //fundamental
			+ Pluck.ar(sig, trig, maxdelaytime , delay/2 , decaytime , coef ); //octave higher
			Out.ar(0, Pan2.ar(sig , pan));

		}).add;

		// organs

		this.organ_2.add;
		this.organ_3.add;
		this.organ_4.add;
		this.organ_5.add;
		this.organ_6.add;

		// winds

		this.flute_1.add;

		// strings

		SynthDef(\strings_1, { arg out, freq=440, amp=0.8, gate=1, pan, freqLag=0.2;
			var env, in, delay, f1, f2;
			f1 = freq.lag(freqLag);
			f2 = freq.lag(freqLag * 0.5);
			delay = 0.25 / f2;
			env = Env.asr(0, 1, 0.3);
			in = WhiteNoise.ar(180);
			in = CombL.ar(in, delay, delay, 1);
			in = Resonz.ar(in, f1, 0.001).abs;
			in = in * EnvGen.kr(env, gate, doneAction:2);
			Out.ar(out, Pan2.ar(in, pan, amp));

		}).add;


		SynthDef(\strings_2, {
			| freq=400, gate=1, amp=0.8 |
			var env = EnvGen.kr(Env.asr(0.1, 1, 0.1), gate, doneAction:2);
			var sig = VarSaw.ar(
				freq,
				width:LFNoise2.kr(1).range(0.2, 0.8)*SinOsc.kr(5, Rand(0.0, 1.0)).range(0.7,0.8))*0.25;
			sig = sig * env * amp;
			Out.ar(0, sig!2);

		}).add;

		//  //  //  //  //  //  //  //
		////     Percussion       ////
		//  //  //  //  //  //  //  //


		// kalimba
		SynthDef(\piano_3, {
			//Kalimba based on bank of ressonators
			|out = 0, freq = 440, amp = 0.1, mix = 0.1, relMin = 2.5, relMax = 3.5|
			var snd;
			// Basic tone is a SinOsc
			snd = SinOsc.ar(freq) * EnvGen.ar(Env.perc(0.005, Rand(relMin, relMax), 1, -8), doneAction: 2);
			// The "clicking" sounds are modeled with a bank of resonators excited by enveloped pink noise
			snd = (snd * (1 - mix)) + (DynKlank.ar(`[
				// the resonant frequencies are randomized a little to add variation
				// there are two high resonant freqs and one quiet "bass" freq to give it some depth
				[240*ExpRand(0.9, 1.1), 2020*ExpRand(0.9, 1.1), 3151*ExpRand(0.9, 1.1)],
				[-7, 0, 3].dbamp,
				[0.8, 0.05, 0.07]
			], PinkNoise.ar * EnvGen.ar(Env.perc(0.001, 0.01))) * mix);
			Out.ar(out, Pan2.ar(snd, 0, amp));

		}).add;


		SynthDef(\perc_4, {
			|freq = 440, t60=3, pitchy=1, amp=0.25, gate=1, pan = 0|
			var sig, exciter;
			exciter = amp * WhiteNoise.ar() * EnvGen.ar(Env.perc(0.001, 0.05), gate) * 0.25;
			sig = DynKlank.ar(
				`[
					[1, 2, 2.803, 3.871, 5.074, 7.81, 10.948, 14.421]/4,   // freqs
					[0.1, 0.044, 0.891, 0.0891, 0.794, 0.1, 0.281, 0.079], // amplitudes
					[1, 0.205, 1, 0.196, 0.339, 0.047, 0.058, 0.047]*t60     // ring times
				],
				exciter,
				freqscale: freq*[1.01, 0.99, 1]);
			DetectSilence.ar(sig, 0.001, 0.5, doneAction:2);
			Out.ar(0, Pan2.ar(sig, pan, amp));

		}).add;


		SynthDef(\piano_6, {
			|freq = 440, t60=1, pitchy=1, amp=0.25, gate=1, pan = 0|
			var sig, exciter;
			exciter = WhiteNoise.ar() * EnvGen.ar(Env.perc(0.001, 0.05), gate) * 0.25;
			sig = DynKlank.ar(
				`[
					[1, 2, 2.803, 3.871, 5.074, 7.81, 10.948, 14.421],   // freqs
					[1, 0.044, 0.0891, 0.0891, 0.794, 0.1, 0.281, 0.079], // amplitudes
					[1, 0.205, 1, 0.196, 0.339, 0.047, 0.058, 0.047]*t60     // ring times
				],
				exciter,
				freqscale: freq);
			DetectSilence.ar(sig, 0.001, 0.5, doneAction:2);
			Out.ar(0, Pan2.ar(sig, pan, amp));

		}).add;


		SynthDef(\piano_7, {
			|freq = 440, t60=0.5, pitchy=1, amp=0.25, gate=1, pan = 0|
			var sig, exciter;
			exciter = WhiteNoise.ar() * EnvGen.ar(Env.perc(0.001, 0.05), gate) * 0.25;
			sig = DynKlank.ar(
				`[
					[1, 2, 2.803, 3.871, 5.074, 7.81, 10.948, 14.421],   // freqs
					[1, 0.044, 0.891, 0.0891, 0.794, 0.1, 0.281, 0.079], // amplitudes
					[1, 0.205, 1, 0.196, 0.339, 0.047, 0.058, 0.047]*t60     // ring times
				],
				exciter,
				freqscale: freq);
			DetectSilence.ar(sig, 0.001, 0.5, doneAction:2);
			Out.ar(0, Pan2.ar(sig, pan, amp));

		}).add;



		// Drums

		this.snare_1.add;


		SynthDef(\snare_2, {|amp = 0.1|
			var snd;
			// a percussive click to give it some attack
			snd = LPF.ar(HPF.ar(WhiteNoise.ar, 300), 8000) * Env.linen(0.001, 0.01, 0.001).ar;
			// sine sweep body. very important!
			snd = snd + (SinOsc.ar(Env([400, 196, 160], [0.04, 0.2], \exp).ar) * Env.perc(0.04, 0.2).ar * 6.dbamp).tanh;
			// sound of snare coils rattling
			snd = snd + (HPF.ar(BPeakEQ.ar(WhiteNoise.ar, 4000, 0.5, 3), 300) * Env.perc(0.05, 0.2).delay(0.01).ar(2) * -3.dbamp);
			// another sound sweep to improve the attack, optional
			snd = snd + (SinOsc.ar(XLine.kr(3000, 1500, 0.01)) * Env.perc(0.001, 0.02).ar);
			// distortion helps glue everything together and acts as a compressor
			snd = (snd * 1.4).tanh;
			snd = Pan2.ar(snd, \pan.kr(0), amp);
			Out.ar(\out.kr(0), amp * snd);



		}).add;

		this.hats_1.add;

		SynthDef(\snare_3, {arg out = 0, amp = 0.1, sinfreq = 180, att = 0.01, rel = 0.2, ffreq = 2000, pan = 0;
			var env, snd1, snd2, sum;
			env = Env.perc(att, rel, amp).kr(doneAction: 2);
			snd1 = HPF.ar(
				in: WhiteNoise.ar,
				freq: ffreq,
				mul: env
			);
			snd2 = SinOsc.ar(freq: sinfreq, mul: env);
			sum = snd1 + snd2;
			Out.ar(out, Pan2.ar(sum, pan));
		}).add;

		this.kick_1.add;


		SynthDef(\kick_5, {arg out = 0, freq, amp = 1, ringTime = 10, rel = 1, dist = 0.5, pan = 0;
			var snd, env;
			snd = Ringz.ar(
				in: Impulse.ar(0), // single impulse
				freq: XLine.ar(freq*4, freq, 0.1),
				decaytime: ringTime);
			env = EnvGen.ar(Env.perc(0.001, rel, amp), doneAction: 2);
			snd = (1.0 - dist) * snd + (dist * (snd.distort));
			snd = snd * env;
			Out.ar(0, Pan2.ar(snd, pan));
		}).add;



		SynthDef(\kick_2, {|amp = 0.1, freq = 100|
			var snd;
			snd = DC.ar(0);
			snd = snd + (SinOsc.ar(XLine.ar(freq * 8, freq * 4, 0.01)) * Env.perc(0.0005, 0.01).ar);
			snd = snd + (BPF.ar(Hasher.ar(Sweep.ar), XLine.ar(freq * 8, freq, 0.01), 0.6) * Env.perc(0.001, 0.02).delay(0.001).ar);
			snd = snd + (SinOsc.ar(XLine.ar(freq * 1.7, freq, 0.01)) * Env.perc(0.0001, 0.3, 1, \lin).delay(0.005).ar(2));
			snd = snd.tanh;
			Out.ar(\out.kr(0), Pan2.ar(snd, \pan.kr(0), amp));
		}).add;





		SynthDef(\kick_3, {|amp = 0.1, freq = 100|
			var snd;
			snd = DC.ar(0);
			snd = snd + (HPF.ar(Hasher.ar(Sweep.ar), freq*13) * Env.perc(0.003, 0.03).ar * 0.5);
			snd = snd + (SinOsc.ar(XLine.ar(freq*12, freq*2, 0.02)) * Env.perc(0.0005, 0.02).ar);
			snd = snd + (SinOsc.ar(XLine.ar(freq*1.5, freq, 0.04)) * Env.perc(0.0005, 0.3).ar(2));
			snd = snd.tanh;
			Out.ar(\out.kr(0), Pan2.ar(snd, \pan.kr(0), amp));
		}).add;


		SynthDef(\kick_4,
			{ arg out = 0, freq = 50, mod_freq = 5, mod_index = 5, sustain = 0.4, amp = 0.8, beater_noise_level = 0.025;
				var pitch_contour, drum_osc, drum_lpf, drum_env;
				var beater_source, beater_hpf, beater_lpf, lpf_cutoff_contour, beater_env;
				var kick_mix;
				pitch_contour = Line.kr(freq*2, freq, 0.02);
				drum_osc = PMOsc.ar(	pitch_contour,
					mod_freq,
					mod_index/1.3,
					mul: 1,
					add: 0);
				drum_lpf = LPF.ar(in: drum_osc, freq: 1000, mul: 1, add: 0);
				drum_env = drum_lpf * EnvGen.ar(Env.perc(0.005, sustain), 1.0, doneAction: 2);
				beater_source = WhiteNoise.ar(beater_noise_level);
				beater_hpf = HPF.ar(in: beater_source, freq: 500, mul: 1, add: 0);
				lpf_cutoff_contour = Line.kr(6000, 500, 0.03);
				beater_lpf = LPF.ar(in: beater_hpf, freq: lpf_cutoff_contour, mul: 1, add: 0);
				beater_env = beater_lpf * EnvGen.ar(Env.perc, 1.0, doneAction: 2);
				kick_mix = Mix.new([drum_env, beater_env]) * 2 * amp;
				Out.ar(out, [kick_mix, kick_mix])
			}

		).add;


		SynthDef(\snare_4,
			{arg out = 0, sustain = 0.1, drum_mode_level = 0.25,
				snare_level = 40, snare_tightness = 1000,
				freq = 405, amp = 0.8;
				var drum_mode_sin_1, drum_mode_sin_2, drum_mode_pmosc, drum_mode_mix, drum_mode_env;
				var snare_noise, snare_brf_1, snare_brf_2, snare_brf_3, snare_brf_4, snare_reson;
				var snare_env;
				var snare_drum_mix;

				drum_mode_env = EnvGen.ar(Env.perc(0.005, sustain), 1.0, doneAction: 2);
				drum_mode_sin_1 = SinOsc.ar(freq*0.53, 0, drum_mode_env * 0.5);
				drum_mode_sin_2 = SinOsc.ar(freq, 0, drum_mode_env * 0.5);
				drum_mode_pmosc = PMOsc.ar(	Saw.ar(freq*0.85),
					184,
					0.5/1.3,
					mul: drum_mode_env*5,
					add: 0);
				drum_mode_mix = Mix.new([drum_mode_sin_1, drum_mode_sin_2, drum_mode_pmosc]) * drum_mode_level;

				// choose either noise source below
				//	snare_noise = Crackle.ar(2.01, 1);
				snare_noise = LFNoise0.ar(20000, 0.1);
				snare_env = EnvGen.ar(Env.perc(0.005, sustain), 1.0, doneAction: 2);
				snare_brf_1 = BRF.ar(in: snare_noise, freq: 8000, mul: 0.5, rq: 0.1);
				snare_brf_2 = BRF.ar(in: snare_brf_1, freq: 5000, mul: 0.5, rq: 0.1);
				snare_brf_3 = BRF.ar(in: snare_brf_2, freq: 3600, mul: 0.5, rq: 0.1);
				snare_brf_4 = BRF.ar(in: snare_brf_3, freq: 2000, mul: snare_env, rq: 0.0001);
				snare_reson = Resonz.ar(snare_brf_4, snare_tightness, mul: snare_level) ;
				snare_drum_mix = Mix.new([drum_mode_mix, snare_reson]) * amp;
				Out.ar(out, [snare_drum_mix, snare_drum_mix])
			}
		).add;


		SynthDef(\hats_2,
			{arg out = 0, freq = 6000, sustain = 0.1, amp = 0.8;
				var root_cymbal, root_cymbal_square, root_cymbal_pmosc;
				var initial_bpf_contour, initial_bpf, initial_env;
				var body_hpf, body_env;
				var cymbal_mix;

				root_cymbal_square = Pulse.ar(freq, 0.5, mul: 1);
				root_cymbal_pmosc = PMOsc.ar(root_cymbal_square,
					[freq*1.34, freq*2.405, freq*3.09, freq*1.309],
					[310/1.3, 26/0.5, 11/3.4, 0.72772],
					mul: 1,
					add: 0);
				root_cymbal = Mix.new(root_cymbal_pmosc);
				initial_bpf_contour = Line.kr(15000, 9000, 0.1);
				initial_env = EnvGen.ar(Env.perc(0.005, 0.1), 1.0);
				initial_bpf = BPF.ar(root_cymbal, initial_bpf_contour, mul:initial_env);
				body_env = EnvGen.ar(Env.perc(0.005, sustain, 1, -2), 1.0, doneAction: 2);
				body_hpf = HPF.ar(in: root_cymbal, freq: Line.kr(9000, 12000, sustain),mul: body_env, add: 0);
				cymbal_mix = Mix.new([initial_bpf, body_hpf]) * amp;
				Out.ar(out, [cymbal_mix, cymbal_mix])
		}).add;

		SynthDef(\perc_1,
			{arg out = 0, sustain = 0.4, drum_mode_level = 0.25,
				freq = 90, drum_timbre = 1.0, amp = 0.8;
				var drum_mode_sin_1, drum_mode_sin_2, drum_mode_pmosc, drum_mode_mix, drum_mode_env;
				var stick_noise, stick_env;
				var drum_reson, tom_mix;

				drum_mode_env = EnvGen.ar(Env.perc(0.005, sustain), 1.0, doneAction: 2);
				drum_mode_sin_1 = SinOsc.ar(freq*0.8, 0, drum_mode_env * 0.5);
				drum_mode_sin_2 = SinOsc.ar(freq, 0, drum_mode_env * 0.5);
				drum_mode_pmosc = PMOsc.ar(	Saw.ar(freq*0.9),
					freq*0.85,
					drum_timbre/1.3,
					mul: drum_mode_env*5,
					add: 0);
				drum_mode_mix = Mix.new([drum_mode_sin_1, drum_mode_sin_2, drum_mode_pmosc]) * drum_mode_level;
				stick_noise = Crackle.ar(2.01, 1);
				stick_env = EnvGen.ar(Env.perc(0.005, 0.01), 1.0) * 3;
				tom_mix = Mix.new([drum_mode_mix, stick_env]) * 2 * amp;
				Out.ar(out, [tom_mix, tom_mix])
			}
		).add;


		SynthDef(\kick_6, {
			|out = 0, pan = 0, amp = 0.3, freq = 100|
			var body, bodyFreq, bodyAmp;
			var pop, popFreq, popAmp;
			var click, clickAmp;
			var snd;

			// body starts midrange, quickly drops down to low freqs, and trails off
			bodyFreq = EnvGen.ar(Env(freq * [2.6, 1.2, 0.5], [0.035, 0.08], curve: \exp));
			bodyAmp = EnvGen.ar(Env.linen(0.005, 0.1, 0.3), doneAction: 2);
			body = SinOsc.ar(bodyFreq) * bodyAmp;
			// pop sweeps over the midrange
			popFreq = XLine.kr(freq*7.5, freq*2.6, 0.02);
			popAmp = EnvGen.ar(Env.linen(0.001, 0.02, 0.001)) * 0.15;
			pop = SinOsc.ar(popFreq) * popAmp;
			// click is spectrally rich, covering the high-freq range
			// you can use Formant, FM, noise, whatever
			clickAmp = EnvGen.ar(Env.perc(0.001, 0.01)) * 0.15;
			click = LPF.ar(Formant.ar(910, 4760, 2110), 3140) * clickAmp;

			snd = body + pop + click;
			snd = snd.tanh;

			Out.ar(out, Pan2.ar(snd, pan, amp));
		}).add;

		SynthDef(\snare_5, {
			|out = 0, pan = 0, amp = 0.3|
			var pop, popAmp, popFreq;
			var noise, noiseAmp;
			var snd;

			// pop makes a click coming from very high frequencies
			// slowing down a little and stopping in mid-to-low
			popFreq = EnvGen.ar(Env([3261, 410, 160], [0.005, 0.01], curve: \exp));
			popAmp = EnvGen.ar(Env.perc(0.001, 0.11)) * 0.7;
			pop = SinOsc.ar(popFreq) * popAmp;
			// bandpass-filtered white noise
			noiseAmp = EnvGen.ar(Env.perc(0.001, 0.15), doneAction: 2);
			noise = BPF.ar(WhiteNoise.ar, 810, 1.6) * noiseAmp;

			snd = (pop + noise) * 1.3;

			Out.ar(out, Pan2.ar(snd, pan, amp));
			//By Nathan Ho aka Snappizz
			//http://sccode.org/1-523
		}).add;

		SynthDef(\hats_3, {
			|out = 0, pan = 0, amp = 0.3|
			var click, clickAmp;
			var noise, noiseAmp;
			var snd;

			// noise -> resonance -> expodec envelope
			noiseAmp = EnvGen.ar(Env.perc(0.001, 0.3, curve: -8), doneAction: 2);
			noise = Mix(BPF.ar(ClipNoise.ar, [4010, 4151], [0.15, 0.56], [1.0, 0.6])) * 0.7 * noiseAmp;

			snd = noise;

			Out.ar(out, Pan2.ar(snd, pan, amp));
			//By Nathan Ho aka Snappizz
			//http://sccode.org/1-523
		}).add;

		// adapted from a post by Neil Cosgrove (other three are original)
		SynthDef(\clap_1, {
			|out = 0, amp = 0.1, pan = 0, dur = 1|
			var env1, env2, snd, noise1, noise2;

			// noise 1 - 4 short repeats
			env1 = EnvGen.ar(
				Env.new(
					[0, 1, 0, 0.9, 0, 0.7, 0, 0.5, 0],
					[0.001, 0.009, 0, 0.008, 0, 0.01, 0, 0.03],
					[0, -3, 0, -3, 0, -3, 0, -4]
				)
			);

			noise1 = WhiteNoise.ar(env1);
			noise1 = HPF.ar(noise1, 600);
			noise1 = LPF.ar(noise1, XLine.kr(7200, 4000, 0.03));
			noise1 = BPF.ar(noise1, 1620, 3);

			// noise 2 - 1 longer single
			env2 = EnvGen.ar(Env.new([0, 1, 0], [0.02, 0.18], [0, -4]), doneAction:2);

			noise2 = WhiteNoise.ar(env2);
			noise2 = HPF.ar(noise2, 1000);
			noise2 = LPF.ar(noise2, 7600);
			noise2 = BPF.ar(noise2, 1230, 0.7, 0.7);

			snd = noise1 + noise2;
			snd = snd * 2;
			snd = snd.softclip;

			Out.ar(out, Pan2.ar(snd,pan,amp));
			//By Nathan Ho aka Snappizz
			//http://sccode.org/1-523
		}).add;



		//  //  //  //  //  //  //  //
		////       Others         ////
		//  //  //  //  //  //  //  //


		SynthDef(\perc_2, {arg out = 0, freq = 25, numharm = 6, atk = 0.01, dur = 0.5, amp = 0.1, pan = 0.5, beatsPercentage = 1.001;
			var snd, env;
			env = Env.perc(atk, dur * 0.3, amp).kr(doneAction: Done.freeSelf);
			snd = LeakDC.ar(Mix(Blip.ar([freq, freq*beatsPercentage], numharm, env)));
			Out.ar(out, Pan2.ar(snd, pan));

		}).add;



		SynthDef(\piano_8, { arg freq = 440, amp = 0.1, att = 0.1, rel = 2, lofreq = 1000, hifreq = 3000;
			var env, snd;
			env = Env.perc(
				attackTime: att,
				releaseTime: rel,
				level: amp
			).kr(doneAction: 2);
			snd = Saw.ar(freq: freq * [0.99, 1, 1.001, 1.008], mul: env);
			snd = LPF.ar(
				in: snd,
				freq: LFNoise2.kr(1).range(lofreq, hifreq)
			);
			snd = Splay.ar(snd);
			Out.ar(0, snd);
			// Basic saw synth for chords and bass
			//By Bruno Ruviaro
			//http://sccode.org/1-54H
		}).add;


		SynthDef(\perc_3, {arg out = 0, freq = 440, amp = 0.2, pan = 0.5, dur = 0.5;
			var snd, env;
			env = Env.perc(0.02, dur/10).kr(doneAction: 2);
			snd = Mix(LFPulse.ar(
				freq: freq * [1, 5/2],
				iphase: 0.0,
				width: 0.5,
				mul: amp));
			snd = snd * env ;
			Out.ar(out, Pan2.ar(snd, pan));
		}).add;



		//  //  //  //  //  //  //  //
		////       Effects         ////
		//  //  //  //  //  //  //  //


		//Steal This Sound
		SynthDef(\choruscompresseffect, {|out =0 gate= 1|
			var source = In.ar(out,2);
			var chorus;
			var env = Linen.kr(gate, 0.1, 1, 0.1, 2);

			chorus= Splay.ar(Array.fill(4,{
				var maxdelaytime= rrand(0.005,0.02);

				DelayC.ar(source[0], maxdelaytime,LFNoise1.kr(Rand(0.1,0.6),0.25*maxdelaytime,0.75*maxdelaytime) )
			}));

			chorus = Compander.ar(4*(source + chorus),source,0.4,1,4);

			XOut.ar(out,env,chorus);

			//From Steal This Sound SC Example
			//By Nick Collins
		}).add;

		SynthDef(\reverb, {
			arg out = 0, gate = 1, roomsize = 100, revtime = 1, damping = 0.6, inputbw = 0.5, spread = 15, drylevel = 1, earlyreflevel = 0.7, taillevel = 0.5, maxroomsize = 300, amp = 0.5;
			var source = In.ar(out,8);
			var reverb;
			var env = Linen.kr(gate, 0.1, 1, 0.1, 2);


			reverb = GVerb.ar(source, roomsize, revtime, damping, inputbw, spread, drylevel, earlyreflevel, taillevel, maxroomsize);
			reverb = reverb * amp ;
			XOut.ar(out,env,reverb);
			//By Zé Craum

		}).add;

		this.loaded = true;

		^this.opts;
	}

	*opts {

		^[\perc, 4,
			\piano, 8,
			\clap, 1,
			\hats, 3,
			\snare, 5,
			\kick, 5,
			\strings, 2,
			\organ, 6].asDict;
	}

	*get {|name, n = 1|
		var max = SynthDefaults.opts.at(name);
		n = n.mod(max);
		^name.asString ++ "_" ++ n.asString;
	}

	*choose {|name|
		var max = SynthDefaults.opts.at(name);
		var n = 1 + max.rand;
		^name.asString ++ "_" ++ n.asString;
	}

	*any {
		var names = SynthDefaults.opts.keys;
		^SynthDefaults.choose(names.choose);
	}
}
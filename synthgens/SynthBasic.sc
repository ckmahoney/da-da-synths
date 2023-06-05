SynthBasic {
	*bass {
		var oscs = [SinOsc, PulseDPW, SawDPW, LFTri];
		var car = oscs.choose;
		var stack = [1/2, 1, 2];
		var cars = oscs.scramble.keep(stack.size);
		var dt = 9.neg - 3.rand;
		var ampMod = 3.pow(cars.size.neg + 2);
		^SynthDef(\basic_bass, {|out = 0, freq = 100, pfreq, amp = 0.1, dur = 1, pan = 0, cps = 2.1, sus = 1,  p = 0.5, t=0, phrase=16, cpc = 4, root = 1|
			var env = EnvGen.ar(Env.pairs([[0, 0.01], [dur/(2+12.rand), 1], [dur/2, 1/2], [dur, 0]], \exp));
			var sig = 0;
			cars.do({|car, i|
				var p = (cars.size - i) / cars.size;
				var fs = [root * 2.pow(dt) + 1, root.neg * 2.pow(dt) + 1].scramble;
				stack.do({|r, i|
					var p = (cars.size - i) / cars.size;
					sig = sig + car.ar(freq * r, mul: p * ampMod * 3.pow(stack.size.neg));
				});

				sig = sig + car.ar(freq * fs, mul: p * ampMod );
			});
			sig = env * amp * Pan2.ar(sig, pan) * 3.pow(-3);
			sig = LPF.ar(HPF.ar(sig, 20), 20000);
			Out.ar(out, sig);
		});
	}

	*chords {
		var oscs = [SinOsc, PulseDPW, SawDPW, LFTri];
		var car = oscs.choose;
		var stack = [3, 4, 6, 8].scramble.keep(2).addFirst(1);
		var cars = oscs.scramble.keep(stack.size);
		var dt = 7.neg - 4.rand;
		var ampMod = 3.pow(stack.size.neg);
		oscs = [SinOsc];

		^SynthDef(\basic_chords, {|out = 0, freq = 100, pfreq, amp = 0.1, dur = 1, pan = 0, cps = 2.1, sus = 1,  p = 0.5, t=0, phrase=16, cpc = 4, root = 1|
			var sig = 0;
			var env = [Env.perc, Env.triangle].choose;
			env = EnvGen.ar(env, timeScale: dur);

			cars.do({|car, i|
				var p = (cars.size - i) / cars.size;
				var fs = [root * 2.pow(dt) + 1, root.neg * 2.pow(dt) + 1].scramble;
				stack.do({|r, i|
					sig = sig + car.ar(freq * r, mul: amp * ampMod);
				});

				sig = sig + car.ar(freq * fs, mul: amp * ampMod );
			});

			sig = env * amp * Pan2.ar(sig, pan);
			sig = LPF.ar(HPF.ar(sig, 20), 20000);
			Out.ar(out, sig);
		});
	}

	*lead {
		var oscs = [SinOsc, PulseDPW, SawDPW, LFTri];
		var stack = [1, 6, 8];
		var cars = oscs.scramble.keep(stack.size);
		var dt = 9.neg;

		var swarm = 3;
		var ampMod = 3.pow(cars.size.neg - swarm.size - 1);


		^SynthDef(\basic_chords, {|out = 0, freq = 100, pfreq, amp = 0.1, dur = 1, pan = 0, cps = 2.1, sus = 1,  p = 0.5, t=0, phrase=16, cpc = 4, root = 1|
			var sig = 0;
			var env = EnvGen.ar(Env.pairs([[0, 0.5.rand], [dur* 2/(3+100.rand), 1], [dur*2/3, 2/3], [dur, 0]], \exp));

			cars.do({|car, i|
				var p = (cars.size - i) / cars.size;
				stack.do({|r, i|
					sig = sig + car.ar(freq * r, mul: p * ampMod * 3.pow(stack.size.neg));
				});
				swarm.do({
					var fs = [1.0.rand * root * 2.pow(dt) + 1, 1.0.rand *root.neg * 2.pow(dt) + 1].scramble;
					sig = sig + car.ar(freq * fs, mul: p * ampMod * 3.pow(swarm.size.neg));
				});
			});

			sig = env * amp * Pan2.ar(sig, pan);
			sig = LPF.ar(HPF.ar(sig, 20), 20000);
			Out.ar(out, sig);
		});
	}
}
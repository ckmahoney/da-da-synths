GenEnv {
	*make {|dur = 1|
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
		^EnvGen.ar(Env.new([a0, a1, a2, a3]/100, [atk, sus, rel]*dur/100, \exp));
	}

	*shortLow {|dur = 1, sustain = 1|
		var breath = 10 + 6.rand;
		var ampMod = 2.pow((1..4).choose);
		// create an envelope measured in seconds
		var atk = 1 + 10.rand;
		var sus = 10 + 38.rand + SinOsc.kr(breath).range(0, ampMod);
		var rel = 100 - atk - sus + 15.rand;

		var a0, a1, a2, a3;
		a0 = 100;
		a1 = 75 + 20.rand;
		a2 = (a1/2) + (a1/3).rand;
		a3 = 0;
		^EnvGen.ar(Env.new([a0, a1, a2, a3]/100, [atk, sus, rel]*dur/100, \exp), timeScale: sustain);

	}

	*decayLow {|dur = 1, sustain = 1|
		var breath = 5 + 6.rand;
		var ampMod = 2.pow((1..4).choose);
		// create an envelope measured in seconds
		var atk = 10 + 20.rand;
		var rel = 30 + 40.rand;
		var sus = 100 - atk - rel;

		var a0, a1, a2, a3;
		a0 = 100;
		a1 = 30 + 40.rand;
		a2 = 80 + 30.rand + SinOsc.kr(breath).range(0, ampMod);
		a3 = 0;
		^EnvGen.ar(Env.new([a0, a1, a2, a3]/100, [atk, sus, rel]*dur/100, \exp), timeScale: sustain);
	}

	*shortHigh {|dur = 1, sustain = 1|
		var breath = 10 + 6.rand;
		var ampMod = 2.pow((1..4).choose);
		// create an envelope measured in seconds
		var atk = 1 + 10.rand;
		var sus = 10 + 28.rand + SinOsc.kr(breath).range(0, ampMod);
		var rel = 100 - atk - sus + 15.rand;

		var a0, a1, a2, a3;
		a0 = 100;
		a1 = 60 + 30.rand;
		a2 = (a1/2) + (a1/3).rand;
		a3 = 0;
		^EnvGen.ar(Env.new([a0, a1, a2, a3]/100, [atk, sus, rel]*dur/100, \exp), timeScale: sustain);
	}

	*decayHigh {|dur = 1, sustain = 1|
		var breath = 5 + 6.rand;
		var ampMod = 2.pow((1..4).choose);
		// create an envelope measured in seconds
		var atk = 5 + 20.rand;
		var rel = 30 + 20.rand;
		var sus = 100 - atk - rel;

		var a0, a1, a2, a3;
		a0 = 100;
		a1 = 20 + 50.rand;
		a2 = 80 + 30.rand + SinOsc.kr(breath).range(0, ampMod);
		a3 = 0;
		^EnvGen.ar(Env.new([a0, a1, a2, a3]/100, [atk, sus, rel]*dur/100, \exp), timeScale: sustain);
	}

}
Pulsy {

	*genDef {|role, voice, name|
		var glide = [18,24, 48, 72,128,1024].choose.reciprocal;
		var pw = switch(role,
			\bass, { 0.4 + 0.1.rand },
			\lead, { 0.25 + 0.2.rand },
			\chords, { 0.05 + 0.3.rand },
			{ ("Unexpected role for Pulsy: " ++ role).throw }
		);
		var df = (voice - 4) * (1..5).choose;
		var vib1, vib2, vib3;
		vib1 = 0.05 + 0.1.rand;
		vib2 = 0.2 + 0.5.rand;
		vib3 = 0.5 + 0.3.rand;
		name = name ? Date.localtime.hash.abs.asString;

		^SynthDef((\pulsy_ ++ name).asSymbol, {|out = 0, freq = 100, pfreq, amp = 0.1, dur = 1, pan = 0, cps = 2.1, sus = 1,  p = 0.5, t=0, phrase=16, cpc = 4|
			var sig, pos;
			var car, mod;
			var ampEnv1, ampEnv2, ampEnv;
			var vib, vibrato;
			var p1, p2, p3;
			p1 = dur * (1/3);
			p2 = (2*p1) + (dur * 1/6).rand;
			p3 = p2 + (dur * 1/3);


			vib = Env.pairs([ [0, 0.001], [p1, vib1], [p2, vib2], [p3, vib3] ], \exp);
			vibrato = SinOsc.kr(cps, mul: df) * EnvGen.ar(vib);

			freq = XLine.ar(pfreq, freq, dur*glide);

			freq = freq + vibrato;

			ampEnv1 = EnvGen.ar([Env.perc, Env.linen, Env.sine, Env.triangle].choose, timeScale: sus);
			ampEnv2 = EnvGen.ar(Env.pairs([[0, 0.005], [dur/IRand(10, 100), 2/3], [dur/IRand(3,6), 1], [dur*4/5, 2/3], [dur, 0]], \exp), timeScale: sus);
			ampEnv = (ampEnv1 + ampEnv2)/2;

			sig = PulseDPW.ar(freq, pw, amp);

			sig = HPF.ar(sig, 20);
			sig = ampEnv * LPF.ar(sig, 19000);

			Out.ar(out, Pan2.ar(sig, pan));
		});
	}
}
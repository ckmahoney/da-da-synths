Buses : SynthLib {
	*basic {
		SynthDef.new(\mono, {|in = 3, out = 0|
			var sig = In.ar(in, 1); // cannot use a control var for nChannels
			Out.ar(out, sig);
		}).add;

		SynthDef.new(\stereo, {|in = 3, out = 0|
			var sig = In.ar(in, 2); // cannot use a control var for nChannels
			Out.ar(out, sig);
		}).add;

		SynthDef.new(\channel, {|in = 3, out = 0, pan = 0, amp = 1|
			var sig = In.ar(in, 1); // cannot use a control var for nChannels
			Out.ar(out, Pan2.ar(sig, pan, amp));
		}).add;

		SynthDef.new(\metronome, {|rate, amp = 0.1|
			Out.ar(0, Impulse.ar(rate, 0.0, amp)!2);
		}).add;
	}
}

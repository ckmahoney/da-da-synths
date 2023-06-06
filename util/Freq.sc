Freq {
	*fit {|r, f, s = 1|
		^if (f.isCollection, { f.collect(Freq.fit(r, _, s)) }, {
			if (f < r, {Freq.fit(r, f * 2, s)}, {
				if (f >= (r * 2.pow(s)), {Freq.fit(r, f/2, s)}, f)})});
	}

	*fundamental {|f|
		^if (f < 1, { Freq.fundamental(f*2) },
			{ if (f > 2, { Freq.fundamental(f/2) }, f) });
	}

	*voiceMote {|mote, voice, span = 1|
		var freqIndex = 1;
		var freq  = mote @ freqIndex;

		freq = Freq.fit(Scalar.twos(voice), freq, span);
		mote = mote.put(freqIndex, freq);

		^mote;
	}

}

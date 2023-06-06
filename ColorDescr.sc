ColorDescr {
	*descrption {
		^"Descriptions of basic effect control paramters. Provides value boundaries, the control param name, and a default value wich rends the control transparent when applied.";
	}

	*hpf {
		^(
			symbol: \hpf,
			descr: "The minimum allowed frequency for the signal",
			min: 20,
			max: 20000,
			default: 20,
		)
	}

	*lpf {
		^(
			symbol: \lpf,
			descr: "The maximum allowed frequency for the signal",
			min: 20,
			max: 20000,
			default: 20000
		)
	}

	*fb {
		^(
			symbol: \fb,
			descr: "How much of the wet signal is fed back into the effect",
			min: 0,
			max: 0.95,
			default: 0
		)
	}

	*decay {
		^(
			symbol: \decay,
			descr: "The time in seconds to fade from line level to zero",
			min: 1/512,
			max: 128,
			default: 2
		)
	}

	*dt {
		^(
			symbol: \dt,
			descr: "The amount of time between copies",
			min: 1/512,
			max: 128,
			default: 0.5,
		)
	}
}

CD : ColorDescr {
	*descrption {
		^"Combination of existing parameters for creating macros.";
	}

	*make {|controls|
		^([()] ++ controls).reduce({|params, descr|
			params.put(descr.at(\symbol), descr.at(\default));
		});
	}

	*with {|params, controls|
		^params ++ CD.make(controls);
	}

	*filters {
		^CD.make([ColorDescr.hpf, ColorDescr.lpf]);
	}

	*delayTime {
		^CD.make([ColorDescr.dt, ColorDescr.decay]);
	}
}

ColorParams {
	*description {
		^"Description for the synth controls for macro effects.";
	}

	*delay_1 {
		^CD.with(CD.delayTime ++ CD.filters, [CD.fb]);
	}

	*reverb {
		^CD.with(CD.filters, [CD.decay, CD.fb]);
	}
}

ColorDo {
	*bounds {|val, descr|
		^case(
			{ val < descr.at(\min) }, { descr.at(\min) },
			{ val > descr.at(\max) }, { descr.at(\max) },
			val
		);
	}

	/* Static, non-resonant high pass and low pass filters applied to a signal. */
	*filters {|sig, hpf, lpf|
		^LPF.ar(HPF.ar(sig, hpf), lpf);
	}

	/* A mix knob from 0 to 1. 0 is only signal a, 1 is only signal b. */
	*mix {|a, b, mix|
		^((1-mix) * a) + (mix * b);
	}
}


ColorWrap {
	*pool {|sig, fb, nChannels|
		^{
			var pool = LocalIn.ar(nChannels);
			LocalOut.ar(pool + (sig * fb));
		};
	}
}
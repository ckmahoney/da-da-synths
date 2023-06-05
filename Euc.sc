Euc {
	*of {|a, b, o = 1, p = 4|
		var seq = Euc.seq(o, p);
		^seq.collect({|y| if (y ==1, a, b) });
	}
	// taken from https://scsynth.org/t/bresenham-implementation-of-the-euclidean-rhythm-algorithm-in-supercollider/3127
	*seq { |o = 1, p = 4|
		if (o > p, {["Must use a number of hits less than or equal to the total number of events. Got this instead", o, p].throw});
		^(o / p * (0..p - 1)).floor.differentiate.asInteger.put(0, 1);
	}


	*make {|hits, events, a = 1, b = 0|
		var pat = Euc.seq(hits, events);
		^pat.collect({|switch| if (1 == switch, a, b)});
	}

	*makeRhythm {|hits, events|
		var euc = Euc.make(hits, events);
		var ratios = ([[]] ++ euc).reduce({|all, switch|
			if (switch == 0, {
				var prev = all[all.size-1];
				var val = prev+1;
				all[all.size-1] = (val);
			}, {
				all = all.add(1);
			});

			all;
		});

		^ratios;
	}

	*sum {|hits, events|
		"Stop using Euc.sum; it has a misnomer name. Use Euc.toRhythm instead.".postln;
		^Euc.makeRhythm(hits, events);
	}


	/** Given a number of cycles and euc args, creates a rhythm whose sum is nCycles applied from the euc args. */
	*fit {|nCycles, hits, events|
		var marks = Euc.makeRhythm(hits, events);
		var scale = marks.maxItem.log2.floor;
		var ratios = Freq.fit(1, marks, scale.abs + 1);
		var whole = ratios.sum;
		var rhythm = ratios.collect({|r| (nCycles * r)/whole});
		^rhythm;
	}

	*hand {|nCycles, offset = 0, ext = 2, resolution = 4|
		var events = 2.pow(resolution);
		var opts = (offset..(offset+ext));
		var hits = 1 + (2*opts.choose);
		^Euc.fit(nCycles, hits, events);
	}

	*hands {|nCycles, n = 4, ext = 1, resolution = 4|
		var events = 2.pow(resolution);
		var opts = (0..(n+ext));
		var used;

		^n.collect({|i|
			var hits = 1 + (2*opts.choose);
			Euc.fit(nCycles, hits, events)
		});
	}

	*foot {|nCycles, offset = 0, ext = 2, resolution = 4|
		var events = 2.pow(resolution);
		var opts = (offset..(offset+ext));
		var hits = 2 + (2*opts.choose);
		^Euc.fit(nCycles, hits, events);
	}

	*feet {|nCycles, n = 4, ext = 1, resolution = 5|
		var events = 2.pow(resolution);
		var opts = (0..(n+ext));
		var hits;

		^n.collect({|i|
			hits = 2 + (2*opts.choose);
			Euc.fit(nCycles, hits, events)
		});
	}

	/** list of source rhythms to be ref'd */
	*combo {|sizes, ext = 0, resolution = 4, feet = 0.5|
	    ^sizes.collect({|nCycles, offset|
			var scale = if (0.5.coin, 1, { if (0.5.coin, 0.5, 2) });
			if (feet.coin, {
				Euc.foot(nCycles * scale, resolution: resolution)/scale;
			}, {
				Euc.hand(nCycles * scale, resolution: resolution)/scale;
			});
		});
	}

	*vary {|pat, subdiv = 1, base = 2|
		var duration = Rhythm.sum(pat);
		var pos = (1 - base.pow(subdiv).reciprocal);
		var head = Tala.trim(pat, duration * pos);
   		var tail = Tala.tail(pat, pos);
		var tailScale = if (0.25.coin, 0.5, 2);

		tail = Euc.combo(tail).flatten;
		^head ++ tail;
	}
}

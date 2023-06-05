Sampler {
	*makePerc {|role, label, server|
		var buffStuff, loadMsgs = [], afterLoadMsgs = [], freeMsgs = [], buffers = [];

		var gain = 0;
		var opts = switch (role,
			\kick, {
				gain = 2;
				PathName("~/stage/assets/samples/kick").files;
			},
			\perc, {
				gain = 6;
				PathName("~/stage/assets/samples/snare").files;
			},
			\hats, {
				gain = 8;
				PathName("~/stage/assets/samples/hats/short").files;
		});
		var sample = opts.choose.asAbsolutePath;
		var buffer, loadMsg, def;
		var name = ("sampler_" ++ label).asSymbol;
		var ampGain = 1 + 3.pow((10-gain).neg);

		server = server ? Server.default;

		buffer = Buffer.new(server);

		def = SynthDef.new(name, {|out, bufnum, freq=300, amp=0.1, dur=4, pan = 0, sus = 1, pfreq = 100, t = 0, cpc = 4, phrase = 16, root = 1, level = 5|

			var sig = ampGain * amp * PlayBuf.ar(2, bufnum, BufRateScale.kr(bufnum) * root);
			Out.ar( out, sig );

			DetectSilence.ar(sig, doneAction: 2);
		});

		loadMsg = [\b_allocRead, buffer.bufnum, sample, 0, -1];
		buffStuff = (
			buffers: [buffer],
			loadMsgs: [loadMsg],
			afterLoadMsgs: [],
			freeMsgs: [buffer.freeMsg]
		);



		^[buffStuff, def];

	}

	*makePercStack {|role, label, server, n = 3|
		var buffStuff, loadMsgs = [], afterLoadMsgs = [], freeMsgs = [], buffers = [];
		var def;
		var gain = 0;
		var opts = switch (role,
			\kick, {
				gain = 2;
				PathName("~/stage/assets/samples/kick").files;
			},
			\perc, {
				gain = 6;
				PathName("~/stage/assets/samples/snare").files;
			},
			\hats, {
				gain = 8;
				PathName("~/stage/assets/samples/hats/short").files;
		});
		var samples = opts.scramble.keep(n).collect({|p|p.asAbsolutePath});


		var name = ("sampler_stack_" ++ label).asSymbol;
		var ampGain = 1 + 3.pow((10-gain).neg);
		var ampMod = 3.pow(n.neg);

		buffers = n.collect({ Buffer.new(server) });
		server = server ? Server.default;

		def = SynthDef.new(name, {|out, buffers, ratio=1.5, amp=0.1, dur=4, pan = 0, sus = 1, pfreq = 100, t = 0, cpc = 4, phrase = 16, root = 1, level = 5|
			var sig = 0;
			buffers.do({|bufnum|
				var next = PlayBuf.ar(2, bufnum, BufRateScale.kr(bufnum) * ratio * root);
				sig = sig + next;
			});
			sig = amp * sig;
			// sig = Pan2.ar(sig);
			sig = HPF.ar(LPF.ar(sig, 15000), 20);
			Out.ar( out, sig );

			DetectSilence.ar(sig, doneAction: 2);
		});

		loadMsgs = buffers.collect({|buffer, i|
			var sample= samples[i];
			[\b_allocRead, buffer.bufnum, sample, 0, -1];
		});
		freeMsgs = buffers.collect({|buffer, i|
			buffer.freeMsg;
		});

		buffStuff = (
			buffers: buffers,
			loadMsgs: loadMsgs,
			afterLoadMsgs: [],
			freeMsgs: freeMsgs,
		);


		^[buffStuff, def];

	}

}

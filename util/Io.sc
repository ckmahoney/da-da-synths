Io {
	*new {|args|

	}


	*rm {|fp|
		if (fp.isString) {
			var cmd = "rm " ++ fp;
			cmd.unixCmd;
		};
	}

	*get {|path|
		if (File.exists(path), {
			var contents = File.readAllString(path);
			var result = contents.interpret;
			^result;
		}, {
			this.log("Nothing found for file: " ++ path);
			^nil;
		});
	}

	*load {|asset, dev_mode = false|
		var basedir = if (dev_mode, "develop/write/", "write/");
		var path = (basedir ++ asset ++ ".sc").standardizePath;
		var obj = this.get(path);

		if (obj.isNil.not, { ^obj }, true);
	}


	*autoload {|dev_mode = false|
		var	deps = [
			"api/api",
			"utils/size",
			"utils/nrt",
			"harmony/harmony",
			"play/seq",
			"production/dsp",
			"production/gens",
			"production/phrasing",
			"production/production",
			"production/record",
			"synths/synth",
			"synths/basic-kit",
			"synths/basic-effects",
			"synths/post-effects",
			"rhythm/amps",
			"rhythm/loops",
			"utils/elements"
		];

		deps.do({|asset|
			this.load(asset, dev_mode);
		});
	}

	*write {|data, path|
		File.use(path.standardizePath, "w", {|f|
			f.write(data);
		});
		^("Wrote data to " ++ path);
	}

	*writeJson {|data, path|
		File.use(path.standardizePath, "w", {|f|
			var d = Io.toJsonStr(data);
			~d = d;
			f.write(d);
		});
		^("Wrote json data to " ++ path);
	}

	*parseJsonVal {|v|
		if (Check.isListy(v), { ^v.collect({|y| Io.parseJsonVal(y) }) }, {
			if (Check.isDicty(v), {
				// synthony only uses Events keyed with symbols
				var res = ();
				v.keysValuesDo({|key, val|
					res.put(key.asSymbol, Io.parseJsonVal(val))
				});
				^res
			}, {
				var rounding = 0.000000001;
				var maybe = v.asInt;
				if ((maybe.class == Integer) && (maybe.asString == v), {
					^maybe;
				});
				maybe = v.asFloat;
				if ((maybe.class == Float) && ("0.0" != maybe.asString), {
					^maybe;
				});

				^v.asSymbol;
			});
		});
		["Error while attempting to parse a json value: ", v].throw;
	}

	*readJson {|p|
		var path = p.standardizePath;

		^if (File.exists(path), {
			var json = path.parseYAMLFile;
			Io.parseJsonVal(json).debug("finished parseJsonVal");

		}, {
			("Could not parse JSON from file: File does not exist. Path: " ++ path).throw;
		});
	}

	*log {|msg, name = "sclang-log"|
		msg = Date.localtime.asString ++ "    " ++ msg;
		File.use(name.standardizePath, "a", {|f|
			f.write("\n");
			f.write(msg.asCompileString);
		});
		^msg.postln;
	}

	*logTest {|msg|
		^this.log(msg, "sclang-log-test");
	}


	*logErr {|msg|
		^this.log(msg, "sclang-err-log");
	}


	id {|str = false|
		if (str, {this.asString.postln}, {this.postln});
	}

	*exit {|server|
		server = server ? Server.default;
		if (server.isNil.not, { server.quit; server.remove });
		0.exit;
	}


	*dictyToJson {|x|
		var ok = if (Check.isDicty(x).not, { ["Can't convert this to a json object because it isn't dicty:", x].throw});
		var str = '{';

		x.keysValuesDo({|k, v|
			var name = '"' ++ k.asString ++ '"' ++ ":";
			var val = if (Check.isDicty(v), { Io.dictyToJson(v) }, { Io.toJsonStr(v) });
			str = str ++ name ++ val ++ ",";
		});

		// drop trailing comma
		^(str.drop(-1) ++ '}');
	}

	*toJsonArray {|y|
		var ok = if (Check.isListy(y).not, {["Can't make a json array out of this:", y].throw});
		var res = "[";

		y.do({|x|
			var el = if(x.isNumber, { x }, {
				if (Check.isStringy(x), { ('"' ++ x.asCompileString ++ '"') }, {
					if (Check.isListy(x), { Io.toJsonArray(x) }, {
						if (Check.isDicty(x).not, { x.asString }, {
							Io.dictyToJson(x) })})})});

			res = res ++ el ++ ",";
		});

		res = res.drop(-1) ++ "]";

		^res;
	}

	*toJsonStr {|x, str = ""|
		var res = if(x.isNumber, { x }, {
			if (Check.isStringy(x), { ('"' ++ x.asString ++ '"') }, {
				if (Check.isListy(x), { Io.toJsonArray(x) }, {
					if (Check.isDicty(x).not, { x.asCompileString }, {
						Io.dictyToJson(x) })})})});

		^res;
	}


}


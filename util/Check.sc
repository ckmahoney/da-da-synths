Check {
	classvar <>runcheckOn = false;
	classvar <>notices = true;

	*runcheck {|message, test|
		^if (Check.runcheckOn == true, {
			^if (test.value == false, { message = "This was expected but did not happen: " + message; Io.logErr(message).error; Exception.new(message).throw; false }, true);
		}, nil);
	}

	*notice {|message|
		if (Check.notices == true, {
			message = "Notice: " + message; Io.log(message);
		});
	}

	*bool {|x|
		^(x == true) || (x == false)
	}
	*list_of {|els, fn|
		^els.collect(fn).every(_ == true)
	}

	*isListy {|x|
		^(x.class != String) && (x.isArray || (List == x.class));
	}

	*isDicty {|x|
	    ^(x.class == Dictionary) || (x.class == Event)
	}

	*isSingular {|x|
		^(Check.isListy(x).not && Check.isDicty(x).not);
	}

	*isStringy {|x|
		^(x.class == String) || (x.class == Symbol);
	}


	*cps {|x|
		var minCps_ = 0.25;
		var maxCps_ = 999;
		^if(x.isNumber.not, false, {
			(x > minCps_) && (x < maxCps_);
		});
	}

	*duration {|x|
		var message = ["Duration needs to be a float greater than 0, unbounded. Got this instead", x];
		if (x.isNumber.not, {message.postln; ^false});
		if (x == 0, {message.postln; ^false});
		^true;
	}


	*instdef {|x|
		^if((x.isCollection.not || (x.size != 5)), false, {

			var typechecks = [
				{|x| Check.voice(x) },
				{|x| Check.role(x) },
				{|x| Check.duty(x) },
				{|x| Check.tone(x) },
				{|x| Check.presence(x) }
			];

			x.collect({|y, i| typechecks.at(i).value(y) }).every(_ == true);
		});
	}



	*range {|x|
	    ^x.isNumber
		&& (x >= 0)
		&& (x <= 1.0)
	}



}
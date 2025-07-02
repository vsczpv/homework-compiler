
let jesu_cristo: (unit -> int) = lambda none : unit -> int {
	return 1;
};

let eeee: (unit -> unit) = lambda none : unit -> unit {
	return!;
};

let other: (int int -> int) = lambda x y : int int -> int {
	return x + y;
};

let main: (unit -> unit) = lambda none : unit -> unit {

	let b: [int; 4];
	let r: int;

	(b[1]) = other (1 + 2) r;

	if (5 < 2) {
		r = 1;
	} else if (2 == 7) {
		r = 2;
	} else {
		r = 3;
	};

	let i: int;
	i = 0;

	while (i != 4) {
		b[i] = i;
		i = i + 1;
	};

	/*
	i = 0;
	while (i < 4) {
		out_int (b[i]);
	};
	*/

	return!;
};



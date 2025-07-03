let identity: (int -> int) = lambda v : int -> int {
	return v;
};

let fibonacci: (int -> unit) = lambda n : int -> unit {

	let a: int;
	let b: int;
	let t: int;

	a = 0;
	b = 1;
	t = 0;

	let i: int;
	i = 0;

	while (i < n) {

// lol		out_int a;

		t = a + b;
		a = b;
		b = t;

		i = i + 1;
	};

	return!;
};

let main: (unit -> unit) = lambda none : unit -> unit {

//	fibonacci 200;
	
	let n: int;
	n = 1000000000;


	let a: int;
	let b: int;
	let t: int;

	a = 0;
	b = 1;
	t = 0;

	while (a < n) {

//		if ( (a & 1) == 0) {
			out_int (identity a);
//		} else {
//			out_int (a + a);
//		};

		t = a + b;
		a = b;
		b = t;
	};

	return!;
};



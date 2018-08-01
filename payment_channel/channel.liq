[%%version 0.3]

type storage = {
  channel_sender: key;
  channel_recipient: key;
  start_date: timestamp;
  channel_timeout: nat;
  signatures: (bytes, key_hash) map;
};;

type action = 
  | Close of bytes * signature * tez
  | Timeout
;;

let%init storage (parameter: key * key * nat) = {
  channel_recipient= parameter.(0);
  channel_sender= parameter.(1);
  channel_timeout= parameter.(2);
  start_date= Current.time ();
  signatures= (Map []: (bytes, key_hash) map);
};;

(* Transform the tez value to a bytes proof *)
let tez2bytes (value: tez) = 
	0x12
	[@@inline]
;;

let%entry main (parameter: action) (storage: storage) = match parameter with
| Close (h, signature, value) -> (
	(* get signer address *)
	let signer = 
		if Crypto.check (storage.channel_recipient) signature h then 
			Some (Crypto.hash_key storage.channel_recipient) 
		else if Crypto.check (storage.channel_sender) signature h then 
			Some (Crypto.hash_key storage.channel_sender) 
		else 
			(None: key_hash option)
	  in

	(* generate the proof *)
	let proof = Crypto.sha256 (tez2bytes value) in

	if proof <> h then Current.failwith ();

	match signer with
	| None -> Current.failwith () 
	| Some (signer') -> 
		if Map.mem proof storage.signatures then (
			let storage = storage.signatures <- Map.add proof (signer') storage.signatures in
			(([]: operation list), storage)
		) else 
			match Map.find proof storage.signatures with
			| Some (s) -> 
				if (s <> signer') then (
					let op = Contract.call (Account.default (Crypto.hash_key storage.channel_recipient)) value () in
					let op' = Contract.call (Account.default (Crypto.hash_key storage.channel_sender)) (Current.balance () - value) () in
					( [op; op'], storage )
				) 
				else (Current.failwith () )
			| None -> 
				Current.failwith () 
	)
| Timeout -> (
	if storage.start_date + storage.channel_timeout > Current.time () then 
		Current.failwith ()
	else 
		let op' = Contract.call (Account.default (Crypto.hash_key storage.channel_sender)) (Current.balance ()) () in
		( [op'], storage )
)
;;
[%%version 0.36]

type status = Open | Closed;;

type storage = {
	status: status;
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
	status= Open;
	channel_recipient= parameter.(0);
	channel_sender= parameter.(1);
	channel_timeout= parameter.(2);
	start_date= Current.time ();
	signatures= (Map []: (bytes, key_hash) map);
};;

(* calculate a byteproof *)
let calc_proof (storage: storage) (value: tez) = 
	0x12
	[@@inline]
;;

let is_closed (storage: storage) = match storage.status with
| Open -> false
| Closed -> true
[@@inline]
;;

let%entry main (parameter: action) (storage: storage) = 
	if is_closed storage then Current.failwith "Closed channel";
	match parameter with
	(* close channel by proof *)
	| Close (hproof, signature, value) -> (
		(* check the proof *)
		if Crypto.sha256 (calc_proof storage value) <> hproof then Current.failwith "Invalid proof";

		(* get signer address *)
		let signer = 
			if Crypto.check (storage.channel_recipient) signature hproof then 
				Crypto.hash_key storage.channel_recipient
			else if Crypto.check (storage.channel_sender) signature hproof then 
				Crypto.hash_key storage.channel_sender
			else 
				Current.failwith "Invalid signer"
		in
		
		(* get the proof from signatures *)
		match Map.find hproof storage.signatures with
		| None -> ( (* if not present, associate hproof with signer *)
			let storage = storage.signatures <- Map.add hproof signer storage.signatures in
			(([]: operation list), storage))
		| Some s -> ( (* if present, and signer <> oldsigner, close the channel *)
			if (s <> signer) then (
				let op = Contract.call (Account.default (Crypto.hash_key storage.channel_recipient)) value () in
				let op' = Contract.call (Account.default (Crypto.hash_key storage.channel_sender)) (Current.balance () - value) () in
				let storage = storage.status <- Closed in
				( [op; op'], storage )
			) 
			else (Current.failwith "Calling from the same signer" )
		))

	(* close channel by timeout *)
	| Timeout -> (
		if storage.start_date + storage.channel_timeout > Current.time () then 
			Current.failwith "Timeout not already reached" 
		else 
			let op' = Contract.call (Account.default (Crypto.hash_key storage.channel_sender)) (Current.balance ()) () in
			let storage = storage.status <- Closed in
			( [op'], storage )
	)
;;
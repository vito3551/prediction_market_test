type outcome = int
type probability = int
type probabilities = (outcome, probability) map
type bids = (nat, nat) map
type auction_map = (string, bids) map 

let empty_qps_map : (nat, nat) map = Map.empty
let empty_auction_map : (string, bids) map = Map.empty
let empty_Q_map : (string, nat) map = Map.empty

type market_storage = {
    num_outcomes : nat;
    q_total : nat;
    lqt_token : nat option;
    token_ids : (nat, nat) map;
    qps_total : bids;
    clearing_prices : bids;
    auction_bids : (string, bids) map; (* might be a big map later *)
    auction_Q : (string, nat) map;
    auction_Qp : (string, bids) map;
    market_allocations : (string, bids) map;
}

let a : string = "hello"
let p_map : (string, int) map = Map.literal [
    ("a", 1);
    ("b", 2)
]
let pb : bids = Map.literal [(0n,10n); (1n,75n); (2n, 15n)]
let me : string = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"

let my_ms : market_storage = {
    num_outcomes = 3n;
    q_total = 0n;
    lqt_token = (None : nat option);
    token_ids = empty_qps_map;
    qps_total = empty_qps_map;
    clearing_prices = empty_qps_map;
    auction_bids = Map.literal [(me, pb)];
    auction_Q = Map.literal [(me, 100n)]; (* empty_Q_map *) 
    auction_Qp = empty_auction_map;
    market_allocations = empty_auction_map;
}

let check_p (prob : (int, int) map) : unit = 
    let predicate = fun (i, j : outcome * probability) -> assert (j >= 0 && j <= 100)
    in Map.iter predicate prob

(* this function is based on "Folded Operations over Maps" *)
let sum_prob (prob : (int, int) map) : int = 
    let folded = fun (i, proba : int * (outcome * probability)) -> (i + proba.1)in 
    (Map.fold folded prob 0) 

let get_probabilities (addr : string) (ms : market_storage) : bids = 
    match Map.find_opt addr ms.auction_bids with
    | None -> (failwith "error_PROBS_DOESNT_EXIST" : bids) 
    | Some p -> p

let get_Qps (addr : string) (ms : market_storage) : bids = 
    match Map.find_opt addr ms.auction_Qp with
    | None -> empty_qps_map (* (failwith "error_QPS_DOESNT_EXIST" : bids) *)
    | Some qp -> qp

let get_Q (addr : string) (ms : market_storage) : nat = 
    match Map.find_opt addr ms.auction_Q with
    | None -> (failwith "error_Q_DOESNT_EXIST" : nat)
    | Some q -> q

let calculate_Qp (addr, ms : string * market_storage) : bids =
    let q : nat = get_Q addr ms in
    let qps_m = empty_qps_map in
    let prob = get_probabilities addr ms in
    let multiqp : (nat * nat) -> nat = fun (i, proba : nat * nat) -> 
        let prev_qps_m = match Map.find_opt i qps_m with
        | Some x -> x
        | None -> 0n in
        prev_qps_m + (proba * q) in
    (Map.map multiqp prob : bids)

let update_q_total (addr, ms : string * market_storage) : market_storage =
    let user_Q = get_Q addr ms in
    {ms with q_total = ms.q_total + user_Q}

let main (addr, ms : string * market_storage) : market_storage =
    let new_ms = update_q_total (addr, ms) in
    let usr_qp : (nat, nat) map = calculate_Qp (addr, new_ms) in 
    let a_Qp : (string, bids) map = Map.update (me : string) (Some usr_qp) new_ms.auction_Qp in
    let n_ms = {new_ms with auction_Q = a_Qp} in
    n_ms
    
    
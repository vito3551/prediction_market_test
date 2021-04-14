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

let calculate_Qp (addr : string) (ms : market_storage) : bids =
    let q : nat = get_Q addr ms in
    let qps_m = get_Qps addr ms in
    let prob = get_probabilities addr ms in
    let multiqp : (nat * nat) -> nat = fun (i, proba : nat * nat) -> 
        let prev_qps_m = match Map.find_opt i qps_m with
        | Some x -> x
        | None -> 0n in
        prev_qps_m + (proba * q) in
    (Map.map multiqp prob : bids)

let update_q_total (addr : string) (ms : market_storage) : market_storage =
    let user_Q = get_Q addr ms in
    {ms with q_total = ms.q_total + user_Q}

(* this function is based on "Map Operations over Maps" *)
let update_qps_total (addr : string) (ms : market_storage) : bids = 
    let qps_tot = ms.qps_total in
    let qps_user = get_Qps addr ms in
    let new_qp_tot : (nat * nat) -> nat = fun (i, qp_val : nat * nat) ->
        let prev_qps_tot  = match Map.find_opt i qps_tot with
        | Some x -> x
        | None -> 0n in
        prev_qps_tot + qp_val in
    (Map.map new_qp_tot qps_user : bids)

(* this function is based on "Map Operations over Maps" *)
let calculate_clearing_price (ms : market_storage) : bids =
    let q_tot : nat = ms.q_total in
    let qps_tot : (nat, nat) map = ms.qps_total in
    let clearing_prs : (nat, nat) map = ms.clearing_prices in
    let new_clear_price : (nat * nat) -> nat = fun (i, qp_val_tot : nat * nat) ->
        let prev_clearing_prs = match Map.find_opt i clearing_prs with
        | Some x -> x
        | None -> 0n in
        prev_clearing_prs + (qp_val_tot / q_tot) in 
    (Map.map new_clear_price qps_tot : (nat, nat) map)

(* CALCULATE USER ALLOCATION *)
let get_allocations (addr : string) (ms : market_storage) : bids =
    match Map.find_opt addr ms.market_allocations with
    | None -> empty_qps_map (* (failwith "error_USR_ALLOCATION_DOESNT_EXIST" : bids) *)
    | Some a -> a

let calculate_allocation (addr : string) (ms : market_storage) : (nat, nat) map =
    let qps_usr = get_Qps addr ms in
    let clr_prices : (nat, nat) map = ms.clearing_prices (* need to call calculate_clearing_price before this one*) in
    let alloc_usr = get_allocations addr ms in
    let new_alloc : (nat * nat) -> nat = fun (i, qp_val : nat * nat) -> (
        let spec_price = match (Map.find_opt i clr_prices) with
        | Some p -> p
        | None -> 0n in
        let prev_usr_alloc = match (Map.find_opt i alloc_usr) with
        | Some a -> a
        | None -> 0n in
        prev_usr_alloc + (qp_val / spec_price) ) in
    (Map.map new_alloc qps_usr : bids)

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

let main (addr, ms : string * market_storage) : market_storage =
    let new_ms = update_q_total addr ms in
    let usr_qp : (nat, nat) map = calculate_Qp addr new_ms in 
    let a_Qp : (string, bids) map = Map.update (me : string) (Some usr_qp) new_ms.auction_Qp in
    let n_ms = {new_ms with auction_Qp = a_Qp} in
    let qps_tot1 = update_qps_total addr n_ms in
    let n1_ms = {n_ms with qps_total = qps_tot1} in
    let cp_1 = calculate_clearing_price n1_ms in
    let n2_ms = {n1_ms with clearing_prices = cp_1} in
    let usr_alloc = calculate_allocation addr n2_ms in
    let a_alloc : (string, bids) map = Map.update (me : string) (Some usr_alloc) n2_ms.market_allocations in
    let n3_ms = {n2_ms with market_allocations = a_alloc} in
    n3_ms

    
    
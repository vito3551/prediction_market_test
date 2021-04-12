
type token_id = nat
type outcome = nat
type probability = nat
type qp = nat
type quantity = nat
type price = nat
type allocation = nat

let percent = 1000n


type probabilities = (outcome, probability) map
type qps = (outcome, qp) map
type clearing_prices = (outcome, price) map
type user_allocations = (outcome, allocation) map

let empty_ids_map : (outcome, token_id) map = Map.empty
let empty_bids_map : (address, probabilities) map = Map.empty
let empty_Q_map : (address, quantity) map = Map.empty
let empty_Qp_map : (address, qps) map = Map.empty
let empty_qps_map : qps = Map.empty
let empty_clearing_prices_map : clearing_prices = Map.empty
let empty_market_allocations : (address, user_allocations) map = Map.empty

type market_storage = {
    num_outcomes : nat;
    q_total : nat;
    lqt_token : nat option;
    token_ids : (outcome, token_id) map;
    qps_total : qps;
    clearing_prices : (outcome, price) map;
    auction_bids : (address, probabilities) map; (* might be a big map later *)
    auction_Q : (address, quantity) map;
    auction_Qp : (address, qps) map;
    market_allocations : (address, user_allocations) map;
}

let create_market_storage (num_outcomes : nat) : market_storage = {
    num_outcomes = num_outcomes;
    q_total = 0n;
    lqt_token = (None : nat option);
    token_ids = empty_ids_map;
    qps_total = empty_qps_map;
    clearing_prices = empty_clearing_prices_map;
    auction_bids = empty_bids_map;
    auction_Q = empty_Q_map;
    auction_Qp = empty_Qp_map;
    market_allocations = empty_market_allocations;
}

type storage = market_storage

let get_probabilities (addr : address) (ms : market_storage) : probabilities = 
    match Map.find_opt addr ms.auction_bids with
    | None -> (failwith "error_PROBS_DOESNT_EXIST" : probabilities) 
    | Some p -> p

let get_specific_prob (addr : address) (ms : market_storage) (outcome : outcome) : probability = 
    let probs = get_probabilities addr ms in
    match Map.find_opt outcome probs with
    | None -> (failwith "error_SPECIFIC_PROB_DOESNT_EXIST" : probability)
    | Some p -> p


(* this function is based on "Iterated Operations over Maps" *)
let check_prob_geq0_and_leq1 (addr : address) (ms : market_storage) : unit =
    let prob = get_probabilities addr ms in
    let predicate = fun (i, j : outcome * probability) -> assert (j >= 0n && j <= 100n)
    in Map.iter predicate prob

(* this function is based on "Folded Operations over Maps" *)
let sum_prob (m : probabilities) : nat = 
    let aggregate = fun (i, prob : nat * (outcome * probability)) -> (i + prob.1) in
    Map.fold aggregate m 0n

let check_prob_sum_to_one (addr : address) (ms : market_storage) : bool =
    let prob : probabilities = get_probabilities addr ms in
    let sum_probs : nat = sum_prob prob in
    (sum_probs = 100n) 

let get_Qps (addr : address) (ms : market_storage) : qps = 
    match Map.find_opt addr ms.auction_Qp with
    | None -> (failwith "error_QPS_DOESNT_EXIST" : qps)
    | Some qp -> qp

let get_Q (addr : address) (ms : market_storage) : quantity = 
    match Map.find_opt addr ms.auction_Q with
    | None -> (failwith "error_Q_DOESNT_EXIST" : quantity)
    | Some q -> q

(* this function is based on "Map Operations over Maps" *)
let calculate_Qp (addr : address) (ms : market_storage) : qps =
    let q : quantity = get_Q addr ms in
    let qps_m = get_Qps addr ms in 
    let prob : probabilities = get_probabilities addr ms in
    let multiqp : (nat * nat) -> nat = fun (i, proba : outcome * probability) -> 
        let prev_qps_m = match Map.find_opt i qps_m with
        | Some x -> x
        | None -> (failwith "foo" : nat) in
        prev_qps_m + (proba * q) in
    (Map.map multiqp prob : qps)

let update_q_total (addr : address) (ms : market_storage) : market_storage =
    let user_Q = get_Q addr ms in
    {ms with q_total = ms.q_total + user_Q}

(* this function is based on "Map Operations over Maps" *)
let update_qps_total (addr : address) (ms : market_storage) : qps = 
    let qps_tot = ms.qps_total in
    let qps_user = get_Qps addr ms in
    let new_qp_tot : (nat * nat) -> nat = fun (i, qp_val : outcome * qp) ->
        let prev_qps_tot  = match Map.find_opt i qps_tot with
        | Some x -> x
        | None -> (failwith "foo" : nat) in
        prev_qps_tot + qp_val in
    (Map.map new_qp_tot qps_user : qps)

(* this function is based on "Map Operations over Maps" *)
let calculate_clearing_price (ms : market_storage) : clearing_prices =
    let q_tot : nat = ms.q_total in
    let qps_tot : (nat, nat) map = ms.qps_total in
    let clearing_prs : (nat, nat) map = ms.clearing_prices in
    let new_clear_price : (nat * nat) -> nat = fun (i, qp_val_tot : outcome * qp) ->
        let prev_clearing_prs = match Map.find_opt i clearing_prs with
        | Some x -> x
        | None -> (failwith "foo" : nat) in
        prev_clearing_prs + (qp_val_tot / q_tot) in 
    (Map.map new_clear_price qps_tot : (nat, nat) map)

(* CALCULATE USER ALLOCATION *)
let get_allocations (addr : address) (ms : market_storage) : user_allocations =
    match Map.find_opt addr ms.market_allocations with
    | None -> (failwith "error_USR_ALLOCATION_DOESNT_EXIST" : user_allocations)
    | Some a -> a

type qp_mapper = ( nat * nat ) -> nat

let calculate_allocation (addr : address) (ms : market_storage) : (nat, nat) map =
    let qps_usr = get_Qps addr ms in
    let clr_prices : (nat, nat) map = ms.clearing_prices (* need to call calculate_clearing_price before this one*) in
    let alloc_usr = get_allocations addr ms in
    let new_alloc : qp_mapper = fun (i, qp_val : outcome * qp) -> (
        let spec_price = match (Map.find_opt i clr_prices) with
        | Some p -> p
        | None -> (failwith "foo" : nat) in
        let prev_usr_alloc = match (Map.find_opt i alloc_usr) with
        | Some a -> a
        | None -> (failwith "foo" : nat) in
        prev_usr_alloc + (qp_val / spec_price) ) in
    (Map.map new_alloc qps_usr : qps)

let sum_alloc_times_price (qps_m : qps) (cpr_map : clearing_prices) : nat =
    let agg = fun (i, qp_map : nat * (outcome * qp)) ->
        let spec_price = match (Map.find_opt qp_map.0 cpr_map) with
        | Some p -> p
        | None -> (failwith "foo" : nat) in
        i + (qp_map.1 * spec_price) in
    (Map.fold agg qps_m 0n)

let check_alloc_times_price (addr : address) (ms : market_storage) : bool =
    let qps_usr = get_Qps addr ms in
    let clr_prices = ms.clearing_prices in
    let usr_Q = get_Q addr ms in
    let alloc_x_price = sum_alloc_times_price qps_usr clr_prices in
    (alloc_x_price = usr_Q)

let test_xxx (ms : unit) : nat =
    1n

let main (num_outcomes : nat) : unit =
    unit
    
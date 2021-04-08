
type token_id = nat
type outcome = nat
type probability = nat
type qp = nat
type quantity = nat
type price = nat

let percent = 1000n


type probabilities = (outcome, probability) map
type qps = (outcome, qp) map
let clearing_prices = (outcome, price) map

let empty_ids_map : (outcome, token_id) map = Map.empty
let empty_bids_map : (address, probabilities) map = Map.empty
let empty_Q_map : (address, quantity) map = Map.empty
let empty_Qp_map : (address, qps) map = Map.empty
let empty_qps_map : qps = Map.empty
let empty_clearing_prices_map : clearing_prices = Map.empty

type market_storage = {
    num_outcomes : nat;
    q_total : nat;
    lqt_token : nat option;
    token_ids : (outcome, token_id) map;
    qps_total : qps;
    clearing_prices : clearing_prices;
    auction_bids : (address, probabilities) map; (* might be a big map later *)
    auction_Q : (address, quantity) map;
    auction_Qp : (address, qps) map;
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
    (* let check_p (prob : probabilities) : unit = *)
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

(* this function is absed on "Map Operations over Maps" *)
let calculate_Qp (addr : address) (ms : market_storage) : qps =
    let q : quantity = get_Q addr ms in
    let qps_m = get_Qps addr ms in 
    let prob : probabilities = get_probabilities addr ms in
    let multiqp : (nat * nat) -> nat = fun (i, proba : outcome * probability) -> 
        let prev_qps_m = match Map.find_opt i qps_m with
        | Some x -> x
        | None -> (failwith "foo" : nat) in
        x + (proba * q) in
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
        x + qp_val in
    (Map.map new_qp_tot qps_user : qps)

(* this function is based on "Map Operations over Maps" *)
let calculate_clearing_price (ms : market_storage) clearing_prices =
    let q_tot = ms.q_total in
    let qps_tot = ms.qps_total in
    let clearing_prs = ms.clearing_prices in
    let new_clear_price : (nat * nat) -> nat = fun (i, qp_val_tot : outcome * qp) ->
        let prev_clearing_prs = match Map.find_opt i clearing_prs with
        | Some x -> x
        | None -> (failwith "foo" : nat) in
        x + (qp_val_tot / q_tot) in 
    (Map.map new_clear_price qps_tot : qps)


(* CALCULATE USER ALLOCATION *)

let main (num_outcomes : nat) : unit =
    unit
    
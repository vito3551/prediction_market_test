type bids = (nat, nat) map
type auction_map = (string, bids) map
let fee : nat = 1n
 

let empty_qps_map : (nat, nat) map = Map.empty
let empty_auction_map : (string, bids) map = Map.empty
let empty_Q_map : (string, nat) map = Map.empty

type market_storage = {
    num_outcomes : nat;
    q_total : nat; (* total quantity bought by everyone *)
    min_qps_total : nat; (* maybe change the name to sum_of_min_qps_val *) (* This is the sum of each users min val of (quantity times probability) *)
    lqt_token : nat option; (* Liquidity token *)
    token_ids : (nat, nat) map; (* ids of each outcome token *)
    qps_total : bids; (* map with all quantity * probability *)
    clearing_prices : bids; (* the price of each outcome after the clearing stage *)
    auction_bids : (string, bids) map; (* might be a big map later *) (* map of mpa that contains each users probability for each outcome *)
    auction_Q : (string, nat) map; (* map that contains each users bought in quantity *)
    auction_Qp : (string, bids) map; (* map that contains each users quantity * probability *)
    market_allocations : (string, bids) map; (* each users allocation after the auction stage *)
    contribution_variables : (string, nat) map; (* map that contains each users min value of (quantity * probabilities) *)
    contribution_to_pool : (string, bids) map; (* map that contains each users contribution to the pool *) 
    uniswap_pool : bids; (* available quantity of each outcome *)
    market_invariant : nat; (* the product of the uniswap_pool *)
}

type return = (operation list) * market_storage

(* AUCTION STAGE *)
(* When a new user enters the auction his/hers purchased quantity and choosen probabilites
 will be stored in the market storage. Then this script will calculate (quantity times probabilities)
 as well as the min value of (quantity times probabilities). The min value will be stored in a map and
 it will also be added to the total of each users min value.  
 
 After the auction satge, the total of all users (quantity times probabilities) as well as the 
 total quantity purchased  by all users will be used to calculate the clearing prices.
 Once we have the clearing prices we can can calculate each users allocation in the market.  
 The sum of each users min value of (quantity times probabilities) will then be used to calculate
 the swap-pool and each users min value of (quantity times probabilities) will be used to calculate
 his/hers contributipon to the pool 
 
 Since we can't use floats due to decimal points error we will instead left shift by 48 bits
 whenever we need good decimal point precission. *)

(* get the map that contains a specific user's probabilities *)
let get_probabilities (addr : string) (ms : market_storage) : bids = 
    match Map.find_opt addr ms.auction_bids with
    | None -> (failwith "error_PROBS_DOESNT_EXIST" : bids) 
    | Some p -> p

(* check that a user's probabilities belongs to [0,1] *)
let check_p (prob : (nat, nat) map) : unit = 
    let predicate = fun (i, j : nat * nat) -> assert (j >= 0n && j <= 100n)
    in Map.iter predicate prob

(* sum all of the values in a map *)
let sum_prob (prob : (nat, nat) map) : nat = 
    let folded = fun (i, proba : nat * (nat * nat)) -> (i + proba.1)in 
    (Map.fold folded prob 0n)

(* check that a user's probabilities sum to one *)
let check_prob_sum_to_one (addr : string) (ms : market_storage) : bool =
    let prob = get_probabilities addr ms in
    let sum_probs : nat = sum_prob prob in
    (sum_probs = 100n)  

(* get the map that contains a specific user's quantity * probability *)
let get_Qps (addr : string) (ms : market_storage) : bids = 
    match Map.find_opt addr ms.auction_Qp with
    | None -> (failwith "error_QPS_DOESNT_EXIST" : bids) (* empty_qps_map *)
    | Some qp -> qp

(* get a specific user's purchased quantity *)
let get_Q (addr : string) (ms : market_storage) : nat = 
    match Map.find_opt addr ms.auction_Q with
    | None -> (failwith "error_Q_DOESNT_EXIST" : nat)
    | Some q -> q

(* returns a map that contains a specific user's quantity * probability *)
let calculate_Qp (addr : string) (ms : market_storage) : bids =
    let q : nat = get_Q addr ms in
    let prob = get_probabilities addr ms in
    let multiqp : (nat * nat) -> nat = fun (i, proba : nat * nat) -> 
        (Bitwise.shift_left (proba * q) 48n) in
    (Map.map multiqp prob : bids)

(* adds a specific user's purchased quantity to the total quantity *)
let update_q_total (addr : string) (ms : market_storage) : market_storage =
    let user_Q = get_Q addr ms in
    {ms with q_total = ms.q_total + user_Q}

(* adds each user's quantity * probability for each outcome to the market total *)
let update_qps_total (addr : string) (ms : market_storage) : bids = 
    let qps_tot = ms.qps_total in
    let qps_user = get_Qps addr ms in
    let new_qp_tot : (nat * nat) -> nat = fun (i, qp_val : nat * nat) ->
        let prev_qps_tot  = match Map.find_opt i qps_tot with
        | Some x -> x
        | None -> 0n in
        prev_qps_tot + qp_val in
    (Map.map new_qp_tot qps_user : bids)

(* calculate the price of each outcome after the auction stage *)
let calculate_clearing_price (ms : market_storage) : bids =
    let q_tot : nat = ms.q_total in
    let qps_tot : (nat, nat) map = ms.qps_total in
    let new_clear_price : (nat * nat) -> nat = fun (i, qp_val_tot : nat * nat) ->
        (qp_val_tot / q_tot) in 
    (Map.map new_clear_price qps_tot : (nat, nat) map)

(* ALLOCATIONS after the auction stage *)

(* get the map that contains a specific user's allocation for each outcome *)
let get_allocations (addr : string) (ms : market_storage) : bids =
    match Map.find_opt addr ms.market_allocations with
    | None -> empty_qps_map (* (failwith "error_USR_ALLOCATION_DOESNT_EXIST" : bids) *)
    | Some a -> a

(* calculate the allocation for each outcome that a user ends up with *)
let calculate_allocation (addr : string) (ms : market_storage) : (nat, nat) map =
    let qps_usr = get_Qps addr ms in
    let clr_prices : (nat, nat) map = ms.clearing_prices (* need to call calculate_clearing_price before this one*) in
    let new_alloc : (nat * nat) -> nat = fun (i, qp_val : nat * nat) -> (
        let spec_price = match (Map.find_opt i clr_prices) with
        | Some p -> p
        | None -> 0n in
        ((Bitwise.shift_left qp_val 48n) / spec_price) ) in (* To use int-math just do: ((Bitwise.shift_left qp_val 48n) / spec_price) *) (* Without int-math do: ((qp_val ) / spec_price) *)
    (Map.map new_alloc qps_usr : bids)

(* sum of allocation * clearing price for a user *)
let sum_alloc_times_price (alloc_m : bids) (cpr_map : bids) : nat =
    let agg = fun (i, q_map : nat * (nat * nat)) ->
        let spec_price = match (Map.find_opt q_map.0 cpr_map) with
        | Some p -> p
        | None -> (failwith "foo" : nat) in
        i + (q_map.1 * spec_price) in
    (Map.fold agg alloc_m 0n)

(* check that the sum of allocation * clearing price equals to the user's purchased quantity *)
let check_alloc_times_price (addr : string) (ms : market_storage) : bool =
    let alloc_usr = get_allocations addr ms in
    let clr_prices = ms.clearing_prices in
    let usr_Q = get_Q addr ms in
    let alloc_x_price = sum_alloc_times_price alloc_usr clr_prices in
    let a_x_p = (Bitwise.shift_right alloc_x_price 96n) / 100n in (* (Bitwise.shift_right alloc_x_price 48n) *)
    abs (a_x_p - usr_Q) <= 1n

(* UNISWAP *)

(* takes two values and returns the smallest one *)
let min (x : nat) (y : nat) = if x < y then x else y

(* returns the smallest value from a map *) 
let get_min_val (m : (nat, nat) map) : nat = 
    let folded = fun (i, j : nat * (nat * nat)) ->
        let mv = min i j.1 in
        (i * 0n) + mv in
    (Map.fold folded m (Bitwise.shift_left 10000000n 48n))

(* for a specific user get the min value of quantity * probability and add it together with the other users min value *)
let total_min_qps (addr : string) (ms : market_storage) : market_storage = 
    let usr_Qp = get_Qps addr ms in
    let usr_min_Qp = get_min_val usr_Qp in
    {ms with min_qps_total = ms.min_qps_total + usr_min_Qp}

(* calculate the avilable quantity of each outcome that will be in the uniswap pool *)
let calculate_uniswap_market (ms : market_storage) : bids =
    let min_Qp_tot = ms.min_qps_total in
    let clr_price = ms.clearing_prices in
    let uniswap_market = fun (i, price : nat * nat) ->
        ((Bitwise.shift_left min_Qp_tot 48n) / price) in //(Bitwise.shift_left (min_Qp_tot / price) 48n)
    (Map.map uniswap_market clr_price)

(* calculate the contribution factor to the swap-pool for a user *) 
let calculate_usr_contribution_variable (addr : string) (ms : market_storage) : (string, nat) map =
    let usr_Qp = get_Qps addr ms in
    let usr_min_Qp = get_min_val usr_Qp in
    let contrib_map = ms.contribution_variables in
    let contrib_map = Map.update addr (Some usr_min_Qp) contrib_map in
    contrib_map
    
(* Calculate the contribution of each outcome to the pool from a user *)
let calculate_usr_pool_contribution (addr : string) (ms : market_storage) : bids =
    let contribution_variables_map = ms.contribution_variables in
    let usr_contribution_variable = match Map.find_opt addr contribution_variables_map with 
        | None -> (failwith "error_USR_CONTRIB_VAL_DOESNT_EXIST" : nat)
        | Some v -> v in
    let clearing_prices = ms.clearing_prices in
    let usr_contribution : (nat * nat) -> nat = fun (i, price : nat * nat) ->
        ((Bitwise.shift_left usr_contribution_variable 48n) / price) in
    (Map.map usr_contribution clearing_prices)


(* update usr allocation after contributing to the swap-pool *)
//let subtract_contribution (addr : string) (ms : market_storage) : 

(* put the uniswap_pool in the market_storage *)
let add_uniswap_mrk_to_storage (ms : market_storage) : market_storage =
    let uniswap_mrk = calculate_uniswap_market ms in
    {ms with uniswap_pool = uniswap_mrk}

(* the product of each quantity from the the uniswap_pool *)
let calculate_market_invariant (ms : market_storage) : nat =
    let uniswap_mrk = ms.uniswap_pool in
    let prod = fun (i, key_val : nat * (nat * nat)) ->
        (i * key_val.1) in
    (Map.fold prod uniswap_mrk 1n)

(* swap a choosen nr of of one outocome token for some nr of another outcome token*)
let swap_token (ms : market_storage) (token_sell : nat) (token_sell_val : nat) (token_buy : nat) : market_storage = 
    (* let invariant = ms.market_invariant in *)
    let old_pool = ms.uniswap_pool in
    let token_buy_pool = match (Map.find_opt token_buy old_pool) with 
        | None -> (failwith "error_Val_DOSNT_EXIST_in_UNISWAP_POOL" : nat)
        | Some v -> v in 
    let token_sell_pool = match (Map.find_opt token_sell ms.uniswap_pool) with
        | None -> (failwith "error_Val_DOSNT_EXIST_in_UNISWAP_POOL" : nat)
        | Some v -> v in
    let token_sell_new = token_sell_pool + (Bitwise.shift_left token_sell_val 48n) in
    let token_buy_new = (token_sell_pool * token_buy_pool) / token_sell_new in
    let new_pool = Map.update token_sell (Some token_sell_new) old_pool in
    let new_pool = Map.update token_buy (Some token_buy_new) new_pool in
    {ms with uniswap_pool = new_pool}

let update_usr_alloc_after_swap (ms : market_storage) (addr : string) (token_sell : nat) (token_sell_new_val : nat) (token_buy : nat) (token_buy_new_val : nat) : bids =
    let usr_alloc = get_allocations addr ms in
    let usr_alloc = Map.update token_sell (Some token_sell_new_val) usr_alloc in
    let usr_alloc = Map.update token_buy (Some token_buy_new_val) usr_alloc in
    usr_alloc

(* add a choosen value to all of the values inside a map *)
let add_to_val_in_map (m : bids) (val_to_add : nat) : bids = 
    let add = fun (i, j : nat * nat) ->
        (j + val_to_add) in
    (Map.map add m)

(* subtract a choosen value from all of the values in map *)
let sub_from_val_in_map (m : bids) (val_to_sub : nat) : bids =
    let sub = fun (i, j : nat * nat) ->
        abs (j - val_to_sub) in
    (Map.map sub m)

(* the product of all values in a map *)
let get_product_of_val_from_map (m : bids) : nat =
    let prod = fun (i, key_val : nat * (nat * nat)) ->
        i * key_val.1 in
    (Map.fold prod m 1n)

let get_val_from_map (m : bids) (key : nat) : nat =
    match Map.find_opt key m with
    | None -> (failwith "error_key_DOESNT_EXIST" : nat)
    | Some v -> v

let update_usr_alloc_after_Kswap (addr : string) (ms : market_storage) (token_sell_val : nat) (token_buy : nat) (bought_val : nat) : market_storage =
    let old_usr_alloc = get_allocations addr ms in
    let old_usr_val = get_val_from_map old_usr_alloc token_buy in
    let temp_usr_alloc = Map.remove token_buy old_usr_alloc in
    let temp_usr_alloc = sub_from_val_in_map temp_usr_alloc (Bitwise.shift_left token_sell_val 48n) in
    let new_usr_alloc = Map.update token_buy (Some (bought_val + old_usr_val)) temp_usr_alloc in
    let new_alloc = Map.update addr (Some new_usr_alloc) ms.market_allocations in
    {ms with market_allocations = new_alloc}

let swap_Ktokens (addr : string) (ms : market_storage) (token_sell_val : nat) (token_buy : nat) : market_storage =
    let invariant = ms.market_invariant in
    let old_pool : (nat, nat) map = ms.uniswap_pool in
    let old_val = get_val_from_map old_pool token_buy in 
    let temp_pool : (nat, nat) map = Map.remove token_buy old_pool in
    let temp_pool : (nat, nat) map = add_to_val_in_map temp_pool (Bitwise.shift_left token_sell_val 48n) in
    let temp_invariant = get_product_of_val_from_map temp_pool in
    let new_buy_val = invariant / temp_invariant in
    let usr_bought : nat = abs (old_val - new_buy_val) in
    //let ms = update_usr_alloc_after_Kswap addr ms token_sell_val token_buy usr_bought in 
    let new_pool = Map.update token_buy (Some new_buy_val) temp_pool in
    {ms with uniswap_pool = new_pool}







            

let pb0 : bids = Map.literal [(0n,10n); (1n,75n); (2n, 15n)]
let pb1 : bids = Map.literal [(0n, 15n); (1n, 70n); (2n, 15n)]
let pb2 : bids = Map.literal [(0n, 5n); (1n, 65n); (2n, 30n)]
let me : string = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
let orka : bids = Map.remove 0n pb2

type people = (string * string * string)
let users : people = ("u1", "u2", "u3")
let u1 : string = "u1"

let my_ms : market_storage = {
    num_outcomes = 3n;
    q_total = 0n;
    min_qps_total = 0n;
    lqt_token = (None : nat option);
    token_ids = empty_qps_map;
    qps_total = empty_qps_map;
    clearing_prices = empty_qps_map;
    auction_bids = Map.literal [(users.0, pb0); (users.1, pb1); (users.2, pb2)];
    auction_Q = Map.literal [(users.0, 100n); (users.1, 200n); (users.2, 250n)];  (* empty_Q_map *) 
    auction_Qp = empty_auction_map;
    market_allocations = empty_auction_map;
    contribution_variables = empty_Q_map;
    contribution_to_pool = empty_auction_map;
    uniswap_pool = empty_qps_map; 
    market_invariant = 0n;
}

let main (addr, ms : people * market_storage) : return =
    (* user 0 *)
    (* Add usr0 Q-val to Q-tot in the market_storage*)
    let new_ms = update_q_total addr.0 ms in
    (* Check if usr0 probs sum to 100*)
    let prob_usr0 = match Map.find_opt addr.0 new_ms.auction_bids with
    | None -> (failwith "error_PROBS_DOESNT_EXIST" : bids) 
    | Some p -> p in
    let sum_prob_usr0 = sum_prob prob_usr0 in
    if sum_prob_usr0 <> 100n then
        (failwith "error_PROBS_DOESNT_SUMto100" : return)
    else
    (* Calculate Qp for usr0 and add it to the market storage*) 
    let usr0_qp : (nat, nat) map = calculate_Qp addr.0 new_ms in 
    let a0_Qp : (string, bids) map = Map.update (addr.0 : string) (Some usr0_qp) new_ms.auction_Qp in
    let n_ms = {new_ms with auction_Qp = a0_Qp} in
    (* Ad usr0 Qp to Qp_total *)
    let qps_tot0 = update_qps_total addr.0 n_ms in
    let n1_ms = {n_ms with qps_total = qps_tot0} in
    (* Get min Qp val for usr0 and ad it to total_min_qps in the market storage *)
    let n1_ms = total_min_qps addr.0 n1_ms in

    (* user 1 *)
    (* Add usr1 Q-val to Q-tot in the market_storage*)
    let n2_ms = update_q_total addr.1 n1_ms in
    (* Check if usr0 probs sum to 100*)
    let prob_usr1 = match Map.find_opt addr.1 n2_ms.auction_bids with
    | None -> (failwith "error_PROBS_DOESNT_EXIST" : bids) 
    | Some p -> p in
    let sum_prob_usr1 = sum_prob prob_usr1 in
    if sum_prob_usr1 <> 100n then
        (failwith "error_PROBS_DOESNT_SUMto100" : return)
    else 
    (* Calculate Qp for usr1 and ad it to the market storage*) 
    let usr1_qp : bids = calculate_Qp addr.1 n2_ms in
    let a1_Qp = Map.update (addr.1 : string) (Some usr1_qp) n2_ms.auction_Qp in
    let n3_ms = {n2_ms with auction_Qp = a1_Qp} in
    (* Ad usr1 Qp to Qp_total *)
    let qps_tot1 = update_qps_total addr.1 n3_ms in 
    let n4_ms = {n3_ms with qps_total = qps_tot1} in
    (* Get min Qp val for usr0 and ad it to total_min_qps in the market storage *)
    let n4_ms = total_min_qps addr.1 n4_ms in

    (* user 2 *)
    (* Add usr2 Q-val to Q-tot in the market_storage*)

    (* Calculate clearing price *)
    let cp = calculate_clearing_price n4_ms in
    let n5_ms = {n4_ms with clearing_prices = cp} in

    (* Calculate contribution variable for usr 0 and put it in the storage *)
    let usr0_contrib_var = calculate_usr_contribution_variable addr.0 n5_ms in
    let n5_ms = {n5_ms with contribution_variables = usr0_contrib_var} in
    (* Calculate the contribution of each outcome to the pool for usr 0 *)
    let u0_contribution = calculate_usr_pool_contribution addr.0 n5_ms in
    let c0 = Map.update addr.0 (Some u0_contribution) n5_ms.contribution_to_pool in
    let n5_ms = {n5_ms with contribution_to_pool = c0} in

    (* Calculate contribution variable for usr 1 and put it in the storage *)
    let usr1_contrib_var = calculate_usr_contribution_variable addr.1 n5_ms in
    let n5_ms = {n5_ms with contribution_variables = usr1_contrib_var} in
    (* Calculate the contribution of each outcome to the pool for usr 1 *)
    let u1_contribution = calculate_usr_pool_contribution addr.1 n5_ms in
    let c1 = Map.update addr.1 (Some u1_contribution) n5_ms.contribution_to_pool in
    let n5_ms = {n5_ms with contribution_to_pool = c1} in

    (* Calculate allocation for user 0 *)
    let u0_alloc = calculate_allocation addr.0 n5_ms in
    let a0 = Map.update (addr.0 : string) (Some u0_alloc) n5_ms.market_allocations in
    let n6_ms = {n5_ms with market_allocations = a0} in
    (* CHECK that ALLOC TIMES PRICE for usr0 is equal to Q*)
    let usr0_a_times_p = check_alloc_times_price addr.0 n6_ms in
    if usr0_a_times_p <> true  then
        (failwith "error_ALLOC_TIMES_PRICE_ISNT_Q" : return)
    else
    (* Calculate allocation for user 1 *)
    let u1_alloc = calculate_allocation addr.1 n6_ms in
    let a1 = Map.update (addr.1 : string) (Some u1_alloc) n6_ms.market_allocations in
    let n7_ms = {n6_ms with market_allocations = a1} in
    (* CHECK that ALLOC TIMES PRICE for usr1 is equal to Q*)
    let usr1_a_times_p = check_alloc_times_price addr.1 n7_ms in
    if usr1_a_times_p <> true then
        (failwith "error_ALLOC_TIMES_PRICE_ISNT_Q" : return)
    else
    (* n7_ms *)

    (* Calculate uniswap pool *)
    let n7_ms = add_uniswap_mrk_to_storage n7_ms in
    
    (* Calculate the invariant *)
    let invariant = calculate_market_invariant n7_ms in
    let n7_ms = {n7_ms with market_invariant = invariant} in
    
    (* Swap tokens 
    let n7_ms = swap_token n7_ms 1n 2n 2n in
    *)

    (* Swap K tokens *) // for usr0 sell 2 of o1 and o2 and buy 03
    let n7_ms = swap_Ktokens addr.0 n7_ms 2n 2n in

    (* get new invariant *)
    let invariant = calculate_market_invariant n7_ms in
    let n7_ms = {n7_ms with market_invariant = invariant} in
    //n7_ms
    (([] : operation list), n7_ms)

(* let main (addr, ms : string * market_storage) : market_storage =
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
    n3_ms *)

    
    